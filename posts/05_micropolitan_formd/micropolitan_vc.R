

# Set up -----------------------------------------------------------------------
library(dplyr)
library(coriverse)
library(cori.charts)
library(ggplot2)
library(sysfonts)
library(showtext)

sysfonts::font_add_google("Lato")
font_add(
  "TT Hoves",
  regular = "TypeType - TT Hoves Regular.ttf",
  bold = "TypeType - TT Hoves Bold.ttf",
  italic = "TypeType - TT Hoves Italic.ttf",
  bolditalic = "TypeType - TT Hoves Bold Italic.ttf"
)

showtext_auto()
showtext_opts(dpi = 300)

# load counties ----------------------------------------------------------------

## 2023 tigerline counties and 2023 CBSA names/rural def
counties <- tigris::counties(year = 2023) %>%
  sf::st_drop_geometry() %>%
  filter(STATEFP <= 56) %>%
  left_join(cori.utils::state_id_crosswalk %>% select(state_name, state_fips), by = c('STATEFP' = 'state_fips')) %>%
  mutate(
    name_co = paste0(NAMELSAD, ', ', state_name)
  ) %>%
  select(
    geoid_co = GEOID,
    name_co,
    geoid_cbsa = CBSAFP
  ) %>%
  left_join(
    tigris::core_based_statistical_areas(year = 2023) %>%
      sf::st_drop_geometry() %>%
      select(
        geoid_cbsa = CBSAFP,
        name_cbsa = NAMELSAD,
        rural_def_23 = LSAD
      ),
    by = 'geoid_cbsa'
  )

# load populations -------------------------------------------------------------

## load County Housing and Population Estimates for 2021-2023 ------------------
link <- 'https://www2.census.gov/programs-surveys/popest/tables/2020-2023/counties/totals/co-est2023-pop.xlsx'
fp <- file.path(tempdir(), 'co-est2023-pop.xlsx')
download.file(link, fp, mode = 'wb')
pop_file <- readxl::read_excel(fp, skip = 2)

## join CBSA names to county pop estimates -------------------------------------
pop_clean <- pop_file %>%
  select(
    name_co = `Geographic Area`,
    pop_2021 = `...4`,
    pop_2022 = `...5`,
    pop_2023 = `...6`
  ) %>%
  filter(
    !is.na(name_co),
    name_co != 'United States',
    !is.na(pop_2021)
  ) %>%
  mutate(
    name_co = stringr::str_replace(name_co, '^.', '')
  ) %>%
  left_join(counties, by = 'name_co')

## calc avg population in each CBSA over the 3 years ---------------------------
cbsa_pop <- pop_clean %>%
  filter(rural_def_23 == 'M2') %>%
  tidyr::pivot_longer(cols = c(pop_2021, pop_2022, pop_2023),
                      names_to = 'year',
                      values_to = 'pop') %>%
  mutate(
    year = as.numeric(stringr::str_replace(year, '^pop_', ''))
  ) %>%
  group_by(geoid_cbsa, name_cbsa) %>%
  summarise(
    pop_3yr_avg_21_23 = sum(pop, na.rm = TRUE) / 3 # divide by 3 years
  ) %>%
  ungroup()


# load VC data -----------------------------------------------------------------
con <- connect_to_db('venture_capital')
form_d <- read_db(con, 'form_d_businesses')
DBI::dbDisconnect(con)

## clean For D data and join CBSA names ----------------------------------------
form_d_clean <- form_d %>%
  group_by(year, cik, entity_name, sale_date, offering_amount) %>%
  slice_max(accession_number) %>% # take most recent of each amendment within a year
  ungroup() %>%
  filter(
    year %in% c(2021, 2022, 2023),
    !grepl('Oil and Gas', industries), # remove industries that are unlikely to represent traditional VC-raising businesses
    !grepl('Coal Mining', industries),
    !grepl('Other Banking and Financial Services', industries),
    !grepl('Commercial Banking', industries),
    !grepl('Investment Banking', industries),
    !grepl('Construction', industries),
    !grepl('Commercial', industries),
    !grepl('Residential', industries),
    !grepl('REITS and Finance', industries),
    !grepl('Other Real Estate', industries)
  ) %>%
  left_join(counties %>% select(geoid_co, geoid_cbsa, name_cbsa, rural_def_23), by = c('geoid_co')) %>%
  filter(amount_sold > 0) # remove entries where the entity didn't sell any securities

## calc avg VC raised in each CBSA over the 3 years ----------------------------
cbsa_vc <- form_d_clean %>%
  filter(rural_def_23 == 'M2') %>%
  group_by(geoid_cbsa, name_cbsa) %>%
  summarise(
    vc_3yr_avg_21_23 = sum(amount_sold, na.rm = TRUE) / 3
  ) %>%
  ungroup()


# join datasets and identify top 10 --------------------------------------------

## calc avg VC raised per capita by each micro from 2021-23 --------------------
vc_per_cap <- cbsa_vc %>%
  left_join(cbsa_pop, by = c('geoid_cbsa', 'name_cbsa')) %>%
  mutate(
    vc_per_capita_3yr_avg_21_23 = vc_3yr_avg_21_23 / pop_3yr_avg_21_23
  ) %>%
  filter(!name_cbsa %in% c('Batesville, AR Micro Area', 'Rutland, VT Micro Area')) %>% # remove micros where most investments are NOT traditional VC-raising companies
  slice_max(vc_per_capita_3yr_avg_21_23, n = 10) # select top 10

## calc avg VC raised by each micro from 2021-23 -------------------------------
vc_total <- cbsa_vc %>%
  filter(!name_cbsa %in% c('Batesville, AR Micro Area', 'Rutland, VT Micro Area')) %>%  # remove micros where most investments are NOT traditional VC-raising companies
  slice_max(vc_3yr_avg_21_23, n = 10) # select top 10


# Charts -----------------------------------------------------------------------

## VC per Capita chart ---------------------------------------------------------

fig <- vc_per_cap %>%
  ggplot2::ggplot(
    aes(
      vc_per_capita_3yr_avg_21_23,
      reorder(name_cbsa, vc_per_capita_3yr_avg_21_23)
    )
  ) +
  ggplot2::geom_col(fill = cori_colors["Squash"]) +
  geom_text(
    aes(
      label = scales::dollar(
        vc_per_capita_3yr_avg_21_23,
        accuracy = 1,
        big.mark = ","
      )
    ),
    fontface = "bold",
    hjust = -.2,
    family = "Lato"
  ) +
  ggplot2::scale_x_continuous(
    labels = NULL,
    expand = expansion(mult = c(0, .25))
  ) +
  theme_cori_horizontal_bars() +
  ggplot2::theme(
    plot.title = element_text(family = "TT Hoves"),
    plot.subtitle = element_text(family = "TT Hoves"),
    panel.grid.major.x = element_blank()
  ) +
  ggplot2::labs(
    title = "Micropolitan areas with the most venture capital funding per capita",
    subtitle = "Three-year averages (2021 - 2023)",
    y = NULL,
    x = NULL,
    caption = 'Source: County Population Estimates (2021-2023), SEC Form D (2021-2023), Office of Management and Budget (2023)
Notes: Venture capital totals exclude Form D entries where the industry is flagged as coal mining, oil and gas, banking, construction, or real estate.'
  )

fig

cori.charts::save_plot(fig = fig,
                       chart_width = 9.5,
                       export_path = './posts/micropolitan_formd/images/top10_vc_per_cap_micropolitans.png')


## Total VC raised Chart -------------------------------------------------------

fig2 <- vc_total %>%
  ggplot2::ggplot(
    aes(
      vc_3yr_avg_21_23,
      reorder(name_cbsa, vc_3yr_avg_21_23)
    )
  ) +
  ggplot2::geom_col(fill = cori_colors["Squash"]) +
  geom_text(
    aes(
      label = scales::dollar(
        vc_3yr_avg_21_23,
        accuracy = 1,
        scale = .000001,
        suffix = "M"
      )
    ),
    fontface = "bold",
    hjust = -.2,
    family = "Lato"
  ) +
  ggplot2::scale_x_continuous(
    labels = NULL,
    expand = expansion(mult = c(0, .25))
  ) +
  theme_cori_horizontal_bars() +
  ggplot2::theme(
    plot.title = element_text(family = "TT Hoves"),
    plot.subtitle = element_text(family = "TT Hoves"),
    panel.grid.major.x = element_blank()
  ) +
  ggplot2::labs(
    title = "Micropolitan areas with the most venture capital funding",
    subtitle = "Three-year averages (2021 - 2023)",
    y = NULL,
    x = NULL,
    caption = "Source: County Population Estimates (2021-2023), SEC Form D (2021-2023), Office of Management and Budget (2023)
Notes: Venture capital totals exclude Form D entries where the industry is flagged as coal mining, oil and gas, banking, construction, or real estate."
  )

fig2


cori.charts::save_plot(fig = fig2,
                       chart_width = 9.5,
                       export_path = './posts/micropolitan_formd/images/top10_total_vc_micropolitans.png')



# stats for blog post ----------------------------------------------------------

## Share of all Micro VC raised by the by the Top 10 Micros over the 3 years ----
pct_micro_vc <- form_d_clean %>%
  filter(rural_def_23 == 'M2') %>%
  mutate(
    top10_flag = ifelse(name_cbsa %in% vc_total$name_cbsa, 1, 0), # flag the 10 micros that raised the most VC on average
    micro_total_avg_21_23 = sum(amount_sold, na.rm = TRUE) / 3 # calc 3 year avg for all micropolitans
    ) %>%
  group_by(top10_flag) %>%
  mutate(top10_total_avg_21_23 = sum(amount_sold, na.rm = TRUE) / 3) %>% # calc 3 year avg for top 10 micropolitans as a whole
  ungroup() %>%
  mutate(
    pct_top10_21_23 = top10_total_avg_21_23 / micro_total_avg_21_23 # calc share of micro VC that went to the top 10 micros
  ) %>%
  filter(top10_flag == 1) %>%
  select(
    micro_total_avg_21_23,
    top10_total_avg_21_23,
    pct_top10_21_23
  ) %>%
  distinct()


## Populations of the Top 10 VC per Cap Micros ---------------------------------
micros_by_pop <- cbsa_pop %>%
  filter(name_cbsa %in% vc_per_cap$name_cbsa)

## Number of Micros that raised VC in the 3 years ------------------------------
num_vc_micros <- form_d_clean %>%
  filter(rural_def_23 == 'M2') %>%
  summarise(num_micros_w_vc_21_23 = length(unique(name_cbsa)))


## Number of unique businesses in the Top 10 VC Per Capita Micros --------------
entries_for_top_vc_micros <- form_d_clean %>%
  filter(name_cbsa %in% vc_total$name_cbsa)

unique_entities_for_top_vc_micros <- entries_for_top_vc_micros %>%
  select(
    year,
    name_cbsa,
    entity_name,
    amount_sold
  ) %>%
  distinct() %>%
  group_by(name_cbsa) %>%
  summarise(
    unique_biz_21_23 = length(unique(entity_name)),
    min = min(amount_sold),
    max = max(amount_sold)
  ) %>%
  ungroup()


## Number of unique businesses in the Top 10 Total VC Micros -------------------
entries_for_top_vc_per_cap_micros <- form_d_clean %>%
  filter(name_cbsa %in% vc_per_cap$name_cbsa)

unique_entities_for_top_vc_per_cap_micros <- entries_for_top_vc_per_cap_micros %>%
  select(
    year,
    name_cbsa,
    entity_name,
    amount_sold
  ) %>%
  distinct() %>%
  group_by(name_cbsa) %>%
  summarise(
    unique_biz_21_23 = length(unique(entity_name)),
    min = min(amount_sold),
    max = max(amount_sold)
  ) %>%
  ungroup()







# OLD ANALYSIS -----------------------------------------------------------------

# googlesheets4::sheet_write(data = vc_per_cap,
#                            ss = 'https://docs.google.com/spreadsheets/d/1NVc9gY9ZnDKqfzSjujScrcNvIc_sqwB4AAJNefSkwFQ/edit#gid=0',
#                            sheet = 'VC 2019-21')


# vc_per_cap_wo_oil <- form_d_clean %>%
#   left_join(cbsa_pop, by = c('year', 'geoid_co')) %>%
#   filter(!grepl('Oil and Gas', industries)) %>%
#   group_by(year, name_cbsa) %>%
#   summarise(
#     amount_sold = sum(amount_sold, na.rm = TRUE),
#     cbsa_pop = cbsa_pop
#   ) %>%
#   ungroup() %>%
#   distinct() %>%
#   group_by(micropolitan = name_cbsa) %>%
#   summarise(
#     avg_vc = sum(amount_sold, na.rm = TRUE) / 3,
#     avg_pop = sum(cbsa_pop, na.rm = TRUE) / 3,
#     vc_per_capita = sum(amount_sold, na.rm = TRUE) / sum(cbsa_pop, na.rm = TRUE)
#   ) %>%
#   arrange(desc(vc_per_capita))


# googlesheets4::sheet_write(data = vc_per_cap_wo_oil,
#                            ss = 'https://docs.google.com/spreadsheets/d/1NVc9gY9ZnDKqfzSjujScrcNvIc_sqwB4AAJNefSkwFQ/edit#gid=0',
#                            sheet = 'VC 2019-21 (w/o Oil & Gas)')


# vc_per_cap_wo_oil_realestate_banking <- form_d_clean %>%
#   left_join(cbsa_pop, by = c('year', 'geoid_co')) %>%
#   filter(
#     !grepl('Oil and Gas', industries),
#     !grepl('Other Banking and Financial Services', industries),
#     !grepl('Commercial Banking', industries),
#     !grepl('Investment Banking', industries),
#     !grepl('Commercial', industries),
#     !grepl('Residential', industries),
#     !grepl('REITS and Finance', industries),
#     !grepl('Other Real Estate', industries)
#     ) %>%
#   group_by(year, name_cbsa) %>%
#   summarise(
#     amount_sold = sum(amount_sold, na.rm = TRUE),
#     cbsa_pop = cbsa_pop
#   ) %>%
#   ungroup() %>%
#   distinct() %>%
#   group_by(micropolitan = name_cbsa) %>%
#   summarise(
#     avg_vc = sum(amount_sold, na.rm = TRUE) / 3,
#     avg_pop = sum(cbsa_pop, na.rm = TRUE) / 3,
#     vc_per_capita = sum(amount_sold, na.rm = TRUE) / sum(cbsa_pop, na.rm = TRUE)
#   ) %>%
#   arrange(desc(vc_per_capita))


# googlesheets4::sheet_write(data = vc_per_cap_wo_oil_realestate_banking,
#                            ss = 'https://docs.google.com/spreadsheets/d/1NVc9gY9ZnDKqfzSjujScrcNvIc_sqwB4AAJNefSkwFQ/edit#gid=0',
#                            sheet = 'VC 2019-21 (w/o Oil & Gas, Banking, or Real Estate)')


# full_sheet <- form_d_clean %>%
#   left_join(cbsa_pop, by = c('year', 'geoid_co')) %>%
#   filter(amount_sold != 0) %>%
#   select(
#     year,
#     quarter,
#     filing_id = accession_number,
#     # county = name_co,
#     # state = state_abbr,
#     micropolitan = name_cbsa,
#     entity_name,
#     industries,
#     vc_amount = amount_sold
#   ) %>%
#   distinct() %>%
#   arrange(
#     year,
#     quarter,
#     micropolitan
#   )


# googlesheets4::sheet_write(data = full_sheet,
#                            ss = 'https://docs.google.com/spreadsheets/d/1NVc9gY9ZnDKqfzSjujScrcNvIc_sqwB4AAJNefSkwFQ/edit#gid=0',
#                            sheet = 'Full micropolitan data')


