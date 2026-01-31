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
counties <- tigris::counties(year = 2023, state = 'CO') %>% 
  sf::st_drop_geometry() %>% 
  # filter(STATEFP <= 56) %>% 
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
# link <- 'https://www2.census.gov/programs-surveys/popest/tables/2020-2023/counties/totals/co-est2023-pop.xlsx'
# fp <- file.path(tempdir(), 'co-est2023-pop.xlsx')
# download.file(link, fp, mode = 'wb')
# pop_file <- readxl::read_excel(fp, skip = 2)
# 
# ## join CBSA names to county pop estimates -------------------------------------
# pop_clean <- pop_file %>% 
#   select(
#     name_co = `Geographic Area`,
#     pop_2021 = `...4`,
#     pop_2022 = `...5`,
#     pop_2023 = `...6`
#   ) %>% 
#   filter(
#     !is.na(name_co),
#     name_co != 'United States',
#     !is.na(pop_2021)
#   ) %>% 
#   mutate(
#     name_co = stringr::str_replace(name_co, '^.', '')
#   ) %>% 
#   left_join(counties, by = 'name_co')
# 
# ## calc avg population in each CBSA over the 3 years ---------------------------
# cbsa_pop <- pop_clean %>% 
#   filter(rural_def_23 == 'M2') %>% 
#   tidyr::pivot_longer(cols = c(pop_2021, pop_2022, pop_2023),
#                       names_to = 'year',
#                       values_to = 'pop') %>% 
#   mutate(
#     year = as.numeric(stringr::str_replace(year, '^pop_', ''))
#   ) %>% 
#   group_by(geoid_cbsa, name_cbsa) %>% 
#   summarise(
#     pop_3yr_avg_21_23 = sum(pop, na.rm = TRUE) / 3 # divide by 3 years
#   ) %>% 
#   ungroup()


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
    year %in% c(2014:2023),
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
  filter(amount_sold > 0) %>% # remove entries where the entity didn't sell any securities
  filter(state_abbr == 'CO')

# Rifle ----

rifle <- form_d_clean %>% 
  filter(grepl('Rifle', name_cbsa)) %>% 
  select(
    `Filing year` = year,
    `Filing quarter` = quarter,
    `Metro/Micropolitan name` = name_cbsa,
    `County name` = name_co,
    `State` = state_abbr,
    `Filing number` = accession_number,
    `Business name` = entity_name,
    `Founded year` = founded_year,
    `Industries` = industries,
    `Fundraising start date` = sale_date,
    `Amount raised (as of filing date)` = amount_sold,
    street1,
    street2,
    city,
    zipcode,
    phone_number,
    related_persons
    # everything()
  ) %>% 
  arrange(`Filing year`, `Filing quarter`, `Metro/Micropolitan name`, `County name`)


colorado <- form_d_clean %>% 
  filter(
    year == 2023,
    !is.na(name_co)
    ) %>% 
  select(
    `Filing year` = year,
    `Filing quarter` = quarter,
    `Metro/Micropolitan name` = name_cbsa,
    `County name` = name_co,
    `State` = state_abbr,
    `Filing number` = accession_number,
    `Business name` = entity_name,
    `Founded year` = founded_year,
    `Industries` = industries,
    `Fundraising start date` = sale_date,
    `Amount raised (as of filing date)` = amount_sold,
    street1,
    street2,
    city,
    zipcode,
    phone_number,
    related_persons
    # everything()
  ) %>% 
  arrange(`Filing year`, `Filing quarter`, `Metro/Micropolitan name`, `County name`)

readr::write_csv(rifle, '~/Downloads/rifle_micro_area_vc_2014_2023.csv')
readr::write_csv(colorado, '~/Downloads/colorado_vc_2023.csv')


# ## calc avg VC raised in each CBSA over the 3 years ----------------------------
# cbsa_vc <- form_d_clean %>% 
#   filter(rural_def_23 == 'M2') %>% 
#   group_by(geoid_cbsa, name_cbsa) %>% 
#   summarise(
#     vc_3yr_avg_21_23 = sum(amount_sold, na.rm = TRUE) / 3
#   ) %>% 
#   ungroup() 


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

