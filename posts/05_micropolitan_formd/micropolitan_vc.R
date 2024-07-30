

# Set up -----------------------------------------------------------------------
library(dplyr)
library(coriverse)
library(cori.charts)
library(ggplot2)
library(sysfonts)
library(showtext)

# Loads Lato from the google font repository and adds it to sysfonts
sysfonts::font_add_google("Lato")

# Loads TT Hoves (has to be installed on your computer)
font_add(
  "TT Hoves",
  regular = "TypeType - TT Hoves Regular.ttf",
  bold = "TypeType - TT Hoves Bold.ttf",
  italic = "TypeType - TT Hoves Italic.ttf",
  bolditalic = "TypeType - TT Hoves Bold Italic.ttf"
)

# Ensures that any newly opened graphics devices will use showtext to draw text
showtext_auto()
# Sets default density per inch for exports
showtext_opts(dpi = 300)


# load 2022 CBSA populations ---------------------------------------------------
years <- c(2022)

population <- lapply(years, function(y) {
  
  acs_dta <- tidycensus::get_acs(geography = 'cbsa',
                                 variables = 'B01001A_001',
                                 year = y) %>% 
    mutate(year = y)
  
}) %>% bind_rows() %>% 
  select(
    year,
    geoid_cbsa = GEOID,
    cbsa_pop = estimate
  )

cbsa_county_xwalk <- tigris::counties(year = 2021) %>%
  sf::st_drop_geometry() %>%
  left_join(tigris::core_based_statistical_areas(year = 2021) %>% sf::st_drop_geometry(), by = 'CBSAFP') %>%
  select(
    geoid_co = GEOID.x,
    geoid_cbsa = GEOID.y,
    name_cbsa = NAMELSAD.y
  )

cbsa_pop <- cbsa_county_xwalk %>% 
  left_join(population, by = 'geoid_cbsa') %>% 
  filter(!is.na(year))


# load VC data -----------------------------------------------------------------
con <- connect_to_db('venture_capital')
form_d <- read_db(con, 'form_d_businesses')
DBI::dbDisconnect(con)

form_d_clean <- form_d %>% 
  group_by(year, cik, entity_name, sale_date, offering_amount) %>% 
  slice_max(amount_sold) %>% # take most recent of each amendment within a year
  ungroup() %>% 
  filter(
    year %in% c(2022),
    rural_def == 'Micro',
    !grepl('Oil and Gas', industries),
    !grepl('Coal Mining', industries),
    !grepl('Other Banking and Financial Services', industries),
    !grepl('Commercial Banking', industries),
    !grepl('Investment Banking', industries),
    !grepl('Construction', industries),
    !grepl('Commercial', industries),
    !grepl('Residential', industries),
    !grepl('REITS and Finance', industries),
    !grepl('Other Real Estate', industries)
  )

# join datasets and calc VC per capita in 2022 ---------------------------------
vc_per_cap <- form_d_clean %>% 
  left_join(cbsa_pop %>% select(!cbsa_pop), by = c('year', 'geoid_co')) %>% 
  group_by(name_cbsa) %>% 
  summarise(
    amount_sold = sum(amount_sold, na.rm = TRUE),
    # cbsa_pop = cbsa_pop,
    # vc_per_capita = sum(amount_sold, na.rm = TRUE) / sum(cbsa_pop, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  distinct() %>% 
  left_join(cbsa_pop %>% select(name_cbsa, cbsa_pop) %>% distinct(), by = c('name_cbsa')) %>% 
  mutate(
    vc_per_capita = amount_sold / cbsa_pop
  ) %>% 
  arrange(desc(vc_per_capita)) %>% 
  slice_max(vc_per_capita, n = 10)


# Chart ------------------------------------------------------------------------

fig <- vc_per_cap %>%
  ggplot2::ggplot(
    aes(
      vc_per_capita,
      # name_cbsa
      # Make RIN communities appear in descending order
      # based upon their population below the poverty level
      reorder(name_cbsa, vc_per_capita)
    )
  ) +
  # Set the bar color the the CORI "Emerald" color
  ggplot2::geom_col(fill = cori_colors["Squash"]) +
  # Add data labels to the bars
  geom_text(
    aes(
      # You can adjust the scales function depending on the desired
      # number format (e.g., percent, dollar, etc.)
      label = scales::dollar(
        vc_per_capita,
        # accuracy determines what number to round to (e.g., accuracy = 0.01 will show 2 decimal places)
        accuracy = 1,
        # big.mark determines the character used between every 3 digits to separate thousands
        big.mark = ","
      )
    ),
    fontface = "bold",
    # Provide spacing between the data label and the bar position
    hjust = -.2,
    # Data labels need to have their font family explicitly set to "Lato"
    family = "Lato"
  ) +
  ggplot2::scale_x_continuous(
    # labels determines whether tick labels are shown
    labels = NULL,
    # You can provide an expansion multiplier to the axis to ensure that
    # data labels will have enough space
    expand = expansion(mult = c(0, .25))
  ) +
  # Call the horizontal bar theme to pull in default CORI theming
  theme_cori_horizontal_bars() +
  # Override any defaults styles using the ggplot2::theme() function AFTER
  # calling theme_cori_horizontal_bars()
  ggplot2::theme(
    # Set title fonts to Lato, as TT Hoves is unavailable
    # Where possible, use TT Hoves for titles (default)
    plot.title = element_text(family = "TT Hoves"),
    plot.subtitle = element_text(family = "TT Hoves"),
    # Remove x gridlines
    panel.grid.major.x = element_blank()
  ) +
  # Provide Title, subtitle, etc.
  ggplot2::labs(
    title = "Micropolitan areas with the most venture capital funding per capita in 2022",
    # subtitle = "For select RIN communities (2019)",
    y = NULL,
    x = NULL,
    caption = "Source: ACS 5-year estimates (2022), SEC Form D (2022)
Notes: Venture capital totals exclude Form D entries where the industry is flagged as coal mining, oil and gas, banking, construction, or real estate."
  )

fig


cori.charts::save_plot(fig = fig,
                       chart_width = 9.5,
                       export_path = './posts/micropolitan_formd/images/top10_vc_micropolitans.png')

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


