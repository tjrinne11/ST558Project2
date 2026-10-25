library(httr)
library(jsonlite)
library(tidyverse)

dir.create("~/ST558Project2/data", showWarnings = FALSE)
dir.create("~/ST558Project2/www", showWarnings = FALSE)

# Census PUMS API 
api_key <- "c6758c38f49e633cc5c6631c8251876e5bc70c58"
url <- paste0(
  "https://api.census.gov/data/2024/acs/acs1/pums?",
  "get=SCHL,SEX,AGEP,GASP,GRPIP,PWGTP&for=state:37&key=", api_key
)

res <- GET(url)
raw_matrix <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

# Format into a  tibble
col_names <- raw_matrix[1, ]
data_rows <- raw_matrix[-1, ]
df_raw <- as_tibble(data_rows)
colnames(df_raw) <- col_names

#Clean and map factors
census_clean <- df_raw %>%
  # Convert columns
  mutate(
    AGEP  = as.numeric(AGEP),
    GASP  = as.numeric(GASP),
    GRPIP = as.numeric(GRPIP),
    PWGTP = as.numeric(PWGTP),
    SCHL  = as.numeric(SCHL),
    SEX   = as.numeric(SEX)
  ) %>%
  # Filter to valid records
  filter(!is.na(AGEP), !is.na(PWGTP)) %>%
  # Map codes to labels
  transmute(
    sex = case_match(
      SEX,
      1 ~ "Male",
      2 ~ "Female",
      .default = "Other"
    ),
    education = case_when(
      SCHL <= 15 ~ "High School or Less",
      SCHL %in% 16:19 ~ "Some College / Associate",
      SCHL %in% 20:21 ~ "Bachelor's Degree",
      SCHL >= 22 ~ "Advanced Degree",
      TRUE ~ "Not Reported"
    ),
    age         = AGEP,
    gas_cost    = replace_na(GASP, 0),
    rent_burden = replace_na(GRPIP, 0),
    weight      = PWGTP
  ) %>%
  slice_sample(n = 20000)

# Save to CSV
write_csv(census_clean, "data/nc_census_pums.csv")