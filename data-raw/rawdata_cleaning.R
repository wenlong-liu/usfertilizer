require(tidyverse)

# county level data of fertilizer application.
#Source: https://www.sciencebase.gov/catalog/item/5851b2d1e4b0f99207c4f238
raw_data = read_csv("data-raw/CNTY_FERT_1987-2012.csv")
#summary(raw_data)

# County summary from US census bureau.
# Source: https://www.census.gov/geo/maps-data/data/gazetteer2010.html
county_raw = read.table("data-raw/Gaz_counties_national.txt", sep = "\t", header=TRUE)
# remove duplicates in county data.
county_data = county_raw %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  # select certin columns.
  select(GEOID, ALAND, AWATER,INTPTLAT, INTPTLONG) %>%
  mutate(FIPSno = GEOID) %>%
  select(-GEOID)

# combine county data with county level fertilizer data.
county_summary = left_join(raw_data,county_data, by = "FIPSno")

# data cleanning
us_fertilizer_county = county_summary %>%
  # remove some columns with FIPS numbers.
  select(-c(FIPS_st, FIPS_co,FIPSno)) %>%
  # wide to long dataset.
  gather(Fert.Type, Quantity, farmN1987:nonfP2012) %>%
  # separate the fert.type into three columns: farm type, fertilizer, year.
  mutate(Year = str_sub(Fert.Type, start = -4),
         Fertilizer = str_sub(Fert.Type, start = -5, end = -5),
         Farm.Type = str_sub(Fert.Type, start = 1, end = 4)
  ) %>%
  # repalce nonf into nonfarm
  mutate(Farm.Type = ifelse(Farm.Type == "nonf", "nonfarm", "farm")) %>%
  # remove Fert.Type
  select(-Fert.Type)

devtools::use_data(us_fertilizer_county)
