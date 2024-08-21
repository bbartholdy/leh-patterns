library(DBI)
library(dplyr)
library(stringr)
library(tidyr)
source("R/add_cols.R")
con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")

#dbGetQuery(con, "SELECT * FROM read_csv('~/Nextcloud/Uni/data/other_osteo/word/medieval/*dental_path.lst.csv', delim = '|');")

m_dental_path <- dbGetQuery(con, "SELECT * FROM read_csv('~/data/other_osteo/word/medieval/*dental_path.lst.csv', delim = '|')")
pm_dental_path <- dbGetQuery(con, "SELECT * FROM read_csv('~/data/other_osteo/word/post-medieval/*dental_path.lst.csv', delim = '|')")

m_bones_inv <- dbGetQuery(con, "SELECT * FROM read_csv('~/data/other_osteo/word/medieval/*bones_present.lst.csv', delim = '|')")
pm_bones_inv <- dbGetQuery(con, "SELECT * FROM read_csv('~/data/other_osteo/word/post-medieval/*bones_present.lst.csv', delim = '|')")

pm_bones_meas <- dbGetQuery(con, "SELECT * FROM read_csv('~/data/other_osteo/word/post-medieval/*_metrics.lst.csv', delim = '|')")
m_bones_meas <- dbGetQuery(con, "SELECT * FROM read_csv('~/data/other_osteo/word/medieval/*_metrics.lst.csv', delim = '|')")


# Data cleaning -----------------------------------------------------------

femur_length_df <- pm_bones_meas |>
  add_row(m_bones_meas) |>
  add_column_names("dental_path") |>
  filter(
    str_detect(EXPANSION, "FeL1"),
    str_detect(AGE, "SUB-ADULT", negate = T), # only adults
  ) |>
  separate_wider_delim(EXPANSION, delim = " ", names = c("measurement", "side"))

femur_data <- femur_length_df |>
  select(CONTEXT, SITECODE, measurement, side, VALUE)

hypoplasia_df <- add_row(m_dental_path, pm_dental_path) |>
  add_column_names("dental_path") |>
  filter(
    EXPANSION == "Hypoplasia", # only hypoplasia data
    str_detect(AGE, "SUB-ADULT", negate = T), # only adults
    str_detect(GROUP, "permanent") # remove any remaining deciduous teeth
    ) |>
  mutate(VALUE = if_else(VALUE == 9, NA, VALUE)) |>
  mutate(ID = paste0(SITECODE, "_", CONTEXT)) |>
  mutate(tooth = str_extract(GROUP, "T..")) # extract T and the two following characters


hypoplasia_data <- hypoplasia_df |>
  select(ID, tooth, VALUE)

hypoplasia_data_wide <- hypoplasia_data |>
  pivot_wider(names_from = tooth, values_from = VALUE)

dental_inventory <- m_bones_inv |>
  add_row(pm_bones_inv) |>
  filter(
    BONE_GP == "Permanent teeth",  # only dental data
    str_detect(AGE, "SUB-ADULT", negate = T) # only adults
  ) |>
  separate_wider_delim(BONES_PRESENT, delim = " ", names = c("region", "side", "type")) |>
  #rename(type = tooth) |>
  mutate(
    region = case_match(
      region,
      "Maxilla" ~ "U",
      "Mandible" ~ "L"
    ),
    tooth = paste0(region, side, type),
    tooth = case_when(
      stringr::str_detect(tooth, "C$") ~ paste0(tooth, "1"), # add 1 to canine notation (e.g. ULC -> ULC1)
      TRUE ~ tooth
    )
  ) |>
  mutate(ID = paste0(SITECODE, "_", CONTEXT)) |>
  select(ID, tooth)

dental_inventory_wide <- dental_inventory |>
  mutate(presence = "present") |>
  pivot_wider(names_from = "tooth", values_from = "presence", values_fill = NA)

demography <- hypoplasia_df |>
  select(ID, CEMETERY, SEX, AGE) |>
  distinct(ID, .keep_all = T)

usethis::use_data(demography, overwrite = T)

readr::write_csv(femur_data, "inst/extdata/femur-measurements.csv")
readr::write_csv(demography, "inst/extdata/demography.csv")
readr::write_csv(dental_inventory_wide, "inst/extdata/dental-inventory.csv")
readr::write_csv(hypoplasia_data_wide, "inst/extdata/enamel-hypoplasia.csv")
