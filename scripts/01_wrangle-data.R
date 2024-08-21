library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(teethr)
#devtools::load_all("~/Nextcloud/Uni/Rstuff/packages/teethr/")
devtools::load_all()
#source("R/utils.R")

dental_inventory <- readr::read_csv("inst/extdata/dental-inventory.csv")
hypoplasia_data <- readr::read_csv("inst/extdata/enamel-hypoplasia.csv")

# Data wrangling ---------------------------------------------------------

dental_inventory_long <- dental_inventory |>
  pivot_longer(-ID, names_to = "tooth", values_to = "presence") |>
  mutate(
    tooth = gsub("P3", "P1", tooth),
    tooth = gsub("P4", "P2", tooth),
    presence = if_else(is.na(presence), 0, 1)
  ) |>
  dental_join(notation = "text") |>
  mutate(tooth = factor(tooth, levels = tooth_notation$text))

# filter complete dentitions (excluding M3s)
dental_inventory_complete <- dental_inventory |>
  select(!ends_with("3")) |>
  remove_missing() |>
  select(ID)

hypoplasia_complete_wide <- hypoplasia_data |>
  select(!ends_with("8")) |> # remove M3s
  right_join(dental_inventory_complete)

hypoplasia_long <- hypoplasia_data |>
  dental_longer(-ID) |>
  dental_recode(tooth, from = "FDI", to = "text") |>
  dental_join(notation = "text") |>
  mutate(
    tooth = factor(tooth, levels = tooth_notation$text),
    hypoplasia_severity = stringi::stri_extract(score, regex = "\\d$"),
    hypoplasia_bin = if_else(score == 0, 0, 1),
    hypoplasia_location = stringi::stri_extract(score, regex = "^\\d"),
    can_score = if_else(is.na(score), 0, 1),
    leh_severity = if_else(hypoplasia_severity == "4", "0", hypoplasia_severity),
    leh_bin = case_match(
      leh_severity,
      "0" ~ 0,
      c("1","2","3") ~ 1
    )
  ) |>
  group_by(ID) |>
  mutate(
    leh_present = if_else(sum(leh_bin, na.rm = T) == 0, 0, 1), # is there at least one lesion present?
  ) |>
  ungroup()
  

# hypoplasia individuals with complete dentition
hypoplasia_complete_long <- hypoplasia_long |>
  right_join(dental_inventory_complete) |>
  filter(!is.na(can_score))

# No missing hypoplasia scores (excluding M3s)
hypoplasia_scored <- hypoplasia_data |>
  select(!ends_with("8")) |>
  remove_missing()

hypoplasia_scored_long <- hypoplasia_complete_long |>
  right_join(select(hypoplasia_scored, ID)) |>
  filter(!stringi::stri_detect(tooth, regex = "3$")) |> # remove M3s
  mutate(tooth = factor(tooth, levels = tooth_notation$text))

hypoplasia_present_long <- hypoplasia_scored_long |>
  filter(leh_present == 1)

hypoplasia_present_wide <- hypoplasia_present_long |>
  select(ID, tooth, score) |>
  mutate(score = if_else(score == 0, NA, 1)) |>
  pivot_wider(names_from = "tooth", values_from = "score")

usethis::use_data(dental_inventory, overwrite = T)
usethis::use_data(dental_inventory_long, overwrite = T)
usethis::use_data(hypoplasia_data, overwrite = T)
usethis::use_data(hypoplasia_long, overwrite = T)
usethis::use_data(hypoplasia_complete_wide, overwrite = T)
usethis::use_data(hypoplasia_complete_long, overwrite = T)
usethis::use_data(hypoplasia_present_long, overwrite = T)
usethis::use_data(hypoplasia_present_wide, overwrite = T)
