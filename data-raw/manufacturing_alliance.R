## code to prepare `manufacturing_alliance` dataset goes here
library(strayr)
library(dplyr)

manufacturing_alliance <- anzsic2006 |>
  filter(anzsic_division_code %in% c("C", "S")) |>
  mutate(sub_sector = case_when(
    anzsic_subdivision_code %in% c("11", "12") & !anzsic_group_code %in% c("111", "112") ~ "Food and beverage, including tobacco",
    anzsic_subdivision_code == "16" ~ "Print and graphic arts",
    anzsic_subdivision_code %in% c("17", "18", "21") & !anzsic_group_code %in% c("182", "184", "214") & !anzsic_class_code == "2122" ~ "Chemicals, hydrocarbons, and refining",
    anzsic_group_code == "184" ~ "Pharmaceutical manufacturing",
    anzsic_subdivision_code == "19" | anzsic_group_code == "182" | anzsic_class_code == "2431" ~ "Plastics, rubber and cable-making",
    anzsic_subdivision_code == "20" & anzsic_group_code != "201" ~ "Manufactured mineral products",
    anzsic_class_code == "2394" ~ "Aerospace",
    anzsic_subdivision_code %in% c("22", "24") | anzsic_class_code %in% c("2122", "9499") | anzsic_group_code %in% c("214", "259", "942") ~ "General manufacturing and engineering",
    TRUE ~ "Manufacturing not covered by Manufacturing Alliance"
  ))


usethis::use_data(manufacturing_alliance, compress = "xz", overwrite = TRUE)
