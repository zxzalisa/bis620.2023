## code to prepare `DATASET` dataset goes here

accel <- readRDS("accel.rds")
usethis::use_data(accel, overwrite = TRUE)
