
#complile & organise dashboard data
{
  RDS_folder <- "data processing/RDS Files/"
  
  rds_files <- list.files(path = RDS_folder)
  
  message("Combining RDS files")
  message("")
  
  data <- NULL
  for (i in rds_files) {
    message(i)
    d2 <- readRDS(paste0(RDS_folder, i))
    data <- rbind(data, d2)
    rm(d2)
  }
  
  data <- data %>%
    mutate(SCENARIO_VALUE = gsub("_", " ", SCENARIO_VALUE)) %>%
    mutate(RELEASE_VERSION = gsub("_", " ", RELEASE_VERSION))
  
  data <- mutate(
    data,
    ATTRIBUTE = case_when(
      substr(ATTRIBUTE, 1, 11) == "Gross Value" ~ paste0(
        "GVA - ",
        substr(
          ATTRIBUTE,
          stri_locate_last_fixed(ATTRIBUTE, ">>") + 3,
          nchar(ATTRIBUTE)
        )
      ),
      substr(ATTRIBUTE, 1, 11) != "Gross Value" ~ ATTRIBUTE
    )
  )
  
  saveRDS(data, "data/Dashboard_Data.rds")
  message("")
  message("AID Data has been updated")
}
