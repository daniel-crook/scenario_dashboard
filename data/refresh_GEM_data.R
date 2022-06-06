
#complile & organise dashboard data
gem_rds_folder <- "data processing/GEM RDS Files/"

list.of.indicators <-
  readxl::read_xlsx("data processing/Input Files/List_Of_Indicators_GEM.xlsx")

message("Combining RDS files")
message("")

gem_rds_filename <-
  as.data.frame(list(
    "files" = list.files(path = gem_rds_folder),
    "mtime" = file.mtime(list.files(path = gem_rds_folder, full.names = T))
  )) %>% arrange(., desc(mtime)) %>% select(-mtime)

gem_rds_filename <-
  gem_rds_filename$files %>% str_replace_all(".rds", "")

for (i in 1:length(gem_rds_filename)) {
  if (i == 1) {
    message(gem_rds_filename[i])
    d <-
      readRDS(paste0(gem_rds_folder, gem_rds_filename[i], ".rds"))
    gem_data <- d[[1]] %>%
      mutate(SCENARIO_RELEASE = gem_rds_filename[i]) %>% clean.names() %>%
      filter(variable %in% list.of.indicators$Mnemonic)
    hist_end <- d[[2]]
  } else {
    message(gem_rds_filename[i])
    gem_db_s <-
      readRDS(paste0(gem_rds_folder, gem_rds_filename[i], ".rds"))
    if (inherits(gem_db_s, "list")) {
      gem_db_s <- gem_db_s[[1]]
    }
    gem_db_s <- gem_db_s %>% clean.names() %>%
      filter(variable %in% list.of.indicators$Mnemonic) %>%
      transmute(Dates, variable, value, SCENARIO_RELEASE = gem_rds_filename[i])
    gem_data <- rbind(gem_data, gem_db_s)
    rm(gem_db_s)
  }
}
gem_data_list <-
  list("gem_data" = gem_data, "hist_end" = hist_end)
saveRDS(gem_data_list, "data/GEM_Dashboard_Data.rds")

message("")
message("GEM Data has been updated")
