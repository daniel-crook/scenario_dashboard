
gem_db_folder <- "C:/AID/GCS/Mar-2022/"

gem_db_filename <-
  list.files(path = gem_db_folder) %>% str_replace_all(., ".db", "")

gem_rds_folder <- "data processing/GEM RDS Files/"

gem_rds_filename <-
  list.files(path = gem_rds_folder) %>% str_replace_all(".rds", "")

list.of.indicators <-
  openxlsx::read.xlsx("data processing/List_Of_Indicators_GEM.xlsx") %>%
  dplyr::rename(Sector = Division)

new_gem_db <-
  gem_db_filename[!(gem_db_filename %in% gem_rds_filename)]

if (length(new_gem_db) == 0) {
  gem_data_list <- readRDS("data/GEM_Dashboard_Data.rds")
} else {
  for (i in new_gem_db) {
    db <- oxoedb(paste0(gem_db_folder, i, ".db"),
                 sect = unique(list.of.indicators$Sector))
    saveRDS(db, file = paste0(gem_rds_folder, i, ".rds"))
    rm(db)
  }
  
  # wd <- "S:/BIS Consultancy/Product Development/Scenarios_Dashboard/Dashboard"
  # setwd(wd)
  
  gem_rds_filename <-
    as.data.frame(list(
      "files" = list.files(path = gem_rds_folder),
      "mtime" = file.mtime(list.files(
        path = gem_rds_folder, full.names = T
      ))
    )) %>% arrange(., desc(mtime)) %>% select(-mtime)
  
  gem_rds_filename <-
    gem_rds_filename$files %>% str_replace_all(".rds", "")
  
  for (i in 1:length(gem_rds_filename)) {
    if (i == 1) {
      print(gem_rds_filename[i])
      d <-
        readRDS(paste0(gem_rds_folder, gem_rds_filename[i], ".rds"))
      gem_data <- d[[1]] %>%
        mutate(SCENARIO_RELEASE = gem_rds_filename[i])
      hist_end <- d[[2]]
    } else {
      print(gem_rds_filename[i])
      gem_db_s <-
        readRDS(paste0(gem_rds_folder, gem_rds_filename[i], ".rds")) 
      if(inherits(gem_db_s, "list")) {
        gem_db_s <- gem_db_s[[1]]
      }
      gem_db_s <-
        transmute(gem_db_s, Dates, variable, value, SCENARIO_RELEASE = gem_rds_filename[i])
      gem_data <- rbind(gem_data, gem_db_s)
      rm(gem_db_s)
    }
  }
  gem_data_list <-
    list("gem_data" = gem_data, "hist_end" = hist_end)
  saveRDS(gem_data_list, "data/GEM_Dashboard_Data.rds")
  gem_data_list <- readRDS("data/GEM_Dashboard_Data.rds")
}

gem_data <-
  gem_data_list[[1]] %>% clean.names() %>%
  filter(variable %in% list.of.indicators$Mnemonic)

gem_data <-
  gem_data %>% separate(SCENARIO_RELEASE, c('Scenario', 'Release'), sep = "_Q") %>% mutate(., Release = paste0("Q", Release))

gem_data[-2] <-
  lapply(gem_data[-2], gsub, pattern = "_", replacement = " ") %>% as.data.frame()

hist_end <- gem_data_list[[2]] %>% clean.names()
rm(gem_data_list)

gem_data <- left_join(gem_data, hist_end, by = "variable") %>%
  transmute(., Dates, Mnemonic = variable, value = as.numeric(value), Scenario, Release, hist_end)
### ideally automate this fill NAs date -- based on end of last financial year or equal to end hist_end date for GDP
gem_data$hist_end[is.na(gem_data$hist_end)] <- "2021-06-01"

list.of.indicators <-
  select(list.of.indicators, Mnemonic, Attribute, Units, Aggregation, Category)
gem_data <-
  left_join(gem_data, list.of.indicators, by =  "Mnemonic") %>% 
  add.var.col.gem()

categories <- unique(gem_data$Category)

var_list <- NULL
for(i in 1:length(categories)){
  var_list[[i]] <- unique(gem_data$Attribute[gem_data$Category == categories[i]])
}

names(var_list) <- categories

