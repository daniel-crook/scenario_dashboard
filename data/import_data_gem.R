
gem_data_list <- readRDS("data/GEM_Dashboard_Data.rds")

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
gem_data$hist_end[is.na(gem_data$hist_end)] <- ymd(paste0(year(Sys.Date() %m-% months(6)),"-06-01"))

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
