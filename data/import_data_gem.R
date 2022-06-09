
gem_data_list <- readRDS("data/GEM_Dashboard_Data.rds")

list.of.indicators <-
  readxl::read_xlsx("data processing/Input Files/List_Of_Indicators_GEM.xlsx") %>%
  dplyr::rename(Sector = Division)

gem_data <-
  gem_data_list[[1]]

gem_data <-
  transmute(
    gem_data,
    Dates,
    variable,
    value,
    Scenario = str_before_first(str_before_nth(SCENARIO_RELEASE, "_", str_count(SCENARIO_RELEASE, "_") -
                                1),"_GEM"),
    Release = str_after_nth(SCENARIO_RELEASE, "_", str_count(SCENARIO_RELEASE, "_") -
                              1)
  )

gem_data[-2] <-
  lapply(gem_data[-2], gsub, pattern = "_", replacement = " ") %>% as.data.frame()

hist_end <- gem_data_list[[2]] %>% clean.names()
rm(gem_data_list)

gem_data <- left_join(gem_data, hist_end, by = "variable") %>%
  transmute(.,
            Dates,
            Mnemonic = variable,
            value = as.numeric(value),
            Scenario,
            Release,
            hist_end)
gem_data$hist_end[is.na(gem_data$hist_end)] <-
  ymd(paste0(year(Sys.Date() %m-% months(6)), "-06-01"))

list.of.indicators <-
  select(list.of.indicators,
         Mnemonic,
         Attribute,
         Units,
         Aggregation,
         Category)

gem_data <-
  left_join(gem_data, list.of.indicators, by =  "Mnemonic") %>%
  add.var.col.gem()

categories <- unique(gem_data$Category)

var_list <- NULL
for (i in 1:length(categories)) {
  var_list[[i]] <-
    unique(gem_data$Attribute[gem_data$Category == categories[i]])
}

names(var_list) <- categories
return(gem_data)
