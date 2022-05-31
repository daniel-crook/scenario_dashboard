
# Get File path from input -------------------------------------------------

if (!(is.integer(input$gem_directory))) {
  GEM_db_folder <-
    file.path(parseDirPath(volumes, input$gem_directory), "/")
  file <- list.files(GEM_db_folder, pattern = ".db")
} else if (!(is.integer(input$gem_file))) {
  full_db_path <-
    file.path(parseFilePaths(volumes, input$gem_file))[4]
  GEM_db_folder <- paste0(str_before_last(full_db_path, "/"), "/")
  file <- str_after_last(full_db_path, "/")
}

gem_db_folder <- "C:/AID/GCS/Mar-2022/"

gem_db_filename <-
  list.files(path = gem_db_folder) %>% str_replace_all(".db", "")

# Name Check --------------------------------------------------------------
{
correct_names <- keep(gem_db_filename, grepl("_GEM_",gem_db_filename))

correct_names <- keep(correct_names, grepl("_V",correct_names))

for(i in correct_names){
  i <- correct_names[1]
  a <- str_count(i,"_")
  b <- str_before_first(str_after_nth(i,"_",a-1),"_")
  date <- format(parse_date_time(b,
                         "my",
                         tz = "UTC"
  ), "%b-%y")
  if(is.na(date)) {correct_names <- correct_names[correct_names != i]}
}

incorrect_names <- gem_db_filename[!(gem_db_filename %in% correct_names)]
}

gem_rds_folder <- "data processing/GEM RDS Files/"

gem_rds_filename <-
  list.files(path = gem_rds_folder) %>% str_replace_all(".rds", "")

for (i in file) {
  print(i)
  GEM_db_filepath <- paste0(GEM_db_folder, i)

  list.of.indicators <-
    openxlsx::read.xlsx("data processing/List_Of_Indicators_GEM.xlsx") %>%
    dplyr::rename(Sector = Division)
  
  new_gem_db <-
    gem_db_filename[!(gem_db_filename %in% gem_rds_filename)]
  for (i in file) {
    if (gem_db_filename %in% gem_rds_filename) {
      print(
        paste0(
          Scenario_name,
          "_",
          release_version,
          "_Output_Data.rds RDS file already exists"
        )
      )
    } else {
      for (i in new_gem_db) {
        db <- oxoedb(paste0(gem_db_folder, i, ".db"),
                     sect = unique(list.of.indicators$Sector))
        saveRDS(db, file = paste0(gem_rds_folder, i, ".rds"))
        rm(db)
      }
      
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
          if (inherits(gem_db_s, "list")) {
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
    }
    