

# Get File path from input -------------------------------------------------

volumes <- getVolumes()()

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

gem_db_folder <- "C:/AID/GCS/Mar 2022/"

gem_db_filename <-
  list.files(path = gem_db_folder) %>% str_replace_all(".db", "")

# Name Check --------------------------------------------------------------
{
  # correct_names <- keep(gem_db_filename, grepl("_GEM_",gem_db_filename))
  #
  # correct_names <- keep(correct_names, grepl("_V",correct_names))
  #
  # for(i in correct_names){
  #   i <- correct_names[1]
  #   a <- str_count(i,"_")
  #   b <- str_before_first(str_after_nth(i,"_",a-1),"_")
  #   date <- format(parse_date_time(b,
  #                          "my",
  #                          tz = "UTC"
  #   ), "%b-%y")
  #   if(is.na(date)) {correct_names <- correct_names[correct_names != i]}
  # }
  #
  # incorrect_names <- gem_db_filename[!(gem_db_filename %in% correct_names)]
}
#setwd("C:/Users/dcrook/Documents/scenario_dashboard/")
gem_rds_folder <- "data processing/GEM RDS Files/"

gem_rds_filename <-
  list.files(path = gem_rds_folder) %>% str_replace_all(".rds", "")

list.of.indicators <-
  openxlsx::read.xlsx("data processing/List_Of_Indicators_GEM.xlsx") %>%
  dplyr::rename(Sector = Division)

for (i in gem_db_filename) {
  start_time <- Sys.time()
  print(i)
  
  if (i %in% gem_rds_filename) {
    print(paste0("The RDS file -- ",
                 i,
                 ".rds -- already exists"))
  } else {
    # a quick work around for the "space" bug in the read_oedb function
    old_wd <- getwd()
    setwd(gem_db_folder)
    db <- oxoedb(
      paste0(i, ".db"),
      sect = unique(list.of.indicators$Sector)
    )
    setwd(old_wd)
    
    saveRDS(db, file = paste0(gem_rds_folder, i, ".rds"))
    rm(db)
  }
  end_time <- Sys.time()
  print(difftime(end_time, start_time, units = "secs"))
}