
# Get File path from input -------------------------------------------------

volumes <- getVolumes()()

if (!(is.integer(input$gem_directory))) {
  GEM_db_folder <-
    file.path(parseDirPath(volumes, input$gem_directory), "/")
  file <- as.data.frame(list(
      "files" = list.files(path = GEM_db_folder, pattern = ".db"),
      "mtime" = file.mtime(list.files(path = GEM_db_folder, full.names = T))
    )) %>% arrange(., mtime) %>% select(-mtime)
  file <- file$files
  
} else if (!(is.integer(input$gem_file))) {
  full_db_path <-
    file.path(parseFilePaths(volumes, input$gem_file))[4]
  GEM_db_folder <- paste0(str_before_last(full_db_path, "/"), "/")
  file <- str_after_last(full_db_path, "/")
}

# GEM_db_folder <- "C:/AID/AEMO/Dashboard Bases/"
# 
# file <- as.data.frame(list(
#   "files" = list.files(path = GEM_db_folder, pattern = ".db"),
#   "mtime" = file.mtime(list.files(path = GEM_db_folder))
# )) %>% arrange(., mtime) %>% select(-mtime)
# file <- file$files


file <- file %>% str_replace_all(".db", "")

message("Processing Dbs....")

# Name Check --------------------------------------------------------------
{
  # correct_names <- keep(file, grepl("_GEM_",file))
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
  # incorrect_names <- file[!(file %in% correct_names)]
}

gem_rds_folder <- "data processing/GEM RDS Files/"

gem_rds_filename <-
  list.files(path = gem_rds_folder) %>% str_replace_all(".rds", "")

#setwd("C:/Users/dcrook/Documents/scenario_dashboard/")

list.of.indicators <-
  readxl::read_xlsx("data processing/Input Files/List_Of_Indicators_GEM.xlsx") 

for (i in file) {
  start_time <- Sys.time()
  print(i)
  
  if (i %in% gem_rds_filename) {
    print(paste0("The RDS file -- ",
                 i,
                 ".rds -- already exists"))
  } else {
    # a quick work around for the "space" bug in the read_oedb function
    old_wd <- getwd()
    setwd(GEM_db_folder)
    db <- oxoedb(
      paste0(i, ".db"),
      sect = unique(list.of.indicators$Division)
    )
    setwd(old_wd)
    
    saveRDS(db, file = paste0(gem_rds_folder, i, ".rds"))
    rm(db)
  }
  end_time <- Sys.time()
  print(difftime(end_time, start_time, units = "secs"))
}