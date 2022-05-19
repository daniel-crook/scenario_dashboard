###########################################################################
# Extracting AEMO Data ----------------------------------------------------
# Author: Daniel Crook ----------------------------------------------------
# Date: 11/04/2022 --------------------------------------------------------
###########################################################################

# 0.0 Clear Environment ---------------------------------------------------
start_time <- Sys.time()

# 1.0 Load Libraries ------------------------------------------------------
{
  easypackages::libraries("openxlsx", "oxgraphs", "oemdlRtools", "stringr", "stringi")
  #oxgraphs::ox_setup() #ox_setup loads several packages for us (i.e. dplyr, zoo...)
}

###########################################################################
###########################################################################
# 2.0 Set Parameters & Working Directory ----------------------------------
{
  #specify flat file names
  indicator_list_file_name <- "List_Of_Indicators.xlsx"
  meta_data_file_name <- "Metadata_File.xlsx"
  
  #invisible(readline(prompt = "Have you updated your dates? If yes, press [enter] to proceed"))
  #set dates
  st_yr <- 1999
  hist_end_fin_yr <- 2021
  est_actual_end_yr <- 2022
  end_yr <- 2054
  
  #invisible(readline(prompt = "Have you updated your db folder? If yes, press [enter] to proceed"))
  AID_db_folder <- file.path(parseDirPath(volumes, input$directory), "/")
  
}

file <- list.files(AID_db_folder)

for (i in file) {
  print(i)
  AID_db_filepath <- paste0(AID_db_folder, i)
  
  ############################################################################
  ############################################################################
  
  # 2.1 Parameters that don't require adjusting -----------------------------
  {
    #set building mapping frame
    state_list <-
      c("AUS", "NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT")
    state_list_states <-
      c("AUS", "NSW", "VIC", "QSL", "SAL", "WAL", "TAS", "NTY", "ACT")
    state_id <- c("0", "1", "2", "3", "4", "5", "6", "7", "8")
    Buildings_Series_State_Mapping <-
      data.frame(State = state_list, ID = state_id)
    
    AID_db_filename <- substr(
      AID_db_filepath,
      stringi::stri_locate_last_fixed(AID_db_filepath, "/") + 1,
      nchar(AID_db_filepath)
    )
    
    #Check file type
    if (substr(AID_db_filename,
               nchar(AID_db_filename) - 2,
               nchar(AID_db_filename)) != ".db") {
      name_checks1 <- "Failed - File is not a AID db"
      stop("File is not a AID db")
    } else {
      name_checks1 <- "Passed"
    }
    
    Scenario_name <-
      toupper(substr(
        AID_db_filename,
        1,
        stringi::stri_locate_first_fixed(AID_db_filename, "_") - 1
      ))
    
    #Adjust Scenario Names
    if (Scenario_name == "EXPORTSUPERPOWER") {
      Scenario_name <- "EXPORT_SUPERPOWER"
      name_checks2 <- "Passed"
    } else if (Scenario_name == "SLOWGROWTH") {
      Scenario_name <- "SLOW_GROWTH"
      name_checks2 <- "Passed"
    } else if (Scenario_name == "RAPIDDECARB") {
      Scenario_name <- "RAPID_DECARB"
      name_checks2 <- "Passed"
    } else if (Scenario_name == "SUSTAINABLEGROWTH") {
      Scenario_name <- "SUSTAINABLE_GROWTH"
      name_checks2 <- "Passed"
    } else if (Scenario_name == "CENTRAL") {
      name_checks2 <- "Passed"
    } else {
      name_checks2 <- "Failed - invalid scenario name"
      stop("Invalid scenario name")
    }
    
    #Split off Release Date
    if (as.numeric(str_count(AID_db_filename, "AID_")) == 1 &
        as.numeric(str_count(AID_db_filename, "_")) == 3) {
      date <- format(parse_date_time(
        substr(
          AID_db_filename,
          stringi::stri_locate_last_fixed(AID_db_filename, "AID_") + 4,
          stringi::stri_locate_last_fixed(AID_db_filename, "_") - 1
        ),
        "my",
        tz = "UTC"
      ), "%b-%y")
      name_checks3 <- "Passed"
    } else {
      name_checks3 <- "Failed - missing date"
      stop("Missing date")
    }
    
    #Split off Version Number
    if (as.numeric(str_count(AID_db_filename, "_V")) == 1) {
      version <- substr(
        AID_db_filename,
        stringi::stri_locate_last_fixed(AID_db_filename, "_V") + 2,
        nchar(AID_db_filename) - 3
      )
      name_checks4 <- "Passed"
    } else {
      name_checks4 <- "Failed - missing version number"
      stop("Missing version number")
    }
    
    #Change Scenario Status if 99 (final base)
    if (version == "99") {
      Scenario_status <- "Final"
    } else {
      Scenario_status <- "Draft"
    }
    
    #release version
    release_version <- paste0(gsub("-", "", date), "_V", version)
  }
  
  # 2.2 Loop over all ".db" files in folder ---------------------------------
  rds_files <- list.files("data processing/RDS Files/")
  
  if (paste0(Scenario_name, "_", release_version, "_Output_Data.rds") %in% rds_files) {
    print(
      paste0(
        Scenario_name,
        "_",
        release_version,
        "_Output_Data.rds RDS file already exists"
      )
    )
  } else {
    # 3.0 Create Functions ----------------------------------------------------
    {
      project_forward_YoY <- function(x) {
        while (any(is.na(x))) {
          last_dp <- length(which(!is.na(x)))
          x[last_dp + 1] <- x[last_dp - 3] * (x[last_dp] / x[last_dp - 4])
        }
        x
      }
      
      project_forward_Equal <- function(x) {
        while (any(is.na(x))) {
          last_dp <- length(which(!is.na(x)))
          x[last_dp + 1] <- x[last_dp]
        }
        x
      }
    }
    
    # 4.0 Read in Flat Files --------------------------------------------------
    {
      list.of.indicators <-
        openxlsx::read.xlsx("data processing/Input Files/List_of_Indicators.xlsx") %>%
        dplyr::rename(Sector = Division) # List of Indicators to extract from AID
      
      Rebasing_Coeff <-
        openxlsx::read.xlsx("data processing/Input Files/RebasingCoefficients.xlsx") %>%
        dplyr::select(-Division, -Series_Name) %>%
        tidyr::gather(key = Region, value = Coefficients, -Series_ID) #Price Deflator Coefficients for rebasing
      
      meta_data_df <-
        openxlsx::read.xlsx("data processing/Input Files/Metadata_File.xlsx")
    }
    
    # 5.0 Get AID Data --------------------------------------------------------
    {
      AID_db <-
        oxgraphs::oxaiddb(
          db = AID_db_filepath,
          sect = list.of.indicators$Sector[list.of.indicators$Database == "AID"],
          vars = list.of.indicators$Mnemonic[list.of.indicators$Database == "AID"]
        )
      
      #saveRDS(AID_db[[1]], file = file.path("data processing/RDS Files/AID_db_", Scenario_name, ".rds")))
      AID_db_States <- AID_db[[1]] %>%
        dplyr::filter(
          variable %in% (
            list.of.indicators %>%
              dplyr::filter(Sector == "STATES") %>%
              dplyr::select(ID) %>%
              as.vector() %>%
              t()
          )
        ) %>%
        dplyr::mutate(Division = "STATES") %>%
        dplyr::filter(lubridate::year(Dates) >= st_yr) %>%
        dplyr::filter(lubridate::year(Dates) <= end_yr) %>%
        as.data.frame() %>%
        dplyr::transmute(
          YYYYQ = Dates,
          Series = variable,
          value = value,
          Division = Division,
          Source = AID_db_filename,
          Extract_Date = Sys.Date(),
          Scenario = Scenario_name,
          Region = stringi::stri_sub(Series, -10, -8),
          #Creates a region column
          Series_ID = Series
        )
      
      for (i in state_list_states) {
        AID_db_States$Series <- AID_db_States$Series %>%
          lapply(function(Series)
            gsub("_STATES", "", Series)) %>%
          unlist() #converts list to character vector of all the elements
        
        #Replace _STATES with blank - returns class list
        AID_db_States$Series_ID <- AID_db_States$Series_ID %>%
          lapply(function(Series_ID)
            gsub("_STATES", "", Series_ID)) %>% #Replace _STATES with blank - returns class list
          lapply(function(Series_ID)
            gsub(paste0(i), "", Series_ID)) %>% #Replace {NSW,QLD...} with blank - returns class list
          unlist() #converts list to character vector of all the elements
        
        #Replace _STATES with blank - returns class list
        AID_db_States$Region <- AID_db_States$Region %>%
          lapply(function(Region)
            gsub("QSL", "QLD", Region)) %>% # - returns class list
          lapply(function(Region)
            gsub("SAL", "SA", Region)) %>% # - returns class list
          lapply(function(Region)
            gsub("WAL", "WA", Region)) %>% # - returns class list
          lapply(function(Region)
            gsub("NTY", "NT", Region)) %>% #- returns class list
          unlist() #converts list to character vector of all the elements
      }
      
      AID_db_Buildings <- AID_db[[1]] %>%
        dplyr::filter(
          variable %in% (
            list.of.indicators %>%
              dplyr::filter(Sector == "BUILDING") %>%
              dplyr::select(ID) %>%
              as.vector() %>%
              t()
          )
        ) %>%
        dplyr::mutate(Division = "BUILDING") %>%
        dplyr::filter(lubridate::year(Dates) >= st_yr) %>%
        dplyr::filter(lubridate::year(Dates) <= end_yr) %>%
        as.data.frame() %>%
        dplyr::transmute(
          YYYYQ = Dates,
          Series = variable,
          value = value,
          Division = Division,
          Source = AID_db_filename,
          Extract_Date = Sys.Date(),
          Scenario = Scenario_name,
          Region = stringi::stri_sub(Series, -11, -11),
          #Creates a region column
          Series_ID = Series
        ) %>%
        dplyr::left_join(Buildings_Series_State_Mapping, by = c("Region" = "ID")) %>%
        dplyr::select(-Region) %>%
        dplyr::rename(Region = State)
      
      for (i in state_id) {
        AID_db_Buildings$Series <- AID_db_Buildings$Series %>%
          lapply(function(Series)
            gsub("_BUILDING", "", Series)) %>%
          unlist() #converts list to character vector of all the elements
        
        #Replace _STATES with blank - returns class list
        AID_db_Buildings$Series_ID <- AID_db_Buildings$Series_ID %>%
          lapply(function(Series_ID)
            gsub("_BUILDING", "", Series_ID)) %>% #Replace _STATES with blank - returns class list
          lapply(function(Series_ID)
            gsub(paste0("_", i, "N"), "", Series_ID)) %>% #Replace {_0N,_1N...} with blank - returns class list
          unlist() #converts list to character vector of all the elements
      }
      
      AID_db_Demog <- AID_db[[1]] %>%
        dplyr::filter(
          variable %in% (
            list.of.indicators %>%
              dplyr::filter(Sector == "DEMOG") %>%
              dplyr::select(ID) %>%
              as.vector() %>%
              t()
          )
        ) %>%
        dplyr::mutate(Division = "DEMOG") %>%
        dplyr::filter(lubridate::year(Dates) >= st_yr) %>%
        dplyr::filter(lubridate::year(Dates) <= end_yr) %>%
        as.data.frame() %>%
        dplyr::transmute(
          YYYYQ = Dates,
          Series = variable,
          value = value,
          Division = Division,
          Source = AID_db_filename,
          Extract_Date = Sys.Date(),
          Scenario = Scenario_name,
          Region = case_when(
            stri_sub(Series, 3, 3) == "N" ~ stri_sub(Series, 1, 2),
            stri_sub(Series, 4, 4) == "N" ~ stri_sub(Series, 1, 3)
          ),
          #Creates a region column
          Series_ID = Series
        )
      
      for (i in state_list) {
        AID_db_Demog$Series <- AID_db_Demog$Series %>%
          lapply(function(Series)
            gsub("_DEMOG", "", Series)) %>%
          unlist() #converts list to character vector of all the elements
        
        #Replace _STATES with blank - returns class list
        AID_db_Demog$Series_ID <- AID_db_Demog$Series_ID %>%
          lapply(function(Series_ID)
            gsub("_DEMOG", "", Series_ID)) %>% #Replace _Demog with blank - returns class list
          lapply(function(Series_ID)
            gsub(paste0(i), "", Series_ID)) %>% #Replace {NSW,QLD...} with blank - returns class list
          unlist() #converts list to character vector of all the elements
      }
      
      AID_db_Total <-
        AID_db_States %>% dplyr::bind_rows(AID_db_Buildings) %>% dplyr::bind_rows(AID_db_Demog)
    }
    
    # 6.0 Rebase Data ---------------------------------------------------------
    {
      AID_db_Total_rebased <- AID_db_Total %>%
        dplyr::left_join(Rebasing_Coeff, by = c("Region", "Series_ID")) %>%
        dplyr::mutate(Rebased_Value = value * Coefficients) %>%
        dplyr::select(-value, -Coefficients) %>%
        dplyr::rename(value = Rebased_Value)
    }
    
    # 7.0 Adding SFD, Ind_Prod, Svcs_GVA & dropping Pvt/Pub C & IF ------------
    {
      Output_Data <- data.frame()
      for (j in state_list) {
        print(j)
        temp_df <-
          AID_db_Total_rebased %>% dplyr::filter(Region == j) %>%
          dplyr::select(-Series, -Division)  %>%
          tidyr::spread(key = Series_ID, value = value) %>%
          dplyr::mutate(SFDLCC = CLCC + GCLCC + IFLCC + GILCC) %>%
          dplyr::mutate(INDPRODLCC = GVACLCC + GVADLCC + GVAELCC) %>%
          dplyr::mutate(
            SVCSGVALCC = GVAG1LCC + GVAG2LCC + GVAHLCC + GVAI1LCC +
              GVAI2LCC + GVAJLCC + GVAK1LCC + GVAK2LCC + GVAK3LCC +
              GVALLCC + GVAMLCC + GVANLCC + GVAOLCC + GVAPLCC
          ) 
        if(j == "AUS") {temp_df <- temp_df %>%
          dplyr::mutate(
            POPINC = NOMTOT + NATTOT)} else {temp_df <- temp_df %>%
                                              dplyr::mutate(
                                                POPINC = NOMTOT + NATTOT + NIMTOT)}
        temp_df <- temp_df %>%
          dplyr::select(-GCLCC, -CLCC, -GILCC, -IFLCC) %>%
          tidyr::gather(key = Series_ID,
                        value = value,
                        -YYYYQ,
                        -(Source:Region))
        Output_Data <- Output_Data %>% bind_rows(temp_df)
      }
    }
    
    
    # 8.0 Extending Forecasts past FY50 and adding hist/fc column -------------
    {
      Mapping_Table <-
        Output_Data %>% dplyr::mutate(Series_Name = paste0(Series_ID, Region)) %>% select(-YYYYQ, -value) %>% unique()
      
      Date_Df <-
        data.frame(YYYYQ = seq(
          as.Date("1999/3/1"),
          by = "quarter",
          length.out = (as.numeric(end_yr) - as.numeric(st_yr)) * 4 + 2
        )) %>%
        dplyr::left_join(
          Output_Data %>% dplyr::mutate(Series_Name = paste0(Series_ID, Region)) %>%
            dplyr::select(-Source:-Series_ID) %>%
            tidyr::spread(key = Series_Name, value = value),
          by = c("YYYYQ")
        )
      
      Date_Df[Date_Df == 0] <- 0.001
      
      Df <-
        sapply(Date_Df %>% select(-YYYYQ), FUN = project_forward_YoY) %>% unlist() %>% as.data.frame()
      
      Df$YYYYQ <- Date_Df$YYYYQ
      
      Df <-
        Df %>% select(length(Df), 1:{
          length(Df) - 1
        }) %>% tidyr::gather(key = Series_Name, value = value, -YYYYQ) %>%
        dplyr::left_join(Mapping_Table, by = "Series_Name") %>% select(-Series_Name)
      
      Output_Data <- Df
      
      Output_Data$Yr_Qtr <- Output_Data$YYYYQ %>%
        zoo::as.yearqtr() %>% #requires data to be in yearqtr class
        zoo::yearqtr() #converts date to yearqtr format
      
      # Adding in Column that identifies History vs. Forecast
      Output_Data <- Output_Data %>%
        tidyr::separate(Yr_Qtr,
                        c("Year", "Quarter"),
                        sep = " ",
                        remove = F) %>%
        dplyr::transmute(
          YYYYQ,
          Source,
          Extract_Date,
          Scenario,
          Region,
          Series_ID,
          value,
          Year = as.numeric(Year),
          Quarter
        ) %>%
        dplyr::mutate(FY = ifelse(Quarter %in% c("Q1", "Q2"), Year, Year + 1)) %>% #Creates Financial Year Column
        dplyr::select(-Scenario) %>%
        dplyr::mutate(Scenario = Scenario_name) #For Financial Years less than or equal to
      #last year of hist (specified in section #3.0), mark as History or else mark as Forecast
    }
    
    # 9.0 Aggregating to FY ---------------------------------------------------
    {
      Output_Data <-
        Output_Data %>% dplyr::left_join(
          list.of.indicators %>%
            dplyr::select(Series_ID, Aggregation_Rule) %>%
            unique(),
          by = "Series_ID"
        )
      
      Output_Data_FY_Sum <-
        Output_Data %>% dplyr::filter(Aggregation_Rule == "Sum") %>%
        dplyr::select(-Year, -Quarter, -Aggregation_Rule) %>%
        dplyr::group_by(Source, Extract_Date, Region, Series_ID, Scenario, FY) %>%
        dplyr::summarise(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup()
      
      Output_Data_FY_Max <-
        Output_Data %>% dplyr::filter(Aggregation_Rule == "Max") %>%
        dplyr::filter(Quarter == "Q2") %>%
        dplyr::select(-Year, -Quarter, -Aggregation_Rule, -YYYYQ) %>%
        dplyr::transmute(Source,
                         Extract_Date,
                         Region,
                         Series_ID,
                         value = value / 1000,
                         FY,
                         Scenario)
      
      Output_Data_FY <- Output_Data_FY_Sum %>%
        dplyr::bind_rows(Output_Data_FY_Max)
      
    }
    
    # 10.0 Map Metadata to FY Output File -------------------------------------
    {
      Output_Data_FY <-
        Output_Data_FY %>% left_join(meta_data_df, by = c("Series_ID" = "MNEMONIC")) %>%
        dplyr::select(-Source, -Extract_Date) %>%
        dplyr::filter(FY > st_yr) %>%
        dplyr::transmute(
          MPE_DATE = FY,
          STATE = Region,
          RELEASE_VERSION = release_version,
          SCENARIO_VALUE = Scenario,
          ATTRIBUTE = Attribute,
          UNIT = Unit,
          VALUE = value,
          DESC = Comments,
          STATUS = Scenario_status,
          Series_ID = Series_ID
        ) %>%
        dplyr::mutate(TEMPORAL = "Annual") %>%
        dplyr::arrange(RELEASE_VERSION,
                       STATE,
                       ATTRIBUTE,
                       MPE_DATE,
                       SCENARIO_VALUE) %>%
        as.data.frame()
      
      Output_Data_FY[(Output_Data_FY$STATE == "AUS") &
                       (Output_Data_FY$ATTRIBUTE == "Gross State Product"), "ATTRIBUTE"] <-
        "Gross Domestic Product"
    }
    
    # 11.0 Extract, Wrangle & Aggregate Macro Data to FY ----------------------
    {
      AID_db_Macro <- AID_db[[1]] %>%
        dplyr::filter(
          variable %in% (
            list.of.indicators %>%
              dplyr::filter(Database == "AID") %>%
              dplyr::filter(Sector == "MACRO") %>%
              dplyr::select(ID) %>%
              as.vector() %>%
              t()
          )
        ) %>%
        dplyr::mutate(Division = "MACRO") %>%
        dplyr::filter(lubridate::year(Dates) >= st_yr) %>%
        dplyr::filter(lubridate::year(Dates) <= end_yr) %>%
        as.data.frame() %>%
        dplyr::transmute(
          YYYYQ = Dates,
          Series = variable,
          value = value,
          Division = Division,
          Source = AID_db_filename,
          Extract_Date = Sys.Date(),
          Scenario = Scenario_name,
          Region = "AUS",
          Series_ID = Series
        )
      
      #Replace '_MACRO' with blank - returns class list
      AID_db_Macro$Series_ID <- AID_db_Macro$Series_ID %>%
        lapply(function(Series_ID)
          gsub("_MACRO", "", Series_ID)) %>% #Replace _STATES with blank - returns class list
        unlist() #converts list to character vector of all the elements
      
      Output_Data_Macro <- AID_db_Macro
      
      Mapping_Table <-
        Output_Data_Macro %>% dplyr::mutate(Series_Name = paste0(Series_ID, Region)) %>% select(-YYYYQ, -value) %>% unique()
      
      Date_Df <-
        data.frame(YYYYQ = seq(
          as.Date("1999/3/1"),
          by = "quarter",
          length.out = (as.numeric(end_yr) - as.numeric(st_yr)) * 4 + 2
        )) %>%
        dplyr::left_join(
          Output_Data_Macro %>% dplyr::mutate(Series_Name = paste0(Series_ID, Region)) %>%
            dplyr::select(-Series, -Division:-Series_ID) %>%
            tidyr::spread(key = Series_Name, value = value),
          by = c("YYYYQ")
        )
      
      Df_CPI <-
        sapply(Date_Df %>% select(-YYYYQ, -RXDAUS), FUN = project_forward_YoY) %>% unlist() %>% as.data.frame()
      Df_RXD <-
        sapply(Date_Df %>% select(-YYYYQ, -CPIAUS), FUN = project_forward_Equal) %>% unlist() %>% as.data.frame()
      
      Df_CPI$YYYYQ <- Date_Df$YYYYQ
      Df_RXD$YYYYQ <- Date_Df$YYYYQ
      
      Df <- Df_CPI %>% dplyr::left_join(Df_RXD, by = c("YYYYQ")) %>%
        dplyr::select(3, 1:2) %>%
        tidyr::gather(key = Series_Name, value = value, -YYYYQ) %>%
        dplyr::left_join(Mapping_Table, by = "Series_Name") %>% select(-Series_Name)
      
      Output_Data_Macro <- Df
      
      Output_Data_Macro$Yr_Qtr <- Output_Data_Macro$YYYYQ %>%
        zoo::as.yearqtr() %>% #requires data to be in yearqtr class
        zoo::yearqtr() #converts date to yearqtr format
      
      Output_Data_Macro <- Output_Data_Macro %>%
        tidyr::separate(Yr_Qtr,
                        c("Year", "Quarter"),
                        sep = " ",
                        remove = F) %>%
        dplyr::transmute(
          YYYYQ,
          Division,
          Source,
          Extract_Date,
          Scenario,
          Region,
          Series_ID,
          value,
          Year = as.numeric(Year),
          Quarter
        ) %>%
        dplyr::mutate(FY = ifelse(Quarter %in% c("Q1", "Q2"), Year, Year + 1)) %>%
        tidyr::spread(key = Series_ID, value = value) %>%
        dplyr::mutate(RXD_AUD = RXD) %>%
        dplyr::select(-RXD) %>%
        dplyr::rename(RXD = RXD_AUD) %>%
        tidyr::gather(key = Series_ID,
                      value = value,
                      -(YYYYQ:Region),
                      -Year,
                      -Quarter,
                      -FY) %>%
        dplyr::select(-Scenario) %>%
        dplyr::mutate(Scenario = Scenario_name) %>%
        #Aggregating to FY (Avg.)
        dplyr::left_join(
          list.of.indicators %>%
            dplyr::select(Series_ID, Aggregation_Rule) %>%
            unique(),
          by = "Series_ID"
        ) %>%
        dplyr::select(-Year, -Quarter, -Aggregation_Rule) %>%
        dplyr::group_by(Division,
                        Source,
                        Extract_Date,
                        Region,
                        Series_ID,
                        Scenario,
                        FY) %>%
        dplyr::summarise(value = mean(value, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(meta_data_df, by = c("Series_ID" = "MNEMONIC")) %>%
        dplyr::select(-Division, -Source, -Extract_Date) %>%
        dplyr::transmute(
          MPE_DATE = FY,
          STATE = Region,
          RELEASE_VERSION = release_version,
          SCENARIO_VALUE = Scenario,
          ATTRIBUTE = Attribute,
          UNIT = Unit,
          VALUE = value,
          DESC = Comments,
          STATUS = Scenario_status,
          Series_ID = Series_ID
        ) %>%
        dplyr::mutate(FORECAST_FLAG = "",
                      TEMPORAL = "Annual") %>%
        dplyr::arrange(RELEASE_VERSION,
                       STATE,
                       ATTRIBUTE,
                       MPE_DATE,
                       SCENARIO_VALUE) %>%
        dplyr::filter(MPE_DATE > st_yr)
    }
    
    # 12.0 Combine Macro & State Data, and Write Output -----------------------
    {
      Tot_Output_Data_FY <-
        Output_Data_FY %>% bind_rows(Output_Data_Macro)
      Tot_Output_Data_FY <- Tot_Output_Data_FY %>%
        dplyr::select(
          MPE_DATE,
          STATE,
          RELEASE_VERSION,
          STATUS,
          SCENARIO_VALUE,
          TEMPORAL,
          ATTRIBUTE,
          UNIT,
          VALUE,
          DESC,
          FORECAST_FLAG,
          Series_ID
        ) %>%
        dplyr::mutate(
          test_forecast_flag = ifelse(
            MPE_DATE >= st_yr &
              MPE_DATE < est_actual_end_yr,
            "A",
            ifelse(MPE_DATE == est_actual_end_yr, "EA", "F")
          )
        ) %>%
        dplyr::mutate(testcol = paste0(MPE_DATE, "-06-30")) %>%
        dplyr::mutate(MPE_DATE = testcol) %>%
        dplyr::mutate(FORECAST_FLAG = test_forecast_flag) %>%
        dplyr::select(-testcol, -test_forecast_flag)
      
      Tot_Output_Data_FY[is.na(Tot_Output_Data_FY)] <-
        "" #Convert NA to blanks
      
      #write.xlsx(Tot_Output_Data_FY, file.path(fp,"Output_Files_Excel/",paste0(Scenario_name, "_Output_Data_", Sys.Date(), "_.xlsx")))
      
      #write.csv(Tot_Output_Data_FY,file.path(fp,"Output_Files_CSV",paste0(Scenario_name, "_Output_Data_", Sys.Date(), "_.csv")),row.names = FALSE,fileEncoding = "UTF-8")
      
      saveRDS(Tot_Output_Data_FY,
              file = file.path(
                "data processing/RDS Files/",
                paste0(Scenario_name, "_", release_version, "_Output_Data.rds")
              ))
      
      #tidy folder - remove ".sel" & ".csv" files
      file <- list.files()[grepl("file", list.files())]
      file.remove(from = file)
      
    }
    
    end_time <- Sys.time()
    print(difftime(end_time, start_time, units = "secs"))
    
  }
}