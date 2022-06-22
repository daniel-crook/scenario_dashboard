
#setwd("S:/BIS Consultancy/Product Development/Scenarios_Dashboard/Dashboard/")
  
scenario_table <- fread("data processing/Input Files/Scenarios_Table.csv", encoding = "UTF-8")
# scenario_table$`Climate Warming settings` <- gsub("â","",scenario_table$`Climate Warming settings`)
# return(scenario_table)