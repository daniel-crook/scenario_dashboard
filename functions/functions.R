
## -- function that removes the Australi name from the end of GEM data -- ##
clean.names <- function(x) {
  x$variable <- gsub("_AUSTRALI", "", x$variable)
  return(x)
}

## -- function that sorts the version_list in desc order -- ##
version.sort <- function(x) {
  version_list <-
    version_list %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
  
  version_list$Release_Date <-
    parse_date_time(version_list$Release_Date, "my")
  
  version_list <-
    arrange(version_list, desc(Release_Date), desc(Version)) %>%
    mutate(., RELEASE_VERSION = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
    select(-Release_Date, -Version)
}

## -- function that adds a concat variable column for the comparison tab -- ##
add.var.col <- function(x) {
  mutate(x,
         variable = paste(ATTRIBUTE,
                          STATE,
                          SCENARIO_VALUE,
                          RELEASE_VERSION,
                          sep = ", "))
}

## -- function that adds a concat variable column for the state comp tab -- ##
add.sc.col <- function(x) {
  mutate(x,
         sc_variable = paste(Series_ID,
                             SCENARIO_VALUE,
                             RELEASE_VERSION,
                             sep = ", "))
}

## -- function that adds a concat variable column for the industry comp tab -- ##
add.ic.col <- function(x) {
  mutate(x,
         ic_variable = paste("GVA - Total",
                             STATE,
                             SCENARIO_VALUE,
                             RELEASE_VERSION,
                             sep = ", "))
}

## -- function that adds a concat variable column for the industry comp tab -- ##
add.ic.fy.col <- function(x) {
  mutate(x, ic_fy_variable = paste(
    STATE,
    SCENARIO_VALUE,
    RELEASE_VERSION,
    sep = ", "
  ))
}

## -- function that adds a concat variable column for the gem checks tab -- ##
add.var.col.gem <- function(x) {
  mutate(x,
         variable = paste(Attribute,
                          Scenario,
                          Release,
                          sep = ", "))
}

### --- forecast line function --- ###
vline <- function(x) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = "#495057")
  )
}

custom.min <- function(x) {
  if (length(x) > 0)
    min(x)
  else
    2000
}

custom.max <- function(x) {
  if (length(x) > 0)
    max(x)
  else
    2053
}

ox_pallette <- function () {
  ox_pallette <- c(
    "#BD1B21",
    "#003469",
    "#D1A21E",
    "#7B7C77",
    "#00ADDC",
    "#00793F",
    "#965793",
    "#DE6328",
    "#000000",
    "#FF66FF",
    "#800000"
  )
}
