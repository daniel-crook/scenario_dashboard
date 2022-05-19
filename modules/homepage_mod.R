


# 1.0 Module UI -----------------------------------------------------------

homepage_ui <- function(id) {
  ns = NS(id)
  tabPanel(
    "Homepage",
    titlePanel(h1(
      strong("Welcome to the Scenario Dashboard")
    )),
    h5(
      "The dashboard has been designed to be used during scenario development, allowing for several comparisons between databases to be conducted (by version, by scenario or by region).
      The dashboard also includes several off-platform calculated series for further checking of forecasts as well as CAGR/Period average calculations that may be useful during report writing.
      We will continue to develop this dashboard, gradually generalising it so that it can be easily used across all projects and also by others."
    ),
    fluidRow(column(3, h4(
      strong("AEMO Data Last Updated: "),
      br(),
      uiOutput(ns("AID_data_update_date"))
      #format(file.info("data/Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p")
    )),
    column(3, h4(
      strong("GEM Data Last Updated: "),
      br(),
      format(file.info("data/GEM_Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p")
    )),
    column(3, h4(
      strong("AID Helper File: "),
      br(),
      format(
        file.info("data processing/Input Files/List_of_Indicators.xlsx")$mtime,
        format = "%d %b %Y %I:%M %p"
      )
    )),
    column(3, h4(
      strong("GEM Helper File: "),
      br(),
      format(
        file.info("data processing/List_Of_Indicators_GEM.xlsx")$mtime,
        format = "%d %b %Y %I:%M %p"
      )
    ))),
    h5(
      strong("Note:"),
      " i haven't managed to get the buttons to work on this page, for the time being you will have to ",
      strong("use the navbar for navigation.")
    ),
    fluidRow(
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; background-color:",
                 oxgraphs::ox_pallette()[1],
                 "; border-color:",
                 oxgraphs::ox_pallette()[1],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(10, h4(
                 strong("Variable Comparison"),
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[1],
                   "; color: white"
                 )
               )),
               column(2,
                      h1(
                        icon("fas fa-chart-line"),
                        style = paste0(
                          "background-color:",
                          oxgraphs::ox_pallette()[1],
                          "; color: white; margin-top: 0.1em"
                        )
                      ))),
               h5(
                 "General comparison of AID variables across scenarios: by version, scenario or region.",
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[1],
                   "; color: white"
                 )
               )
             )),
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; background-color:",
                 oxgraphs::ox_pallette()[2],
                 "; border-color:",
                 oxgraphs::ox_pallette()[2],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(10, h4(
                 strong("State Composition"),
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[2],
                   "; color: white"
                 )
               )),
               column(2,
                      h1(
                        icon("fas fa-map-marked-alt"),
                        style = paste0(
                          "background-color:",
                          oxgraphs::ox_pallette()[2],
                          "; color: white; margin-top: 0.1em"
                        )
                      ))),
               h5(
                 "Compare how the scenarios affect the state compositions for a particular variable: by version, scenario or region.",
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[2],
                   "; color: white"
                 )
               )
             )),
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; background-color:",
                 oxgraphs::ox_pallette()[3],
                 "; border-color:",
                 oxgraphs::ox_pallette()[3],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(10, h4(
                 strong("Industry Comparison"),
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[3],
                   "; color: white"
                 )
               )),
               column(2,
                      h1(
                        icon("fas fa-industry"),
                        style = paste0(
                          "background-color:",
                          oxgraphs::ox_pallette()[3],
                          "; color: white; margin-top: 0.1em"
                        )
                      ))),
               h5(
                 "Compare how the scenarios affect an industries GVA share for a region: by version, scenario or region.",
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[3],
                   "; color: white"
                 )
               )
             )),
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; background-color:",
                 oxgraphs::ox_pallette()[4],
                 "; border-color:",
                 oxgraphs::ox_pallette()[4],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(10, h4(
                 strong("Demographics"),
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[4],
                   "; color: white"
                 )
               )),
               column(2,
                      h1(
                        icon("fas fa-user"),
                        style = paste0(
                          "background-color:",
                          oxgraphs::ox_pallette()[4],
                          "; color: white; margin-top: 0.1em"
                        )
                      ))),
               h5(
                 "Compare the demographic outlook across scenarios, with a further breakdown on population growth.",
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[4],
                   "; color: white"
                 )
               )
             )),
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; background-color:",
                 oxgraphs::ox_pallette()[5],
                 "; border-color:",
                 oxgraphs::ox_pallette()[5],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(10, h4(
                 strong("GEM Checks"),
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[5],
                   "; color: white"
                 )
               )),
               column(2,
                      h1(
                        icon("fas fa-globe-asia"),
                        style = paste0(
                          "background-color:",
                          oxgraphs::ox_pallette()[5],
                          "; color: white; margin-top: 0.1em"
                        )
                      ))),
               h5(
                 "General variable of GEM variables across scenarios as well as energy specific charts: by version, scenario or region.",
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[5],
                   "; color: white"
                 )
               )
             )),
    ),
    wellPanel(
      style = paste0(
        "border: 5px solid; border-color:",
        oxgraphs::ox_pallette()[9],
        "; margin-bottom: 0.5em"
      ),
      h3(strong("Scenario Descriptions:")),
      tableOutput(ns("Table"))
    ),
    wellPanel(
      style = paste0(
        "border: 5px solid; border-color:",
        oxgraphs::ox_pallette()[9],
        "; margin-bottom: 0.5em"
      ),
      actionBttn(ns("refresh"),
                 label = "Refresh"),
      shinyDirButton(
        ns("directory"),
        "Folder select",
        "Please select a folder",
        style = paste0(
          "background-color:",
          oxgraphs::ox_pallette()[2],
          "; color: white"
        )
      ),
      verbatimTextOutput(ns("directorypath")),
      #tableOutput(ns("Table"))
    )
  )
}


# 2.0 Module Server -------------------------------------------------------

homepage_server <- function(id) {
  moduleServer(id, function(input, output, session, data) {
    observe({
      output$Table <- function() {
        kbl(scenario_table, format = "html") %>%
          kable_styling(
            bootstrap_options = c("striped", "hover", "condensed"),
            full_width = T
          ) %>%
          column_spec(1, bold = T) %>%
          row_spec(c(0:length(scenario_table$Scenario)), extra_css = "border-bottom: 1px solid; border-top: 1px solid;")
      }
    })
    
    observe({
      volumes <-
        c(
          Local = getVolumes()()[["Windows (C:)"]],
          Home = fs::path_home()
        )
      shinyDirChoose(
        input,
        "directory",
        roots = volumes,
        session = session,
        restrictions = system.file(package = "base"),
        allowDirCreate = FALSE
      )
      if (!(is.integer(input$directory))) {
        source("data processing/DataProcessing_AEMO_input_directory.R",
               local = TRUE)
      }
      
      output$directorypath <- renderPrint({
        if (is.integer(input$directory)) {
          cat("No directory has been selected (shinyDirChoose)")
        } else {
          file.path(parseDirPath(volumes, input$directory), "/")
        }
      })
      
    })
    
    observeEvent(input$refresh, {
      source("data/refresh_AID_data.R", local = TRUE)
      session$reload()
    })
    
    observe({
      output$AID_data_update_date <- renderUI({paste0(format(file.info("data/Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p"))})
    })
    
    source("data/import_data.R", local = TRUE)
    return(data)
  })
}



# 3.0 Test Module ---------------------------------------------------------

# gc_version_demo <- function() {
#
#   select <- data.frame(SCENARIO_VALUE = c("CENTRAL","EXPORT_SUPERPOWER","SUSTAINABLE_GROWTH","RAPID_DECARB"))
#   ui <- navbarPage("Module Demo",
#                    gc_version_ui("Version", "By Region"))
#   server <- function(input, output, session) {
#     callModule(gc_version_server,"Version")
#     }
#   shinyApp(ui, server)
# }
#
# gc_version_demo()
