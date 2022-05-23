



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
    fluidRow(column(
      6,
      wellPanel(
        style = paste0(
          "border: 5px solid; border-color:",
          oxgraphs::ox_pallette()[9],
          "; margin-bottom: 0.5em"
        ),
        h4(strong("Updating AID Data"),
           uiOutput(ns(
             "aid_data_update_date"
           ))),
        fluidRow(column(4, fluidRow(h4("1.", style = "margin-top:0.2em; margin-left:0.75em"),
          
            shinyDirButton(
              ns("aid_directory"),
              "Select Folder",
              "Please select a folder",
              buttonType = 'md',
              style = paste0(
                "background-color:",
                oxgraphs::ox_pallette()[2],
                "; color: white; margin-left:1.25em"
              )
            ),
            h4("or", style = "margin-top:0.2em; margin-left:0.5em; margin-right:0.5em"),
            
            shinyFilesButton(
              ns("aid_file"),
              "Select File",
              "Please select a folder",
              multiple = F,
              buttonType = 'md',
              style = paste0(
                "background-color:",
                oxgraphs::ox_pallette()[2],
                "; color: white"
              )
            ))),
          column(8,h4(verbatimTextOutput(ns("aid_directorypath")), style = "margin-top:0em"))
        ),
        fluidRow(column(4, fluidRow(h4("2.", style = "margin-top:0.2em; margin-left:0.75em; margin-right:0.75em"),
                                                                         actionButton(
                                                                           ns("aid_process_folder_file"),
                                                                           label = "Process Folder/File",
                                                                           style = paste0(
                                                                             "background-color:",
                                                                             oxgraphs::ox_pallette()[2],
                                                                             "; color: white"
                                                                           )
                                                                         ))),
                 column(8,h4(verbatimTextOutput(ns("aid_data_processed")), style = "margin-top:0em"))),
        br(),
        fluidRow(column(4, fluidRow(h4("3.", style = "margin-top:0.2em; margin-left:0.75em; margin-right:0.75em"),
                                                                         actionButton(
                                                                           ns("aid_refresh"),
                                                                           label = "Refresh",
                                                                           style = paste0(
                                                                             "background-color:",
                                                                             oxgraphs::ox_pallette()[2],
                                                                             "; color: white"
                                                                           )
                                                                         ))))
      ) 
    )),
    fluidRow(column(3, h4(
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
      volumes <- getVolumes()()
      shinyDirChoose(
        input,
        "aid_directory",
        roots = volumes,
        session = session,
        restrictions = system.file(package = "base"),
        allowDirCreate = FALSE
      )
      
      shinyFileChoose(
        input,
        "aid_file",
        roots = volumes,
        session = session,
        restrictions = system.file(package = "base")
      )
      
      output$aid_directorypath <- renderPrint({
        if (is.integer(input$aid_directory) & is.integer(input$aid_file)) {
          cat("No folder/file has been selected")
        } else if (!(is.integer(input$aid_directory))) {
          file.path(parseDirPath(volumes, input$aid_directory), "/")
        } else if (!(is.integer(input$aid_file))) {
          file.path(parseFilePaths(volumes, input$aid_file))[4]
        }
      })
      
    })
    
    observeEvent(input$aid_process_folder_file, {
      source("data processing/DataProcessing_AEMO_input_directory.R",
                      local = TRUE)
      output$aid_data_processed <-
        renderPrint({
          "Data process complete"
        })
    })
    
    
    
    observeEvent(input$aid_refresh, {
      source("data/refresh_AID_data.R", local = TRUE)
      session$reload()
    })
    
    observe({
      output$aid_data_update_date <-
        renderUI({
          h5(strong("AID Data Last Updated: "), paste0(format(
            file.info("data/Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p"
          )))
        })
    })
    
    source("data/import_data.R", local = TRUE)
    return(data)
  })
}
