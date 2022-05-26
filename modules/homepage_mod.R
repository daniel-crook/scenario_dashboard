

# 1.0 Module UI -----------------------------------------------------------

homepage_ui <- function(id) {
  ns = NS(id)
  tabPanel(
    "Homepage",
    titlePanel(h1(
      strong("Welcome to the Scenario Dashboard")
    )),
    # h5(
    #   strong("Software Files Last Updated: "),
    #   paste0(format(with_tz(as.data.frame(
    #     commits(repo = getActiveDocumentContext()$path[1]))$when, tzone = "Australia/Sydney"),
    #     format = "%d %b %Y %I:%M %p"
    #   ))
    # ),
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
        h4(strong("Steps to update AID data"),
           uiOutput(ns(
             "aid_data_update_date"
           )), style = "margin-top: -0.5em"),
        fluidRow(column(
          4, fluidRow(
            h4("1.", style = "margin-top:0.2em; margin-left:0.75em"),
            
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
              "Please select a file",
              multiple = F,
              buttonType = 'md',
              style = paste0(
                "background-color:",
                oxgraphs::ox_pallette()[2],
                "; color: white"
              )
            )
          )
        ),
        column(
          8, h4(verbatimTextOutput(ns(
            "aid_directorypath"
          ), placeholder = TRUE), style = "margin-top:0em")
        )),
        fluidRow(column(4, fluidRow(
          h4("2.", style = "margin-top:0.2em; margin-left:0.75em; margin-right:0.75em"),
          actionButton(
            ns("aid_process_folder_file"),
            label = "Process Folder/File",
            style = paste0(
              "background-color:",
              oxgraphs::ox_pallette()[2],
              "; color: white"
            )
          )
        )),
        column(
          8, h4(verbatimTextOutput(
            ns("aid_data_processed"), placeholder = TRUE
          ), style = "margin-top:0em")
        )),
        fluidRow(column(4, fluidRow(
          h4("3.", style = "margin-top:0.2em; margin-left:0.75em; margin-right:0.75em"),
          actionButton(
            ns("aid_refresh"),
            label = "Refresh",
            style = paste0(
              "background-color:",
              oxgraphs::ox_pallette()[2],
              "; color: white"
            )
          )
        )),
        column(
          8, h4(verbatimTextOutput(
            ns("aid_data_refreshed"), placeholder = TRUE
          ), style = "margin-top:0em")
        )),
        h4(strong("AID data is used in:")),
        fluidRow(column(
          6, wellPanel(
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
              "General comparison of level & growth forecasts of variables across scenarios: by version, scenario or region.",
              style = paste0(
                "background-color:",
                oxgraphs::ox_pallette()[1],
                "; color: white"
              )
            )
          )
        ),
        column(
          6,
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
          )
        )),
        fluidRow(column(
          6, wellPanel(
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
          )
        ),
        column(
          6, wellPanel(
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
          )
        )),
      )
    ),
    column(
      6,
      wellPanel(
        style = paste0(
          "border: 5px solid; border-color:",
          oxgraphs::ox_pallette()[9],
          "; margin-bottom: 0.5em"
        ),
        h4(strong("Steps to update GEM data"),
           uiOutput(ns(
             "gem_data_update_date"
           )), style = "margin-top: -0.5em"),
        fluidRow(column(
          4, fluidRow(
            h4("1.", style = "margin-top:0.2em; margin-left:0.75em"),
            
            shinyDirButton(
              ns("gem_directory"),
              "Select Folder",
              "Please select a folder",
              buttonType = 'md',
              style = paste0(
                "background-color:",
                oxgraphs::ox_pallette()[1],
                "; color: white; margin-left:1.25em"
              )
            ),
            h4("or", style = "margin-top:0.2em; margin-left:0.5em; margin-right:0.5em"),
            
            shinyFilesButton(
              ns("gem_file"),
              "Select File",
              "Please select a file",
              multiple = F,
              buttonType = 'md',
              style = paste0(
                "background-color:",
                oxgraphs::ox_pallette()[1],
                "; color: white"
              )
            )
          )
        ),
        column(
          8, h4(verbatimTextOutput(ns(
            "gem_directorypath"
          ), placeholder = TRUE), style = "margin-top:0em")
        )),
        fluidRow(column(4, fluidRow(
          h4("2.", style = "margin-top:0.2em; margin-left:0.75em; margin-right:0.75em"),
          actionButton(
            ns("gem_process_folder_file"),
            label = "Process Folder/File",
            style = paste0(
              "background-color:",
              oxgraphs::ox_pallette()[1],
              "; color: white"
            )
          )
        )),
        column(
          8, h4(verbatimTextOutput(
            ns("gem_data_processed"), placeholder = TRUE
          ), style = "margin-top:0em")
        )),
        fluidRow(column(4, fluidRow(
          h4("3.", style = "margin-top:0.2em; margin-left:0.75em; margin-right:0.75em"),
          actionButton(
            ns("gem_refresh"),
            label = "Refresh",
            style = paste0(
              "background-color:",
              oxgraphs::ox_pallette()[1],
              "; color: white"
            )
          )
        )),column(
          8, h4(verbatimTextOutput(
            ns("gem_data_refreshed"), placeholder = TRUE
          ), style = "margin-top:0em")
        )),
        h4(strong("GEM data is used in:")),
        fluidRow(column(
          6, wellPanel(
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
          )
        ))
      )
    )),
    fluidRow(column(3, h4(
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
    )))
  )
}

# 2.0 Module Server -------------------------------------------------------

homepage_server <- function(id) {
  moduleServer(id, function(input, output, session, data) {
    # Scenario Description Table ----------------------------------------------
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
    
    # Shiny AID Directory/File Select -----------------------------------------
    
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
          cat("Please select a folder/file")
        } else if (!(is.integer(input$aid_directory))) {
          file.path(parseDirPath(volumes, input$aid_directory), "/")
        } else if (!(is.integer(input$aid_file))) {
          file.path(parseFilePaths(volumes, input$aid_file))[4]
        }
      })
      
    })
    
    # Shiny GEM Directory/File Select -----------------------------------------
    
    observe({
      volumes <- getVolumes()()
      shinyDirChoose(
        input,
        "gem_directory",
        roots = volumes,
        session = session,
        restrictions = system.file(package = "base"),
        allowDirCreate = FALSE
      )
      
      shinyFileChoose(
        input,
        "gem_file",
        roots = volumes,
        session = session,
        restrictions = system.file(package = "base")
      )
      
      output$gem_directorypath <- renderPrint({
        if (is.integer(input$gem_directory) & is.integer(input$gem_file)) {
          cat("Please select a folder/file")
        } else if (!(is.integer(input$gem_directory))) {
          file.path(parseDirPath(volumes, input$gem_directory), "/")
        } else if (!(is.integer(input$gem_file))) {
          file.path(parseFilePaths(volumes, input$gem_file))[4]
        }
      })
      
    })
    
    # Process AID db ----------------------------------------------------------
    
    observeEvent(input$aid_process_folder_file, {
      source("data processing/DataProcessing_AEMO_input_directory.R",
             local = TRUE)
      output$aid_data_processed <-
        renderPrint({
          "Data process complete"
        })
    })
    
    # Refresh AID Button ------------------------------------------------------
    
    observeEvent(input$aid_refresh, {
      output$aid_data_refreshed <-
        renderPrint({
          "Refreshing Dashboard..."
        })
      source("data/refresh_AID_data.R", local = TRUE)
      session$reload()
    })
    
    # AID data last updated output --------------------------------------------
    
    observe({
      output$aid_data_update_date <-
        renderUI({
          h5(strong("AID Data Last Updated: "), paste0(format(
            file.info("data/Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p"
          )))
        })
    })
    
    # AID data last updated output --------------------------------------------
    
    observe({
      output$gem_data_update_date <-
        renderUI({
          h5(strong("GEM Data Last Updated: "), paste0(format(
            file.info("data/GEM_Dashboard_Data.rds")$mtime,
            format = "%d %b %Y %I:%M %p"
          )))
        })
    })
    
    # Source Data -------------------------------------------------------------
    source("data/import_data.R", local = TRUE)
    return(data)
  })
}
