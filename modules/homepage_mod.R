




# 1.0 Module UI -----------------------------------------------------------

homepage_ui <- function(id) {
  ns = NS(id)
  tabPanel(
    "Homepage",
    titlePanel(h1(
      strong("Welcome to the Scenario Dashboard")
    )),
    
    mainPanel(
      fluidRow(
        h4(
          "The dashboard has been designed to be used during scenario development, allowing for several comparisons between databases to be conducted (by version, by scenario or by region).
        The dashboard also includes several off-platform calculated series for further checking of forecasts as well as CAGR/Period average calculations that may be useful during report writing."
        )
      ),
      fluidRow(column(3, h4(
        strong("AEMO Data Last Updated: "),
        br(),
        format(file.info("data/Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p")
      )),
      column(3, h4(
        strong("GEM Data Last Updated: "),
        br(),
        format(file.info("data/GEM_Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p")
      )),
      column(3, h4(
        strong("GEM Data Last Updated: "),
        br(),
        format(file.info("data/GEM_Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p")
      )),
      column(3, h4(
        strong("GEM Data Last Updated: "),
        br(),
        format(file.info("data/GEM_Dashboard_Data.rds")$mtime, format = "%d %b %Y %I:%M %p")
      ))),
      h4(
        strong("Note:"),
        " i haven't managed to get the buttons to work on this page, for will have to ",
        strong("use the navbar for navigation"),
        " for the time being."
      )
    ),
    fluidRow(
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; border-color:",
                 oxgraphs::ox_pallette()[1],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(
                 9,
                 actionButton(
                   inputId = "vc_button",
                   h5(strong("Variable Comparison")),
                   style = paste0(
                     "background-color:",
                     oxgraphs::ox_pallette()[1],
                     "; color: white"
                   )
                 )
               ))
             )),
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; border-color:",
                 oxgraphs::ox_pallette()[2],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(
                 9, actionButton(
                   "sc_button",
                   h5(strong("State Composition")),
                   style = paste0(
                     "background-color:",
                     oxgraphs::ox_pallette()[2],
                     "; color: white"
                   )
                 )
               ))
             )),
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; border-color:",
                 oxgraphs::ox_pallette()[3],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(
                 9, actionButton(
                   "ic_button",
                   h5(strong("Industry Comparison")),
                   style = paste0(
                     "background-color:",
                     oxgraphs::ox_pallette()[3],
                     "; color: white"
                   )
                 )
               ))
             )),
      column(3,
             wellPanel(
               style = paste0(
                 "border: 5px solid; border-color:",
                 oxgraphs::ox_pallette()[5],
                 "; margin-bottom: 0.5em"
               ),
               fluidRow(column(
                 9, actionButton(
                   "gc_button",
                   h5(strong("GEM Checks")),
                   style = paste0(
                     "background-color:",
                     oxgraphs::ox_pallette()[5],
                     "; color: white"
                   )
                 )
               ))
             ))
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

homepage_server <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    # Variable Comparison Button ----------------------------------------------
    observeEvent(input$vc_button, {
      updateTabsetPanel(
        session = parentSession,
        inputId = input$navbar,
        selected = h5(strong("Variable Comparison"))
      )
    })
    
    observeEvent(input$sc_button, {
      updateNavbarPage(
        session = parentSession,
        inputId = input$navbar,
        selected = h5(strong("State Composition"))
      )
    })
    
    observeEvent(input$ic_button, {
      updateNavbarPage(
        session = parentSession,
        inputId = input$navbar,
        selected = h5(strong("Industry Comparison"))
      )
    })
    
    observeEvent(input$gc_button, {
      updateNavbarPage(
        session = parentSession,
        inputId = input$navbar,
        selected = h5(strong("GEM_Checks"))
      )
    })
    
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
