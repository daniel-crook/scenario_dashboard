
# 1.0 Module UI -----------------------------------------------------------

vc_region_ui <- function(id, title) {
  ns = NS(id)
  tabPanel(title = title,
           sidebarLayout(sidebarPanel(
             selectInput(
               ns("Scenario"),
               label = h4("Scenario"),
               c(Scenario = '', select$SCENARIO_VALUE),
               selected = select$SCENARIO_VALUE[1]
             ),
             selectInput(
               ns("Scenario2"),
               label = h4("Scenario 2"),
               c(Scenario = '', select$SCENARIO_VALUE),
               selected = select$SCENARIO_VALUE[2]
             ),
             prettyCheckboxGroup(
               ns("checkGroup"),
               label = h3("Select Variable(s):"),
               choices = NULL,
               selected = NULL,
               shape = "round", 
               outline = TRUE,  
               status = "primary"
             )
           ),
           mainPanel()))
}

# 2.0 Module Server -------------------------------------------------------

vc_region_server <- function(input, output, session) {
  observe({
    updatePrettyCheckboxGroup(
      session,
      "checkGroup",
      label = NULL,
      paste0(input$Scenario, ", ", input$Scenario2),
      selected = paste0(input$Scenario, ", ", input$Scenario2),
      prettyOptions = list(shape = "round", outline = TRUE, status = "primary")
    )
  })
}

# 3.0 Test Module ---------------------------------------------------------

# vc_region_demo <- function() {
# 
#   select <- data.frame(SCENARIO_VALUE = c("CENTRAL","EXPORT_SUPERPOWER","SUSTAINABLE_GROWTH","RAPID_DECARB"))
#   ui <- navbarPage("Module Demo",
#                    vc_region_ui("Version", "By Region"))
#   server <- function(input, output, session) {
#     callModule(vc_region_server,"Version")
#     }
#   shinyApp(ui, server)
# }
# 
# vc_region_demo()
