
# 1.0 Module UI -----------------------------------------------------------

settings_ui <- function(id) {
  ns = NS(id)
  tabPanel("Homepage",
           fluidRow(
             column(
               1,
               shinyFilesButton(
                 ns("company_logo"),
                 "Select New Logo",
                 "Please select a Company logo image file",
                 multiple = F,
                 buttonType = 'md',
                 style = paste0(
                   "background-color:",
                   oxgraphs::ox_pallette()[3],
                   "; color: white"
                 )
               )
             ),
             column(5, h4(
               verbatimTextOutput(ns("company_logopath"), placeholder = TRUE), style = "margin-top:0em"
             )),
             column(
               3,
               uiOutput(ns("refresh_logo"))
             )
           ))
}

# 2.0 Module Server -------------------------------------------------------

settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
   
     ns = NS(id)
    volumes <- getVolumes()()
    
    # Company Logo Button -----------------------------------------------------
    observe({
      shinyFileChoose(
        input,
        "company_logo",
        roots = volumes,
        session = session,
        filetypes = c('png'),
        restrictions = system.file(package = "base")
      )
      
      output$company_logopath <- renderPrint({
        if (is.integer(input$company_logo)) {
          cat("Please select a png file")
        } else if (!(is.integer(input$company_logo))) {
          file.path(parseFilePaths(volumes, input$company_logo))[4]
        }
      })
      
      if (!(is.integer(input$company_logo))) {
        output$refresh_logo <- renderUI({
          actionButton(
            ns("ch_logo_and_refresh"),
            label = "Change Logo & Refresh",
            style = paste0(
              "background-color:",
              oxgraphs::ox_pallette()[3],
              "; color: white"
            )
          )
        })
      } else {
        output$refresh_logo <- renderUI({
          NULL
        })
      }
      
    })
    
    # Change Logo & Refresh ---------------------------------------------------
    
    observeEvent(input$ch_logo_and_refresh, {
      company_logo_path <-
        file.path(parseFilePaths(volumes, input$company_logo))[4]
      
      file.copy(from = company_logo_path,
                to = "www/company_logo.png",
                overwrite = T)
      
      session$reload()
    })
    
  })
}
