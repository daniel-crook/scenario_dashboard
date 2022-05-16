rm(list = ls())

library(shiny)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(oxgraphs)

# 0.0 Data Processing -----------------------------------------------------

#### source data processing script

# 1.0 Static Elements -----------------------------------------------------

select <- data.frame(
    SCENARIO_VALUE = c(
        "CENTRAL",
        "EXPORT_SUPERPOWER",
        "SUSTAINABLE_GROWTH",
        "SLOW_CHANGE",
        "RAPID_DECARB"
    )
)

my_theme <- bs_theme(bootswatch = "spacelab",
                     primary = ox_pallette()[2],
                     info = ox_pallette()[2],
                     font_scale = 1.0)

# 2.0 Import Functions ----------------------------------------------------

lapply(list.files(
    path = "functions/",
    full.names = TRUE,
    pattern = "*.R"
),
source)

# 3.0 Import Modules ------------------------------------------------------

lapply(list.files(
    path = "modules/",
    full.names = TRUE,
    pattern = "*.R"
),
source)

# 4.0 UI ------------------------------------------------------------------

ui <- tagList(
    tags$head(tags$script(type = "text/javascript", src = "code.js")),
    navbarPage(
        title = div(img(src = 'AEMO logo.png', style = "margin-top: 0px",
                        height = 75)),
        windowTitle = "Scenario Dashboard",
        position = "static-top",
        theme = my_theme,
        tabPanel(
            h5(strong("Variable Comparison")),
            tabsetPanel(
                type = "pills",
                vc_region_ui("vc_region", "By Region"),
                tabPanel("VC - By Version"),
                tabPanel("VC - By Scenario")
                )
        ),
        tabPanel(
           h5(strong("State Composition")),
            tabsetPanel(
                type = "pills",
                tabPanel("SC - By Region"),
                tabPanel("SC - By Version"),
                tabPanel("SC - By Scenario")
            )
        ),
        tabPanel(
            h5(strong("Industry Comparison")),
            tabsetPanel(
                type = "pills",
                tabPanel("IC - By Region"),
                tabPanel("IC - By Version"),
                tabPanel("IC - By Scenario")
            )
        ),
        tabPanel(
            h5(strong("Additional Checks")),
            tabsetPanel(
                type = "pills",
                tabPanel("IC - Energy Mix/Intensity"),
                tabPanel("IC - GVA Breakdown"),
                tabPanel("IC - % of GDP")
                )
        )
    )
)

# 5.0 Server --------------------------------------------------------------

server <- function(input, output, session) {
    callModule(vc_region_server, "vc_region")
}

# 6.0 Run App -------------------------------------------------------------

shinyApp(ui = ui, server = server)
