rm(list = ls())

library(shiny)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(stringi)
library(stringr)
library(oxgraphs)
library(plotly)
library(DT)
library(strex)
library(data.table)
library(kableExtra)
ox_setup()

fp <- dirname(getActiveDocumentContext()$path)


setwd(fp)

# 0.0 Data Processing -----------------------------------------------------

# source("Data Processing/DataProcessing_AEMO.R")

# 0.1 Import Functions ----------------------------------------------------

source("functions/functions.R")

# 0.2 Import Processed AID Data -------------------------------------------

source("data/import_data.R")

# 0.3 Import GEM Data -----------------------------------------------------

source("data/import_data_gem.R")

# 0.3 Import GEM Data -----------------------------------------------------

source("data/import_scenario_table.R")

# 1.0 Static Elements -----------------------------------------------------

my_theme <- bs_theme(
    bootswatch = "lumen",
    primary = ox_pallette()[2],
    info = ox_pallette()[2],
    font_scale = 1.0
)

# 2.0 Import Modules ------------------------------------------------------

lapply(list.files(
    path = "modules/",
    full.names = TRUE,
    pattern = "*.R"
),
source)

# 3.0 UI ------------------------------------------------------------------

ui <-  tagList(
    tags$head(tags$script(type = "text/javascript", src = "code.js")),
    navbarPage(
        title = div(img(
            src = 'AEMO logo.png',
            style = "margin-top: 0px",
            height = 75
        )),
        id = "navbar",
        windowTitle = "Scenario Dashboard",
        position = "static-top",
        theme = my_theme,
        tabPanel(h5(icon("fas fa-home"), strong("Home")),
                 homepage_ui("homepage")),
        tabPanel(
            h5(strong("Variable Comparison")),
            tabsetPanel(
                type = "pills",
                vc_version_ui("vc_version"),
                vc_scenario_ui("vc_scenario"),
                vc_region_ui("vc_region")
            )
        ),
        tabPanel(
            h5(strong("State Composition")),
            tabsetPanel(
                type = "pills",
                sc_version_ui("sc_version"),
                sc_scenario_ui("sc_scenario"),
                sc_region_ui("sc_region")
            )
        ),
        tabPanel(
            h5(strong("Industry Comparison")),
            tabsetPanel(
                type = "pills",
                ic_version_ui("ic_version"),
                ic_scenario_ui("ic_scenario"),
                ic_region_ui("ic_region"),
                ic_variable_ui("ic_variable")
            )
        ),
        tabPanel(
            h5(strong("GEM Checks")),
            tabsetPanel(
                type = "pills",
                gc_version_ui("gc_version"),
                gc_scenario_ui("gc_scenario"),
                tabPanel("AC - % of GDP")
            )
        )
    )
)

# 4.0 Server --------------------------------------------------------------

server <- function(input, output, session) {
    
    #callModule(homepage_server, "homepage")
    homepage_server("homepage", parentSession = session)
    
    callModule(vc_version_server, "vc_version")
    callModule(vc_scenario_server, "vc_scenario")
    callModule(vc_region_server, "vc_region")
    
    callModule(sc_version_server, "sc_version")
    callModule(sc_scenario_server, "sc_scenario")
    callModule(sc_region_server, "sc_region")
    
    callModule(ic_version_server, "ic_version")
    callModule(ic_scenario_server, "ic_scenario")
    callModule(ic_region_server, "ic_region")
    callModule(ic_variable_server, "ic_variable")
    
    callModule(gc_version_server, "gc_version")
    callModule(gc_scenario_server, "gc_scenario")
    #bs_themer()
}

# 5.0 Run App -------------------------------------------------------------

shinyApp(ui = ui, server = server)
