
rm(list = ls())

list.of.packages <-
    c(
        "shiny",
        "bslib",
        "htmltools",
        "shinyWidgets",
        "stringi",
        "stringr",
        "oxgraphs",
        "plotly",
        "DT",
        "strex",
        "data.table",
        "kableExtra",
        "shinyFiles",
        "shinyjs",
        "waiter",
        "shinyFeedback"
    )

new.packages <- list.of.packages[!(list.of.packages %in%
                                       installed.packages()[, "Package"])]
if (length(new.packages)) {
    install.packages(new.packages)
}
invisible(lapply(list.of.packages, require, character.only = TRUE))

ox_setup()

fp <- dirname(getActiveDocumentContext()$path)

setwd(fp)

# 0.0 Data Processing -----------------------------------------------------

#source("data processing/DataProcessing_AEMO.R")

# 0.1 Import Functions ----------------------------------------------------

source("functions/functions.R")

# 0.2 Import Processed AID Data -------------------------------------------

source("data/import_data.R")

# 0.3 Import GEM Data -----------------------------------------------------

source("data/import_data_gem.R")

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
    useWaiter(),
    waiterPreloader(html = spin_folding_cube()),
    navbarPage(
        title = div(img(
            src = 'company_logo.png',
            style = "margin-top: 0px",
            height = "75px"
        )),
        id = "navbar",
        windowTitle = "Scenario Dashboard",
        position = "static-top",
        theme = my_theme,
        tabPanel(title = h4(strong(icon("fas fa-home"), "Home")),
                 homepage_ui("homepage")),
        navbarMenu(title = h4(strong("Austalia In Detail")),
        tabPanel(
            h5(icon("fas fa-chart-line"), strong("Variable Comparison")),
            tabsetPanel(
                type = "pills",
                vc_version_ui("vc_version"),
                vc_scenario_ui("vc_scenario"),
                vc_region_ui("vc_region")
            )
        ),
        tabPanel(
            h5(icon("fas fa-map-marked-alt"), strong("State Composition")),
            tabsetPanel(
                type = "pills",
                sc_version_ui("sc_version"),
                sc_scenario_ui("sc_scenario"),
                sc_region_ui("sc_region")
            )
        ),
        tabPanel(
            h5(icon("fas fa-industry"), strong("Industry Comparison")),
            tabsetPanel(
                type = "pills",
                ic_version_ui("ic_version"),
                ic_scenario_ui("ic_scenario"),
                ic_region_ui("ic_region"),
                ic_industry_ui("ic_industry")
            )
        ),
        tabPanel(
            h5(icon("fas fa-user"), strong("Demographics")),
            tabsetPanel(
                type = "pills",
                d_region_breakdown_ui("d_region_breakdown"),
                d_total_increase_by_region_ui("d_total_increase_by_region")
            )
        )),
        navbarMenu(title = h4(strong("Global Economic Model")),
        tabPanel(
            h5(icon("fas fa-globe-asia"), strong("GEM Checks")),
            tabsetPanel(
                type = "pills",
                gc_version_ui("gc_version"),
                # gc_scenario_ui("gc_scenario"),
                tabPanel("AC - % of GDP")
            )
        )),
        
        tabPanel(
          h4(strong(icon("fas fa-cog"), "Settings")),
          settings_ui("settings")
        )
    )
)

# 4.0 Server --------------------------------------------------------------

server <- function(input, output, session) {
    
    homepage_server("homepage")
    
    vc_version_server("vc_version", data)
    vc_scenario_server("vc_scenario", data)
    vc_region_server("vc_region", data)
    
    sc_version_server("sc_version", data)
    sc_scenario_server("sc_scenario", data)
    sc_region_server("sc_region", data)
    
    ic_version_server("ic_version", data)
    ic_scenario_server("ic_scenario", data)
    ic_region_server("ic_region", data)
    ic_industry_server("ic_industry", data)
    
    d_region_breakdown_server("d_region_breakdown", data)
    d_total_increase_by_region_server("d_total_increase_by_region", data)
    
    gc_version_server("gc_version", gem_data)
    # gc_scenario_server("gc_scenario", gem_data)
    
    settings_server("settings")
}

# 5.0 Run App -------------------------------------------------------------

shinyApp(
    ui = ui,
    server = server,
    options = list(launch.browser = TRUE)
)

