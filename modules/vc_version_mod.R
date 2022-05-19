
# 1.0 Module UI -----------------------------------------------------------

vc_version_ui <- function(id) {
  ns = NS(id)
  tabPanel("By Version",
           sidebarLayout(
             sidebarPanel(
               chooseSliderSkin(skin = "Shiny", color = ox_pallette()[2]),
               wellPanel(
                 style = paste0(
                   "border: 5px solid; border-color:",
                   oxgraphs::ox_pallette()[2],
                   "; margin-bottom: 0.5em"
                 ),
                 fluidRow(
                   column(2, h4("Display:", style = "margin-top: 0.15em")),
                   column(
                     5,
                     radioGroupButtons(
                       ns("display"),
                       NULL,
                       c("Level Values", "% y/y"),
                       selected = "% y/y",
                       justified = TRUE,
                       status = "primary"
                     )
                   ),
                   column(
                     5,
                     radioGroupButtons(
                       ns("state_national"),
                       NULL,
                       c("State", "National"),
                       justified = TRUE,
                       status = "primary"
                     )
                   ),
                   style = "margin-bottom:-2.0em; margin-top:-0.75em"
                 )
               ),
               wellPanel(
                 style = paste0(
                   "border: 5px solid; border-color:",
                   oxgraphs::ox_pallette()[2],
                   "; margin-bottom: 0.5em"
                 ),
                 
                 fluidRow(column(
                   6,
                   selectInput(
                     ns("Scenario"),
                     label = h4(str_to_title("Scenario"), style = "margin-bottom:-0.1em"),
                     sort(unique(data$SCENARIO_VALUE)),
                     selectize = FALSE
                   )
                 ),
                 column(
                   6,
                   selectInput(
                     ns("State"),
                     label = h4("State", style = "margin-bottom:-0.1em"),
                     sort(unique(data$STATE)),
                     selectize = FALSE
                   )
                 ), style = "margin-bottom:-2em; margin-top:-2em"),
                 h4("Attribute", style = "margin-bottom: 0.2em; margin-top: 1em"),
                 fluidRow(column(
                   12,
                   selectInput(
                     ns("Attribute"),
                     label = NULL,
                     sort(unique(data$ATTRIBUTE)),
                     selectize = FALSE
                   )
                 ),
                 style = "margin-bottom:-2em")
               ),
               wellPanel(
                 style = paste0("border: 5px solid; border-color:",
                                oxgraphs::ox_pallette()[2]),
                 h4(style = "margin-top: -0.5em; margin-bottom: 0.25em", "Select Variables"),
                 fluidRow(column(
                   12,
                   prettyCheckboxGroup(
                     ns("Selections"),
                     label = NULL,
                     choices = unique(data$variable[data$STATE == "ACT" &
                                                      data$SCENARIO_VALUE == "Central" &
                                                      data$ATTRIBUTE == "Attached Dwellings"])[1],
                     selected = unique(data$variable[data$STATE == "ACT" &
                                                       data$SCENARIO_VALUE == "Central" &
                                                       data$ATTRIBUTE == "Attached Dwellings"])[1],
                     shape = "round",
                     outline = TRUE,
                     status = "primary"
                   )
                 )),
                 fluidRow(
                   column(4, h4("Choose Dates: ", style = "margin-top: 0.2em")),
                   column(3,
                          textInput(
                            ns("CAGR_start"),
                            label = NULL,
                            value = "2021"
                          ),),
                   column(1,
                          h4("-", style = "margin-top: 0.2em"),),
                   column(3,
                          textInput(
                            ns("CAGR_end"),
                            label = NULL,
                            value = "2053"
                          ),),
                   style = "margin-bottom: -2em; margin-top: -1em"
                 )
               )
             ),
             mainPanel(plotlyOutput(ns("Plot"), height = '600px'),
                       tableOutput(ns("Table")))
           ))
}

# 2.0 Module Server -------------------------------------------------------

vc_version_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  
# State/National Button Update --------------------------------------------
  observe({
    if (input$state_national == "State") {
      region <- sort(unique(data$STATE[data$STATE != "AUS"]))
      attribute <- sort(unique(data$ATTRIBUTE[data$STATE != "AUS" &
                                                data$SCENARIO_VALUE == input$Scenario]))
    } else {
      region <- sort(unique(data$STATE[data$STATE == "AUS"]))
      attribute <- sort(unique(data$ATTRIBUTE[data$STATE == "AUS" &
                                                data$SCENARIO_VALUE == input$Scenario]))
    }
    
    updateSelectInput(session,
                      "State",
                      label = NULL,
                      region,
                      selected = NULL)
    
    updateSelectInput(session,
                      "Attribute",
                      label = NULL,
                      attribute,
                      selected = NULL)
    
  })
  
# Update checkboxgroup options based on selected inputs -------------------
  observe({
    if (length(data$variable[data$SCENARIO_VALUE == input$Scenario &
                             data$STATE == input$State &
                             data$ATTRIBUTE == input$Attribute]) >= 1) {
    version_list <-
      data.frame(
        ATTRIBUTE = input$Attribute,
        STATE = input$State,
        SCENARIO_VALUE = input$Scenario,
        RELEASE_VERSION = unique(data$RELEASE_VERSION[data$SCENARIO_VALUE == input$Scenario &
                                                        data$ATTRIBUTE == input$Attribute])
      ) %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
    
    version_list$Release_Date <-
      parse_date_time(version_list$Release_Date, "my")
    
    version_list <-
      arrange(version_list, desc(Release_Date), desc(Version)) %>%
      mutate(., RELEASE_VERSION = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
      select(-Release_Date, -Version) %>%
      add.var.col(.)
    
    updatePrettyCheckboxGroup(
      session,
      "Selections",
      label = NULL,
      version_list$variable,
      selected =
        if (length(version_list$variable) >= 3) {
          version_list$variable[1:3]
        } else {
          version_list$variable[1]
        },
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
    }
  })
  
# Render Plot -------------------------------------------------------------
  observe({
    vc_version_data <- filter(data,
                              variable %in% input$Selections) %>%
      transmute(., Dates,
                variable,
                value = round(value, 2))
    if (input$display == "% y/y") {
      vc_version_data <- growth(vc_version_data, 1)
    }
    vc_version_data <-
      mutate(vc_version_data, value = round(value, 2)) %>%
      spread(., variable, value)
    vc_version_data <-
      vc_version_data[, append("Dates", input$Selections)]
    
    output$Plot <- renderPlotly({
      fig  <-  plot_ly(
        vc_version_data,
        x = ~ Dates,
        y = vc_version_data[[input$Selections[1]]],
        name = input$Selections[1],
        type = 'scatter',
        mode = 'lines',
        color = I(ox_pallette()[1])
      ) %>%
        layout(
          shapes = vline(data[(data$FORECAST_FLAG == "EA") &
                                (data$variable == input$Selections[1]), "Dates"]),
          yaxis = list(title = if (input$display == "% y/y") {
            "% y/y"
          } else {
            "Number"
          }),
          xaxis = list(title = "Year"),
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.15
          )
        ) %>%
        add_annotations(
          x = data[(data$FORECAST_FLAG == "EA") &
                     (data$variable == input$Selections[1]), "Dates"],
          y = 1,
          text = "              Forecast",
          yref = "paper",
          showarrow = FALSE
        )
      
      if (length(input$Selections) >= 2) {
        for (i in 2:length(input$Selections)) {
          fig <- fig %>% add_trace(
            y = vc_version_data[[input$Selections[i]]],
            color = I(ox_pallette()[i]),
            name = input$Selections[i]
          )
        }
      }
      return(fig)
    })
  })
  
# Render Table ------------------------------------------------------------
  observe({
    if (length(input$Selections) >= 2) {
      for (i in 2:length(input$Selections)) {
        if (i == 2) {
          common_dates <-
            intersect(data$Dates[data$variable == input$Selections[1]], data$Dates[data$variable == input$Selections[i]])
        } else {
          common_dates <-
            intersect(common_dates, data$Dates[data$variable == input$Selections[i]])
        }
      }
    } else {
      common_dates <- data$Dates[data$variable == input$Selections[1]]
    }
    
    cagr_dates <-
      c(
        custom.min(common_dates),
        "2010",
        "2021",
        "2025",
        "2030",
        custom.max(common_dates)
      )
    
    output$Table <- renderTable({
      vc_version_table_data2 <-
        filter(
          data,
          variable %in% input$Selections &
            Dates %in% c(input$CAGR_start, input$CAGR_end)
        ) %>%
        mutate(value = round((((value / lag(value)) ^ (1 / {
          Dates - lag(Dates)
        }) - 1
        ) * 100), 2)) %>%
        transmute(Period = paste0("(", lag(Dates), " - ", Dates, ")"),
                  variable, value) %>%
        filter(Period == paste0("(", input$CAGR_start, " - ", input$CAGR_end, ")")) %>%
        spread(variable, value) %>%
        mutate(Period = paste0("Custom CAGR\n" , Period))
      
      vc_version_table_data <-
        filter(data,
               variable %in% input$Selections &
                 Dates %in% cagr_dates) %>%
        mutate(value = round((((value / lag(value)) ^ (1 / {
          Dates - lag(Dates)
        }) - 1
        ) * 100), 2)) %>%
        transmute(Period = paste0("(", lag(Dates), " - ", Dates, ")"),
                  variable, value) %>%
        filter(
          Period != paste0("(NA - ", custom.min(common_dates), ")") &
            Period != paste0(
              "(",
              custom.max(common_dates),
              " - ",
              custom.min(common_dates),
              ")"
            )
        ) %>%
        spread(variable, value) %>%
        cbind(Names = c("2000s", "2010s", "Short run", "Medium run", "Long run")) %>%
        mutate(Period = paste0(Names, "\n" , Period)) %>%
        select(-Names)
      
      vc_version_table_data <-
        vc_version_table_data[, append("Period", input$Selections)]
      
      vc_version_table_data <-
        rbind(vc_version_table_data, vc_version_table_data2)
    },
    spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  })
  
})
}

# 3.0 Test Module ---------------------------------------------------------

# vc_version_demo <- function() {
#
#   select <- data.frame(SCENARIO_VALUE = c("CENTRAL","EXPORT_SUPERPOWER","SUSTAINABLE_GROWTH","RAPID_DECARB"))
#   ui <- navbarPage("Module Demo",
#                    vc_version_ui("Version", "By Region"))
#   server <- function(input, output, session) {
#     callModule(vc_version_server,"Version")
#     }
#   shinyApp(ui, server)
# }
#
# vc_version_demo()
