
# 1.0 Module UI -----------------------------------------------------------

ic_variable_ui <- function(id) {
  ns = NS(id)
  tabPanel("By Variable",
           sidebarLayout(
             sidebarPanel(
               chooseSliderSkin(skin = "Shiny", color = ox_pallette()[2]),
               wellPanel(
                 style = paste0(
                   "border: 5px solid; border-color:",
                   oxgraphs::ox_pallette()[2],
                   "; margin-bottom: 0.5em"
                 ),
                 fluidRow(column(
                   3, h4("Display:", style = "margin-top: -0.5em")
                 )),
                 fluidRow(
                 column(
                   5,
                   radioGroupButtons(
                     ns("display"),
                     NULL,
                     #choiceNames = list(icon("fal fa-chart-line"), icon('fal fa-chart-bar')),
                     choices = c("Line chart", "Bar chart"),
                     selected = "Line chart",
                     justified = TRUE,
                     status = "primary"
                   )
                 ),
                 column(
                   5,
                   radioGroupButtons(
                     inputId = ns("title"),
                     NULL,
                     c("Title On", "Title Off"),
                     selected = "Title On",
                     justified = TRUE,
                     status = "primary"
                   )
                 ),
                 column(
                   6,
                   actionGroupButtons(
                     inputIds = c(ns("pvt"), ns("pub"), ns("mcu"), ns("reset")),
                     labels = c("Pvt S", "Pub S", "MCU", "Reset"),
                     status = "primary"
                 )
                 ),
                 style = "margin-bottom:-1.0em")
               ),
               wellPanel(
                 style = paste0(
                   "border: 5px solid; border-color:",
                   oxgraphs::ox_pallette()[2],
                   "; margin-bottom: 0.5em"
                 ),
                 fluidRow(
                   column(3,
                          selectInput(
                            ns("State"),
                            label = h4(str_to_title("State"), style = "margin-bottom:-0.1em"),
                            sort(unique(data$STATE)),
                            selectize = FALSE
                          )),
                   column(5,
                          selectInput(
                            ns("Scenario"),
                            label = h4(str_to_title("Scenario"), style = "margin-bottom:-0.1em"),
                            sort(unique(data$SCENARIO_VALUE)),
                            selectize = FALSE
                          )),
                   column(4,
                          selectInput(
                            ns("Version"),
                            label = h4(str_to_title("Version"), style = "margin-bottom:-0.1em"),
                            sort(unique(data$RELEASE_VERSION)),
                            selectize = FALSE
                          )),
                   style = "margin-bottom:-2em; margin-top:-2em"
                 )
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
                                                      data$RELEASE_VERSION == "May22 V1"])[1],
                     selected = unique(data$variable[data$STATE == "ACT" &
                                                       data$SCENARIO_VALUE == "Central" &
                                                       data$RELEASE_VERSION == "May22 V1"])[1],
                     shape = "round",
                     outline = TRUE,
                     status = "primary"
                   )
                 )),
                 fluidRow(
                   column(4, h4("Choose Dates: ", style = "margin-top: 0.2em")),
                   column(3,
                          textInput(
                            ns("Period_start"),
                            label = NULL,
                            value = "2021"
                          ),),
                   column(1,
                          h4("-", style = "margin-top: 0.2em"),),
                   column(3,
                          textInput(
                            ns("Period_end"),
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

ic_variable_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  # Sort Version Select Input -----------------------------------------------
  observe({
    rv_list <-
      data.frame(RELEASE_VERSION = unique(data$RELEASE_VERSION[data$SCENARIO_VALUE == input$Scenario])) %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
    rv_list$Release_Date <-
      parse_date_time(rv_list$Release_Date, "my")
    
    rv_list <-
      arrange(rv_list, desc(Release_Date), desc(Version)) %>%
      mutate(., RELEASE_VERSION = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
      select(-Release_Date, -Version)
    
    updateSelectInput(
      session,
      "Version",
      label = NULL,
      rv_list$RELEASE_VERSION,
      selected = as.list(rv_list$RELEASE_VERSION[1])
    )
  })
  
  # Update checkboxgroup options based on selected inputs -------------------
  observe({
    if (length(data$variable[data$SCENARIO_VALUE == input$Scenario &
                             data$RELEASE_VERSION == input$Version &
                             data$STATE == input$State]) >= 1) {
    version_list <-
      data.frame(
        ATTRIBUTE = unique(data$ATTRIBUTE[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version]),
        STATE = input$State,
        SCENARIO_VALUE = input$Scenario,
        RELEASE_VERSION = input$Version,
        Series_ID = unique(data$Series_ID[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version 
                                            ])
      )  %>%
      add.var.col(.) %>%
      add.sc.col(.) %>%
      add.ic.col(.)
    
    updatePrettyCheckboxGroup(
      session,
      "Selections",
      label = NULL,
      sort(version_list$variable),
      selected = sort(version_list$variable)[1:4],
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
    }
  })
  
  # PVT S Button ------------------------------------------------------------
  observeEvent(input$pvt, {
    version_list <-
      data.frame(
        ATTRIBUTE = unique(data$ATTRIBUTE[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version]),
        STATE = input$State,
        SCENARIO_VALUE = input$Scenario,
        RELEASE_VERSION = input$Version,
        Series_ID = unique(data$Series_ID[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version 
        ])
      )  %>%
      add.var.col(.)
    
    updatePrettyCheckboxGroup(
      session,
      "Selections",
      label = NULL,
      sort(version_list$variable),
      selected = sort(version_list$variable)[which(str_detect(sort(version_list$variable), "Accommodation") |
                                               str_detect(sort(version_list$variable), "Administrative") |
                                               str_detect(sort(version_list$variable), "Arts") |
                                               str_detect(sort(version_list$variable), "Finance") |
                                               str_detect(sort(version_list$variable), "Information") |
                                               str_detect(sort(version_list$variable), "Other") |
                                               str_detect(sort(version_list$variable), "Prof") |
                                               str_detect(sort(version_list$variable), "Rental") |
                                               str_detect(sort(version_list$variable), "Retail") |
                                               str_detect(sort(version_list$variable), "Transport") |
                                               str_detect(sort(version_list$variable), "Wholesale"))],
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
  })
  
  # Pub S Button ------------------------------------------------------------
  observeEvent(input$pub, {
    version_list <-
      data.frame(
        ATTRIBUTE = unique(data$ATTRIBUTE[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version]),
        STATE = input$State,
        SCENARIO_VALUE = input$Scenario,
        RELEASE_VERSION = input$Version,
        Series_ID = unique(data$Series_ID[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version 
        ])
      )  %>%
      add.var.col(.)
    
    updatePrettyCheckboxGroup(
      session,
      "Selections",
      label = NULL,
      sort(version_list$variable),
      selected = sort(version_list$variable)[which(str_detect(sort(version_list$variable), "Education") |
                                               str_detect(sort(version_list$variable), "Health") |
                                               str_detect(sort(version_list$variable), "Public"))],
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
  })
  
  # MCU Button ------------------------------------------------------------
  observeEvent(input$mcu, {
    version_list <-
      data.frame(
        ATTRIBUTE = unique(data$ATTRIBUTE[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version]),
        STATE = input$State,
        SCENARIO_VALUE = input$Scenario,
        RELEASE_VERSION = input$Version,
        Series_ID = unique(data$Series_ID[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version 
        ])
      )  %>%
      add.var.col(.)
    
    updatePrettyCheckboxGroup(
      session,
      "Selections",
      label = NULL,
      sort(version_list$variable),
      selected = sort(version_list$variable)[which(str_detect(sort(version_list$variable), "Construction") |
                                               str_detect(sort(version_list$variable), "Manufacturing") |
                                               str_detect(sort(version_list$variable), "Electricity"))],
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
  })
  
  # Reset Button ------------------------------------------------------------
  observeEvent(input$reset, {
    version_list <-
      data.frame(
        ATTRIBUTE = unique(data$ATTRIBUTE[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version]),
        STATE = input$State,
        SCENARIO_VALUE = input$Scenario,
        RELEASE_VERSION = input$Version,
        Series_ID = unique(data$Series_ID[substr(data$ATTRIBUTE, 1, 3) == "GVA" & 
                                            data$SCENARIO_VALUE == input$Scenario &
                                            data$RELEASE_VERSION == input$Version 
        ])
      )  %>%
      add.var.col(.)
    
    updatePrettyCheckboxGroup(
      session,
      "Selections",
      label = NULL,
      sort(version_list$variable),
      selected = sort(version_list$variable)[1:4],
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
  })
  
  # Render Plot -------------------------------------------------------------
  observe({
    output$Plot <- renderPlotly({
      bar_dates <- seq(2020, 2050, 10)
      
      ic_variable_data <- filter(data,
                                 ic_variable %in% unique(data$ic_variable[data$variable %in% input$Selections]))
      
      ic_variable_data_a <- filter(ic_variable_data,
                                   variable %in% input$Selections)
      ic_variable_data_b <-
        filter(
          ic_variable_data,
          Series_ID %in% c("GVAA_BLCC", "INDPRODLCC", "GVAFLCC", "SVCSGVALCC")
        )
      ic_variable_data_b <-
        aggregate(
          x = ic_variable_data_b$value,
          by = list(
            ic_variable_data_b$Dates,
            ic_variable_data_b$ic_variable
          ),
          FUN = sum
        ) %>%
        transmute(.,
                  Dates = Group.1,
                  ic_variable = Group.2,
                  GVATOT = x)
      ic_variable_data <- merge(ic_variable_data_a,
                                ic_variable_data_b,
                                by = c("Dates", "ic_variable")) %>%
        transmute(.,
                  Dates,
                  variable,
                  value = round(VALUE / GVATOT * 100, 2))
      if (input$display != "Line chart") {
        ic_variable_data <-
          trail_avg(ic_variable_data, bar_dates[2] - bar_dates[1]) %>%
          mutate(., value = round(value, 2)) %>%
          filter(., Dates %in% bar_dates)
        ic_variable_data$Dates <- as.factor(ic_variable_data$Dates)
      }
      ic_variable_data <- spread(ic_variable_data, variable, value)
      
      fig <-
        if (input$display != "Line chart") {
          {
            fig <- plot_ly(
              ic_variable_data,
              x = ~ Dates,
              y = ic_variable_data[[input$Selections[1]]],
              type = 'bar',
              name = str_before_first(input$Selections[1], ", "),
              color = I(ox_pallette()[1])
            ) %>%
              layout(
                yaxis = list(ticksuffix = "%", title = "% of Total GVA",
                             showgrid = F,
                             showline = T,
                             linecolor = "#495057",
                             ticks = "outside",
                             tickcolor = "#495057"
                ),
                xaxis = list(
                  title = "",
                  zerolinecolor = "#495057",
                  showgrid = F,
                  showline = T,
                  linecolor = "#495057",
                  ticks = "outside",
                  tickcolor = "#495057"),
                legend = list(
                  orientation = "h",
                  xanchor = "center",
                  x = 0.5,
                  y = -0.15
                ),
                barmode = 'group'
              )
            if (length(input$Selections) >= 2) {
              for (i in 2:length(input$Selections)) {
                fig <- fig %>% add_trace(y = ic_variable_data[[input$Selections[i]]],
                                         color = I(ox_pallette()[i]),
                                         name = str_before_first(input$Selections[i], ", "))
              }
            }
            if (input$title == "Title On") {
              fig <- fig %>%
                layout(title = list(
                  text = paste0(
                    str_after_first(input$Selections[1], ", "),
                    " - By Region Comparison"
                  ),
                  x = 0.05,
                  y = 1,
                  font = list(
                    family = "segoe ui",
                    size = 24,
                    color = "#495057"
                  )
                )) }
            return(fig)
          }
        } else {
          {
            fig <- plot_ly(
              ic_variable_data,
              x = ~ Dates,
              y = ic_variable_data[[input$Selections[1]]],
              name = str_before_first(input$Selections[1], ", "),
              type = 'scatter',
              mode = 'lines',
              color = I(ox_pallette()[1]),
              hoverlabel = list(namelength = -1)
            ) %>%
              layout(
                shapes = vline(data[(data$FORECAST_FLAG == "EA") &
                                      (data$variable == input$Selections[1]), "Dates"]),
                yaxis = list(ticksuffix = "%",
                             showgrid = F,
                             showline = T,
                             linecolor = "#495057",
                             ticks = "outside",
                             tickcolor = "#495057"
                ),
                xaxis = list(
                  title = "",
                  zerolinecolor = "#495057",
                  showgrid = F,
                  showline = T,
                  linecolor = "#495057",
                  ticks = "outside",
                  tickcolor = "#495057"),
                legend = list(
                  orientation = "h",
                  xanchor = "center",
                  x = 0.5,
                  y = -0.05
                ),
                margin = list(l = 0, r = 0, b = 0, t = 50),
                hovermode = "x unified"
              ) %>%
              add_annotations(
                x = data[(data$FORECAST_FLAG == "EA") &
                           (data$variable == input$Selections[1]), "Dates"],
                y = 1,
                text = "              Forecast",
                yref = "paper",
                showarrow = FALSE
              ) %>% 
              add_annotations(
                x = min(ic_variable_data$Dates[!is.na(ic_variable_data[[input$Selections[1]]])]),
                y = 1.035,
                text = "% of Total GVA",
                yref = "paper",
                xanchor = "left",
                showarrow = FALSE
              )
            if (length(input$Selections) >= 2) {
              for (i in 2:length(input$Selections)) {
                fig <- fig %>% add_trace(y = ic_variable_data[[input$Selections[i]]],
                                         color = I(ox_pallette()[i]),
                                         name = str_before_first(input$Selections[i], ", "),
                                         hoverlabel = list(namelength = -1)
                )
              }
            }
            if (input$title == "Title On") {
              fig <- fig %>%
                layout(title = list(
                  text = paste0(
                    str_after_first(input$Selections[1], ", "),
                    " - By Variable Comparison"
                  ),
                  x = 0.035,
                  y = 1.2,
                  font = list(
                    family = "segoe ui",
                    size = 24,
                    color = "#495057"
                  )
                )) }
            return(fig)
          }
        }
      
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
    
    period_dates <-
      c(
        custom.min(common_dates),
        "2010",
        "2021",
        "2025",
        "2030",
        custom.max(common_dates),
        input$Period_start,
        input$Period_end
      )
    
    period_names <-
      c("2000s",
        "2010s",
        "Short run",
        "Medium run",
        "Long run",
        "",
        "Custom Period")
    
    output$Table <- renderTable({
      ic_variable_table_data <- filter(data,
                                       ic_variable %in% unique(data$ic_variable[data$variable %in% input$Selections]))
      
      ic_variable_table_data_a <- filter(ic_variable_table_data,
                                         variable %in% input$Selections)
      ic_variable_table_data_b <-
        filter(
          ic_variable_table_data,
          Series_ID %in% c("GVAA_BLCC", "INDPRODLCC", "GVAFLCC", "SVCSGVALCC")
        )
      ic_variable_table_data_b <-
        aggregate(
          x = ic_variable_table_data_b$value,
          by = list(
            ic_variable_table_data_b$Dates,
            ic_variable_table_data_b$ic_variable
          ),
          FUN = sum
        ) %>%
        transmute(.,
                  Dates = Group.1,
                  ic_variable = Group.2,
                  GVATOT = x)
      ic_variable_table_data <- merge(
        ic_variable_table_data_a,
        ic_variable_table_data_b,
        by = c("Dates", "ic_variable")
      ) %>%
        transmute(.,
                  Dates,
                  variable,
                  value = round(VALUE / GVATOT * 100, 2))
      
      for (i in c(2:6, length(period_dates))) {
        ic_variable_table_data_1 <-
          ic_variable_table_data %>%
          trail_avg(., p = {
            as.numeric(period_dates[i]) - as.numeric(period_dates[i - 1])
          }) %>%
          filter(., Dates == period_dates[i]) %>%
          transmute(
            Period = paste0(
              period_names[i - 1],
              " (",
              period_dates[i - 1],
              " - ",
              period_dates[i],
              ")"
            ),
            variable,
            value = round(value, 2)
          )
        
        ic_variable_table_data_1 <-
          spread(ic_variable_table_data_1, variable, value)
        
        if (i == 2) {
          ic_variable_p_avg_table <- ic_variable_table_data_1
        } else {
          ic_variable_p_avg_table <-
            rbind(ic_variable_p_avg_table, ic_variable_table_data_1)
        }
      }
      names(ic_variable_p_avg_table) <-
        gsub(paste0(", ",str_after_first(input$Selections[1], ", ")),
             "",
             names(ic_variable_p_avg_table))
      
      return(ic_variable_p_avg_table)
      
    },
    spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  })
  
})
}
