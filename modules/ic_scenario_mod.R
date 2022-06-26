
# 1.0 Module UI -----------------------------------------------------------

ic_scenario_ui <- function(id) {
  ns = NS(id)
  tabPanel("By Scenario",
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
                     7,
                     radioGroupButtons(
                       ns("display"),
                       NULL,
                       c("Line", "Final Year Bar"),
                       selected = "Line",
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
                   style = "margin-bottom:-2.0em"
                 )
               ),
               wellPanel(
                 style = paste0(
                   "border: 5px solid; border-color:",
                   oxgraphs::ox_pallette()[2],
                   "; margin-bottom: 0.5em"
                 ),
                 fluidRow(column(
                   3,
                   selectInput(
                     ns("State"),
                     label = h4(str_to_title("State"), style = "margin-bottom:-0.1em"),
                     sort(unique(data$STATE)),
                     selectize = FALSE
                   )
                 ),
                 column(9,
                        uiOutput(ns(
                          "attribute"
                        ))), style = "margin-bottom:-2em; margin-top:-2em")
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
                     choices = NULL,
                     selected = NULL,
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

ic_scenario_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns = NS(id)
    
    ## --- update checkboxgroup options based on select inputs --- ###
    observe({
      if (input$display == "Line" & length(input$Attribute) == 1) {
        data_f <- filter(data, data$ATTRIBUTE == input$Attribute)
        
        sv_list <-
          unique(data_f[c("SCENARIO_VALUE", "RELEASE_VERSION")]) %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
        
        sv_list$Release_Date <-
          parse_date_time(sv_list$Release_Date, "my")
        
        sv_list <-
          arrange(sv_list,
                  SCENARIO_VALUE,
                  desc(Release_Date),
                  desc(Version)) %>%
          transmute(SCENARIO_VALUE,
                    RELEASE_VERSION = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
          distinct(SCENARIO_VALUE, .keep_all = TRUE)
        
        variable_list <-
          data.frame(
            ATTRIBUTE = input$Attribute,
            STATE = input$State,
            SCENARIO_VALUE = sv_list$SCENARIO_VALUE,
            RELEASE_VERSION = sv_list$RELEASE_VERSION
          ) %>%
          add.var.col(.)
        
        updatePrettyCheckboxGroup(
          session,
          "Selections",
          label = NULL,
          as.list(variable_list$variable),
          selected = as.list(variable_list$variable),
          prettyOptions = list(
            shape = "round",
            outline = TRUE,
            status = "primary"
          )
        )
      } else if (input$display != "Line") {
        sv_list <-
          unique(data[c("SCENARIO_VALUE", "RELEASE_VERSION")]) %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
        
        sv_list$Release_Date <-
          parse_date_time(sv_list$Release_Date, "my")
        
        sv_list <-
          arrange(sv_list,
                  SCENARIO_VALUE,
                  desc(Release_Date),
                  desc(Version)) %>%
          transmute(SCENARIO_VALUE,
                    RELEASE_VERSION = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
          distinct(SCENARIO_VALUE, .keep_all = TRUE)
        
        variable_list <-
          data.frame(
            STATE = input$State,
            SCENARIO_VALUE = sv_list$SCENARIO_VALUE,
            RELEASE_VERSION = sv_list$RELEASE_VERSION
          ) %>%
          add.ic.fy.col(.)
        
        updatePrettyCheckboxGroup(
          session,
          "Selections",
          label = NULL,
          as.list(variable_list$ic_fy_variable),
          selected = as.list(variable_list$ic_fy_variable),
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
        output$Plot <- renderPlotly({
          validate(need({length(input$Selections) >= 1 &
                          length(input$display) == 1 &
                          length(input$State) == 1}, ""))
          # aggregate gva categories
          aggregate_vars <-
            c("SVCSGVALCC",
              "GVAFLCC",
              "GVAELCC",
              "GVADLCC",
              "GVACLCC",
              "GVAA_BLCC")
          
          if (input$display == "Line") {
            ic_scenario_data <- filter(data,
                                       ic_variable %in% unique(data$ic_variable[data$variable %in% input$Selections]))
            ic_scenario_data_a <- filter(ic_scenario_data,
                                         variable %in% input$Selections)
          } else if (input$display == "Final Year Bar") {
            ic_scenario_data <- filter(
              data,
              ic_fy_variable %in% input$Selections &
                Series_ID %in% aggregate_vars
            )
            ic_scenario_data_a <- ic_scenario_data
          }
          
          ic_scenario_data_b <-
            filter(ic_scenario_data,
                   Series_ID %in% aggregate_vars)
          
          validate(need(NROW(ic_scenario_data_b) >= 1, ""))
          
          ic_scenario_data_b <-
            aggregate(
              x = ic_scenario_data_b$value,
              by = list(
                ic_scenario_data_b$Dates,
                ic_scenario_data_b$ic_variable
              ),
              FUN = sum
            ) %>%
            transmute(Dates = Group.1,
                      ic_variable = Group.2,
                      GVATOT = x)
          ic_scenario_data <- merge(ic_scenario_data_a,
                                    ic_scenario_data_b,
                                    by = c("Dates", "ic_variable")) %>%
            transmute(Dates,
                      variable,
                      value = round(VALUE / GVATOT * 100, 2))
          
          if (input$display == "Final Year Bar") {
            {
              #Find max common date
              date_vars <-
                data.frame(date_vars = input$Selections) %>%
                mutate(., date_vars = paste0("GVA - Mining, ", date_vars)) %>% as.list()
              if (length(date_vars) >= 2) {
                for (i in 2:length(date_vars)) {
                  if (i == 2) {
                    common_dates <-
                      intersect(data$Dates[data$variable == date_vars[1]], data$Dates[data$variable == date_vars[i]])
                  } else {
                    common_dates <-
                      intersect(common_dates, data$Dates[data$variable == date_vars[i]])
                  }
                }
              } else {
                common_dates <- data$Dates[data$variable == date_vars[1]]
              }
            }
            
            ic_scenario_data <-  ic_scenario_data %>%
              filter(Dates == custom.max(common_dates)) %>%
              select(-Dates) %>%
              transmute(
                attribute = str_before_first(variable, ", "),
                variable = str_after_nth(variable, ", ", 2),
                value
              )
          }
          ic_scenario_data <-
            spread(ic_scenario_data, variable, value)
          
          scenario_names <- colnames(ic_scenario_data)[-1]
          
          fig <-
            if (input$display == "Final Year Bar") {
              {
                fig <- plot_ly(
                  ic_scenario_data,
                  y = ~ attribute,
                  x = ~ ic_scenario_data[[scenario_names[1]]],
                  type = 'bar',
                  orientation = 'h',
                  name = scenario_names[1],
                  color = I(ox_pallette()[1]),
                  hoverlabel = list(namelength = -1)
                ) %>%
                  layout(
                    xaxis = list(
                      ticksuffix = "%",
                      title = "",
                      showgrid = F,
                      showline = T,
                      linecolor = "#495057",
                      ticks = "outside",
                      tickcolor = "#495057"
                    ),
                    yaxis = list(
                      title = "",
                      zerolinecolor = "#495057",
                      showgrid = F,
                      showline = T,
                      linecolor = "#495057",
                      ticks = "outside",
                      tickcolor = "#495057"
                    ),
                    legend = list(
                      orientation = "h",
                      xanchor = "center",
                      x = 0.5,
                      y = -0.05
                    ),
                    margin = list(
                      l = 0,
                      r = 0,
                      b = 0,
                      t = 50
                    ),
                    hovermode = "y unified",
                    barmode = 'group'
                  ) %>%
                  add_annotations(
                    x = 0,
                    y = 1.035,
                    text = paste0(
                      "% of Total GVA, Period Average (",
                      custom.max(common_dates),
                      ")"
                    ),
                    xref = "paper",
                    yref = "paper",
                    xanchor = "left",
                    showarrow = FALSE
                  )
                if (length(scenario_names) >= 2) {
                  for (i in 2:length(scenario_names)) {
                    fig <- fig %>% add_trace(
                      x = ic_scenario_data[[scenario_names[i]]],
                      color = I(ox_pallette()[i]),
                      name = scenario_names[i]
                    )
                  }
                }
                if (input$title == "Title On") {
                  fig <- fig %>%
                    layout(title = list(
                      text = paste0(input$State,
                                    " - By Scenario Comparison"),
                      x = 0.035,
                      y = 1.2,
                      font = list(
                        family = "segoe ui",
                        size = 24,
                        color = "#495057"
                      )
                    ))
                }
                return(fig)
              }
            } else if (input$display == "Line") {
              validate(need(length(input$Attribute) == 1, ""))
              {
                fig <- plot_ly(
                  ic_scenario_data,
                  x = ~ Dates,
                  y = ic_scenario_data[[input$Selections[1]]],
                  name = str_after_nth(input$Selections[1], ", ", 2),
                  type = 'scatter',
                  mode = 'lines',
                  color = I(ox_pallette()[1]),
                  hoverlabel = list(namelength = -1)
                ) %>%
                  layout(
                    shapes = vline(data[(data$FORECAST_FLAG == "EA") &
                                          (data$variable == input$Selections[1]), "Dates"]),
                    yaxis = list(
                      ticksuffix = "%",
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
                      tickcolor = "#495057"
                    ),
                    legend = list(
                      orientation = "h",
                      xanchor = "center",
                      x = 0.5,
                      y = -0.05
                    ),
                    margin = list(
                      l = 0,
                      r = 0,
                      b = 0,
                      t = 50
                    ),
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
                    x = min(ic_scenario_data$Dates[!is.na(ic_scenario_data[[input$Selections[1]]])]),
                    y = 1.035,
                    text = "% of Total GVA",
                    yref = "paper",
                    xanchor = "left",
                    showarrow = FALSE
                  )
                if (length(input$Selections) >= 2) {
                  for (i in 2:length(input$Selections)) {
                    fig <-
                      fig %>% add_trace(
                        y = ic_scenario_data[[input$Selections[i]]],
                        color = I(ox_pallette()[i]),
                        name = str_after_nth(input$Selections[i], ", ", 2),
                        hoverlabel = list(namelength = -1)
                      )
                  }
                }
                if (input$title == "Title On") {
                  fig <- fig %>%
                    layout(title = list(
                      text = paste0(
                        str_before_nth(input$Selections[1], ", ", 2),
                        " - By Scenario Comparison"
                      ),
                      x = 0.035,
                      y = 1.2,
                      font = list(
                        family = "segoe ui",
                        size = 24,
                        color = "#495057"
                      )
                    ))
                }
                return(fig)
              }
            }
        })
    })
    
    # # Render Table ------------------------------------------------------------
    
    observe({
      if (length(input$Selections) >= 2 & length(input$Attribute) == 1 & input$display == "Line") {
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
          ic_scenario_table_data <- filter(data,
                                           ic_variable %in% unique(data$ic_variable[data$variable %in% input$Selections]))

          ic_scenario_table_data_a <- filter(ic_scenario_table_data,
                                             variable %in% input$Selections)
          ic_scenario_table_data_b <-
            filter(
              ic_scenario_table_data,
              Series_ID %in% c("GVAA_BLCC", "INDPRODLCC", "GVAFLCC", "SVCSGVALCC")
            )
          validate(need(NROW(ic_scenario_table_data_b) >= 1, ""))
          ic_scenario_table_data_b <-
            aggregate(
              x = ic_scenario_table_data_b$value,
              by = list(
                ic_scenario_table_data_b$Dates,
                ic_scenario_table_data_b$ic_variable
              ),
              FUN = sum
            ) %>%
            transmute(
              .,
              Dates = Group.1,
              ic_variable = Group.2,
              GVATOT = x
            )
          ic_scenario_table_data <- merge(
            ic_scenario_table_data_a,
            ic_scenario_table_data_b,
            by = c("Dates", "ic_variable")
          ) %>%
            transmute(.,
                      Dates,
                      variable,
                      value = round(VALUE / GVATOT * 100, 2))

          for (i in c(2:6, length(period_dates))) {
            ic_scenario_table_data_1 <-
              ic_scenario_table_data %>%
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

            ic_scenario_table_data_1 <-
              spread(ic_scenario_table_data_1, variable, value)

            if (i == 2) {
              ic_scenario_p_avg_table <- ic_scenario_table_data_1
            } else {
              ic_scenario_p_avg_table <-
                rbind(ic_scenario_p_avg_table,
                      ic_scenario_table_data_1)
            }
          }
          ic_scenario_p_avg_table <-
            ic_scenario_p_avg_table[, append("Period", input$Selections)]

          names(ic_scenario_p_avg_table) <-
            gsub(paste0(str_before_nth(input$Selections[1], ", ", 2), ", "),
                 "",
                 names(ic_scenario_p_avg_table))
          return(ic_scenario_p_avg_table)
        },
        spacing = "s", striped = TRUE, hover = TRUE, align = "l")
      }
      })
    
    
    # Add Relative To UI dynamically ------------------------------------------
    
    observe({
      if (input$display == "Line") {
        output$attribute <- renderUI({
          selectInput(
            ns("Attribute"),
            label = h4(str_to_title("Attribute"), style = "margin-bottom:-0.1em"),
            sort(unique(data$ATTRIBUTE[substr(data$ATTRIBUTE, 1, 3) == "GVA"])),
            selectize = FALSE
          )
        })
      } else {
        output$attribute <- renderUI({
          NULL
        })
      }
    })
    
  })
}
