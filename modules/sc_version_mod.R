
# 1.0 Module UI -----------------------------------------------------------

sc_version_ui <- function(id) {
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
                 fluidRow(column(
                   3, h4("Display:", style = "margin-top: -0.5em")
                 )),
                 fluidRow(
                   column(
                     7,
                     radioGroupButtons(
                       ns("display"),
                       NULL,
                       c("Line", "Diff-bar"),
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
                     sort(unique(data$STATE[data$STATE != "AUS"])),
                     selectize = FALSE
                   )
                 ), style = "margin-bottom:-2em; margin-top:-2em"),
                 h4("Attribute", style = "margin-bottom: 0.2em; margin-top: 1em"),
                 fluidRow(column(
                   12,
                   selectInput(
                     ns("Attribute"),
                     label = NULL,
                     sort(unique(data$ATTRIBUTE[data$STATE != "AUS"])),
                     selectize = FALSE
                   )
                 ),
                 style = "margin-bottom:-2em"
                 ),
                 uiOutput(ns("relative"))
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

sc_version_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns = NS(id)
    
    # Update Attribute SelectInput based on Scenario --------------------------
    observe({
      attribute <- sort(unique(data$ATTRIBUTE[data$STATE != "AUS" &
                                                data$SCENARIO_VALUE == input$Scenario]))
      
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
                                                            data$ATTRIBUTE == input$Attribute]),
            Series_ID = unique(data$Series_ID[data$ATTRIBUTE == input$Attribute])
          ) %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
        
        version_list$Release_Date <-
          parse_date_time(version_list$Release_Date, "my")
        
        version_list <-
          arrange(version_list, desc(Release_Date), desc(Version)) %>%
          mutate(., RELEASE_VERSION = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
          select(-Release_Date, -Version) %>%
          add.var.col(.) %>%
          add.sc.col(.)
        
        if (length(input$relative_to) == 0) {
          NULL
        } else if (length(input$relative_to) == 1 & input$display == "Diff-bar") {
          version_list <-
            filter(version_list, RELEASE_VERSION != input$relative_to)
        }
        
        updatePrettyCheckboxGroup(
          session,
          "Selections",
          label = NULL,
          version_list$variable,
          selected = if (length(version_list$variable) >= 3) {
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
      if (str_before_last(input$Selections[1], ", ") == paste(input$Attribute, input$State, input$Scenario, sep = ", ")) {
        vars <-  append(input$Selections, paste(input$Attribute,
                                              input$State,
                                              input$Scenario,
                                              input$relative_to, sep = ", "))
      
      if (length(vars) >= 2) {
        for (i in 2:length(vars)) {
          if (i == 2) {
            common_dates <-
              intersect(data$Dates[data$variable == vars[1]], data$Dates[data$variable == vars[i]])
          } else {
            common_dates <-
              intersect(common_dates, data$Dates[data$variable == vars[i]])
          }
        }
      } else {
        common_dates <- data$Dates[data$variable == vars[1]]
      }
      
      bar_dates <- c(data$Dates[data$FORECAST_FLAG == "EA" &
                            data$variable == vars[1]], custom.max(common_dates))
      
      if (length(input$relative_to) == 1 &
          input$display == "Diff-bar" &
          (paste(input$Attribute,
                input$State,
                input$Scenario,
                input$relative_to, sep = ", ") != input$Selections[1])) {
        
       relative_var <- data.frame(variable = paste(input$Attribute,
                               input$State,
                               input$Scenario,
                               input$relative_to, sep = ", "))
        
        sc_version_data <- filter(data,
                                  sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]) |
                                  sc_variable %in% unique(data$sc_variable[data$variable %in% relative_var$variable]))
      } else {
        sc_version_data <- filter(data,
                                  sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]))
      }
      
      sc_version_data_a <-
        filter(sc_version_data, STATE %in% input$State)
      sc_version_data_b <-
        filter(sc_version_data, STATE == "AUS") %>%
        transmute(., Dates, SCENARIO_VALUE, RELEASE_VERSION, AUS_VALUE = VALUE)
      sc_version_data <- merge(
        sc_version_data_a,
        sc_version_data_b,
        by = c("Dates", "SCENARIO_VALUE", "RELEASE_VERSION")
      ) %>%
        transmute(.,
                  Dates,
                  variable,
                  value = round(VALUE / AUS_VALUE * 100, 2))
      if (length(input$relative_to) == 1 & input$display == "Diff-bar"  &
          str_before_last(input$Selections[1],", ") == paste(input$Attribute,
                  input$State,
                  input$Scenario, sep = ", ")) {
        sc_version_data <-
          trail_avg(sc_version_data, bar_dates[2] - bar_dates[1]) %>%
          mutate(., value = round(value, 2)) %>% 
          filter(., Dates == bar_dates[2])
        sc_version_data$Dates <- as.factor(sc_version_data$Dates)}
      
      sc_version_data <- spread(sc_version_data, variable, value)
      
      if (length(input$relative_to) == 1 & input$display == "Diff-bar" &
          (paste(input$Attribute,
                 input$State,
                 input$Scenario,
                 input$relative_to, sep = ", ") != input$Selections[1])) {
      for (i in 1:length(input$Selections)) {
        sc_version_data[input$Selections[i]] <- sc_version_data[input$Selections[i]] - sc_version_data[relative_var$variable[1]]
      }
      sc_version_data <- select(sc_version_data,-relative_var$variable[1]) %>% 
        melt("Dates") %>% select(-Dates) %>% mutate(., variable = str_after_last(as.character(variable),", ")) %>% 
        spread(., variable,value)
      }
      
      #output$Table <- renderTable({sc_version_data})
      
      output$Plot <- renderPlotly({
        
        fig <-
          if (length(input$relative_to) == 1 & input$display == "Diff-bar"  &
              paste(input$Attribute,
                      input$State,
                      input$Scenario,
                      input$relative_to, sep = ", ") != input$Selections[1] &
              length(input$Selections) >= 1) {
            {
              fig <- plot_ly(
                sc_version_data,
                x = sc_version_data[[str_after_last(input$Selections[1],", ")]],
                y = str_after_last(input$Selections[1],", "),
                type = 'bar',
                orientation = 'h',
                color = I(ox_pallette()[1]),
                name = str_after_last(input$Selections[1], ", "),
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
                  margin = list(l = 0, r = 0, b = 0, t = 50),
                  hovermode = "x unified"
                ) %>%
                add_annotations(
                  x = 0,
                  y = 1.035,
                  text = paste0("% of National, Period Average (",bar_dates[1]," - ",bar_dates[2],")"),
                  xref = "paper",
                  yref = "paper",
                  xanchor = "left",
                  showarrow = FALSE
                )
              if (length(input$Selections) >= 2) {
                for (i in 2:length(input$Selections)) {
                  fig <- fig %>% add_trace(x = sc_version_data[[str_after_last(input$Selections[i],", ")]],
                                           y = str_after_last(input$Selections[i],", "),
                                           color = I(ox_pallette()[i]),
                                           name = str_after_last(input$Selections[i],", "))
                }
              }
              if (input$title == "Title On") {
                fig <- fig %>%
                  layout(title = list(
                    text = paste0(
                      str_before_last(input$Selections[1], ","),
                      " - By Version Comparison"
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
          } else if (input$display == "Line") {
            {
              fig <- plot_ly(
                sc_version_data,
                x = ~ Dates,
                y = sc_version_data[[input$Selections[1]]],
                name = str_after_last(input$Selections[1], ", "),
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
                  x = min(sc_version_data$Dates),
                  y = 1.035,
                  text = "% of National",
                  yref = "paper",
                  xanchor = "left",
                  showarrow = FALSE
                )
              if (length(input$Selections) >= 2) {
                for (i in 2:length(input$Selections)) {
                  fig <- fig %>% add_trace(
                    y = sc_version_data[[input$Selections[i]]],
                    color = I(ox_pallette()[i]),
                    name = str_after_last(input$Selections[i], ", "),
                    hoverlabel = list(namelength = -1)
                  )
                }
              }
              if (input$title == "Title On") {
                fig <- fig %>%
                  layout(title = list(
                    text = paste0(
                      str_before_last(input$Selections[1], ","),
                      " - By Version Comparison"
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
      }
    })
    
    # Render Table ------------------------------------------------------------
    observe({
      if (str_before_last(input$Selections[1], ", ") == paste(input$Attribute, input$State, input$Scenario, sep = ", ")) {
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
          "Custom Period"
          )

      output$Table <- renderTable({
        sc_version_table_data <- filter(data,
                                        sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]))
        sc_version_table_data_a <-
          filter(sc_version_table_data, STATE %in% input$State)
        sc_version_table_data_b <-
          filter(sc_version_table_data, STATE == "AUS") %>%
          transmute(., Dates, SCENARIO_VALUE, RELEASE_VERSION, AUS_VALUE = VALUE)
        sc_version_table_data <- merge(
          sc_version_table_data_a,
          sc_version_table_data_b,
          by = c("Dates", "SCENARIO_VALUE", "RELEASE_VERSION")
        ) %>%
          transmute(.,
                    Dates,
                    variable,
                    value = round(VALUE / AUS_VALUE * 100, 2))

        for (i in c(2:6, length(period_dates))) {
          sc_version_table_data_1 <-
            sc_version_table_data %>%
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

          sc_version_table_data_1 <-
            spread(sc_version_table_data_1, variable, value)

          if (i == 2) {
            sc_version_p_avg_table <- sc_version_table_data_1
          } else {
            sc_version_p_avg_table <-
              rbind(sc_version_p_avg_table, sc_version_table_data_1)
          }
        }
        sc_version_p_avg_table <-
          sc_version_p_avg_table[, append("Period", input$Selections)]

        names(sc_version_p_avg_table) <-
          gsub(paste0(str_before_last(input$Selections[1], ", "), ", "),
               "",
               names(sc_version_p_avg_table))
        return(sc_version_p_avg_table)
      },
      spacing = "s", striped = TRUE, hover = TRUE, align = "l")
      }
    })
  
    # Add Relative To UI dynamically ------------------------------------------
    observe({
      version_list <-
        data.frame(
          ATTRIBUTE = input$Attribute,
          STATE = input$State,
          SCENARIO_VALUE = input$Scenario,
          RELEASE_VERSION = unique(data$RELEASE_VERSION[data$SCENARIO_VALUE == input$Scenario &
                                                          data$ATTRIBUTE == input$Attribute]),
          Series_ID = unique(data$Series_ID[data$ATTRIBUTE == input$Attribute])
        ) %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
      
      version_list$Release_Date <-
        parse_date_time(version_list$Release_Date, "my")
      
      version_list <-
        arrange(version_list, desc(Release_Date), desc(Version)) %>%
        mutate(., RELEASE_VERSION = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
        select(-Release_Date, -Version)
      
      if (input$display == "Diff-bar") {
        output$relative <- renderUI({
          fluidRow(column(
            12,
          selectInput(
            ns("relative_to"),
            h4("Relative to:", style = "margin-bottom: 0.2em; margin-top: 1em"),
            version_list$RELEASE_VERSION,
            selectize = FALSE
          )),
          style = "margin-bottom:-2em")})
          
      } else {output$relative <- renderUI({NULL})}
    })
    
    
      
    })
}
