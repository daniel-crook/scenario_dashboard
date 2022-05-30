

# 1.0 Module UI -----------------------------------------------------------

sc_region_ui <- function(id) {
  ns = NS(id)
  tabPanel("By Region",
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
                     9,
                     radioGroupButtons(
                       ns("display"),
                       NULL,
                       c("State Share", "Nested Pie", "Cont. to GDP bar"),
                       selected = "State Share",
                       justified = TRUE,
                       status = "primary"
                     )
                   ),
                   column(
                     6,
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
                       inputIds = c(ns("big4"), ns("small4"), ns("all")),
                       labels = c("Big", "Small", "All"),
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
                     ns("Version"),
                     label = h4("Version", style = "margin-bottom:-0.1em"),
                     sort(unique(data$RELEASE_VERSION)),
                     selectize = FALSE
                   )
                 ), style = "margin-bottom:-2em; margin-top:-2em"),
                 uiOutput(ns("attribute"))
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
                     choices = unique(data$variable[data$RELEASE_VERSION == "May22 V1" &
                                                      data$SCENARIO_VALUE == "Central" &
                                                      data$ATTRIBUTE == "Attached Dwellings"])[1],
                     selected = unique(data$variable[data$RELEASE_VERSION == "May22 V1" &
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
                          ), ),
                   column(1,
                          h4("-", style = "margin-top: 0.2em"), ),
                   column(3,
                          textInput(
                            ns("Period_end"),
                            label = NULL,
                            value = "2053"
                          ), ),
                   style = "margin-bottom: -2em; margin-top: -1em"
                 )
               )
             ),
             mainPanel(plotlyOutput(ns("Plot"), height = '600px'),
                       tableOutput(ns("Table")))
           ))
}

# 2.0 Module Server -------------------------------------------------------

sc_region_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns = NS(id)
    
    # Filter Version based on Scenario Select ---------------------------------
    
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
    
    # Filter Attribute based on Version select --------------------------------
    
    observe({
      updateSelectInput(session,
                        "Attribute",
                        label = NULL,
                        sort(unique(data$ATTRIBUTE[data$STATE != "AUS" &
                                                     data$RELEASE_VERSION == input$Version])))
    })
    
    # Big States Button -------------------------------------------------------
    
    observeEvent(input$big4, {
      states <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")
      
      version_list <-
        data.frame(
          ATTRIBUTE = input$Attribute,
          STATE = states,
          SCENARIO_VALUE = input$Scenario,
          RELEASE_VERSION = input$Version
        )  %>%
        add.var.col(.)
      
      updatePrettyCheckboxGroup(
        session,
        "Selections",
        label = NULL,
        as.list(version_list$variable),
        selected = as.list(version_list$variable[1:4]),
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    })
    
    # Small States Button -----------------------------------------------------
    
    observeEvent(input$small4, {
      states <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")
      
      version_list <-
        data.frame(
          ATTRIBUTE = input$Attribute,
          STATE = states,
          SCENARIO_VALUE = input$Scenario,
          RELEASE_VERSION = input$Version
        )  %>%
        add.var.col(.)
      
      updatePrettyCheckboxGroup(
        session,
        "Selections",
        label = NULL,
        as.list(version_list$variable),
        selected = as.list(version_list$variable[5:8]),
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    })
    
    # All States Button -------------------------------------------------------
    
    observeEvent(input$all, {
      states <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")
      
      version_list <-
        data.frame(
          ATTRIBUTE = input$Attribute,
          STATE = states,
          SCENARIO_VALUE = input$Scenario,
          RELEASE_VERSION = input$Version
        )  %>%
        add.var.col(.)
      
      updatePrettyCheckboxGroup(
        session,
        "Selections",
        label = NULL,
        as.list(version_list$variable),
        selected = as.list(version_list$variable[1:8]),
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    })
    
    # Update checkgroup options based on select inputs ------------------------
    
    observe({
      if (length(data$variable[data$SCENARIO_VALUE == input$Scenario &
                               data$RELEASE_VERSION == input$Version &
                               data$ATTRIBUTE == input$Attribute]) >= 1) {
        states <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")
        
        version_list <-
          data.frame(
            ATTRIBUTE = if (input$display != "Cont. to GDP bar") {
              input$Attribute
            } else {
              "Gross State Product"
            },
            STATE = states,
            SCENARIO_VALUE = input$Scenario,
            RELEASE_VERSION = input$Version,
            Series_ID = unique(data$Series_ID[data$ATTRIBUTE == input$Attribute])
          )  %>%
          add.var.col(.) %>%
          add.sc.col(.)
        
        updatePrettyCheckboxGroup(
          session,
          "Selections",
          label = NULL,
          as.list(version_list$variable),
          selected = as.list(version_list$variable[1:8]),
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
      if (str_after_nth(input$Selections[1], ", ", 2) == paste(input$Scenario, input$Version, sep = ", ")) {
        sc_region_data <- filter(data,
                                 sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]))
        AUS_GDP <-
          paste("Gross Domestic Product, AUS",
                input$Scenario,
                input$Version,
                sep = ", ")
        
        if (input$display == "Cont. to GDP bar" &
            str_before_first(input$Selections[1], ", ") == "Gross State Product") {
          sc_region_data <-
            transmute(sc_region_data, Dates, variable, value) %>%
            ctg(., AUS_GDP, p = 1) %>%
            mutate(., value = round(value,2)) %>% 
            spread(variable, value)
          sc_region_data <- sc_region_data[-1,]
        } else {
          sc_region_data_a <-
          filter(sc_region_data, STATE %in% unique(data$STATE[data$variable %in% input$Selections]))
        sc_region_data_b <-
          filter(sc_region_data, STATE == "AUS") %>%
          transmute(., Dates, SCENARIO_VALUE, RELEASE_VERSION, AUS_VALUE = VALUE)
        sc_region_data <- merge(
          sc_region_data_a,
          sc_region_data_b,
          by = c("Dates", "SCENARIO_VALUE", "RELEASE_VERSION")
        ) %>%
          transmute(.,
                    Dates,
                    variable,
                    value = round(VALUE / AUS_VALUE * 100, 2))
        if (input$display == "Nested Pie") {
          sc_region_data <- sc_region_data %>%
            filter(., Dates %in% c(input$Period_start, input$Period_end))
          sc_region_data$Dates <- as.factor(sc_region_data$Dates)
         }
        sc_region_data <- spread(sc_region_data, variable, value)
        }
        if (input$display == "Nested Pie") {

          if(round(rowSums(sc_region_data[input$Selections])[[1]],1) != 100) {
            sc_region_data <- mutate(sc_region_data, "Attached Dwellings, Unselected, Central, May22 V3" = round(100 - rowSums(sc_region_data[input$Selections]),1))}

            sc_region_data <- melt(sc_region_data, "Dates") %>%
            mutate(variable = str_before_first(str_after_first(as.character(variable), ", "), ", ")) %>%
            mutate(variable = factor(
              variable,
              levels = c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT", if(round(rowSums(sc_region_data[input$Selections])[[1]],1) != 100)
                {"Unselected"})
            )) %>%
            arrange(variable) %>%
            mutate(value = round(value, 1))
        }
      
        
        output$Plot <- renderPlotly({
          if (input$display == "Nested Pie" &
              str_before_first(input$Selections[1], ", ") == input$Attribute) {
            fig <- plot_ly(
              sc_region_data,
              labels = unique(sc_region_data$variable),
              values = sc_region_data$value[sc_region_data$Dates == input$Period_end],
              type = 'pie',
              hole = 0.7,
              name = input$Period_end,
              marker = list(colors = c(I(ox_pallette(
              )[1:length(input$Selections)]), 'white')),
              hovertemplate = '%{label}, %{percent}',
              insidetextorientation = 'horizontal',
              insidetextfont = list(color = 'white')
            )
            fig <- fig %>% add_pie(
              sc_region_data,
              labels = unique(sc_region_data$variable),
              values = sc_region_data$value[sc_region_data$Dates == input$Period_start],
              type = 'pie',
              hole = 0.55,
              domain = list(x = c(0.15, 0.85), y = c(0.15, 0.85)),
              name = input$Period_start,
              marker = list(colors = c(I(ox_pallette(
              )[1:length(input$Selections)]), 'white')),
              hovertemplate = '%{label}, %{percent}',
              insidetextorientation = 'horizontal',
              textposition = 'inside',
              insidetextfont = list(color = 'white')
            ) %>%
              layout(
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
                )
              ) %>% add_annotations(
                x = 0.6125,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                text = input$Period_start,
                showarrow = T,
                ax = 175,
                ay = -20,
                arrowcolor = "black",
                font = list(
                  color = "black",
                  family = "segoe ui",
                  size = 16,
                  color = "black"
                )

              ) %>% add_annotations(
                x = 0.675,
                y = 0.575,
                xref = "paper",
                yref = "paper",
                text = input$Period_end,
                showarrow = T,
                ax = 110,
                ay = -13,
                arrowcolor = "black",
                font = list(
                  color = "black",
                  family = "segoe ui",
                  size = 16,
                  color = "black"
                )

              )
            if (input$title == "Title On") {
              fig <- fig %>%
                layout(title = list(
                  text = paste0(
                    str_before_first(input$Selections[1], ", "),
                    ", ",
                    str_after_nth(input$Selections[1], ", ", 2),
                    " - By Region Comparison"
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
          } else if (input$display == "State Share" &
                     str_before_first(input$Selections[1], ", ") == input$Attribute) {
            {
              fig <- plot_ly(
                sc_region_data,
                x = ~ Dates,
                y = sc_region_data[[input$Selections[1]]],
                name = str_before_first(str_after_first(input$Selections[1], ", "), ", "),
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
                  x = min(sc_region_data$Dates),
                  y = 1.035,
                  text = "% of National",
                  yref = "paper",
                  xanchor = "left",
                  showarrow = FALSE
                )
              if (length(input$Selections) >= 2) {
                for (i in 2:length(input$Selections)) {
                  fig <- fig %>% add_trace(
                    y = sc_region_data[[input$Selections[i]]],
                    color = I(ox_pallette()[i]),
                    name = str_before_first(str_after_first(input$Selections[i], ", "), ", "),
                    hoverlabel = list(namelength = -1)
                  )
                }
              }
              if (input$title == "Title On") {
                fig <- fig %>%
                  layout(title = list(
                    text = paste0(
                      str_before_first(input$Selections[1], ", "),
                      ", ",
                      str_after_nth(input$Selections[1], ", ", 2),
                      " - By Region Comparison"
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
          } else if (input$display == "Cont. to GDP bar" &
                     str_before_first(input$Selections[1], ", ") == "Gross State Product") {
            input_line <-  AUS_GDP
            input_bar <- input$Selections
            
            fig <-
              plot_ly(
                sc_region_data,
                x = ~ Dates,
                y = ~ sc_region_data[[input_bar[1]]],
                type = 'bar',
                name = str_after_first(str_before_nth(input_bar[1], ", ",2),", "),
                color = I(ox_pallette()[1]),
                hoverlabel = list(namelength = -1)
              ) %>%
              layout(
                shapes = vline(data[(data$FORECAST_FLAG == "EA") &
                                      (data$variable == input_bar[1]), "Dates"]),
                yaxis = list(title = "",
                             ticksuffix = "%",
                             showgrid = F,
                             showline = T,
                             linecolor = "#495057",
                             ticks = "outside",
                             tickcolor = "#495057",
                             tickformat = ","
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
                barmode = 'relative',
                margin = list(l = 0, r = 0, b = 0, t = 50),
                hovermode = "x unified"
              ) %>% 
              add_annotations(
                x = data[(data$FORECAST_FLAG == "EA") &
                           (data$variable == input_bar[1]), "Dates"],
                y = 1,
                text = "              Forecast",
                yref = "paper",
                showarrow = FALSE
              ) %>% 
              add_annotations(
                x = min(sc_region_data$Dates)-0.5,
                y = 1.035,
                text = "% y/y",
                yref = "paper",
                xanchor = "left",
                showarrow = FALSE
              )
            if (length(input_bar) >= 2) {
              for (i in 2:length(input_bar)) {
                fig <- fig %>% add_bars(
                  y = sc_region_data[[input_bar[i]]],
                  color = I(ox_pallette()[i]),
                  name = str_after_first(str_before_nth(input_bar[i], ", ",2),", "),
                  hoverlabel = list(namelength = -1)
                )
              }
            }
            if (length(input_line) >= 0) {
              fig <- fig %>% add_trace(
                y = sc_region_data[[input_line[1]]],
                color = I(ox_pallette()[9]),
                name = str_after_first(str_before_nth(input_line[1], ", ",2),", "),
                type = 'scatter',
                mode = 'lines',
                hoverlabel = list(namelength = -1)
              )
            }
            if (input$title == "Title On") {
              fig <- fig %>%
                layout(title = list(
                  text = paste0(
                    str_after_nth(input_bar[1], ", ",2),
                    " - State Contribution to GDP Growth"
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
        })
      }
      })
    
    # Render Table ------------------------------------------------------------
    
    observe({
      if(input$display != "Cont. to GDP bar") {
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
        sc_region_table_data <- filter(data,
                                       sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]))
        sc_region_table_data_a <-
          filter(sc_region_table_data, STATE %in% unique(data$STATE[data$variable %in% input$Selections]))
        sc_region_table_data_b <-
          filter(sc_region_table_data, STATE == "AUS") %>%
          transmute(., Dates, SCENARIO_VALUE, RELEASE_VERSION, AUS_VALUE = VALUE)
        sc_region_table_data <- merge(
          sc_region_table_data_a,
          sc_region_table_data_b,
          by = c("Dates", "SCENARIO_VALUE", "RELEASE_VERSION")
        ) %>%
          transmute(.,
                    Dates,
                    variable,
                    value = round(VALUE / AUS_VALUE * 100, 2))

        for (i in c(2:6, length(period_dates))) {
          sc_region_table_data_1 <-
            sc_region_table_data %>%
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

          sc_region_table_data_1 <-
            spread(sc_region_table_data_1, variable, value)

          if (i == 2) {
            sc_region_p_avg_table <- sc_region_table_data_1
          } else {
            sc_region_p_avg_table <-
              rbind(sc_region_p_avg_table, sc_region_table_data_1)
          }
        }
        sc_region_p_avg_table <-
          sc_region_p_avg_table[, append("Period", input$Selections)]

        names(sc_region_p_avg_table) <-
          gsub(paste0(str_before_first(input$Selections[1], ", "), ", "),
               "",
               names(sc_region_p_avg_table))
        names(sc_region_p_avg_table) <-
          gsub(paste0(", ", str_after_nth(input$Selections[1], ", ", 2)),
               "",
               names(sc_region_p_avg_table))

        return(sc_region_p_avg_table)
      },
      spacing = "s", striped = TRUE, hover = TRUE, align = "l")
      } else {
        output$Table <- renderTable({NULL})
      }
    })
    
    # Add Relative To UI dynamically ------------------------------------------
    
    observe({
      if (input$display != "Cont. to GDP bar") {
        output$attribute <- renderUI({
          fluidRow(column(
            12,
            selectInput(
              ns("Attribute"),
              label = h4(str_to_title("Attribute"), style = "margin-bottom:-0.1em"),
              sort(unique(data$ATTRIBUTE[data$STATE != "AUS"])),
              selectize = FALSE
            )
          ),
          style = "margin-bottom:-2em")
        })
      } else {
        output$attribute <- renderUI({
          NULL
        })
      }
    })
    
    })
  }
  