
# 1.0 Module UI -----------------------------------------------------------

vc_scenario_ui <- function(id) {
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
                   4, h4("Display:", style = "margin-top: -0.5em")
                 )),
                 fluidRow(
                   column(
                     4,
                     radioGroupButtons(
                       ns("display"),
                       NULL,
                       c("Levels", "% y/y"),
                       selected = "% y/y",
                       justified = TRUE,
                       status = "primary"
                     )
                   ),
                   column(
                     3,
                     radioGroupButtons(
                       ns("state_national"),
                       NULL,
                       c("State", "Aus"),
                       justified = TRUE,
                       status = "primary"
                     )
                   ),
                   column(
                     5,
                     radioGroupButtons(
                       inputId = ns("title"),
                       NULL,
                       c("Title-on", "Title-off"),
                       selected = "Title-on",
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
                 column(
                   9,
                   selectInput(
                     ns("Attribute"),
                     label = h4(str_to_title("Attribute"), style = "margin-bottom:-0.1em"),
                     sort(unique(data$ATTRIBUTE)),
                     selectize = FALSE
                   )
                 ), style = "margin-bottom:-2em; margin-top:-2em")
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
                                                      data$ATTRIBUTE == "Attached Dwellings"])[1],
                     selected = unique(data$variable[data$STATE == "ACT" &
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

vc_scenario_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    # State vs National Filter ------------------------------------------------
    
    observe({
      if (input$state_national == "State") {
        region <- sort(unique(data$STATE[data$STATE != "AUS"]))
        attribute <-
          sort(unique(data$ATTRIBUTE[data$STATE != "AUS"]))
      } else {
        region <- sort(unique(data$STATE[data$STATE == "AUS"]))
        attribute <-
          sort(unique(data$ATTRIBUTE[data$STATE == "AUS"]))
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
    
    # Update Checkgroup options based on selected inputs ----------------------
    
    observe({
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
        selected = if (length(variable_list$variable) >= 3) {
          variable_list$variable
        } else {
          variable_list$variable[1]
        },
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    })
    
    # Render Plot -------------------------------------------------------------
    
    observe({
      vc_scenario_data <- filter(data,
                                 variable %in% input$Selections) %>%
        transmute(., Dates,
                  variable,
                  value = round(value, 2))
      if (input$display == "% y/y") {
        vc_scenario_data <- growth(vc_scenario_data, 1)
      }
      vc_scenario_data <-
        mutate(vc_scenario_data, value = round(value, 2)) %>%
        spread(., variable, value)
      vc_scenario_data <-
        vc_scenario_data[, append("Dates", input$Selections)]
      
      output$Plot <- renderPlotly({
        fig  <-  plot_ly(
          vc_scenario_data,
          x = ~ Dates,
          y = vc_scenario_data[[input$Selections[1]]],
          name = str_after_nth(input$Selections[1], ", ", 2),
          type = 'scatter',
          mode = 'lines',
          color = I(ox_pallette()[1])
        ) %>%
          layout(
            shapes = vline(data[(data$FORECAST_FLAG == "EA") &
                                  (data$variable == input$Selections[1]), "Dates"]),
            yaxis = list(
              title = if (input$display == "% y/y") {
                "% y/y"
              } else {
                "Number"
              },
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
              y = vc_scenario_data[[input$Selections[i]]],
              color = I(ox_pallette()[i]),
              name =  str_after_nth(input$Selections[i], ", ", 2)
            )
          }
        }
        if (input$title == "Title-on") {
          fig <- fig %>%
            layout(title = list(
              text = paste0(
                str_before_nth(input$Selections[1], ",", 2),
                " - By Scenario Comparison"
              ),
              x = 0.05,
              y = 1,
              font = list(
                family = "segoe ui",
                size = 24,
                color = "#495057"
              )
            ))
        }
        return(fig)
      })
    })
    
    # Render Tab;e ------------------------------------------------------------
    
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
        vc_scenario_table_data2 <-
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
        
        vc_scenario_table_data <-
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
        
        vc_scenario_table_data <-
          vc_scenario_table_data[, append("Period", input$Selections)]
        
        vc_scenario_table_data <-
          rbind(vc_scenario_table_data, vc_scenario_table_data2)
        
        names(vc_scenario_table_data) <-
          gsub(paste0(str_before_nth(input$Selections[1], ", ", 2), ", "),
               "",
               names(vc_scenario_table_data))
        return(vc_scenario_table_data)
      },
      spacing = "s", striped = TRUE, hover = TRUE, align = "l")
    })
    
  })
}
