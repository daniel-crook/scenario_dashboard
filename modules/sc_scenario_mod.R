
# 1.0 Module UI -----------------------------------------------------------

sc_scenario_ui <- function(id) {
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
                 fluidRow(column(
                   5,
                   radioGroupButtons(
                     ns("display"),
                     NULL,
                     c("Line chart", "Bar chart"),
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
                 style = "margin-bottom:-2.0em")
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
                     sort(unique(data$STATE[data$STATE != "AUS"])),
                     selectize = FALSE
                   )
                 ),
                 column(
                   9,
                   selectInput(
                     ns("Attribute"),
                     label = h4(str_to_title("Attribute"), style = "margin-bottom:-0.1em"),
                     sort(unique(data$ATTRIBUTE[data$STATE != "AUS"])),
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

sc_scenario_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
# Update checkgroup options based on select inputs ------------------------

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
      selected = as.list(variable_list$variable),
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
  })
  
# Render Plot -------------------------------------------------------------

  observe({
    bar_dates <- seq(2020, 2050, 10)
    
    sc_scenario_data <- filter(data,
                               sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]))
    sc_scenario_data_a <-
      filter(sc_scenario_data, STATE %in% unique(data$STATE[data$variable %in% input$Selections]))
    sc_scenario_data_b <-
      filter(sc_scenario_data, STATE == "AUS") %>%
      transmute(., Dates, SCENARIO_VALUE, RELEASE_VERSION, AUS_VALUE = VALUE)
    sc_scenario_data <- merge(
      sc_scenario_data_a,
      sc_scenario_data_b,
      by = c("Dates", "SCENARIO_VALUE", "RELEASE_VERSION")
    ) %>%
      transmute(.,
                Dates,
                variable,
                value = round(VALUE / AUS_VALUE * 100, 2))
    if (input$display != "Line chart") {
      sc_scenario_data <-
        trail_avg(sc_scenario_data, bar_dates[2] - bar_dates[1]) %>%
        mutate(., value = round(value, 2)) %>%
        filter(., Dates %in% bar_dates)
      sc_scenario_data$Dates <- as.factor(sc_scenario_data$Dates)
    }
    sc_scenario_data <- spread(sc_scenario_data, variable, value)
    
    output$Plot <- renderPlotly({
      fig <-
        if (input$display != "Line chart") {
          {
            fig <- plot_ly(
              sc_scenario_data,
              x = ~ Dates,
              y = sc_scenario_data[[input$Selections[1]]],
              type = 'bar',
              name = str_after_nth(input$Selections[1], ", ", 2),
              color = I(ox_pallette()[1])
            ) %>%
              layout(
                yaxis = list(ticksuffix = "%", title = "% of National",
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
                fig <- fig %>% add_trace(y = sc_scenario_data[[input$Selections[i]]],
                                         color = I(ox_pallette()[i]),
                                         name = str_after_last(input$Selections[i],", "))
              }
            }
            if (input$title == "Title On") {
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
                )) }
            return(fig)
          }
        } else {
          {
            fig <- plot_ly(
              sc_scenario_data,
              x = ~ Dates,
              y = sc_scenario_data[[input$Selections[1]]],
              name = str_after_nth(input$Selections[1], ", ", 2),
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
                x = min(sc_scenario_data$Dates[!is.na(sc_scenario_data[[input$Selections[1]]])]),
                y = 1.035,
                text = "% of National",
                yref = "paper",
                xanchor = "left",
                showarrow = FALSE
              )
            if (length(input$Selections) >= 2) {
              for (i in 2:length(input$Selections)) {
                fig <- fig %>% add_trace(y = sc_scenario_data[[input$Selections[i]]],
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
                    str_before_nth(input$Selections[1], ",", 2),
                    " - By Scenario Comparison"
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
      sc_scenario_table_data <- filter(data,
                                       sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]))
      sc_scenario_table_data_a <-
        filter(sc_scenario_table_data, STATE %in% unique(data$STATE[data$variable %in% input$Selections]))
      sc_scenario_table_data_b <-
        filter(sc_scenario_table_data, STATE == "AUS") %>%
        transmute(., Dates, SCENARIO_VALUE, RELEASE_VERSION, AUS_VALUE = VALUE)
      sc_scenario_table_data <- merge(
        sc_scenario_table_data_a,
        sc_scenario_table_data_b,
        by = c("Dates", "SCENARIO_VALUE", "RELEASE_VERSION")
      ) %>%
        transmute(.,
                  Dates,
                  variable,
                  value = round(VALUE / AUS_VALUE * 100, 2))
      
      for (i in c(2:6, length(period_dates))) {
        sc_scenario_table_data_1 <-
          sc_scenario_table_data %>%
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
        
        sc_scenario_table_data_1 <-
          spread(sc_scenario_table_data_1, variable, value)
        
        if (i == 2) {
          sc_scenario_p_avg_table <- sc_scenario_table_data_1
        } else {
          sc_scenario_p_avg_table <-
            rbind(sc_scenario_p_avg_table, sc_scenario_table_data_1)
        }
      }
      sc_scenario_p_avg_table <-
        sc_scenario_p_avg_table[, append("Period", input$Selections)]
      
      names(sc_scenario_p_avg_table) <-
        gsub(paste0(str_before_nth(input$Selections[1], ", ", 2), ", "),
             "",
             names(sc_scenario_p_avg_table))
      return(sc_scenario_p_avg_table)
    },
    spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  })
  
})
}

# 3.0 Test Module ---------------------------------------------------------

# sc_scenario_demo <- function() {
#
#   select <- data.frame(SCENARIO_VALUE = c("CENTRAL","EXPORT_SUPERPOWER","SUSTAINABLE_GROWTH","RAPID_DECARB"))
#   ui <- navbarPage("Module Demo",
#                    sc_scenario_ui("Version", "By Region"))
#   server <- function(input, output, session) {
#     callModule(sc_scenario_server,"Version")
#     }
#   shinyApp(ui, server)
# }
#
# sc_scenario_demo()
