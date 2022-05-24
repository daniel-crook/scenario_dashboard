

# 1.0 Module UI -----------------------------------------------------------

d_total_increase_by_region_ui <- function(id) {
  ns = NS(id)
  tabPanel("Total Population Increase by Region",
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
                   3, h4("Display:",
                         style = "margin-top: 0.25em"
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
                 style = "margin-bottom:-2.0em; margin-top:-0.75em")
               ),
               wellPanel(
                 style = paste0(
                   "border: 5px solid; border-color:",
                   oxgraphs::ox_pallette()[2],
                   "; margin-bottom: 0.5em"
                 ),
                 fluidRow(
                   column(6,
                          selectInput(
                            ns("Scenario"),
                            label = h4(str_to_title("Scenario"), style = "margin-bottom:-0.1em"),
                            sort(unique(data$SCENARIO_VALUE[data$Series_ID == "POPINC"])),
                            selectize = FALSE
                          )),
                   column(6,
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
                     choices = unique(data$variable[data$Series_ID %in% c("POPINC")  &
                                                      data$SCENARIO_VALUE == "Central" &
                                                      data$RELEASE_VERSION == "May22 V1"]),
                     selected = unique(data$variable[data$Series_ID %in% c("POPINC") & 
                                                       data$SCENARIO_VALUE == "Central" &
                                                       data$RELEASE_VERSION == "May22 V1"]),
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

d_total_increase_by_region_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
  # Sort Version Select Input -----------------------------------------------
  observe({
      rv_list <-
        data.frame(RELEASE_VERSION = unique(data$RELEASE_VERSION[data$SCENARIO_VALUE == input$Scenario &
                                                                   data$Series_ID == "POPINC"])) %>% separate(RELEASE_VERSION, c('Release_Date', 'Version'))
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
    # state_list <- c("NSW","VIC","QLD","WA","SA","TAS","ACT","NT","AUS")
    # 
    # d_input <- data.frame(
    #   SCENARIO_VALUE = input$Scenario,
    #   ATTRIBUTE = "Total Population Increase",
    #   RELEASE_VERSION = input$Release,
    #   STATE = state_list
    # ) %>% 
    #   add.var.col(.)
    # 
    # d_input_varlist <- d_input$variable
    # 
    # 
    # 
    # for(i in state_list) {
    #   if(length(str_subset(d_input_varlist,pattern = fixed(i)))) {
    #     if(!(exists("d_input_f"))){
    #       d_input_f <- NULL
    #     }
    #     d_input_f <- append(d_input_f,str_subset(d_input_varlist, pattern = fixed(i)))}
    # }
    
    updatePrettyCheckboxGroup(
      session,
      "Selections",
      label = NULL,
      choices = unique(data$variable[data$Series_ID %in% c("POPINC") &
                                       data$SCENARIO_VALUE == input$Scenario &
                                       data$RELEASE_VERSION == input$Version]),
      selected = unique(data$variable[data$Series_ID %in% c("POPINC")  &
                                        data$SCENARIO_VALUE == input$Scenario &
                                        data$RELEASE_VERSION == input$Version]),
      prettyOptions = list(
        shape = "round",
        outline = TRUE,
        status = "primary"
      )
    )
  })
  
  # # Render Plot -------------------------------------------------------------
  observe({
    output$Plot <- renderPlotly({
      d_total_increase_by_region_data <-
        filter(data, variable %in% input$Selections) %>%
        transmute(.,
                  Dates = Dates,
                  variable,
                  value = round(as.numeric(value), 0)) %>%
        spread(., variable, value)
      
      input_series <-
        colnames(d_total_increase_by_region_data)[2:length(colnames(d_total_increase_by_region_data))]
      
      state_list <- c("NSW","VIC","QLD","WA","SA","TAS","ACT","NT","AUS")
      
      for(i in state_list) {
        if(length(str_subset(input_series,pattern = fixed(i)))) {
          if(!(exists("input_f"))){
            input_f <- NULL
          }
          input_f <- append(input_f,str_subset(input_series, pattern = fixed(i)))}
      }

      input_line <-  str_subset(input_f, pattern = fixed("AUS"))
      input_bar <- str_subset(input_f, pattern = fixed("AUS"), negate = TRUE)

      fig <-
        plot_ly(
          d_total_increase_by_region_data,
          x = ~ Dates,
          y = ~ d_total_increase_by_region_data[[input_bar[1]]],
          type = 'bar',
          name = str_after_first(str_before_nth(input_bar[1], ", ",2),", "),
          color = I(ox_pallette()[1]),
          hoverlabel = list(namelength = -1)
        ) %>%
        layout(
          shapes = vline(data[(data$FORECAST_FLAG == "EA") &
                                (data$variable == input_series[1]), "Dates"]),
          yaxis = list(title = "",
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
                     (data$variable == input_series[1]), "Dates"],
          y = 1,
          text = "              Forecast",
          yref = "paper",
          showarrow = FALSE
        ) %>% 
      add_annotations(
        x = min(d_total_increase_by_region_data$Dates[!is.na(d_total_increase_by_region_data[[input$Selections[1]]])])-0.5,
        y = 1.035,
        text = "Persons (000s)",
        yref = "paper",
        xanchor = "left",
        showarrow = FALSE
      )
      if (length(input_bar) >= 2) {
        for (i in 2:length(input_bar)) {
          fig <- fig %>% add_bars(
            y = d_total_increase_by_region_data[[input_bar[i]]],
            color = I(ox_pallette()[i]),
            name = str_after_first(str_before_nth(input_bar[i], ", ",2),", "),
            hoverlabel = list(namelength = -1)
          )
        }
      }
      if (length(input_line) >= 0) {
      fig <- fig %>% add_trace(
        y = d_total_increase_by_region_data[[input_line[1]]],
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
                str_after_nth(input_bar[1], ",",2),
                " - Population Breakdown"
            ),
            x = 0.055,
            y = 1.2,
            font = list(
              family = "segoe ui",
              size = 24,
              color = "#495057"
            )
          )) }
      fig

    })
  })
  
  # # Render Table ------------------------------------------------------------
  # observe({
  #   if (length(input$Selections) >= 2) {
  #     for (i in 2:length(input$Selections)) {
  #       if (i == 2) {
  #         common_dates <-
  #           intersect(data$Dates[data$variable == input$Selections[1]], data$Dates[data$variable == input$Selections[i]])
  #       } else {
  #         common_dates <-
  #           intersect(common_dates, data$Dates[data$variable == input$Selections[i]])
  #       }
  #     }
  #   } else {
  #     common_dates <- data$Dates[data$variable == input$Selections[1]]
  #   }
  #
  #   period_dates <-
  #     c(
  #       custom.min(common_dates),
  #       "2010",
  #       "2021",
  #       "2025",
  #       "2030",
  #       custom.max(common_dates),
  #       input$Period_start,
  #       input$Period_end
  #     )
  #
  #   period_names <-
  #     c("2000s",
  #       "2010s",
  #       "Short run",
  #       "Medium run",
  #       "Long run",
  #       "",
  #       "Custom Period")
  #
  #   output$Table <- renderTable({
  #     ic_variable_table_data <- filter(data,
  #                                      ic_variable %in% unique(data$ic_variable[data$variable %in% input$Selections]))
  #
  #     ic_variable_table_data_a <- filter(ic_variable_table_data,
  #                                        variable %in% input$Selections)
  #     ic_variable_table_data_b <-
  #       filter(
  #         ic_variable_table_data,
  #         Series_ID %in% c("GVAA_BLCC", "INDPRODLCC", "GVAFLCC", "SVCSGVALCC")
  #       )
  #     ic_variable_table_data_b <-
  #       aggregate(
  #         x = ic_variable_table_data_b$value,
  #         by = list(
  #           ic_variable_table_data_b$Dates,
  #           ic_variable_table_data_b$ic_variable
  #         ),
  #         FUN = sum
  #       ) %>%
  #       transmute(.,
  #                 Dates = Group.1,
  #                 ic_variable = Group.2,
  #                 GVATOT = x)
  #     ic_variable_table_data <- merge(
  #       ic_variable_table_data_a,
  #       ic_variable_table_data_b,
  #       by = c("Dates", "ic_variable")
  #     ) %>%
  #       transmute(.,
  #                 Dates,
  #                 variable,
  #                 value = round(VALUE / GVATOT * 100, 2))
  #
  #     for (i in c(2:6, length(period_dates))) {
  #       ic_variable_table_data_1 <-
  #         ic_variable_table_data %>%
  #         trail_avg(., p = {
  #           as.numeric(period_dates[i]) - as.numeric(period_dates[i - 1])
  #         }) %>%
  #         filter(., Dates == period_dates[i]) %>%
  #         transmute(
  #           Period = paste0(
  #             period_names[i - 1],
  #             " (",
  #             period_dates[i - 1],
  #             " - ",
  #             period_dates[i],
  #             ")"
  #           ),
  #           variable,
  #           value = round(value, 2)
  #         )
  #
  #       ic_variable_table_data_1 <-
  #         spread(ic_variable_table_data_1, variable, value)
  #
  #       if (i == 2) {
  #         ic_variable_p_avg_table <- ic_variable_table_data_1
  #       } else {
  #         ic_variable_p_avg_table <-
  #           rbind(ic_variable_p_avg_table, ic_variable_table_data_1)
  #       }
  #     }
  #     ic_variable_p_avg_table <-
  #       ic_variable_p_avg_table[, append("Period", input$Selections)]
  #   },
  #   spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  # })
  
})
}
