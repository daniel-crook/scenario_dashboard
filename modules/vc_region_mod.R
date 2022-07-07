
# 1.0 Module UI -----------------------------------------------------------

vc_region_ui <- function(id) {
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
                     4,
                     actionGroupButtons(
                       inputIds = c(ns("big4"), ns("small4"), ns("all")),
                       labels = c("Big", "Small", "All"),
                       status = "primary"
                     )
                   ),
                   style = "margin-bottom:-1.0em"
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
                     sort(unique(data$RELEASE_VERSION[data$SCENARIO_VALUE == sort(unique(data$SCENARIO_VALUE))[1]])),
                     selectize = FALSE
                   )
                 ), style = "margin-bottom:-2em; margin-top:-2em"),
                 h4("Attribute", style = "margin-bottom: 0.2em; margin-top: 1em"),
                 fluidRow(column(
                   12,
                   selectInput(
                     ns("Attribute"),
                     label = NULL,
                     sort(unique(data$ATTRIBUTE[data$STATE == "NSW"])),
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
                     choices = unique(data$variable[data$SCENARIO_VALUE == sort(unique(data$SCENARIO_VALUE))[1] &
                                                      data$RELEASE_VERSION == data$RELEASE_VERSION[data$SCENARIO_VALUE == sort(unique(data$SCENARIO_VALUE))[1]][1] &
                                                      data$ATTRIBUTE == "Attached Dwellings"]),
                     selected = unique(data$variable[data$SCENARIO_VALUE == sort(unique(data$SCENARIO_VALUE))[1] &
                                                       data$RELEASE_VERSION == data$RELEASE_VERSION[data$SCENARIO_VALUE == sort(unique(data$SCENARIO_VALUE))[1]][1] &
                                                       data$ATTRIBUTE == "Attached Dwellings"]),
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
                          ), ),
                   column(1,
                          h4("-", style = "margin-top: 0.2em"), ),
                   column(3,
                          textInput(
                            ns("CAGR_end"),
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

vc_region_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

# Update Version Select options based on Scenario Selection ---------------

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

# Update Attribute Select Options based on Version Selection --------------

    observe({
      updateSelectInput(session,
                        "Attribute",
                        label = NULL,
                        sort(unique(data$ATTRIBUTE[data$STATE == "NSW" &
                                                     data$RELEASE_VERSION == input$Version])))
    })

# Update Checkgroup options based on selected inputs ----------------------
   
    observe({
      if (length(data$variable[data$SCENARIO_VALUE == input$Scenario &
                               data$RELEASE_VERSION == input$Version &
                               data$ATTRIBUTE == input$Attribute]) >= 1) {
        states <- c("AUS","NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")
        
        version_list <-
          data.frame(
            ATTRIBUTE = c(unique(data$ATTRIBUTE[data$STATE == "AUS" & data$Series_ID == unique(data$Series_ID[data$ATTRIBUTE == input$Attribute])]),
                          rep(input$Attribute,length(states)-1)),
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
          selected = as.list(version_list$variable[1:5]),
          prettyOptions = list(
            shape = "round",
            outline = TRUE,
            status = "primary"
          )
        )
      }
    })
    
# Big State Button --------------------------------------------------------

    observeEvent(input$big4, {
      states <- c("AUS" ,"NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")

      version_list <-
        data.frame(
          ATTRIBUTE = c(unique(data$ATTRIBUTE[data$STATE == "AUS" & data$Series_ID == unique(data$Series_ID[data$ATTRIBUTE == input$Attribute])]),
                        rep(input$Attribute,length(states)-1)),
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
        selected = as.list(version_list$variable[2:5]),
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    })

# Small States Button -----------------------------------------------------

    observeEvent(input$small4, {
      states <- c("AUS" ,"NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")

      version_list <-
        data.frame(
          ATTRIBUTE = c(unique(data$ATTRIBUTE[data$STATE == "AUS" & data$Series_ID == unique(data$Series_ID[data$ATTRIBUTE == input$Attribute])]),
                        rep(input$Attribute,length(states)-1)),
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
        selected = as.list(version_list$variable[c(6:9)]),
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    })

# All States Button -------------------------------------------------------

    observeEvent(input$all, {
      states <- c("AUS" ,"NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")

      version_list <-
        data.frame(
          ATTRIBUTE = c(unique(data$ATTRIBUTE[data$STATE == "AUS" & data$Series_ID == unique(data$Series_ID[data$ATTRIBUTE == input$Attribute])]),
                        rep(input$Attribute,length(states)-1)),
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
        selected = as.list(version_list$variable[1:9]),
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    })
    
# Render Plot -------------------------------------------------------------

    observe({
      vc_region_data <- filter(data,
                               variable %in% input$Selections) %>%
        transmute(., Dates,
                  variable,
                  value = round(value, 2))
      if (input$display == "% y/y") {
        vc_region_data <- growth(vc_region_data, 1)
      }
      vc_region_data <-
        mutate(vc_region_data, value = round(value, 2)) %>%
        spread(., variable, value)
      vc_region_data <-
        vc_region_data[, append("Dates", input$Selections)]
      
      output$Plot <- renderPlotly({
        fig  <-  plot_ly(
          vc_region_data,
          x = ~ Dates,
          y = vc_region_data[[input$Selections[1]]],
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
              showgrid = F,
              showline = T,
              linecolor = "#495057",
              ticks = "outside",
              tickcolor = "#495057",
              tickformat = ",",
              ticksuffix = if(input$display == "% y/y"){"%"} else {NULL}
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
              y = -0.15
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
            x = min(vc_region_data$Dates[!is.na(vc_region_data[[input$Selections[1]]])]),
            y = 1.035,
            text = if (input$display == "% y/y") {
              "% y/y"
            } else {
              unique(data$UNIT[data$variable == input$Selections[1]])
            },
            yref = "paper",
            xanchor = "left",
            showarrow = FALSE
          )
        
        if (length(input$Selections) >= 2) {
          for (i in 2:length(input$Selections)) {
            fig <- fig %>% add_trace(
              y = vc_region_data[[input$Selections[i]]],
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
        vc_region_table_data2 <-
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
        
        vc_region_table_data <-
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
        
        vc_region_table_data <-
          vc_region_table_data[, append("Period", input$Selections)]
        
        vc_region_table_data <-
          rbind(vc_region_table_data, vc_region_table_data2)
        
        names(vc_region_table_data) <-
          gsub(paste0(str_before_first(input$Selections[1], ", "), ", "),
               "",
               names(vc_region_table_data))
        names(vc_region_table_data) <-
          gsub(paste0(", ", str_after_nth(input$Selections[1], ", ", 2)),
               "",
               names(vc_region_table_data))
        
        return(vc_region_table_data)
      },
      spacing = "s", striped = TRUE, hover = TRUE, align = "l")
    })
    
  })
}
