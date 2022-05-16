
# 1.0 Module UI -----------------------------------------------------------

ic_version_ui <- function(id) {
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
                   2, h4("Display:", style = "margin-top: 0.15em")
                 ),
                 column(
                   10,
                   radioGroupButtons(
                     ns("display"),
                     NULL,
                     choices = c("Line chart", "Bar chart"),
                     selected = "Line chart",
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
                     sort(unique(data$ATTRIBUTE[substr(data$ATTRIBUTE, 1, 3) == "GVA"])),
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

ic_version_server <- function(input, output, session) {
  

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
      add.ic.col(.)
    
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
  
# Resets Attribute Select Input -------------------------------------------

  observe({
    updateSelectInput(session,
                      "Attribute",
                      label = NULL,
                      sort(unique(data$ATTRIBUTE[data$STATE == input$State &
                                                   data$SCENARIO_VALUE == input$Scenario & 
                                                   substr(data$ATTRIBUTE, 1, 3) == "GVA"])))
  })
  
# Render Plot -------------------------------------------------------------

  observe({
    output$Plot <- renderPlotly({
      bar_dates <- seq(2020, 2050, 10)
      
      ic_version_data <- filter(data,
                                ic_variable %in% unique(data$ic_variable[data$variable %in% input$Selections]))
      
      ic_version_data_a <- filter(ic_version_data,
                                  variable %in% input$Selections)
      ic_version_data_b <-
        filter(
          ic_version_data,
          Series_ID %in% c("GVAA_BLCC", "INDPRODLCC", "GVAFLCC", "SVCSGVALCC")
        )
      ic_version_data_b <-
        aggregate(
          x = ic_version_data_b$value,
          by = list(
            ic_version_data_b$Dates,
            ic_version_data_b$ic_variable
          ),
          FUN = sum
        ) %>%
        transmute(.,
                  Dates = Group.1,
                  ic_variable = Group.2,
                  GVATOT = x)
      ic_version_data <- merge(ic_version_data_a,
                               ic_version_data_b,
                               by = c("Dates", "ic_variable")) %>%
        transmute(.,
                  Dates,
                  variable,
                  value = round(VALUE / GVATOT * 100, 2))
      if (input$display != "Line chart") {
        ic_version_data <-
          trail_avg(ic_version_data, bar_dates[2] - bar_dates[1]) %>%
          mutate(., value = round(value, 2)) %>%
          filter(., Dates %in% bar_dates)
        ic_version_data$Dates <- as.factor(ic_version_data$Dates)
      }
      ic_version_data <- spread(ic_version_data, variable, value)
      
      fig <-
        if (input$display != "Line chart") {
          bar.plot(ic_version_data, input$Selections, "% of Total GVA", "%")
        } else {
          line.plot(ic_version_data, input$Selections, "% of Total GVA", "%")
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
      ic_version_table_data <- filter(data,
                                ic_variable %in% unique(data$ic_variable[data$variable %in% input$Selections]))
      
      ic_version_table_data_a <- filter(ic_version_table_data,
                                  variable %in% input$Selections)
      ic_version_table_data_b <-
        filter(
          ic_version_table_data,
          Series_ID %in% c("GVAA_BLCC", "INDPRODLCC", "GVAFLCC", "SVCSGVALCC")
        )
      ic_version_table_data_b <-
        aggregate(
          x = ic_version_table_data_b$value,
          by = list(
            ic_version_table_data_b$Dates,
            ic_version_table_data_b$ic_variable
          ),
          FUN = sum
        ) %>%
        transmute(.,
                  Dates = Group.1,
                  ic_variable = Group.2,
                  GVATOT = x)
      ic_version_table_data <- merge(ic_version_table_data_a,
                                     ic_version_table_data_b,
                               by = c("Dates", "ic_variable")) %>%
        transmute(.,
                  Dates,
                  variable,
                  value = round(VALUE / GVATOT * 100, 2))
      
      for (i in c(2:6, length(period_dates))) {
        ic_version_table_data_1 <-
          ic_version_table_data %>%
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
        
        ic_version_table_data_1 <-
          spread(ic_version_table_data_1, variable, value)
        
        if (i == 2) {
          ic_version_p_avg_table <- ic_version_table_data_1
        } else {
          ic_version_p_avg_table <-
            rbind(ic_version_p_avg_table, ic_version_table_data_1)
        }
      }
      ic_version_p_avg_table <-
        ic_version_p_avg_table[, append("Period", input$Selections)]
    },
    spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  })
  
}
