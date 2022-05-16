





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
                 fluidRow(
                   column(2, h4("Display:", style = "margin-top: 0.15em")),
                   column(
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
                     actionGroupButtons(
                       inputIds = c(ns("big4"), ns("small4"), ns("all")),
                       labels = c("Big 4", "Small 4", "All States"),
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
                     ns("Version"),
                     label = h4("Version", style = "margin-bottom:-0.1em"),
                     sort(unique(data$RELEASE_VERSION)),
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

sc_region_server <- function(input, output, session) {
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
  
  observe({
    updateSelectInput(session,
                      "Attribute",
                      label = NULL,
                      sort(unique(data$ATTRIBUTE[data$STATE == "NSW" &
                                                   data$RELEASE_VERSION == input$Version])))
  })
  
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
  
  ### --- update checkboxgroup options based on select inputs --- ###
  observe({
    if (length(data$variable[data$SCENARIO_VALUE == input$Scenario &
                             data$RELEASE_VERSION == input$Version &
                             data$ATTRIBUTE == input$Attribute]) >= 1) {
      states <- c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT", "ACT")
      
      version_list <-
        data.frame(
          ATTRIBUTE = input$Attribute,
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
        selected = as.list(version_list$variable[1:4]),
        prettyOptions = list(
          shape = "round",
          outline = TRUE,
          status = "primary"
        )
      )
    }
  })
  
  observe({
    bar_dates <- seq(2020, 2050, 10)
    
    sc_region_data <- filter(data,
                             sc_variable %in% unique(data$sc_variable[data$variable %in% input$Selections]))
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
    if (input$display != "Line chart") {
      sc_region_data <-
        trail_avg(sc_region_data, bar_dates[2] - bar_dates[1]) %>%
        mutate(., value = round(value, 2)) %>%
        filter(., Dates %in% bar_dates)
      sc_region_data$Dates <- as.factor(sc_region_data$Dates)
    }
    sc_region_data <- spread(sc_region_data, variable, value)
    
    output$Plot <- renderPlotly({
      fig <-
        if (input$display != "Line chart") {
          bar.plot(sc_region_data, input$Selections, "% of National", "%")
        } else {
          line.plot(sc_region_data, input$Selections, "% of National", "%")
        }
    })
  })
  
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
    },
    spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  })
  
}

# 3.0 Test Module ---------------------------------------------------------

# sc_region_demo <- function() {
#
#   select <- data.frame(SCENARIO_VALUE = c("CENTRAL","EXPORT_SUPERPOWER","SUSTAINABLE_GROWTH","RAPID_DECARB"))
#   ui <- navbarPage("Module Demo",
#                    sc_region_ui("Version", "By Region"))
#   server <- function(input, output, session) {
#     callModule(sc_region_server,"Version")
#     }
#   shinyApp(ui, server)
# }
#
# sc_region_demo()
