
# 1.0 Module UI -----------------------------------------------------------

gc_version_ui <- function(id) {
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
                     c("Level Values", "% y/y"),
                     selected = "Level Values",
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
                   5,
                   selectInput(
                     ns("Scenario"),
                     label = h4(str_to_title("Scenario"), style = "margin-bottom:-0.1em"),
                     sort(unique(gem_data$Scenario)),
                     selectize = FALSE
                   )
                 ),
                 column(
                   7,
                   selectInput(
                     ns("Attribute"),
                     label = h4("State", style = "margin-bottom:-0.1em"),
                     var_list,
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
                     choices = unique(gem_data$variable[gem_data$Scenario == "Baseline" &
                                                          gem_data$Attribute == "Gross Domestic Product"])[1],
                     selected = unique(gem_data$variable[gem_data$Scenario == "Baseline" &
                                                           gem_data$Attribute == "Gross Domestic Product"])[1],
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
                            value = "2050"
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

gc_version_server <- function(input, output, session) {
  
  # Update checkboxgroup options based on selected inputs -------------------
  observe({
    if (length(gem_data$variable[gem_data$Scenario == input$Scenario &
                             gem_data$Attribute == input$Attribute]) >= 1) {
      version_list <-
        data.frame(
          Attribute = input$Attribute,
          Scenario = input$Scenario,
          Release =  format(sort(as.yearqtr(unique(gem_data$Release[gem_data$Scenario == input$Scenario &
                                              gem_data$Attribute == input$Attribute]), format = "Q%q %Y"), decreasing = TRUE), format = "Q%q %Y")
        ) %>% add.var.col.gem(.)

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
    gc_version_data <- filter(gem_data,
                              variable %in% input$Selections) %>%
      transmute(., Dates,
                variable,
                value = round(as.numeric(value), 2))
    
    if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Sum") {
      gc_version_data <- trail_sum(gc_version_data)
    } else if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Avg") {
      gc_version_data <- trail_avg(gc_version_data)
    }
    if (input$display == "% y/y") {
      gc_version_data <- growth(gc_version_data, 4)
    }
    gc_version_data <-
      mutate(gc_version_data, value = round(value, 2)) %>%
      spread(., variable, value)
    gc_version_data <-
      gc_version_data[, append("Dates", input$Selections)]

    gc_version_input <-
      colnames(gc_version_data)[2:length(colnames(gc_version_data))]

    output$Plot <- renderPlotly({
      fig  <-  plot_ly(
        gc_version_data,
        x = ~ as.Date(Dates),
        y = gc_version_data[[gc_version_input[1]]],
        name = substr(
          gc_version_input[1],
          str_locate(gc_version_input[1], ",") + 2,
          nchar(gc_version_input[1])
        ),
        type = 'scatter',
        mode = 'lines',
        color = I(ox_pallette()[1])
      ) %>%
        layout(
          shapes = vline(unique(gem_data$hist_end[gem_data$variable == gc_version_input[1]])),
          yaxis = list(title = if (input$display == "Level Values") {
            unique(gem_data$Units[gem_data$variable == gc_version_input[1]])
          } else {
            "% y/y"
          }),
          xaxis = list(title = "Year"),
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.2
          )
        ) %>%
        add_annotations(
          x = unique(gem_data$hist_end[gem_data$variable == gc_version_input[1]]),
          y = 1,
          text = "              Forecast",
          yref = "paper",
          showarrow = FALSE
        ) %>%
        layout(title = list(
          text = paste0(
            '<b>',
            paste0(
              str_before_nth(gc_version_input[1], ",", 2),
              " - By Version Comparison"
            ),
            '<b>'
          ),
          x = 0.05,
          y = 0.99,
          font = list(
            family = "segoe ui",
            size = 18,
            color = ox_pallette()[2]
          )
        ))
      if (length(gc_version_input) >= 2) {
        for (i in 2:length(gc_version_input)) {
          fig <- fig %>% add_trace(
            y = gc_version_data[[gc_version_input[i]]],
            color = I(ox_pallette()[i]),
            name = substr(
              gc_version_input[i],
              str_locate(gc_version_input[i], ",") + 2,
              nchar(gc_version_input[i])
            )
          )
        }
      }
      return(fig)
    })
  })

  # Render Table ------------------------------------------------------------
  observe({

    cagr_dates <-
      c("2000",
        "2010",
        "2021",
        "2025",
        "2030",
        "2050")

    output$Table <- renderTable({

      gc_version_table_data <- filter(gem_data,
                                      variable %in% input$Selections)
      if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Sum") {
        gc_version_table_data <- trail_sum(gc_version_table_data)
      } else if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Avg") {
        gc_version_table_data <- trail_avg(gc_version_table_data)
      }
      gc_version_table_data <-
        filter(gc_version_table_data, month(Dates) == 6) %>% mutate(Dates = year(Dates))

      vc_version_table_data2 <- gc_version_table_data %>%
        filter(Dates %in% c(input$CAGR_start, input$CAGR_end)) %>%
        mutate(value = round((((value / lag(value)) ^ (1 / {
          Dates - lag(Dates)
        }) - 1
        ) * 100), 2)) %>%
        transmute(Period = paste0("(", lag(Dates), " - ", Dates, ")"),
                  variable, value) %>%
        filter(Period == paste0("(", input$CAGR_start, " - ", input$CAGR_end, ")")) %>%
        spread(variable, value) %>%
        mutate(Period = paste0("Custom Period\n" , Period))

      vc_version_table_data1 <- gc_version_table_data %>%
        filter(Dates %in% cagr_dates) %>%
        mutate(value = round((((value / lag(value)) ^ (1 / {
          Dates - lag(Dates)
        }) - 1
        ) * 100), 2)) %>%
        transmute(Period = paste0("(", lag(Dates), " - ", Dates, ")"),
                  variable, value) %>%
        filter(Period != paste0("(NA - ", min(cagr_dates), ")") &
                 Period != paste0("(",
                                  max(cagr_dates),
                                  " - ",
                                  min(cagr_dates),
                                  ")")) %>%
        spread(variable, value) %>%
        cbind(Names = c("2000s", "2010s", "Short run", "Medium run", "Long run")) %>%
        mutate(Period = paste0(Names, "\n" , Period)) %>%
        select(-Names)

      vc_version_table_data1 <-
        vc_version_table_data1[, append("Period", input$Selections)]

      vc_version_table_data1 <-
        rbind(vc_version_table_data1, vc_version_table_data2)
      names(vc_version_table_data1) <-
        gsub(paste0(input$Attribute,", ",input$Scenario,", "),"",
             names(vc_version_table_data1))
      return(vc_version_table_data1)
    },
    spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  })
}

# 3.0 Test Module ---------------------------------------------------------

# gc_version_demo <- function() {
#
#   select <- data.frame(SCENARIO_VALUE = c("CENTRAL","EXPORT_SUPERPOWER","SUSTAINABLE_GROWTH","RAPID_DECARB"))
#   ui <- navbarPage("Module Demo",
#                    gc_version_ui("Version", "By Region"))
#   server <- function(input, output, session) {
#     callModule(gc_version_server,"Version")
#     }
#   shinyApp(ui, server)
# }
#
# gc_version_demo()
