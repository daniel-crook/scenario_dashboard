
# 1.0 Module UI -----------------------------------------------------------

gc_scenario_ui <- function(id) {
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
                 fluidRow(
                 column(
                   12,
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
                     choices = sort(unique(gem_data$variable[gem_data$Attribute == "Gross Domestic Product"])),
                     selected = sort(unique(gem_data$variable[gem_data$Attribute == "Gross Domestic Product"])),
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

gc_scenario_server <- function(input, output, session) {
  
  # Update checkboxgroup options based on selected inputs -------------------
  observe({
    if (length(gem_data$variable[gem_data$Attribute == input$Attribute]) >= 1) {
      gem_data_f <- filter(gem_data, gem_data$Attribute == input$Attribute)
      
      sv_list <-
        unique(gem_data_f[c("Scenario", "Release")]) %>% mutate(Release = as.yearqtr(Release, format = "Q%q %Y"))
      
      sv_list <-
        arrange(sv_list,
                Scenario,
                desc(Release)) %>%
        mutate(Release = format(Release, format = "Q%q %Y")) %>%
        distinct(Scenario, .keep_all = TRUE)

      version_list <-
        data.frame(
          Attribute = input$Attribute,
          Scenario = sv_list$Scenario,
          Release = sv_list$Release
        ) %>% add.var.col.gem(.)
      
      updatePrettyCheckboxGroup(
        session,
        "Selections",
        label = NULL,
        version_list$variable,
        selected = version_list$variable,
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
    gc_scenario_data <- filter(gem_data,
                              variable %in% input$Selections) %>%
      transmute(., Dates,
                variable,
                value = round(as.numeric(value), 2))
    if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Sum") {
      gc_scenario_data <- trail_sum(gc_scenario_data)
    } else if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Avg") {
      gc_scenario_data <- trail_avg(gc_scenario_data)
    }
    if (input$display == "% y/y") {
      gc_scenario_data <- growth(gc_scenario_data, 4)
    }
    gc_scenario_data <-
      mutate(gc_scenario_data, value = round(value, 2)) %>%
      spread(., variable, value)
    gc_scenario_data <-
      gc_scenario_data[, append("Dates", input$Selections)]

    gc_scenario_input <-
      colnames(gc_scenario_data)[2:length(colnames(gc_scenario_data))]

    output$Plot <- renderPlotly({
      fig  <-  plot_ly(
        gc_scenario_data,
        x = ~ as.Date(Dates),
        y = gc_scenario_data[[gc_scenario_input[1]]],
        name = substr(
          gc_scenario_input[1],
          str_locate(gc_scenario_input[1], ",") + 2,
          nchar(gc_scenario_input[1])
        ),
        type = 'scatter',
        mode = 'lines',
        color = I(ox_pallette()[1])
      ) %>%
        layout(
          shapes = vline(unique(gem_data$hist_end[gem_data$variable == gc_scenario_input[1]])),
          yaxis = list(title = if (input$display == "Level Values") {
            unique(gem_data$Units[gem_data$variable == gc_scenario_input[1]])
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
          x = unique(gem_data$hist_end[gem_data$variable == gc_scenario_input[1]]),
          y = 1,
          text = "              Forecast",
          yref = "paper",
          showarrow = FALSE
        ) %>%
        layout(title = list(
          text = paste0(
            '<b>',
            paste0(
              str_before_first(gc_scenario_input[1], ","),
              " - By Scenario Comparison"
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
      if (length(gc_scenario_input) >= 2) {
        for (i in 2:length(gc_scenario_input)) {
          fig <- fig %>% add_trace(
            y = gc_scenario_data[[gc_scenario_input[i]]],
            color = I(ox_pallette()[i]),
            name = substr(
              gc_scenario_input[i],
              str_locate(gc_scenario_input[i], ",") + 2,
              nchar(gc_scenario_input[i])
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
      gc_scenario_table_data <- filter(gem_data,
                                      variable %in% input$Selections)

      if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Sum") {
        gc_scenario_table_data <- trail_sum(gc_scenario_table_data)
      } else if (unique(gem_data$Aggregation[gem_data$variable %in% input$Selections]) == "Avg") {
        gc_scenario_table_data <- trail_avg(gc_scenario_table_data)
      }
      gc_scenario_table_data <-
        filter(gc_scenario_table_data, month(Dates) == 6) %>% mutate(Dates = year(Dates))

      vc_version_table_data2 <- gc_scenario_table_data %>%
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

      vc_version_table_data1 <- gc_scenario_table_data %>%
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
        gsub(paste0(input$Attribute,", "),"",
             names(vc_version_table_data1))
      return(vc_version_table_data1)
    },
    spacing = "s", striped = TRUE, hover = TRUE, align = "l")
  })
}

# 3.0 Test Module ---------------------------------------------------------

# gc_scenario_demo <- function() {
#
#   select <- data.frame(SCENARIO_VALUE = c("CENTRAL","EXPORT_SUPERPOWER","SUSTAINABLE_GROWTH","RAPID_DECARB"))
#   ui <- navbarPage("Module Demo",
#                    gc_scenario_ui("Version", "By Region"))
#   server <- function(input, output, session) {
#     callModule(gc_scenario_server,"Version")
#     }
#   shinyApp(ui, server)
# }
#
# gc_scenario_demo()
