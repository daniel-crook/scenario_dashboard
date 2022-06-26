
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
                   3, h4("Display:", style = "margin-top: -0.5em")
                 )),
                 fluidRow(column(
                   6,
                   radioGroupButtons(
                     ns("display"),
                     NULL,
                     c("Levels", "% y/y", "% q/q"),
                     selected = "% y/y",
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
                   10,
                   radioGroupButtons(
                     ns("P_T_A"),
                     NULL,
                     c("Point", "Annual Total", "Annual Average"),
                     selected = "Point",
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
                 fluidRow(
                 column(
                   12,
                   selectInput(
                     ns("Attribute"),
                     label = h4("Attribute", style = "margin-bottom:-0.1em"),
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

gc_scenario_server <- function(id, gem_data) {
  moduleServer(id, function(input, output, session) {
  # Update checkboxgroup options based on selected inputs -------------------
  observe({
    if (input$Attribute %in% gem_data$Attribute) {
      gem_data_f <- filter(gem_data, gem_data$Attribute == input$Attribute[1])

      sv_list <-
        unique(gem_data_f[c("Scenario", "Release")]) %>% separate(Release, c('Release_Date', 'Version'))

      sv_list$Release_Date <-
        parse_date_time(sv_list$Release_Date, "my")

      sv_list <-
        arrange(sv_list,
                Scenario,
                desc(Release_Date),
                desc(Version)) %>%
        transmute(Scenario,
                  Release = paste(format(Release_Date, format = "%b%y"), Version, sep = " ")) %>%
        distinct(Scenario, .keep_all = TRUE)

      version_list <-
        data.frame(
          Attribute = input$Attribute,
          Scenario = sv_list$Scenario,
          Release = sv_list$Release
        ) %>%
        add.var.col.gem(.)

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
    gc_scenario_data <- filter(gem_data,
                              variable %in% input$Selections) %>%
      transmute(., Dates,
                variable,
                value = round(as.numeric(value), 2))
    if (input$P_T_A == "Annual Total") {
      gc_scenario_data <- trail_sum(gc_scenario_data)
    } else if (input$P_T_A == "Annual Average") {
      gc_scenario_data <- trail_avg(gc_scenario_data)
    }
    if (input$display == "% y/y") {
      gc_scenario_data <- growth(gc_scenario_data, 4)
    }
    if (input$display == "% q/q") {
      gc_scenario_data <- growth(gc_scenario_data, 1)
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
        name = str_after_first(input$Selections[1],", "),
        type = 'scatter',
        mode = 'lines',
        color = I(ox_pallette()[1]),
        hoverlabel = list(namelength = -1)
      ) %>%
        layout(
          shapes = vline(unique(gem_data$hist_end[gem_data$variable == gc_scenario_input[1]])),
          yaxis = list(
          showgrid = F,
          showline = T,
          linecolor = "#495057",
          ticks = "outside",
          tickcolor = "#495057",
          tickformat = ",",
          ticksuffix = if(input$display == "% y/y" | input$display == "% q/q"){"%"} else {NULL}
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
          x = unique(gem_data$hist_end[gem_data$variable == gc_scenario_input[1]]),
          y = 1,
          text = "              Forecast",
          yref = "paper",
          showarrow = FALSE
        ) %>%
        add_annotations(
          x = min(gc_scenario_data$Dates[!is.na(gc_scenario_data[[input$Selections[1]]])]),
          y = 1.035,
          text = if (input$display == "% y/y") {
            "% y/y"
          } else if (input$display == "% q/q") {
            "% q/q"
          } else {
            unique(gem_data$Units[gem_data$variable == gc_scenario_input[1]])
          },
          yref = "paper",
          xanchor = "left",
          showarrow = FALSE
        )
      if (length(gc_scenario_input) >= 2) {
        for (i in 2:length(gc_scenario_input)) {
          fig <- fig %>% add_trace(
            y = gc_scenario_data[[gc_scenario_input[i]]],
            color = I(ox_pallette()[i]),
            name = str_after_first(input$Selections[i],", "),
            hoverlabel = list(namelength = -1)
          )
        }
      }
      if (input$title == "Title On") {
        fig <- fig %>%
          layout(title = list(
            text = paste0(
              str_before_first(gc_scenario_input[1], ","),
              " - By Scenario Comparison"
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
    
  })
}

