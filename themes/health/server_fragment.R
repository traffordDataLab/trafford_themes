# Server code for theme: Healthy Lives


#  Adults classified as overweight or obese--------------------------------------------------

nhsennpg <- read_csv("data/nhsennpg.csv") %>%
  select(area_code)

overweight_adult <- read_csv("data/health/overweight_adult.csv") %>%
  filter(indicator == "Overweight (including obesity) prevalence in adults") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

overweight_adult_nhsennpg_mean <- overweight_adult %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

overweight_adult_trend <- bind_rows(overweight_adult %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), overweight_adult_nhsennpg_mean)


output$overweight_adult_plot <- renderGirafe({
  
  if (input$overweight_adult_selection == "Trend") {
    
    gg <- 
      ggplot(
      filter(overweight_adult_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Adults classified as overweight or obese",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of adults classified as overweight or obese in Trafford between 2015/16 and 2022/23 compared with the average of similar authorities and England. Trafford has a similar proportion to the average of similar authorities for all years in the period shown, and a lower proportion than the England average. The trend for both the average of similar authorities and England is broadly consistent whilst Trafford's proportion varies more from year to year. The latest data for 2022/23 shows 60.4% of adults in Trafford were classified as overweight or obese (down from up from 61.7% the previous year), compared to 61.3% for the average of similar authorities and 64% for the England average."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(overweight_adult, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(overweight_adult, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(overweight_adult, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(overweight_adult, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(overweight_adult, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Adults classified as overweight or obese",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:",
           alt = "Box plot comparing the proportion of adults classified as overweight or obese in Trafford with England from 2015/16 to 2022/23. Trafford's proportion compared to England has been statistically similar in half of the years shown (2015/16, 2018/19, 2020/21 and 2021/22) and better in the other half.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_overweight_adult_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_overweight_adult_plot")
})

output$overweight_adult_box <- renderUI({
  withSpinner(
    girafeOutput("overweight_adult_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Active Adults --------------------------------------------------

active_adults <- read_csv("data/health/active_adults.csv") %>%
  filter(indicator == "Percentage of physically active adults") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

active_adults_nhsennpg_mean <- active_adults %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

active_adults_trend <- bind_rows(active_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), active_adults_nhsennpg_mean)


output$active_adults_plot <- renderGirafe({
  
  if (input$active_adults_selection == "Trend") {
    
    gg <- ggplot(
      filter(active_adults_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Physically active adults",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of adults who are physically active in Trafford between 2015/16 and 2022/23 compared with the average of similar authorities and England. In all but 2 of the years shown Trafford has had a higher percentage of physically active adults compared to its comparators (between 0.9 and 3.2 percentage points). The largest gaps between Trafford's percentage and that of its comparators occurred in 2020/21 and 2021/22, however the latest data for 2022/23 shows all 3 at very similar values: 68.4% in Trafford, 67.6% for the average of similar authorities and 67.1% for the national average."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(active_adults, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(active_adults, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(active_adults, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(active_adults, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(active_adults, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Physically active adults",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:",
           alt = "Box plot showing that the proportion of physically active adults in Trafford compared with England was statistically similar for the period shown from 2015/16 to 2022/23, with the exception of 2021/22 where it was better.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_active_adults_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_active_adults_plot")
})

output$active_adults_box <- renderUI({
  withSpinner(
    girafeOutput("active_adults_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Inactive Adults --------------------------------------------------

inactive_adults <- read_csv("data/health/inactive_adults.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

inactive_adults_nhsennpg_mean <- inactive_adults %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

inactive_adults_trend <- bind_rows(inactive_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), inactive_adults_nhsennpg_mean)


output$inactive_adults_plot <- renderGirafe({
  
  if (input$inactive_adults_selection == "Trend") {
    
    gg <- ggplot(
      filter(inactive_adults_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Physically inactive adults",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of adults who are physically inactive in Trafford between 2015/16 and 2022/23 compared with the average of similar authorities and England. In 2015/16 Trafford had the highest percentage (24%), followed by the national average (22.3%) and the average for similar authorities (20.8%). However from 2017/18 onwards Trafford's percentage has been the lowest amongst its comparators, and the lowest percentage of 19.9% occurred in 2018/19. The trends for all 3 lines have been broadly similar since 2018/19 with the values for the averages for England and similar authorities to Trafford being very similar, with Trafford between 0.9 and 2.4 percentage points lower. In 2022/23 21.1% of adults in Trafford were physically inactive compared to 22% for the average of similar authorities and 22.6% nationally."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(inactive_adults, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(inactive_adults, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(inactive_adults, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(inactive_adults, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(inactive_adults, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Physically inactive adults",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:",
           alt = "Box plot showing that the proportion of physically inactive adults in Trafford compared with England was statistically similar for the period shown from 2015/16 to 2022/23.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_inactive_adults_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_inactive_adults_plot")
})

output$inactive_adults_box <- renderUI({
  withSpinner(
    girafeOutput("inactive_adults_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Fairly Active Adults --------------------------------------------------

fairly_active_adults <- read_csv("data/health/fairly_active_adults.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

fairly_active_adults_nhsennpg_mean <- fairly_active_adults %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

fairly_active_adults_trend <- bind_rows(fairly_active_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), fairly_active_adults_nhsennpg_mean)


output$fairly_active_adults_plot <- renderGirafe({
  
  if (input$fairly_active_adults_selection == "Trend") {
    
    gg <- ggplot(
      filter(fairly_active_adults_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Physically fairly active adults",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of adults who are physically fairly active in Trafford between 2017 and 2023 compared with the average of similar authorities and England. The trend for Trafford's comparators over the time period shown has been decreasing from approximately 13% to 11% whereas Trafford's trend has been inconsistent, rising and falling and alternating between having the highest and lowest percentages from one year to the next. In 2022 Trafford recorded its lowest percentage for the time period of 8.6%, compared with 11% for its comparators.  However, the latest data for 2023 shows 11.1% of adults in Trafford being fairly active, the same as the average for similar local authorities, compared with 10.9% nationally."
      ) +
      theme_x()
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_fairly_active_adults_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_fairly_active_adults_plot")
})

output$fairly_active_adults_box <- renderUI({
  withSpinner(
    girafeOutput("fairly_active_adults_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Preventable under 75 mortality rate --------------------------------------------------

mortality_rate <- read_csv("data/health/mortality_rate.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

mortality_rate_nhsennpg_mean <- mortality_rate %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period, unit) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

mortality_rate_trend <- bind_rows(mortality_rate %>% select(area_name, period,value,unit) %>% filter(area_name %in% c("Trafford", "England")), mortality_rate_nhsennpg_mean) 

mortality_rate_persons <- mortality_rate %>%
  filter(unit == "Persons")


output$mortality_rate_plot <- renderGirafe({
  
  if (input$mortality_rate_selection == "Trend") {
    
    gg <- ggplot(
      filter(mortality_rate_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             unit == "Persons"),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Under 75 mortality rate from preventable causes",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS",
        x = NULL,
        y = "per 100,000 population",
        colour = NULL,
        alt = "Line chart showing the mortality rate of under 75 year olds from preventable causes per 100,000 population in Trafford between 2010 and 2022 compared with the average of similar authorities and England. Trafford's rate is very similar to the national, however the values fluctuate more from year to year, being higher or lower. The average for similar authorities to Trafford matches the national trend very closely, however at a lower rate. All 3 lines show a generally decreasing trend from 2010, followed by large increases between 2019-2021 before decreasing sharply in 2022. The latest data shows a Trafford rate of 156.4 (down from 171.7), compared to 153.7 nationally (down from 181.8) and 128.1 for the average of similar authorities (down from 158.3)."
      ) +
      theme_x()
  }
  else if (input$mortality_rate_selection == "Boxplot"){
    
    gg <- ggplot(data = filter(mortality_rate_persons, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(mortality_rate_persons, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(mortality_rate_persons, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(mortality_rate_persons, area_name == "England")$value, '</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(mortality_rate_persons, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Under 75 mortality rate from preventable causes",
           subtitle = NULL,
           caption = "Source: Annual Mortality Extracts, ONS",
           x = NULL, y = "per 100,000 population",
           fill = "Compared with England:",
           alt = "Box plot showing that the mortality rate of under 75 year olds from preventable causes in Trafford compared with England was statistically similar for the period shown from 2010 to 2022.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  } else {
    
    gg <- ggplot(
      filter(mortality_rate_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             unit != "Persons", period %in% c("2015":"2022")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      facet_wrap(~unit, strip.position = "top") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Under 75 mortality rate from preventable causes",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS",
        x = NULL,
        y = "per 100,000 population",
        colour = NULL,
        alt = "Line chart showing the mortality rate of under 75 year olds from preventable causes per 100,000 population in Trafford between 2015 and 2022 compared with the average of similar authorities and England by sex. There is a clear disparity between the sexes with the rate for females much lower than for males across all comparitors and years shown. The trend for the average of similar authorities to Trafford follows, but at a lower rate, that of the national for both females and males. Trafford's trend is broadly similar to England for females, starting just below and finishing just above the national rate. There is more variance with the trend for Trafford males, but similarly starts and ends very close to the national. The latest data for 2022 shows rates of 114.7 (females) and 199.9 (males) in Trafford compared to 107.3 (females) and 202.7 (males) for England and 88.6 (females) and 170.7 (males) for the average of similar authorities."
      ) +
      theme_x()
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_mortality_rate_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_mortality_rate_plot")
})

output$mortality_rate_box <- renderUI({
  withSpinner(
    girafeOutput("mortality_rate_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Healthy life expectancy at birth --------------------------------------------------

healthy_life_expectancy <- read_csv("data/health/healthy_life_expectancy.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

healthy_life_expectancy_nhsennpg_mean <- healthy_life_expectancy %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

healthy_life_expectancy_trend <- bind_rows(healthy_life_expectancy %>% select(area_name, period,value,inequality) %>% filter(area_name %in% c("Trafford", "England")), healthy_life_expectancy_nhsennpg_mean)


output$healthy_life_expectancy_plot <- renderGirafe({
  
  if (input$healthy_life_expectancy_selection == "Sex") {
    
    gg <- ggplot(healthy_life_expectancy_trend,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, ' years</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Healthy life expectancy at birth by sex",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS, Annual Population Survey",
        x = NULL,
        y = "years",
        colour = NULL,
        alt = "Line chart showing the health life expectancy at birth in Trafford between the time periods 2010 to 2012 and 2018 to 2020 compared with the average of similar authorities and England by sex. The trend for the average of similar authorities and England for both sexes is very similar to each other, with the values 2 to 3 years lower for the national average, whereas there is more variation in the values for Trafford's females and males. The latest 3 data points show that females and males in Trafford have similar healthy life expectancies from birth as the average of similar authorities, slightly higher than the national average. The latest data for 2018 to 2020 shows the health life expectancy from birth in Trafford for females to be 66.9 years compared with 65.9 for the average of similar authorities and 63.9 for England. The same data for males shows 66.3 years in Trafford, the same as the average of similar authorities, and 63.1 for England."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(healthy_life_expectancy, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(healthy_life_expectancy, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, ' years</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(healthy_life_expectancy, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(healthy_life_expectancy, area_name == "England")$value, ' years</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(healthy_life_expectancy, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Healthy life expectancy at birth by sex",
           subtitle = NULL,
           caption = "Source: Annual Mortality Extracts, ONS, Annual Population Survey",
           x = NULL,
           y = "years",
           fill = "Compared with England:",
           alt = "Box plot showing the health life expectancy at birth of females and males in Trafford compared with England between the time periods 2010 to 2012 and 2018 to 2020. In 3 of the 9 periods for females and 5 of 9 periods for males, health life expectancy at birth in Trafford was statistically better than England.  In the other periods Trafford was statistically similar to England.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_healthy_life_expectancy_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_healthy_life_expectancy_plot")
})

output$healthy_life_expectancy_box <- renderUI({
  withSpinner(
    girafeOutput("healthy_life_expectancy_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Unequal life expectancy at birth --------------------------------------------------

inequality_life_expectancy <- read_csv("data/health/inequality_life_expectancy.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

inequality_life_expectancy_nhsennpg_mean <- inequality_life_expectancy %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

inequality_life_expectancy_trend <- bind_rows(inequality_life_expectancy %>% select(area_name, period,value,inequality) %>% filter(area_name %in% c("Trafford", "England")), inequality_life_expectancy_nhsennpg_mean)


output$inequality_life_expectancy_plot <- renderGirafe({
  
  if (input$inequality_life_expectancy_selection == "Sex") {
    
    gg <- ggplot(inequality_life_expectancy_trend,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, ' years</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Inequality in life expectancy at birth by sex",
        subtitle = NULL,
        caption = "Source: Annual Mortality Extracts, ONS, IMD 2019, MHCLG",
        x = NULL,
        y = "years",
        colour = NULL,
        alt = "Line chart showing the difference in life expectancy at birth between the most and least deprived areas in Trafford between the periods 2010 to 2012 and 2018 to 2020 compared to the same for the average of similar authorities and England by sex. The average for similar authorities to Trafford is lower for both females (5.4 to 6 years) and males (7.3 to 7.9 years) than Trafford or the national average. Trafford's values interchange between being lower or higher than the national average, notably having the highest values in the period 2014-16. Since then values have decreased, the males at a faster rate, and the gap to the average of similar authorities is closing, whilst the national trend continues a steady rise. The latest data shows an England average of 7.9 years (females) and 9.7 years (males) compared with 7.4 years (females) and 8.5 years (males) in Trafford and 5.7 years (female) and 7.6 years (males) for the average of similar authorities."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(inequality_life_expectancy, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(inequality_life_expectancy, area_name == "Trafford"),
                             aes(x = period, y = value,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, ' years</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, fill = plot_colour_trafford, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(inequality_life_expectancy, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(inequality_life_expectancy, area_name == "England")$value, ' years</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(inequality_life_expectancy, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      facet_wrap(~inequality, strip.position = "top") +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Inequality in life expectancy at birth by sex",
           subtitle = NULL,
           caption = "Source: Annual Mortality Extracts, ONS, IMD 2019, MHCLG",
           x = NULL,
           y = "years",
           alt = "Box plot showing the difference in years of life expectancy at birth between the most and least deprived areas within Trafford and compares it with the same for England between the time periods 2010 to 2012 and 2018 to 2020. The data for Trafford is statistically similar to that of England for both sexes, although there is more variance in the data for males. Trafford's values lie above the interquartile range in 3 of the 9 periods for males, compared with 2 of the 9 for females.") +
      theme_x() 
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_inequality_life_expectancy_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_inequality_life_expectancy_plot")
})

output$inequality_life_expectancy_box <- renderUI({
  withSpinner(
    girafeOutput("inequality_life_expectancy_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

#  Proportion of smoking among persons 18 years and over. --------------------------------------------------

adults_smoking <- read_csv("data/health/adults_smoking.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value)) %>%
  mutate(compared_to_England = sub(" .*", "", compared_to_England))

adults_smoking_nhsennpg_mean <- adults_smoking %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

adults_smoking_trend <- bind_rows(adults_smoking %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), adults_smoking_nhsennpg_mean) 



output$adults_smoking_plot <- renderGirafe({
  
  if (input$adults_smoking_selection == "Trend") {
    
    gg <- ggplot(
      filter(adults_smoking_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Smoking Prevalence in adults (18+)",
        subtitle = NULL,
        caption = "Source: Annual Population Survey, ONS",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the percentage of smoking adults in Trafford between 2011 and 2022 compared to the average of similar authorities and England. 2011: Trafford 18.2%, England 19.8%, Similar Authorities average 18.1%. 2022: Trafford 8%, England 12.7%, Similar Authorities average 10.5%.  The prevalence is going down for all, although Trafford went up and down around the value of Similar authorities while both are below England in general."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(adults_smoking, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(adults_smoking, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(adults_smoking, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(adults_smoking, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(adults_smoking, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(
        title = "Smoking Prevalence in adults (18+)",
        subtitle = NULL,
        caption = "Source: Annual Population Survey, ONS",
        x = NULL, y = "Percentage",
        fill = "Compared with England:",
        alt = "Box plot showing that the percentage of smoking adults in Trafford compared with England was statistically similar in the majority of the periods shown from 2011 to 2022, and better in 2020 and 2022.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_adults_smoking_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_adults_smoking_plot")
})

output$adults_smoking_box <- renderUI({
  withSpinner(
    girafeOutput("adults_smoking_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Smoking adults in manual occupations --------------------------------------------------

adults_smoking_manual <- read_csv("data/health/adults_smoking_manual.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

adults_smoking_manual_nhsennpg_mean <- adults_smoking_manual %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

adults_smoking_manual_trend <- bind_rows(adults_smoking_manual %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), adults_smoking_manual_nhsennpg_mean)


output$adults_smoking_manual_plot <- renderGirafe({
  
  if (input$adults_smoking_manual_selection == "Trend") {
    
    gg <- ggplot(
      filter(adults_smoking_manual_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Smoking adults in routine and manual occupations",
        subtitle = NULL,
        caption = "Source: Annual Population Survey, ONS",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the percentage of smoking adults in routine and manual occupations in Trafford between 2011 and 2022 compared to the average of similar authorities and England. Whilst the trend is decreasing, Trafford's percentages have varied to a much greater extent year to year than its comparators. Between 2019 and 2022 Trafford's percentage has gone from the lowest to the highest and back to the lowest again. Trafford's percentages in 2019 and 2022 of 17.4% are the lowest values recorded during the time period. The latest data for 2022 shows 20.3% for the average of similar authorities and 22.5% for the national average compared to 17.4% in Trafford."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(adults_smoking_manual, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(adults_smoking_manual, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(adults_smoking_manual, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(adults_smoking_manual, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(adults_smoking_manual, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(
        title = "Smoking adults in routine and manual occupations",
        subtitle = NULL,
        caption = "Source: Annual Population Survey, ONS",
        x = NULL, y = "Percentage",
        fill = "Compared with England:",
        alt = "Box plot showing that the percentage of smoking adults in routine and manual occupations in Trafford compared with England was statistically similar for the period shown from 2011 to 2022.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_adults_smoking_manual_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_adults_smoking_manual_plot")
})

output$adults_smoking_manual_box <- renderUI({
  withSpinner(
    girafeOutput("adults_smoking_manual_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Adults with depression --------------------------------------------------

adults_depression <- read_csv("data/health/adults_depression.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

adults_depression_nhsennpg_mean <- adults_depression %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

adults_depression_trend <- bind_rows(adults_depression %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), adults_depression_nhsennpg_mean)


output$adults_depression_plot <- renderGirafe({
  
  if (input$adults_depression_selection == "Trend") {
    
    gg <- ggplot(
      filter(adults_depression_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Prevalence of adults with depression",
        subtitle = NULL,
        caption = "Quality and Outcomes Framework, NHS Digital",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the prevelance of adults with depression in Trafford between 2013/14 and 2022/23 compared with the average of similar authorities and England. The percentages for the average of similar authorities and England are close to each other with the former between 1.3 and 1.8 percentage points lower for the period shown. In contrast Trafford's percentage is higher than its comparitors and increasing at a faster rate. In 2013/14 the gap between Trafford and the average of similar authorities was 2.3 percentage points, however in 2022/23 the gap was 4.5 percentage points. The latest data shows the prevalence of adults with depression in Trafford to be 16.3% compared with 13.2% for the England average and 11.8% for the average of similar authorities."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(adults_depression, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(adults_depression, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(adults_depression, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(adults_depression, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(adults_depression, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Higher" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(
        title = "Prevalence of adults with depression",
        subtitle = NULL,
        caption = "Quality and Outcomes Framework, NHS Digital",
        x = NULL, y = "Percentage",
        fill = "Compared with England:",
        alt = "Box plot showing the prevelance of adults with depression in Trafford between 2013/14 and 2022/23 is statistically higher compared to England. Trafford's values for the periods shown are all above the upper quartile range.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_adults_depression_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_adults_depression_plot")
})

output$adults_depression_box <- renderUI({
  withSpinner(
    girafeOutput("adults_depression_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})
# Adults who walk or cycle 5 times per week ---------

# Load in data and create percentages as well as average of similar authorities
df_adults_walk_cycle <- read_csv("data/health/adults_walking_or_cycling.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 1))

# Plot
output$adults_walk_cycle_plot <- renderGirafe({
  gg <- ggplot(df_adults_walk_cycle,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Adults walking or cycling five times per week",
         subtitle = NULL,
         caption = "Source: DfT",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing the percentage of adults in Trafford regularly participating in walking or cycling activities (at least 5 times per week) compared with the average of similar authorities and England from 2016 to 2022. The latest data shows participation within Trafford to be higher than its comparitors with 35.9% of adults walking or cycling at least 5 times per week compared with 34.2% nationally and 33.3% for the average of similar local authorities. This is an increase of 5.9 percentage points in Trafford from the previous 12 months, where participation lower than both its comparators. National participation has only risen by 0.6 percentage points from the previous 12 months whilst the average of simmilar local authorities has increased by even less, just 0.1 percentage points.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_adults_walk_cycle_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_adults_walk_cycle_plot")
})

# Render the output in the ui object
output$adults_walk_cycle_box <- renderUI({
  withSpinner(
    girafeOutput("adults_walk_cycle_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

#  Total prescribed long acting reversible contraception (LARC) excluding injections rate / 1,000-----------------

contraception_larc <- read_csv("data/health/contraception_larc.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value)) %>%
  mutate(compared_to_England = sub(" .*", "", compared_to_England))

contraception_larc_nhsennpg_mean <- contraception_larc %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

contraception_larc_trend <- bind_rows(contraception_larc %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), contraception_larc_nhsennpg_mean) 



output$contraception_larc_plot <- renderGirafe({
  
  if (input$contraception_larc_selection == "Trend") {
    
    gg <- ggplot(
      filter(contraception_larc_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Total prescribed LARC per 1,000 females aged 15-44 ",
        subtitle = NULL,
        caption = "Source: NHS/ONS",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the prescribed long acting reversible contraception (LARC) excluding injections rate per 1,000 in Trafford between 2014 and 2022 compared to the average of similar authorities and England. 2014: Trafford 36.8, England 50.2, Similar Authorities average 41.1. 2022: Trafford 42.5, England 44.1, Similar Authorities average 37.9. Trafford rate was under the comparators value until 2021 when it was above Similar authorities but below England's rate."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(contraception_larc, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(contraception_larc, area_name == "Trafford"),
                             aes(x = period, y = value, 
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", fill = "#00445e", size = 3) +
      geom_boxplot_interactive(data = filter(contraception_larc, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(contraception_larc, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(contraception_larc, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(
        title = "Total prescribed LARC per 1,000 females aged 15-44",
        subtitle = NULL,
        caption = "Source: NHS/ONS",
        x = NULL, y = "per 1,000",
        alt = "Box plot showing that the prescribed long acting reversible contraception (LARC) excluding injections rate per 1,000 in Trafford compared with England was statistically similar for the period shown from 2014 to 2022.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_contraception_larc_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_contraception_larc_plot")
})

output$contraception_larc_box <- renderUI({
  withSpinner(
    girafeOutput("contraception_larc_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

#  Chlamydia proportion of females aged 15 to 24 screened----------------

chlamydia_screening <- read_csv("data/health/chlamydia_screening.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

chlamydia_screening_nhsennpg_mean <- chlamydia_screening %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

chlamydia_screening_trend <- bind_rows(chlamydia_screening %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), chlamydia_screening_nhsennpg_mean) 



output$chlamydia_screening_plot <- renderGirafe({
  
  if (input$chlamydia_screening_selection == "Trend") {
    
    gg <- ggplot(
      filter(chlamydia_screening_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "Chlamydia proportion of females aged 15 to 24 screened",
        subtitle = NULL,
        caption = "Source: NHS England/ONS",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the Chlamydia proportion of females aged 15 to 24 screened in Trafford between 2016 and 2023 compared to the average of similar authorities and England. 2016: Trafford 27.6%, England 21%, Similar Authorities average 20.5%. 2023: Trafford 17.2%, England 20.4%, Similar Authorities average 19.2%. Trafford proportion have been under the comparators value except from 2016 when it was above England's and Similar authorities' proportion."
      ) +
      theme_x()  
    }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_chlamydia_screening_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_chlamydia_screening_plot")
})

output$chlamydia_screening_box <- renderUI({
  withSpinner(
    girafeOutput("chlamydia_screening_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

#  Broad Spectrum antibiotics as a % of total prescription items for oral antibiotics----------------

antibiotics_broad_s <- read_csv("data/health/antibiotics_broad_s.csv") %>%
  #mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

# antibiotics_broad_s_nhsennpg_mean <- antibiotics_broad_s %>%
#   filter(area_code %in% c(nhsennpg$area_code)) %>%
#   group_by(period) %>%
#   summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
#   mutate(area_name = "Similar Authorities average",
#          period = as_factor(period)) %>%
#   filter(!is.na(value))

# antibiotics_broad_s_trend <- bind_rows(antibiotics_broad_s %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), antibiotics_broad_s_nhsennpg_mean) 


output$antibiotics_broad_s_plot <- renderGirafe({
  
  if (input$antibiotics_broad_s_selection == "Trend") {
    
    gg <- 
      ggplot(
      filter(antibiotics_broad_s, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      scale_x_date(breaks = seq(as.Date("2019-09-01"), max(antibiotics_broad_s$period), by="3 months"), date_labels="%b %Y") +
      labs(
        title = "Broad Spectrum antibiotics as a % of total prescribed",
        subtitle = NULL,
        caption = "Source: https://openprescribing.net",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the Chlamydia proportion of females aged 15 to 24 screened in Trafford between 2016 and 2023 compared to the average of similar authorities and England. 2016: Trafford 27.6%, England 21%, Similar Authorities average 20.5%. 2023: Trafford 17.2%, England 20.4%, Similar Authorities average 19.2%. Trafford proportion have been under the comparators value except from 2016 when it was above England's and Similar authorities' proportion."
      ) +
      theme_x()  
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_antibiotics_broad_s_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_antibiotics_broad_s_plot")
})

output$antibiotics_broad_s_box <- renderUI({
  withSpinner(
    girafeOutput("antibiotics_broad_s_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

#  Cumulative percentage of the eligible population, aged 40 – 74 years, receiving an NHS Health Check--------------------

nhs_health_checks <- read_csv("data/health/nhs_health_checks.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

nhs_health_checks_nhsennpg_mean <- nhs_health_checks %>%
  filter(area_code %in% c(nhsennpg$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

nhs_health_checks_trend <- bind_rows(nhs_health_checks %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), nhs_health_checks_nhsennpg_mean) %>%
  mutate(period = sub("2019/20 Q1 - ", "", period))

output$nhs_health_checks_plot <- renderGirafe({
  
  if (input$nhs_health_checks_selection == "Trend") {
    
    gg <- ggplot(
      filter(nhs_health_checks_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
      aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(aes(tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, size = 2.5, colour = "white") +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
        title = "People receiving an NHS Health Check",
        subtitle = NULL,
        caption = "NHS Health Check Programme, OHID",
        x = "From 2019/20 Q1 to",
        y = "Cumulative percentage",
        colour = NULL,
        alt = "Line chart showing cumulative percentage of the eligible population, aged 40 to 74 years, receiving an NHS Health Check in Trafford from 2019/20 Q1 to 2023/24 Q4 compared with the average of similar authorities and England. 2019/20 Q1: Trafford 1.8%, England 2%, Similar Authorities average 1.8%. 2023/24 Q4: Trafford 38.1%, England 28.1%, Similar Authorities average 25.7%. Trafford cumulative percentage has been above both comparators."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(nhs_health_checks, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(nhs_health_checks, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(nhs_health_checks, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(nhs_health_checks, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(nhs_health_checks, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Higher" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(
        title = "Prevalence of adults with depression",
        subtitle = NULL,
        caption = "Quality and Outcomes Framework, NHS Digital",
        x = NULL, y = "Percentage",
        fill = "Compared with England:",
        alt = "Box plot showing cumulative percentage of the eligible population, aged 40 to 74 years, receiving an NHS Health Check in Trafford from 2019/20 Q1 to 2023/24 Q4 compared with the average of similar authorities and England. Trafford's values for the periods shown are all in the upper quartile range.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_nhs_health_checks_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_nhs_health_checks_plot")
})

output$nhs_health_checks_box <- renderUI({
  withSpinner(
    girafeOutput("nhs_health_checks_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Permanent admissions to residential and nursing care homes per 100,000 aged 65+

# Load in data
df_admissions_care_homes_65p <- read_csv("data/health/admissions_care_homes_65p.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 1))

# Plot
output$admissions_care_homes_65p_plot <- renderGirafe({
  gg <- ggplot(df_admissions_care_homes_65p,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, ' per 100,000 aged 65+</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(-0.05, NA)) +
    labs(title = "Permanent admissions to care homes aged 65+",
         subtitle = NULL,
         caption = "Source: NHS England",
         x = NULL,
         y = "Per 100,000 aged 65+",
         fill = NULL,
         alt = "Line chart showing permanent admissions to residential and nursing care homes per 100,000 aged 65+ in Trafford compared with the average of similar authorities from 2014/15 to 2022/23. 2014/15: Trafford 543, England 658.5, Similar Authorities average 496.6. 2022/23: Trafford 545, England 560.8, Similar Authorities average 467.7. Trafford had been above England and Similar authorities but in 2022/23, Trafford is just below England and slighly above Similar authorities.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_admissions_care_homes_65p_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_admissions_care_homes_65p_plot")
})

# Render the output in the ui object
output$admissions_care_homes_65p_box <- renderUI({
  withSpinner(
    girafeOutput("admissions_care_homes_65p_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Proportion of Social Care Service users who are satisfied with their care and support

# Load in data
df_social_care_satisfaction <- read_csv("data/health/social_care_satisfaction.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 1))

# Plot
output$social_care_satisfaction_plot <- renderGirafe({
  gg <- ggplot(df_social_care_satisfaction,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(-0.05, NA)) +
    labs(title = "% of users satisfied with Social Care Services",
         subtitle = NULL,
         caption = "Source: NHS England",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing Proportion of Social Care Service users who are satisfied with their care and support in Trafford compared with the average of similar authorities from 2014/15 to 2022/23. 2014/15: Trafford 63.8%, England 64.7%, Similar Authorities average 61.9%. 2022/23: Trafford 62.5%, England 64.4%, Similar Authorities average 62.3%. Trafford has been close to England and Similar Authorities except from 2018/19 when Trafford was higher than comparators.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_social_care_satisfaction_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_social_care_satisfaction_plot")
})

# Render the output in the ui object
output$social_care_satisfaction_box <- renderUI({
  withSpinner(
    girafeOutput("social_care_satisfaction_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Percentage of care homes rated overall as good or outstanding

# Load in data
df_care_homes_rating <- read_csv("data/health/care_homes_rating.csv") %>%
  mutate(period = as.Date(paste0("01 ",period), format = "%d %B %Y")) %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 1))

# Plot
output$care_homes_rating_plot <- renderGirafe({
  gg <- 
    ggplot(df_care_homes_rating,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(-0.05, NA)) +
    #scale_x_date(date_labels = "%b %y", date_breaks = "3 month", expand = c(0.06,0.06)) +
    scale_x_date(breaks = seq(as.Date("2019-09-01"), max(df_care_homes_rating$period), by="3 months"), date_labels="%b %Y") +
    labs(title = "% of care homes rated overall as good or outstanding",
         subtitle = NULL,
         caption = "Source: NHS England",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing Proportion of care homes rated overall as good or outstanding in Trafford compared with the average of similar authorities and England from June 2022 to August 2024. June 2022: Trafford 77.6%, England 78.8%, Similar Authorities average 80.7%.  August 2024: Trafford 62.5%, England 76.5%, Similar Authorities average 81.73%. Trafford had been above comparators from June 2023.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_care_homes_rating_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_care_homes_rating_plot")
})

# Render the output in the ui object
output$care_homes_rating_box <- renderUI({
  withSpinner(
    girafeOutput("care_homes_rating_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


