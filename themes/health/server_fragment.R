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
        alt = "Line chart showing the proportion of adults classified as overweight or obese in Trafford between 2015/16 and 2019/20 compared with the average of similar authorities and England. Trafford has a lower proportion than the average of similar authorities for all years in the period shown, and a lower proportion than England except for 2018/19. The trend for both the average of similar authorities and England is broadly consistent whilst Trafford's proportion varies more from year to year. The latest data for 2019/21 shows 60.7% of adults in Trafford were classified as overweight or obese (down from up from 59.3% the previous year), compared to 63.5% in England and 64.7% for the average of similar authorities."
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
           alt = "Box plot comparing the proportion of adults classified as overweight or obese in Trafford with England from 2015/16 to 2020/21. Trafford's proportion compared to England has been statistically similar in 3 of the 6 years shown (2015/16, 2018/19 and 2020/2021) and better in the other 3.") +
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
        alt = "Line chart showing the proportion of adults who are physically active in Trafford between 2015/16 and 2020/21 compared with the average of similar authorities and England. Throughout the period shown, the average of similar authorities to Trafford has had a slightly lower proportion than England, (less than 1 percentage point). In 2015/16 Trafford had the lowest proportion of physically active adults compared to its comparitors (64% compared to 65.3% for the average of similar authorities and 66.1% for England), however for the other 4 years in the period shown Trafford's proportion has been higher. The latest data for 2020/21 showed 68.2% of Trafford's adults were physically active compared with 65.9% in England and 66% for the average of similar authorities."
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
           alt = "Box plot showing that the proportion of physically active adults in Trafford compared with England was statistically similar for the period shown from 2015/16 to 2020/21.") +
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
        alt = "Line chart showing the proportion of adults who are physically inactive in Trafford between 2015/16 and 2020/21 compared with the average of similar authorities and England. Between 2015/16 and 2018/19 the proportion in Trafford decreased from 24% to 19.9%. In 2015/16 Trafford had the highest proportion of physically inactive adults compared with the average of similar authorities and England, however in the following and subsequent years it has been below its comparitors. Over the same period (2015/16 to 2018/19) the trend for the average of similar authorities and England has been broadly consistent and similar to each other. Between 2018/19 and 2019/20 all 3 lines show an increase in the proportion of inactive adults, with the average of similar authorities increasing the most (from 21.2% to 23.9%), followed by England (from 21.4% to 22.9%) and then Trafford (from 19.9% to 20.6%). Trafford increased to 22.2% in 2020/21"
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
           alt = "Box plot showing that the proportion of physically inactive adults in Trafford compared with England was statistically similar for the period shown from 2015/16 to 2020/21.") +
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
        alt = "Line chart showing the proportion of adults who are physically fairly active in Trafford between 2016 and 2020 compared with the average of similar authorities and England. Whilst the proportions for the average of similar authorities and England have been on a generally similar and decreasing trend over the period shown, the proportions in Trafford have gone up and down year to year changing by approximately 1.5 to just over 2 percentage points each time. Between 2019 and 2020 the proportion in Trafford increased whilst the other comparitors decreased. The latest data for 2021 shows  a decrease of 0.8% form the previous year to 11.5% of adults in Trafford that are physically fairly inactive compared with 11.5% in England and 11.9% for the average of similar authorities."
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
        alt = "Line chart showing the mortality rate of under 75 year olds from preventable causes per 100,000 population in Trafford between 2010 and 2020 compared with the average of similar authorities and England. All 3 lines show broadly similar rates and a general decreasing trend over the period shown, however Trafford's rate has fluctuated more than its comparitors. Trafford's highest rate was in 2013 at 161.6 where it was also the highest amongst its comparitors, however the year later it fell to 144.6 which was lower than both the average for similar authorities (150) and England (147.6). Trafford has had the lowest rate amongst its comparitors for 5 of the 11 years shown, including 2020 which has the lowest rate during the time period shown of 131 compared with 140.5 for England and 140.5 for the average of similar authorities."
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
           alt = "Box plot showing that the mortality rate of under 75 year olds from preventable causes in Trafford compared with England was statistically similar for the period shown from 2010 to 2020.") +
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
        alt = "Line chart showing the mortality rate of under 75 year olds from preventable causes per 100,000 population in Trafford between 2015 and 2020 compared with the average of similar authorities and England by sex. There is a clear disparity between the sexes with the rate for females much lower than for males across all comparitors and years shown. The rates for both females and males in Trafford are generally lower than their comparitors, with both recording lower rates in 4 of the 6 years shown and the latest data for 2020 shows Trafford recording the lowest rates for both sexes during this period. The latest data for 2020 shows the under 75 year old mortality rate from preventable causes for females in Trafford as 92.6 per 100,000 compared with 94.7 for the average of similar authorities and 96.1 for England. The same data for males shows Trafford's rate at 171.4 per 100,000 compared with 186.9 for England and 188.1 for the average of similar authorities."
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
        alt = "Line chart showing the health life expectancy at birth in Trafford between the time periods 2010 to 2012 and 2018 to 2020 compared with the average of similar authorities and England by sex. The trend and values for the average of similar authorities and England for both sexes is very similar to each other, whereas there is more variation in the values for Trafford's females and males. Generally speaking the data show that females and males in Trafford have a slightly higher healthy life expectancy from birth than their comparitors, with the age being higher for 7 out of the 9 time periods shown for both sexes. The latest data for 2018 to 2020 shows the health life expectancy from birth in Trafford for females to be 66.9 years compared with 63.3 for the average of similar authorities and 63.9 for England. The same data for males shows 66.3 years in Trafford compared with 63.3 years for the average of similar authorities and 63.1 for England."
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
        alt = "Line chart showing the difference in life expectancy at birth between the most and least deprived areas in Trafford between the periods 2010 to 2012 and 2018 to 2020 compared to the same for the average of similar authorities and England by sex. The trend and values for the average of similar authorities and England for both sexes is broadly similar and consistent, whilst the data for Trafford shows greater variation between the time periods. The difference in life expectancy for females between the worst and least deprived areas has increased over the time periods shown by approximately 1 year, the latest data for 2018 to 2020 shows Trafford with the lowest inequality at 7.4 years compared to 7.7 years for the average of similar authorities and 7.9 years for England. For males the increasing trend is more gradual than the females, but only for the average of similar authorities and England. Trafford's data has been on a downward trend for the last 4 time periods shown, reducing from 10.6 years in 2014 to 2016 (when it was highest amongst its comparitors) to 8.5 years in 2018 to 2020. This compares favourably with the latest data of its comparitors with the average of similar authorities being 9 years and 9.7 years for England."
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
           alt = "Box plot showing the difference in years of life expectancy at birth between the most and least deprived areas within Trafford and compares it with the difference in England and other authorities between the time periods 2010 to 2012 and 2018 to 2020. The data for Trafford is statistically similar to that of England for both sexes, although there is more variance in the data for males.") +
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
        alt = "Box plot showing that the percentage of smoking adults in Trafford compared with England was statistically similar for the period shown from 2011 to 2022.") +
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
        alt = "Line chart showing the percentage of smoking adults in routine and manual occupations in Trafford between 2012 and 2020 compared to the average of similar authorities and England. Trafford's percentage has been higher than its comparitors for much of the time period shown. One notable exception is 2019 where Trafford's percentage decreased by 9 percentage points from the previous year to 17.4% (the lowest percentage recorded in the time period shown) compared to 23.2% in England and 23.2% for the average of similar authorities. However Trafford's percentage rose in 2020 to 20.3% whereas its comparitors continued their decreasing trend leaving Trafford just above the average of similar authorities (20.2%) but still below England (21.4%)."
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
        alt = "Box plot showing that the percentage of smoking adults in routine and manual occupations in Trafford compared with England was statistically similar for the period shown from 2012 to 2020.") +
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
        alt = "Line chart showing the prevelance of adults with depression in Trafford between 2013/14 and 2020/21 compared with the average of similar authorities and England. The percentages for the average of similar authorities and England are very close to each other with the former approximately 0.3 percentage points higher for most of the period shown. In contrast Trafford's percentage is higher than its comparitors and increasing at a faster rate. In 2013/14 the gap between Trafford and the average of similar authorities was 1 percentage points, however in 2020/21 the gap was 3.3 percentage points. The latest data shows the prevelence of adults with depression in Trafford to be 15.4% compared with 12.1% for the average of similar authorities and 12.3% for England."
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
        alt = "Box plot showing the prevelance of adults with depression in Trafford between 2013/14 and 2020/21 compared with the average of similar authorities and England. Trafford's values for the periods shown are all in the upper quartile range.") +
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
         alt = "Line chart showing the percentage of adults in Trafford regularly participating in walking or cycling activities (at least 5 times per week) compared with the average of similar authorities and England from 2015-16 to 2021-22. The latest data shows participation within Trafford to be higher than its comparitors with 39.5% of adults walking or cycling at least 5 times per week compared with 34.2% nationally and 32.3% for the average of similar local authorities. This is an increase of 5.9 percentage points in Trafford from the previous 12 months, where participation lower than both its comparitors. National participation has only risen by 0.6 percentage points from the previous 12 months whilst the average of simmilar local authorities has decreased by 0.2 percentage points.") +
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


