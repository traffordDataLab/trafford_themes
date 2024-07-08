# Server code for theme: Healthy Lives

#  Obesity in 4-5 year olds --------------------------------------------------

obese_r <- read_csv("data/health/obese_reception.csv") 

obese_reception <- obese_r %>%
  filter(indicator == "Reception prevalence of obesity (including severe obesity)") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

cssn <- read_csv("data/cssn.csv") %>%
  select(area_code)

obese_reception_cssn_mean <- obese_reception %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

obese_reception_trend <- bind_rows(obese_reception %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), obese_reception_cssn_mean)

obese_reception_quintiles <- obese_r %>%
  filter(indicator == "Reception prevalence of obesity (including severe obesity), 5 years data combined") %>% 
  mutate(inequality = as_factor(inequality))

obese_r_quintiles_cssn_mean <- obese_reception_quintiles %>%
  filter(area_code %in% cssn$area_code) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value)) 

obese_r_quintiles_plot <- obese_reception_quintiles %>%
  filter(area_name %in% c("England", "Trafford")) %>%
  select(area_name,period,inequality,value) %>%
  bind_rows(obese_r_quintiles_cssn_mean) %>%
  mutate(area_name = factor(area_name, levels = c("Trafford","Similar Authorities average","England")))

obese_reception_wards <- st_read("data/geospatial/electoral_ward.geojson") %>%
  left_join(obese_r %>% filter(area_type == "Electoral Wards") %>% select(area_code, indicator, value), by = "area_code")

output$obese_reception_plot <- renderGirafe({
  
  if (input$obese_reception_selection == "Trend") {
    
    gg <- ggplot(
      filter(obese_reception_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Obese children aged 4-5 years",
        subtitle = NULL,
        caption = "Source: NHS Digital, National Child Measurement Programme",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of children aged 4 to 5 years old classified as obese in Trafford from 2010/11 to 2019/20 compared to the average of similar authorities and England. The Trend for Trafford has been inconsistent during the time period shown, however from 2015/16 onwards the proportion has been lower than its comparitors. The proportions for England and the average for similar authorities have followed a similar trend throughout the time period shown, with England just over 1 percentage point higher, and both have been on a gradually increasing trend since 2014/15. The latest data for 2019/20 shows 7.2% of 4 to 5 years olds in Trafford are classified as obese, (the joint lowest recorded within the time period shown along with 2012/13 and 2015/16) compared with 8.4% for the average of similar authorities and 9.9% for England."
      ) +
      theme_x()
  }
  else if (input$obese_reception_selection == "Boxplot"){
    
    gg <- ggplot(data = filter(obese_reception, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value), colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9",
                               outlier.size = 1, fatten = NULL) +
      geom_point_interactive(data = filter(obese_reception, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(obese_reception, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(obese_reception, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(obese_reception, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = " Obese children aged 4-5 years",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
           x = NULL, y = "Percentage",
           fill = "Compared with England:",
           alt = "Box plot comparing the proportion of children aged 4 to 5 years old classified as obese in Trafford with England from 2010/11 to 2019/20. Trafford's proportion compared to England has been statistically similar in 2 of the 10 years shown (2011/12 and 2014/15) and better in the other 8.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  } else if (input$obese_reception_selection == "Deprivation"){
    gg <-
      ggplot(obese_r_quintiles_plot, aes(x = inequality, y = value, fill = area_name, group = area_name)) +
      geom_col_interactive(aes(tooltip =
                                 paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                        '<span class="plotTooltipMain">', area_name, '</span><br />',
                                        '<span class="plotTooltipPeriod">', inequality, '</span><br />')), 
                          width = 0.5, position = position_dodge(width=0.6)) +
      scale_fill_manual(
        values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      
      scale_x_discrete(labels = wrap_format(13)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = label_percent(scale = 1, accuracy = 1)) +
      labs(
        title = "Obese children aged 4-5 years by deprivation",
        subtitle = "2018/19 - 22/23",
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, MHCLG",
        x = NULL,
        y = NULL,
        alt = "Bar chart showing the proportion of children aged 4 to 5 years old classified as obese in Trafford between 2015/16 to 2019/20 compared to the average of similar authorities and England by deprivation quintiles, from most to least deprived. In each quintile England has the highest proportion. Trafford's proportions are below the average of similar authorities in all quintiles except the average deprived, in which Trafford is 0.1 percentage points higher."
      ) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 0)))
  } else {
    gg <-
      ggplot(obese_reception_wards) + 
      geom_sf_interactive(aes(tooltip =
                                paste0('<span class="plotTooltipValue">', ifelse(is.na(value),value,paste0(value,"%")),'</span><br />',
                                       '<span class="plotTooltipMain">', area_name, '</span><br />'),
                              fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
      scale_fill_gradient(  low = "#b9e0e6",
                            high = "#00445e",
                            space = "Lab",
                            na.value = "grey50",
                            breaks = c(min(obese_reception_wards$value, na.rm=T),max(obese_reception_wards$value, na.rm=T)),
                            label = function(x) paste0(x, "%"),
                            guide = guide_legend(
                              title = NULL,
                              reverse = TRUE,
                              keyheight = unit(3, units = "mm"), 
                              keywidth = unit(6, units = "mm"), 
                              ncol = 2)
      ) +
      labs(
        title = "Obese children aged 4-5 years by ward",
        subtitle = "2020/21 - 2022/23",
        caption = "Source: National Child Measurement Programme, NHS Digital",
        x = NULL,
        y = NULL,
        alt = "Map showing the proportion of children aged 4 to 5 years old classified as obese in each of Trafford's wards between 2017/18 and 2019/20. The proportions are highest in Gorse Hill in the North and Bucklow-St Martins in the West at 11.2% and lowest in the Southern ward of Hale Central at 2.9%. The Central ward of St Mary's next to Bucklow-St Martins has the 2nd highest proportion of 10.8%. Flixton in the West and Longford in the North have proportions below 6% which are noticably lower than their neighbouring wards."
      ) +
      coord_sf(datum = NA) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            legend.position = c(0.5, 1.055))
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_obese_reception_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_obese_reception_plot")
})

output$obese_reception_box <- renderUI({
  withSpinner(
    girafeOutput("obese_reception_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

#  Obesity in 10-11 year olds --------------------------------------------------

obese_y6 <- read_csv("data/health/obese_year6.csv") 

obese_year6 <- obese_y6 %>%
  filter(indicator == "Year 6 prevalence of obesity (including severe obesity)") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

cssn <- read_csv("data/cssn.csv") %>%
  select(area_code)

obese_year6_cssn_mean <- obese_year6 %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

obese_year6_trend <- bind_rows(obese_year6 %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), obese_year6_cssn_mean)

obese_year6_quintiles <- obese_y6 %>%
  filter(indicator == "Year 6 prevalence of obesity (including severe obesity), 5 years data combined") %>% mutate(inequality = as_factor(inequality))

obese_y6_quintiles_cssn_mean <- obese_year6_quintiles %>%
  filter(area_code %in% cssn$area_code) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value)) 

obese_y6_quintiles_plot <- obese_year6_quintiles %>%
  filter(area_name %in% c("England", "Trafford")) %>%
  select(area_name,period,inequality,value) %>%
  bind_rows(obese_y6_quintiles_cssn_mean) %>%
  mutate(area_name = factor(area_name, levels = c("Trafford","Similar Authorities average","England")))

obese_year6_wards <- st_read("data/geospatial/electoral_ward.geojson") %>%
  left_join(obese_y6 %>% filter(area_type == "Electoral Wards") %>% select(area_code, indicator, value), by = "area_code")


output$obese_year6_plot <- renderGirafe({
  
  if (input$obese_year6_selection == "Trend") {
    
    gg <- ggplot(
      filter(obese_year6_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Obese children aged 10-11 years",
        subtitle = NULL,
        caption = "Source: NHS Digital, National Child Measurement Programme",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of children aged 10 to 11 years old classified as obese in Trafford from 2010/11 to 2019/20 compared to the average of similar authorities and England. The Trend for Trafford has been inconsistent during the time period shown, rising and falling, with the proportion at its highest in 2012/13 with 18.8% and at its lowest in 2014/15 with 16%. The proportions for England and the average for similar authorities have followed a broadly similar trend throughout the time period shown, with England roughly 3.5 percentage points higher, and both have been on a gradually increasing trend since 2014/15. The latest data for 2019/20 shows 17.8% of 10 to 11 years olds in Trafford are classified as obese, the same as the average of similar authorities, compared with 21% for England."
      ) +
      theme_x()
  }
  else if (input$obese_year6_selection == "Boxplot"){
    
    gg <- ggplot(data = filter(obese_year6, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(obese_year6, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(obese_year6, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(obese_year6, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(obese_year6, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = " Obese children aged 10-11 years",
           subtitle = NULL,
           caption = "Source: NHS Digital, National Child Measurement Programme",
           x = NULL, y = "Percentage",
           fill = "Compared with England:",
           alt = "Box plot comparing the proportion of children aged 10 to 11 years old classified as obese in Trafford with England from 2010/11 to 2019/20. Trafford's proportion was statistically similar compared to England's in 2012/13 and better in the other 9 years shown.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  } else if (input$obese_year6_selection == "Deprivation"){
    gg <-
      ggplot(obese_y6_quintiles_plot, aes(x = inequality, y = value, fill = area_name, group = area_name)) +
      geom_bar_interactive(aes(tooltip =
                                 paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                        '<span class="plotTooltipMain">', area_name, '</span><br />',
                                        '<span class="plotTooltipPeriod">', inequality, '</span><br />')),
                           stat = "identity", width = 0.5, position = position_dodge(width=0.6)) +
      scale_fill_manual(
        values = c("Trafford" = plot_colour_trafford, "Similar Authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_x_discrete(labels = wrap_format(13)) +
      scale_y_continuous(limits = c(0, NA),
                         labels = label_percent(scale = 1, accuracy = 1)) +
      labs(
        title = "Obese children aged 10-11 years by deprivation",
        subtitle = "2018/19 - 22/23",
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, MHCLG",
        x = NULL,
        y = NULL,
        alt = "Bar chart showing the proportion of children aged 10 to 11 years old classified as obese in Trafford between 2015/16 to 2019/20 compared to the average of similar authorities and England by deprivation quintiles, from most to least deprived. In each quintile England has the highest proportion. Trafford's proportions are below the average of similar authorities in the second most deprived quintile (21.9% compared to 22%) and the second least deprived quintile (15.2% compared to 16%). In the 3 other quintiles Trafford is above the similar authorities average by either 0.2 and 0.5 percentage points."
      ) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 0)))
  } else {
    gg <-
      ggplot(obese_year6_wards) + 
      geom_sf_interactive(aes(tooltip =
                                paste0('<span class="plotTooltipValue">', ifelse(is.na(value),value,paste0(value,"%")),'</span><br />',
                                       '<span class="plotTooltipMain">', area_name, '</span><br />'),
                              fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
      scale_fill_gradient(  low = "#b9e0e6",
                            high = "#00445e",
                            space = "Lab",
                            na.value = "grey50",
                            breaks = c(min(obese_year6_wards$value, na.rm=T),max(obese_year6_wards$value, na.rm=T)),
                            label = function(x) paste0(x, "%"),
                            guide = guide_legend(
                              title = NULL,
                              reverse = TRUE,
                              keyheight = unit(3, units = "mm"), 
                              keywidth = unit(6, units = "mm"), 
                              ncol = 2)) +
      labs(
        title = "Obese children aged 10-11 years by ward",
        subtitle = "2020/21 - 2022/23",
        caption = "Source: National Child Measurement Programme, NHS Digital",
        x = NULL,
        y = NULL,
        alt = "Map showing the proportion of children aged 10 to 11 years old classified as obese in each of Trafford's wards between 2017/18 and 2019/20. The highest proportion is in Bucklow-St Martins in the West at 25.8% followed by Gorse Hill (23%) and Stretford (22.2%) in the North. The lowest proportions are in the Southern wards of Timperley (10.1%) and Hale Barns (10.7%) and the Central ward of Priory (10.8%). The Central ward of Sale Moor and the Southern ward of Timperley have noticably higher proportions than their neighbouring wards and Flixton in the West has a noticably lower proportion than the surrounding wards."
      ) +
      coord_sf(datum = NA) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            legend.position = c(0.5, 1.055))
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_obese_year6_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_obese_year6_plot")
})

output$obese_year6_box <- renderUI({
  withSpinner(
    girafeOutput("obese_year6_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


#  Adults classified as overweight or obese--------------------------------------------------

cipfa <- read_csv("data/cipfa2021.csv") %>%
  select(area_code)

overweight_adult <- read_csv("data/health/overweight_adult.csv") %>%
  filter(indicator == "Percentage of adults (aged 18 plus) classified as overweight or obese") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

overweight_adult_cipfa_mean <- overweight_adult %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

overweight_adult_trend <- bind_rows(overweight_adult %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), overweight_adult_cipfa_mean)


output$overweight_adult_plot <- renderGirafe({
  
  if (input$overweight_adult_selection == "Trend") {
    
    gg <- ggplot(
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

active_adults_cipfa_mean <- active_adults %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

active_adults_trend <- bind_rows(active_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), active_adults_cipfa_mean)


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

inactive_adults_cipfa_mean <- inactive_adults %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

inactive_adults_trend <- bind_rows(inactive_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), inactive_adults_cipfa_mean)


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

fairly_active_adults_cipfa_mean <- fairly_active_adults %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

fairly_active_adults_trend <- bind_rows(fairly_active_adults %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), fairly_active_adults_cipfa_mean)


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


#  Active Children --------------------------------------------------

active_children <- read_csv("data/health/active_children.csv") %>%
  filter(indicator == "Percentage of physically active children and young people") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

active_children_cssn_mean <- active_children %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

active_children_trend <- bind_rows(active_children %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), active_children_cssn_mean)


output$active_children_plot <- renderGirafe({
  
  if (input$active_children_selection == "Trend") {
    
    gg <- ggplot(
      filter(active_children_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Physically active children",
        subtitle = NULL,
        caption = "Source: Active Lives survey, Sport England",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of children who are physically active in Trafford between 2017/18 and 2020/21 compared with the average of similar authorities and England. The proportions for the average of similar authorities and England have been broadly similar and consistent across the period shown, whilst Trafford's has varied a lot, increasing then decreasing year to year. In 2018/19 it had the highest proportion of its comparitors, but all were within 0.8 percentage points of each other. However the following year Trafford's proportion decreased by almost 10 percentage points compared to a decrease in its comparitors proportions between 0.3 and 1.9 percentage points. The latest data for 2020/21 shows that the proportion of physically active children in Trafford has increased and is now similar, although still lower, than its comparitors at 43.1% compared with 44% for the average of similar authorities and 44.6% for England."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(active_children, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(active_children, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(active_children, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(active_children, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(active_children, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "Physically active children",
           subtitle = NULL,
           caption = "Source: Active Lives survey, Sport England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:",
           alt = "Box plot showing that the proportion of physically active children in Trafford compared with England was statistically similar for the period shown from 2017/18 to 2020/21.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_active_children_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_active_children_plot")
})

output$active_children_box <- renderUI({
  withSpinner(
    girafeOutput("active_children_plot", height = "inherit"),
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

mortality_rate_cipfa_mean <- mortality_rate %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period, unit) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

mortality_rate_trend <- bind_rows(mortality_rate %>% select(area_name, period,value,unit) %>% filter(area_name %in% c("Trafford", "England")), mortality_rate_cipfa_mean) 

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

healthy_life_expectancy_cipfa_mean <- healthy_life_expectancy %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

healthy_life_expectancy_trend <- bind_rows(healthy_life_expectancy %>% select(area_name, period,value,inequality) %>% filter(area_name %in% c("Trafford", "England")), healthy_life_expectancy_cipfa_mean)


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

inequality_life_expectancy_cipfa_mean <- inequality_life_expectancy %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period, inequality) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

inequality_life_expectancy_trend <- bind_rows(inequality_life_expectancy %>% select(area_name, period,value,inequality) %>% filter(area_name %in% c("Trafford", "England")), inequality_life_expectancy_cipfa_mean)


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


#  Children Dental Decay--------------------------------------------------

children_dental_decay <- read_csv("data/health/children_dental_decay.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(!is.na(value))

children_dental_decay_cssn_mean <- children_dental_decay %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

children_dental_decay_trend <- bind_rows(children_dental_decay %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), children_dental_decay_cssn_mean)


output$children_dental_decay_plot <- renderGirafe({
  
  if (input$children_dental_decay_selection == "Trend") {
    
    gg <- ggplot(
      filter(children_dental_decay_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "5 year olds with obvious dental decay",
        subtitle = NULL,
        caption = "Source: Dental Public Health Epidemiology Programme for England",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of 5 year olds with visually obvious dental decay in Trafford between 2014/15 and 2018/19 compared with the average of similar authorities and England. The data for England (higher percentage) and the average for similar authorities to Trafford (lower percentage) show a general declining trend with the average for similar authorities decreasing faster, going from 19.3% in 2014/15 to 15.4 in 2018/19 compared with 24.7% and 23.4% for England in the same period. Trafford's data is more erratic by comparison, with its percentage being higher than its comparitors in 2014/15 and 2018/19, 26.4% and 26% respectively. Although Trafford's percentage dropped to 19% in 2016/17, just 1 percentage point above the average of similar authorities, it rose again sharply the following year by 7 percentage points."
      ) +
      theme_x()
  }
  else {
    
    gg <- ggplot(data = filter(children_dental_decay, area_type %in% c("District", "UA")),
                 aes(x = period, y = value)) +
      stat_boxplot(geom = "errorbar", colour = "#C9C9C9", width = 0.2) +
      geom_boxplot_interactive(aes(tooltip = value),
                               colour = "#C9C9C9",
                               outlier.shape = 21, outlier.colour = "#C9C9C9", outlier.size = 1,
                               fatten = NULL) +
      geom_point_interactive(data = filter(children_dental_decay, area_name == "Trafford"),
                             aes(x = period, y = value, fill = compared_to_England,
                                 tooltip =
                                   paste0('<span class="plotTooltipValue">', value, '%</span><br />',
                                          '<span class="plotTooltipMain">', area_name, '</span><br />',
                                          '<span class="plotTooltipPeriod">', period, '</span>')),
                             shape = 21, colour = "#000000", size = 3) +
      geom_boxplot_interactive(data = filter(children_dental_decay, area_name == "England"),
                               aes(x = factor(period), y = value,
                                   tooltip =
                                     paste0('<span class="plotTooltipValue">', filter(children_dental_decay, area_name == "England")$value, '%</span><br />',
                                            '<span class="plotTooltipMain">', "England", '</span><br />',
                                            '<span class="plotTooltipPeriod">', filter(children_dental_decay, area_name == "England")$period, '</span>')
                               ),
                               fill = "#C9C9C9", size = 0.5) +
      scale_fill_manual(values = c("Better" = "#92D050",
                                   "Similar" = "#FFC000",
                                   "Worse" = "#C00000")) +
      scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      labs(title = "5 year olds with obvious dental decay",
           subtitle = NULL,
           caption = "Source: Dental Public Health Epidemiology Programme for England",
           x = NULL, y = "Percentage",
           fill = "Compared with England:",
           alt = "Box plot showing that the proportion of 5 year olds with visually obvious dental decay in Trafford compared with England was statistically similar for the period shown from 2014/15 to 2018/19.") +
      theme_x() +
      theme(
        legend.position = "top",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)
      )
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_children_dental_decay_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_children_dental_decay_plot")
})

output$children_dental_decay_box <- renderUI({
  withSpinner(
    girafeOutput("children_dental_decay_plot", height = "inherit"),
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

adults_smoking_manual_cipfa_mean <- adults_smoking_manual %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

adults_smoking_manual_trend <- bind_rows(adults_smoking_manual %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), adults_smoking_manual_cipfa_mean)


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

adults_depression_cipfa_mean <- adults_depression %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average",
         period = as_factor(period)) %>%
  filter(!is.na(value))

adults_depression_trend <- bind_rows(adults_depression %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), adults_depression_cipfa_mean)


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