# Server code for theme: Children & Families

#  Obesity in 4-5 year olds --------------------------------------------------

obese_r <- read_csv("data/children/obese_reception.csv") 

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

obese_y6 <- read_csv("data/children/obese_year6.csv") 

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


#  Active Children --------------------------------------------------

active_children <- read_csv("data/children/active_children.csv") %>%
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


#  Children Dental Decay--------------------------------------------------

children_dental_decay <- read_csv("data/children/children_dental_decay.csv") %>%
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


# Reduction in % of children in poverty ---------

# Load in data

children_poverty <- read_csv("data/children/children_poverty.csv") #%>%
#mutate(period = as_factor(period)) %>%

cssn <- read_csv("data/cssn.csv") %>%
  select(area_code)

children_poverty_cssn_mean <- children_poverty %>%
  filter(area_code %in% c(cssn$area_code)) %>%
  group_by(period, indicator) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average"#,
         #period = as_factor(period)
  ) %>%
  filter(!is.na(value))

children_poverty_trend <- bind_rows(children_poverty %>% select(area_name, period,value,indicator) %>% filter(area_name %in% c("Trafford", "England")), children_poverty_cssn_mean) 

children_poverty_wards <- st_read("data/geospatial/electoral_ward.geojson") %>%
  left_join(children_poverty %>% filter(grepl("E05", area_code)) %>% select(area_code, indicator, value), by = "area_code")


# Plot
output$children_poverty_plot <- renderGirafe({
  
  if (input$children_poverty_selection == "Rel. Trend") {
    
    gg <- ggplot(
      filter(children_poverty_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             indicator == "Children in relative low income families (under 16s)"),
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
        title = "Children in relative low income families",
        subtitle = NULL,
        caption = "Source: DWP",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of children under the age of 16 in relative low income families in Trafford compared to the average of similar authorities and England between 2014/15 and 2020/21. Over the majority of the time period plotted, Trafford has consistently tracked approximately 1 percentage point above the trend of the similar authorities average, with the England average approximately 4 to 5 percentage points above Trafford. However, the data for 2019/20 shows a decrease in the proportion for Trafford from the previous year to 12%, bringing it almost in line with the average of similar authorities at 12.1%. The England average for comparison is 19.3%, continuing an upward trend. Recent data for 2020/21 brings Trafford at 11.4% 0.6 points under the average of local authorities"
      ) +
      theme_x()
    
    
  } else if (input$children_poverty_selection == "Abs. Trend"){
    
    gg <- ggplot(
      filter(children_poverty_trend, area_name %in% c("Trafford", "Similar Authorities average", "England"),
             indicator == "Children in absolute low income families (under 16s)"),
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
        title = "Children in absolute low income families",
        subtitle = NULL,
        caption = "Source: DWP",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the proportion of children under the age of 16 in absolute low income families in Trafford compared to the average of similar authorities and England between 2014/15 and 2020/21. Over the majority of the time period plotted, Trafford has approximately tracked 1 percentage point above the trend of the similar authorities average, with the England average approximately 4 to 5 percentage points above Trafford. However, data for 2019/20 shows a decrease in the proportion for Trafford from the previous year to 9.7%, bringing it in line with the average of similar authorities. The England average for comparison is 15.8%, continuing an upward trend from 2016/17. Data for 2020/21 brings Trafford to 9.2% 0.5 points lower than the similar authorities average. England has decrease to 15.1%"
      ) +
      theme_x()
    
  } else {
    gg <-
      ggplot(children_poverty_wards) + 
      geom_sf_interactive(aes(tooltip =
                                paste0('<span class="plotTooltipValue">', ifelse(is.na(value),value,paste0(value,"%")),'</span><br />',
                                       '<span class="plotTooltipMain">', area_name, '</span><br />'),
                              fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
      scale_fill_gradient(  low = "#b9e0e6",
                            high = "#00445e",
                            space = "Lab",
                            na.value = "grey50",
                            breaks = c(min(children_poverty_wards$value, na.rm=T),max(children_poverty_wards$value, na.rm=T)),
                            label = function(x) paste0(x, "%"),
                            guide = guide_legend(
                              title = NULL,
                              reverse = TRUE,
                              keyheight = unit(3, units = "mm"), 
                              keywidth = unit(6, units = "mm"), 
                              ncol = 2)
      ) +
      labs(
        title = "Children in relative low income families by ward",
        subtitle = "2021/22",
        caption = "Source: DWP",
        x = NULL,
        y = NULL,
        alt = "Map showing the proportion of children under 16 years living in relative low income families  in each of Trafford's wards in 2020/2021. The wards in the North have high proportions with Clifford having the highest at 30.4% and Longford, Stretford and Gorse Hill between 19.4% and 23.1%. Bucklow-St Martins in the West had a high propotion at 22.5%. Other wards with proportion over 8% are Davyhulme West at 9.7% in the West, St Mary's at 13.5% and Sale Moor at 8.6% in the central area and Village at 11.7% in the South"
      ) +
      coord_sf(datum = NA) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            legend.position = c(0.5, 1.055))
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_children_poverty_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_children_poverty_plot")
  
})

# Render the output in the ui object
output$children_poverty_box <- renderUI({
  withSpinner(
    girafeOutput("children_poverty_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Maintain the low level of 16-17 year olds who are NEET and NEET plus unknown ---------

# Load in data
df_neet <- read_csv("data/children/neet.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(indicator, period, area_name) %>%
  summarise(value = round(mean(value, na.rm = TRUE), digits = 1))

# Plot
output$neet_plot <- renderGirafe({
  
  if (input$neet_selection == "Trend") {
    
    gg <- ggplot(df_neet %>% filter(indicator == "16-17 year olds not in education, employment or training (NEET)"),
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
      labs(title = "Academic age 16-17 year olds NEET",
           subtitle = NULL,
           caption = "Source: DfE",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing the percentage of academic age 16 to 17 year olds who were not in employment, education or training (NEET) in Trafford, compared with the average for similar authorities and England between 2016 and 2021. For the time period shown, Trafford has the lowest NEET percentage of its comparitors and is fairly consistent at approximately 2.3% each year, with a low of 1.8% recorded in 2019. The England average decreased from 2.8% in 2016 to 2.6% in 2018 before increasing again back to 2.8% in 2021. The average for similar authorities has been on a downward trend from 3.2% in 2016 to 2.7% in 2019, however it rose again to 2.9% in 2021.") +
      theme_x()
    
  } else {
    
    gg <- ggplot(df_neet %>% filter(indicator == "16 to 17 year olds not in education, employment or training (NEET) or whose activity is not known"),
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
      labs(title = "Academic age 16-17 year olds NEET or activity not known",
           subtitle = NULL,
           caption = "Source: DfE",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing the percentage of academic age 16 to 17 year olds who were not in employment, education or training (NEET) or whose status was unknown in Trafford, compared with the average for similar authorities and England between 2016 and 2021. In 2016 Trafford recorded 6.1% NEET or unknown compared to 6% for England and 5% for the average of similar authorities. However, since then Trafford's rate has decreased year on year and in 2021 recorded the lowest rate amongst the comparitors with 4.8% compared to 4.8% for the average of similar authorities and 5.5% for England.") +
      theme_x()
    
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_neet_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_neet_plot")
})

# Render the output in the ui object
output$neet_box <- renderUI({
  withSpinner(
    girafeOutput("neet_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})



# Improve school readiness all children and those with a free school meal status ---------

# Load in data
df_school_readiness <- read_csv("data/children/school_readiness.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(indicator, period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$school_readiness_plot <- renderGirafe({
  
  if (input$school_readiness_selection == "Trend") {
    
    gg <- ggplot(df_school_readiness %>% filter(indicator == "School readiness: percentage of children achieving a good level of development at the end of Reception"),
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
      labs(title = "Reception pupils: good level of development",
           subtitle = NULL,
           caption = "Source: DfE",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing the percentage of pupils having a good level of development at the end of Reception in Trafford compared with the average of similar authorities and England between the academic years 2012/13 and 2018/19. For all years except 2016/17 Trafford has had the highest percentage of pupils achieving a good level of development, although in recent years the gap between Trafford and its comparitors has closed. In 2018/19 74.7% of Trafford pupils achieved the measure compared with 74.3% for the average of similar authorities and 71.8% for England.") +
      theme_x()
    
  } else {
    
    gg <- ggplot(df_school_readiness %>% filter(indicator == "School Readiness: percentage of children with free school meal status achieving a good level of development at the end of Reception"),
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
      labs(title = "Reception pupils: good level of development (FSM)",
           subtitle = NULL,
           caption = "Source: DfE",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing the percentage of pupils with Free School Meals eligibility (FSM) having a good level of development at the end of Reception in Trafford compared with the average of similar authorities and England between the academic years 2012/13 and 2018/19. For the first 3 academic years of the time period shown Trafford's percentage was higher than that of its comparitors. However between 2014/15 and 2015/16 it fell from 54.7% to 47.3%, below both the average of similar authorities (51.1%) and England 54.4%. Trafford's percentage stayed below both comparitors for the 2 following academic years, before rising to 56% in 2018/19, just below the England average of 56.5% but above the average of similar authorities at 53.1%.") +
      theme_x()
    
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_school_readiness_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_school_readiness_plot")
})

# Render the output in the ui object
output$school_readiness_box <- renderUI({
  withSpinner(
    girafeOutput("school_readiness_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Percentage of pupils reaching the expected standard at the end of key stage 2 in reading, writing and mathematics ---------

# Load in data
df_expected_standard_ks2 <- read_csv("data/children/expected_standard_ks2.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 1))

# Plot
output$expected_standard_ks2_plot <- renderGirafe({
  gg <- ggplot(df_expected_standard_ks2,
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
    labs(title = "Pupils reaching expected standard at KS2 (RWM)",
         subtitle = NULL,
         caption = "Source: DfE",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing that in the academic years from 2015/16 to 2021/22 with data not available for academic years 2019/20 and 2020/21. Trafford have had a higher percentage of pupils achieving the expected standard at Key Stage 2 in Reading, Writing and Maths compared with the average of similar authorities and England during this period. Trafford, the similar authorities and England had lower percentages of achievement compared to previous years. In 2021/22 67% of Trafford's pupils achieved the expected standard, compared with 77% in 2018/19. The average of similar authorities had 60.5% and England 58% of achievement in 2021/22") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_expected_standard_ks2_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_expected_standard_ks2_plot")
})

# Render the output in the ui object
output$expected_standard_ks2_box <- renderUI({
  withSpinner(
    girafeOutput("expected_standard_ks2_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Percentage achieving 9-5 in English & mathematics at the end of key stage 4 ---------

# Load in data
df_grades_5_or_above_ks4 <- read_csv("data/children/grades_5_or_above_ks4.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 1))

# Plot
output$grades_5_or_above_ks4_plot <- renderGirafe({
  gg <- ggplot(df_grades_5_or_above_ks4,
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
    labs(title = "Pupils with grades 5 or above in English and maths GCSEs",
         subtitle = NULL,
         caption = "Source: DfE",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing percentage of pupils with grades 5 or above in English and maths GCSEs in Trafford compared with the average of similar authorities and England from academic year 2016/17 to 2022/23. 2016/17: Trafford 62.7, England 42.9, Similar Authorities average 46.1. 2022/23: Trafford 63.4, England 45.5, Similar Authorities average 49.0. Trafford has been more than 10% above comparators.") +
    theme_x()

  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_grades_5_or_above_ks4_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })

  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_grades_5_or_above_ks4_plot")
})

# Render the output in the ui object
output$grades_5_or_above_ks4_box <- renderUI({
  withSpinner(
    girafeOutput("grades_5_or_above_ks4_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Average Attainment 8 score ---------

# Load in data
df_progress_8_score <- read_csv("data/children/progress_8_score.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 2))

# Plot
output$progress_8_score_plot <- renderGirafe({
  gg <- ggplot(df_progress_8_score,
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
    labs(title = "Average Progress 8 score",
         subtitle = NULL,
         caption = "Source: DfE",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing average Progress 8 score in Trafford compared with the average of similar authorities and England from academic year 2016/17 to 2022/23. 2016/17: Trafford 0.10, England 0, Similar Authorities average 0. 2022/23: Trafford 0.24, England -0.03, Similar Authorities average 0. Trafford has been above comparators for all years.") +
    theme_x()

  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_progress_8_score_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })

  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_progress_8_score_plot")
})

# Render the output in the ui object
output$progress_8_score_box <- renderUI({
  withSpinner(
    girafeOutput("progress_8_score_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Population vaccination coverage - MMR for one dose (2 years old) ---------
  

# Load in data
df_vaccination_mmr_2y <- read_csv("data/children/vaccination_mmr.csv") %>%
  filter(indicator == "Population vaccination coverage: MMR for one dose (2 years old)") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 1))

df_vaccination_mmr_5y <- read_csv("data/children/vaccination_mmr.csv") %>%
  filter(indicator == "Population vaccination coverage: MMR for two doses (5 years old)") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 1))

# Plot
output$vaccination_mmr_plot <- renderGirafe({
  if (input$vaccination_mmr_selection == "2y Trend") {
  gg <- 
    ggplot(df_vaccination_mmr_2y,
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
    labs(title = "MMR vaccination coverage for one dose (2 years old)",
         subtitle = NULL,
         caption = "Source: OHID",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing MMR vaccination coverage for one dose (2 years old) in Trafford compared with the average of similar authorities and England from year 2015/16 to 2022/23. 2015/16: Trafford 97.7%, England 91.9%, Similar Authorities average 93.7%. 2022/23: Trafford 92.9%, England 89.3%, Similar Authorities average 91.9%.  Trafford has been above comparators for all years but the gap has narrowed in later years.") +
    theme_x()
  } else {
  
  gg <- 
    ggplot(df_vaccination_mmr_5y,
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
    labs(title = "MMR vaccination coverage for two doses (5 years old)",
         subtitle = NULL,
         caption = "Source: OHID",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing MMR vaccination coverage for two doses (5 years old) in Trafford compared with the average of similar authorities and England from year 2015/16 to 2022/23. 2015/16: Trafford 95%, England 88.2%, Similar Authorities average 89%. 2022/23: Trafford 89%, England 84.5%, Similar Authorities average 89.1%.  Trafford has been above comparators but the last two years when Trafford has been slightly below Similar Authorities") +
    theme_x()
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_vaccination_mmr_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_vaccination_mmr_plot")
})

# Render the output in the ui object
output$vaccination_mmr_box <- renderUI({
  withSpinner(
    girafeOutput("vaccination_mmr_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Children cautioned or sentenced per 10,000 aged 10-17. ---------

# Load in data
df_children_offending <- read_csv("data/children/children_offending.csv") %>%
  filter(measure == "Per 10,000 aged 10-17") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 0))

# Plot
output$children_offending_plot <- renderGirafe({
  gg <- ggplot(df_children_offending,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, ' Per 10,000 aged 10-17</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(-0.05, NA)) +
    labs(title = "Young people cautioned or sentenced aged 10 to 17",
         subtitle = NULL,
         caption = "Source: YJB",
         x = NULL,
         y = "Per 10,000 aged 10-17",
         fill = NULL,
         alt = "Line chart showing Children cautioned or sentenced per 10,000 aged 10-17 in Trafford compared with the average of similar authorities from 2013/14 to 2022/23. 2013/14: Trafford 52, England 80, Similar Authorities average 64. 2022/23: Trafford 14, England 23, Similar Authorities average 18. Trafford has been below England and Similar authorities for all years. The rate is decreasing through the years for all.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_children_offending_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_children_offending_plot")
})

# Render the output in the ui object
output$children_offending_box <- renderUI({
  withSpinner(
    girafeOutput("children_offending_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Percentage of children looked after who had their annual health assessment ---------

# Load in data
df_cla_health_assessment <- read_csv("data/children/cla_health_assessment.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 0))

# Plot
output$cla_health_assessment_plot <- renderGirafe({
  gg <- ggplot(df_cla_health_assessment,
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
    labs(title = "Children looked after: health assessment",
         subtitle = NULL,
         caption = "Source: DfT",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing Percentage of children looked after who had their annual health assessment in Trafford compared with the average of similar authorities from 2017/18 to 2022/23. 2017/18: Trafford 95, England 88, Similar Authorities average 86. 2022/23: Trafford 87, England 89, Similar Authorities average 88. From 2020/21, Trafford had been below England and Similar authorities with the difference decreasing in 2022/23") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_cla_health_assessment_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_cla_health_assessment_plot")
})

# Render the output in the ui object
output$cla_health_assessment_box <- renderUI({
  withSpinner(
    girafeOutput("cla_health_assessment_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Percentage of children looked after who had their teeth checked by a dentist ---------

# Load in data
df_cla_dental_check <- read_csv("data/children/cla_dental_check.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value,na.rm=TRUE), 0))

# Plot
output$cla_dental_check_plot <- renderGirafe({
  gg <- ggplot(df_cla_dental_check,
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
    labs(title = "Children looked after: dental check",
         subtitle = NULL,
         caption = "Source: DfT",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing percentage of children looked after who had their teeth checked by a dentist in Trafford compared with the average of similar authorities from 2017/18 to 2022/23. 2017/18: Trafford 93, England 84, Similar Authorities average 84. 2022/23: Trafford 62, England 76, Similar Authorities average 75. From 2018/19, Trafford had been below England and Similar authorities.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_cla_dental_check_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_cla_dental_check_plot")
})

# Render the output in the ui object
output$cla_dental_check_box <- renderUI({
  withSpinner(
    girafeOutput("cla_dental_check_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})
