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
  left_join(obese_r %>% filter(area_type == "Electoral Wards (2021)") %>% select(area_code, indicator, value), by = "area_code")

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
        alt = "Line chart showing the proportion of children in Trafford aged 4 to 5 years old classified as obese from 2010/11 to 2023/24 compared to the average of similar authorities and England. In all by 3 of the time periods shown, and from 2015/16 onwards, the proportion in Trafford has been lower than its comparitors. The proportions for England and the average for similar authorities have followed a similar trend throughout the time period shown with England around 1.5 percentage points higher. The exception is a spike in the national average in 2020/21 (14.4%) which is the highest value shown, coinciding with the COVID-19 pandemic. For this period there are no corresponding values for local authorities. The latest data for 2023/24 shows increases for all compared to the previous year, with 7.3% in Trafford classified as obese, (up from 6.5 the previous year which is the lowest within the time period shown) compared with 8.2% for the average of similar authorities (up from 7.7%) and 9.6% for England (up from 9.2%)."
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
           alt = "Box plot comparing the proportion of children aged 4 to 5 years old classified as obese in Trafford with England from 2012/13 to 2023/24. Trafford's proportion compared to England has been statistically similar in 1 of the 12 years shown (2014/15) and better in the rest, with the exception of 2020/21 when no comparison can be made as there are no corresponding values for local authorities.") +
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
        subtitle = "2019/20 - 2023/24",
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, MHCLG",
        x = NULL,
        y = NULL,
        alt = "Bar chart showing the proportion of children aged 4 to 5 years old classified as obese in Trafford between 2019/20 to 2023/24 compared to the average of similar authorities and England by deprivation quintiles, from most to least deprived. In each quintile England has the highest proportion and Trafford the lowest. From most to least deprived, Trafford's proportion goes from 10.8% to 5.3% compared with 12.2% to 6.0% for the similar authorities average and 12.7% to 6.4% for the national average."
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
        subtitle = "2021/22 - 2023/24",
        caption = "Source: National Child Measurement Programme, NHS Digital",
        x = NULL,
        y = NULL,
        alt = "Map showing the proportion of children aged 4 to 5 years old classified as obese in each of Trafford's wards between 2021/22 and 2023/24. The proportions are highest in the Northern wards of Gorse Hill (9.7%) and Stretford (9.3%), Bucklow-St Martins in the West (9.2%) and Village (9.1%) in the South. The lowest proportions are in the Central wards of Ashton upon Mersey and Brooklands (4%) and the Southern ward of Hale Barns (4.3%)."
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
  left_join(obese_y6 %>% filter(area_type == "Electoral Wards (2021)") %>% select(area_code, indicator, value), by = "area_code")


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
        alt = "Line chart showing the proportion of children aged 10 to 11 years old classified as obese in Trafford from 2012/13 to 2023/24 compared to the average of similar authorities and England. Trafford's percentages have been the lowest of its comparators in 2 of the 12 periods shown (2021/22 and 2023/24), and between them during the rest. The proportions for England and the average for similar authorities have followed a similar trend to each other with England roughly 3.5 percentage points higher, whilst Trafford's trend has been more variable. A spike in the national average of 25.5% in 2020/21, coinciding with the COVID-19 pandemic, is the highest value shown and there are no corresponding values for local authorities. The latest data shows Trafford's proportion at 17.6%, compared with 18% for the average of similar authorities and 22.1% for the England average."
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
           alt = "Box plot comparing the proportion of children aged 10 to 11 years old classified as obese in Trafford with England from 2012/13 to 2023/24. Trafford's proportion was statistically similar compared to England's in 2012/13 and better in the rest, with the exception of 2020/21 when no comparison can be made as there are no corresponding values for local authorities.") +
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
        subtitle = "2019/20 - 2023/24",
        caption = "Source: National Child Measurement Programme, NHS Digital; IMD2019, MHCLG",
        x = NULL,
        y = NULL,
        alt = "Bar chart showing the proportion of children aged 10 to 11 years old classified as obese in Trafford between 2019/20 to 2023/24 compared to the average of similar authorities and England by deprivation quintiles, from most to least deprived. In each quintile the national average is the highest. Trafford's proportions are lower than its comparators in all quintiles except for most deprived, where it is 27.8% compared to 27.3% for the average of similar authorities and 28.4% for the national average. In the least deprived quintile Trafford's proportion of children aged 10 to 11 years old classified as obese is 12.9%, the same as the average of similar authorities, and 14.1% for the national average."
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
        subtitle = "2021/22 - 2023/24",
        caption = "Source: National Child Measurement Programme, NHS Digital",
        x = NULL,
        y = NULL,
        alt = "Map showing the proportion of children aged 10 to 11 years old classified as obese in each of Trafford's wards between 2021/22 and 2023/24. The highest proportion is in the Northern wards of Stretford (26.9%) and Clifford (24.5%) and Bucklow-St Martins in the West (24.4%). The Northern and Western wards generally show higher proportions than centrally and the South. The Southern wards of Hale Central (8.3%) and Bowdon (9.8%) have the lowest proportions."
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
        alt = "Line chart showing the proportion of children who are physically active in Trafford between 2017/18 and 2023/24 compared with the average of similar authorities and England. The proportions for the average of similar authorities and England have been broadly similar and consistent across the period shown, whilst Trafford's has been more varied. The lowest proportion recorded in the period shown is 37.7% for Trafford in 2019/20, compared with 44.9% for England and 47.2% for the average of similar authorities. The highest proportion is also Trafford in 2023/24 with 49.9% compared with 49.3% for the average of similar authorities and 47.8% for England."
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
           alt = "Box plot showing that the proportion of physically active children in Trafford compared with England was statistically similar for the period shown from 2017/18 to 2023/24. The only exception is 2021/22 where data for Trafford is not available so no comparison can be made.") +
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
        alt = "Line chart showing the proportion of 5 year olds with visually obvious dental decay in Trafford between 2014/15 and 2021/22 in 2-year gaps compared with the average of similar authorities and England. Trafford's proportion is higher than its comparators in 3 of the 4 time periods shown and the trend has been more variable, whilst the average for similar authorities has been the lowest and remained consistent for the latest 3 periods. The latest data for 2021/22 is 24.5% for Trafford compared with 23.7% for the national average and 17.9% for the average of similar authorities."
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
           alt = "Box plot showing that the proportion of 5 year olds with visually obvious dental decay in Trafford compared with England was statistically similar for the period shown from 2014/15 to 2021/22.") +
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
        alt = "Line chart showing the proportion of children under the age of 16 in relative low income families in Trafford compared to the average of similar authorities and England between 2017/18 and 2023/24. Over the majority of the time period plotted, Trafford's proportion and trend has been very similar to the average of similar authorities (between 0.3 and 1.4 percentage points), with the national average much higher. The latest data for 2023/24 shows the proportion in Trafford at 14.3% compared with 13.5% for the average of similar authorities and 22.1% for the national average."
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
        alt = "Line chart showing the proportion of children under the age of 16 in absolute low income families in Trafford compared to the average of similar authorities and England between 2017/18 and 2023/24. Over the majority of the time period plotted, Trafford's proportion and trend has been very similar to the average of similar authorities (between 0.5 and 1.1 percentage points), with the national average much higher. Between 2017/18 and 2021/22 Trafford's proportion and the average of similar authorities was on a decreasing trend, However since then all comparators have increased. The latest data for Trafford is 12.4% compared with 11.5% for the average of similar authorities and 19.1% for the national average."
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
        alt = "Map showing the proportion of children under 16 years living in relative low income families in each of Trafford's wards in 2021/22. The wards with the highest proportions are Clifford in the North (34.4%) and Bucklow-St Martins in the West (27.3%). The Northern wards all have high percentages and much higher than their neighbouring wards. The Southern ward of Timperley has the lowest percentage (2.8%), followed by the neighbouring central ward of Brooklands (3.6%). All the southern wards are below 8%, with the exception of Village (14%)."
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
           alt = "Line chart showing the percentage of academic age 16 to 17 year olds who were not in employment, education or training (NEET) in Trafford, compared with the average for similar authorities and England between 2019 and 2024. For the time period shown, Trafford has the lowest NEET percentages of its comparitors except for 2019. The trend follows that of its comparitors, and all 3 have been on an upward trend since 2022. The highest value for Trafford of 2.3% was recorded in 2021, and the lowest of 1.6% in 2022. The latest data shows 2.1% NEET in Trafford compared to 2.6% and 3.2% for the averages of similar authorities and England respectively.") +
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
           alt = "Line chart showing the percentage of academic age 16 to 17 year olds who were not in employment, education or training (NEET) or whose status was unknown in Trafford, compared with the average for similar authorities and England between 2016/17 and 2023/24. Trafford's percentage has been on a downward trend for the period shown, going from the highest amongst its comparators to the lowest. In 2016/17 Trafford value was 6.1% compared with 6% for the national average and 4.3% for the average of similar authorities. The latest proportions are 3.9% for Trafford, 4.5% for the average of similar authorities and 5.4% for the national average.") +
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
           alt = "Line chart showing the percentage of pupils having a good level of development at the end of Reception in Trafford compared with the average of similar authorities and England for the academic years 2021/22 and 2023/24. Trafford's percentage is higher than its comparators across the 2 years, however the increase is less than the others, going from 71.6% to 72.6% compared to 67.3% to 70% for the average of similar authorites and 65.2% to 67.7% for the national average.") +
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
           alt = "Line chart showing the percentage of pupils with Free School Meals eligibility (FSM) having a good level of development at the end of Reception in Trafford compared with the average of similar authorities and England for the academic years 2021/22 and 2023/24. Trafford's percentage is the same for the first 2 years (47.5%), rising slightly to 47.7% in 2023/24, just slightly below the average of similar authorities at 47.9%. The national average is highest, going from 49.1% to 51.5% in the latest 2 periods.") +
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
         alt = "Line chart showing that in the academic years from 2016/17 to 2023/24, with data not available for academic years 2019/20 and 2020/21, Trafford have had a higher percentage of pupils achieving the expected standard at Key Stage 2 in Reading, Writing and Maths compared with the average of similar authorities and England during this period. For the latest 3 academic years the percentages for all are lower than for the previous data point in 2018/19. Trafford's pecentage has increased in the 2 latest data points from 67% to 70%, compared with 61.2% to 62.6% for the average of similar authorties and 59% to 61% for the national average.") +
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
         alt = "Line chart showing percentage of pupils with grades 5 or above in English and maths GCSEs in Trafford compared with the average of similar authorities and England from academic year 2016/17 to 2023/24. The trend lines for all comparators is very similar, with Trafford above the average for similar authorities and England throughout the period. The highest values for each occurred in 2020/21 with 69.8% for Trafford, 56.8% for the average of similar authorities and 51.9% for England. Following this all comparators show a downward trend until 2022/23 with the latest data showing a slight increase: Trafford 66.4%, the average of similar authorities 51.9% and England 46.2%.") +
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
         y = "Score",
         fill = NULL,
         alt = "Line chart showing average Progress 8 score in Trafford compared with the average of similar authorities and England from academic year 2016/17 to 2023/24 with no data between 2019/20 and 2020/21. Trafford has been above its comparators for all years, and on an upward trend in the last 3 at a faster rate, going from 0.11% in 2021/22 to 0.35% in 2023/24. In comparison over the same period the average for similar authorities went from 0.09% to 0.11% and the national average stayed at -0.03%.") +
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
         alt = "Line chart showing MMR vaccination coverage for one dose (2 years old) in Trafford compared with the average of similar authorities and England from year 2016/17 to 2023/24. Trafford has been above its comparators for all years but the gap has narrowed in later years and all 3 show a slight decline. Trafford's percentage has gone from 96.1% in 2016/17 to 93%, compared with 92.4% to 91.7% for the average of similar authorities and 91.6% and 88.9% for the national average over the same period.") +
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
         alt = "Line chart showing MMR vaccination coverage for one dose (5 years old) in Trafford compared with the average of similar authorities and England from year 2016/17 to 2023/24. Trafford has been above its comparators for all years but the gap has narrowed in later years. Trafford's percentage has gone from 95.4% in 2016/17 to 89.8%, compared with 88.6% to 88.5% for the average of similar authorities and 87.6% and 83.9% for the national average over the same period.") +
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
         alt = "Line chart showing young people cautioned or sentenced per 10,000 aged 10-17 in Trafford compared with the average of similar authorities from 2013/14 to 2023/24. Trafford has been below its comparators for all years except 2014/15 and 2018/19 where it was just above the average of similar authorities. The rate has been on a decreasing trend through the years for all. Trafford's rate has gone from 52 per 10,000 in 2013/14 to 14 per 10,000 in 2023/24 compared with the average of similar authorities of 58 per 10,000 in 2013/14 to 20 per 10,000 in 2023/24 and the national average of 80 per 10,000 in 2013/14 to 23 per 10,000 in 2023/24.") +
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
         alt = "Line chart showing Percentage of children looked after who had their annual health assessment in Trafford compared with the average of similar authorities and England from 2017/18 to 2023/24. Trafford was above its comparators for the first 2 periods, however since then it has fallen below both, going from 95% in 2017/18 to 83% in 2023/24. This compares with the average of similar authorities of 85% in 2017/18 to 91% in 2023/24 and the national average of 88% in 2017/18 to 91% in 2023/24.") +
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
         alt = "Line chart showing percentage of children looked after in Trafford who had a dental check compared with the average of similar authorities from 2017/18 to 2023/24. Apart from 2017/18, Trafford have been consistently below its comparators who have had very similar percentages to each other throughout the periods shown. All show a significant drop in 2020/21, and although the percentages have risen since, the gap is growing between Trafford and its comparators. Trafford's percentage went from 93% in 2017/18 to 61% in 2023/24 compared with 83% to 80% for similar authorities and 84% to 79% for England.") +
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
