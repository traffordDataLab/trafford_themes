# Server code for theme: Economy & Homes

# Percentage receiving Universal Credit (UC) ---------

# Load in data

cipfa <- read_csv("data/cipfa2021.csv") %>%
  select(area_code)

universal_credit <- read_csv("data/economy/universal_credit.csv") %>%
  mutate(period = as.Date(paste0("01 ",period), format = "%d %B %Y")) %>%
  filter(measure == "rate")

universal_credit_cipfa_mean <- universal_credit %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value))

universal_credit_trend <- bind_rows(universal_credit %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), universal_credit_cipfa_mean) 

universal_credit_wards <- st_read("data/geospatial/electoral_ward.geojson") %>%
  left_join(universal_credit %>% filter(grepl("E05", area_code)) %>% select(area_code, indicator, value), by = "area_code")


# Plot
output$universal_credit_plot <- renderGirafe({
  
  if (input$universal_credit_selection == "Trend") {
    
    gg <- ggplot(
      filter(universal_credit_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
      scale_x_date(date_labels = "%b %y", date_breaks = "3 month", expand = c(0.06,0.06)) +
      labs(
        title = "Universal Credit rate - aged 16 to 64",
        subtitle = NULL,
        caption = "Source: DWP",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing universal credit claims as a proportion of people aged 16 to 64 in Trafford compared with the average of similar authorities and England from September 2019 to September 2022. The average of similar authorities and England closely follow each other throughout the time period with Trafford following the same trend but lower than its comparitors. Since May 2020 the proportion of Universal Credit claimants in Trafford has been consistently 2 to 3 percentage points below its comparitors. The latest data for September 2022 shows Trafford's claimant rate at 11% compared with 14% for the average of similar authorities and 14.2% for England."
      ) +
      theme_x()
  } else {
    gg <-
      ggplot(universal_credit_wards) + 
      geom_sf_interactive(aes(tooltip =
                                paste0('<span class="plotTooltipValue">', ifelse(is.na(value),value,paste0(value,"%")),'</span><br />',
                                       '<span class="plotTooltipMain">', area_name, '</span><br />'),
                              fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
      scale_fill_gradient(  low = "#b9e0e6",
                            high = "#00445e",
                            space = "Lab",
                            na.value = "grey50",
                            breaks = c(min(universal_credit_wards$value, na.rm=T),max(universal_credit_wards$value, na.rm=T)),
                            label = function(x) paste0(x, "%"),
                            guide = guide_legend(
                              title = NULL,
                              reverse = TRUE,
                              keyheight = unit(3, units = "mm"), 
                              keywidth = unit(6, units = "mm"), 
                              ncol = 2)
      ) +
      labs(
        title = "Universal Credit rate - aged 16 to 64 by ward",
        subtitle = "April 2024",
        caption = "Source: DWP,ONS",
        x = NULL,
        y = NULL,
        alt = "Map showing the proportion of Universal Credit claims as a proportion of people aged 16 to 64 in each of Trafford's wards in September 2022. Bucklow-St Martins in the West had the highest propotion of all wards at 25.6%. The wards in the North have high proportions with Clifford having the highest at 22.1% and Longford, Stretford and Gorse Hill between 15.4% and 19.2%.  Other wards with proportion over 10% are St Mary's at 14.8% and Sale Moor at 12.1% in the central area and Village at 12.9% in the South"
      ) +
      coord_sf(datum = NA) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            legend.position = c(0.5, 1.055))
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_universal_credit_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_universal_credit_plot")
  
})

# Render the output in the ui object
output$universal_credit_box <- renderUI({
  withSpinner(
    girafeOutput("universal_credit_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Claimant Count (CC) rate ---------


# Load in data


claimant_count <- read_csv("data/economy/claimant_count.csv") %>%
  mutate(period = as.Date(paste0("01 ",period), format = "%d %B %Y")) %>%
  filter(measure == "Percentage")

claimant_count_cipfa_mean <- claimant_count %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value))

claimant_count_trend <- bind_rows(claimant_count %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), claimant_count_cipfa_mean) 

claimant_count_wards <- st_read("data/geospatial/electoral_ward.geojson") %>%
  left_join(claimant_count %>% filter(grepl("E05", area_code)) %>% select(area_code, indicator, value), by = "area_code")

# Plot
output$claimant_count_plot <- renderGirafe({
  
  if (input$claimant_count_selection == "Trend") {
    
    gg <- ggplot(
      filter(claimant_count_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
      scale_x_date(date_labels = "%b %y", date_breaks = "3 month", expand = c(0.06,0.06)) +
      labs(
        title = "Claimant Count rate - aged 16 to 64",
        subtitle = NULL,
        caption = "Source: ONS",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing jobseekers allowance and universal credit claims as a proportion of residents aged 16 to 64 in Trafford compared with the average of similar authorities and England from September 2019 to September 2022. All three lines follow a very similar trend, with Trafford having the lowest proportion, followed by the average of similar local authorities and then England. Latest data for September 2022 shows a claimant count rate of 3.1% in Trafford, 3.5% for the average of similar authorities and 3.8% for England. The claimant count rate of 3.1% in Trafford has remained consistent since May 2022, whereas the average for similar local authorities of 3.5% has remained consistent since July 2022. In comparison, the rate for England of 3.8% increased by 0.1 percentage points from the previous month."
      ) +
      theme_x()
  } else {
    gg <-
      ggplot(claimant_count_wards) + 
      geom_sf_interactive(aes(tooltip =
                                paste0('<span class="plotTooltipValue">', ifelse(is.na(value),value,paste0(value,"%")),'</span><br />',
                                       '<span class="plotTooltipMain">', area_name, '</span><br />'),
                              fill = value), color = "#FFFFFF", size = 0.5, alpha = 0.8) +
      scale_fill_gradient(  low = "#b9e0e6",
                            high = "#00445e",
                            space = "Lab",
                            na.value = "grey50",
                            breaks = c(min(claimant_count_wards$value, na.rm=T),max(claimant_count_wards$value, na.rm=T)),
                            label = function(x) paste0(x, "%"),
                            guide = guide_legend(
                              title = NULL,
                              reverse = TRUE,
                              keyheight = unit(3, units = "mm"), 
                              keywidth = unit(6, units = "mm"), 
                              ncol = 2)
      ) +
      labs(
        title = "Claimant Count rate - aged 16 to 64 by ward",
        subtitle = "April 2024",
        caption = "Source: ONS",
        x = NULL,
        y = NULL,
        alt = "Map showing the proportion of Claimant Count as a proportion of people aged 16 to 64 in each of Trafford's wards in September 2022. The wards in the North have high proportions with Clifford having the highest overall at 6.3% and Longford, Stretford and Gorse Hill between 4.9% and 5.6%. Bucklow-St Martins in the West had the second highest proportion at 5.7%. Wards with proportions over 3% are Sale Moor at 3.3% and St Mary's at 3.4% in the Central area and Village at 3.1% in the South"
      ) +
      coord_sf(datum = NA) +
      theme_x() +
      theme(plot.subtitle = element_text(size = 11),
            legend.position = c(0.5, 1.055))
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_claimant_count_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_claimant_count_plot")
  
})

# Render the output in the ui object
output$claimant_count_box <- renderUI({
  withSpinner(
    girafeOutput("claimant_count_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})



# Improve the number of affordable housing completions ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderGirafe({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     girafeOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Reduction in % of children in poverty ---------

# Load in data

children_poverty <- read_csv("data/economy/children_poverty.csv") #%>%
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
df_neet <- read_csv("data/economy/neet.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(indicator, period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

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


# Improve the number of people being re-housed (from Trafford’s housing waiting list) ---------

# Load in data


# Plot
#output$[INDICATOR NAME]_plot <- renderGirafe({

#})

# Render the output in the ui object
# output$[INDICATOR NAME]_box <- renderUI({
#   withSpinner(
#     girafeOutput("[INDICATOR NAME]_plot", height = "inherit"),
#     type = 4,
#     color = plot_colour_spinner,
#     size = 1,
#     proxy.height = "250px"
#   )
# })


# Reduce % of households fuel poverty levels ---------

# Load in data

fuel_poverty <- read_csv("data/economy/fuel_poverty.csv") %>%
  mutate(period = as_factor(period))

fuel_poverty_cipfa_mean <- fuel_poverty %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average"
  ) %>%
  filter(!is.na(value))

fuel_poverty_trend <- bind_rows(fuel_poverty %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), fuel_poverty_cipfa_mean) 

# Plot
output$fuel_poverty_plot <- renderGirafe({
  
  if (input$fuel_poverty_selection == "Trend") {
    
    gg <- ggplot(
      filter(fuel_poverty_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Households in fuel poverty",
        subtitle = NULL,
        caption = "Source: BEIS\nData from 2019 based on low income, low energy efficiency,\nother years data based on low income, high cost.",
        x = NULL,
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the percentage of households in fuel poverty in Trafford compared with the average of similar authorities and England between 2014 and 2020. Whilst the lines for the average of similar authorities to Trafford and the England average show a broadly consistent trend, with the latter approximately 1 percentage point higher throughout the time period, the trend for Trafford has been irratic. Trafford's percentages have been higher, lower and in between its comparitors. Following a change in methodology in 2019, all lines show a sharp increase from the previous year with Trafford having 12.8% of households in fuel poverty compared with 11.8% for the average of similar authorities and 13.4% in England.Trafford has decreased to 12.3% in 2020 coming closer to the average of similar authorities at 12.1% whilst England decreased slighly to 13.2%"
      ) +
      theme_x()
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_fuel_poverty_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_fuel_poverty_plot")
  
})

# Render the output in the ui object
output$fuel_poverty_box <- renderUI({
  withSpinner(
    girafeOutput("fuel_poverty_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Improve overall employment rate (aged 16-64) (%) ---------

# Load in data

employment_rate <- read_csv("data/economy/employment_rate.csv") #%>%
  #mutate(period = as_factor(period)) %>%
  #filter(!is.na(value))

employment_rate_cipfa_mean <- employment_rate %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average"#,
         #period = as_factor(period)
         ) %>%
  filter(!is.na(value))

employment_rate_trend <- bind_rows(employment_rate %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), employment_rate_cipfa_mean) %>%
  mutate(period = str_sub(period, start = 10))

# Plot
output$employment_rate_plot <- renderGirafe({
  
  if (input$employment_rate_selection == "Trend") {
    
    gg <- ggplot(
      filter(employment_rate_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
        title = "Employment rate - aged 16 to 64",
        subtitle = NULL,
        caption = "Source: Annual Population Survey",
        x = "12 months ending",
        y = "Percentage",
        colour = NULL,
        alt = "Line chart showing the employment rate amongst those aged 16 to 64 in Trafford compared to the average of similar authorities and England between the 12 months ending March 2012 and the 12 months ending March 2023. The average for similar authorities to Trafford and England follow a broadly similar trend with the England rate approximately between 1 and 2 percentage points below. From the period in 2016 to 2018 and in 2020, Trafford's rate was above the average of similar local authorities and England. Since then Trafford's rate has been on a downward trend and at the period ending March 2023 is now below its comparitors at 73.5%, with the England average at 75.7% and the average of similar authorities at 78.5%."
      ) +
      theme_x()
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_employment_rate_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_employment_rate_plot")

})

# Render the output in the ui object
output$employment_rate_box <- renderUI({
  withSpinner(
    girafeOutput("employment_rate_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Improve employees paid at/above the real living wage ---------

# Load in data
df_real_living_wage <- read_csv("data/economy/real_living_wage.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 1))

# Plot
output$real_living_wage_plot <- renderGirafe({
  gg <- ggplot(df_real_living_wage,
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
    scale_x_continuous(breaks = seq(from = min(df_real_living_wage$period), to = max(df_real_living_wage$period), by = 1)) +
    labs(title = "Employees paid at or above the real living wage",
         subtitle = NULL,
         caption = "Source: ONS",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing that a lower percentage of employees are paid at or above the real living wage in Trafford compared to the average of similar authorities and England. Between 2016 and 2021, Trafford's percentage has been on an upward trend, with the only exception being a 0.3 percentage point drop in 2020. Data for 2021 shows 80.7% of employees are paid at or above the real living wage in Trafford, compared to 82.8% across England and 84.9% for the average of similar authorities.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_real_living_wage_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_real_living_wage_plot")
})

# Render the output in the ui object
output$real_living_wage_box <- renderUI({
  withSpinner(
    girafeOutput("real_living_wage_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Improve school readiness all children and those with a free school meal status ---------

# Load in data
df_school_readiness <- read_csv("data/economy/school_readiness.csv") %>%
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
df_expected_standard_ks2 <- read_csv("data/economy/expected_standard_ks2.csv") %>%
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

