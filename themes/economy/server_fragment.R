# Server code for theme: Economy & Homes

# Percentage receiving Universal Credit (UC) ---------

# Load in data

cipfa <- read_csv("data/cipfalga0724.csv") %>%
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
        alt = "Line chart showing universal credit claims as a proportion of people aged 16 to 64 in Trafford compared with the average of similar authorities and England from July 2021 to July 2024. The average of similar authorities and England closely follow each other throughout the time period with Trafford following the same trend but at a lower rate than its comparators. In July 2021 the similar authorities average was 14.6% compared with 14.3% for England and 11.6% in Trafford. The latest data for July 2024 shows 17.4% for the similar authorities average compared with 16.7% for the national average and 12.9% for Trafford."
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
        alt = "Map showing the proportion of Universal Credit claims as a proportion of people aged 16 to 64 in each of Trafford's wards in April 2024. Bucklow-St Martins in the West had the highest propotion of all wards at 29.5%. The wards in the North have high proportions with 25.8% in Clifford and Longford, Stretford and Gorse Hill between 18.4% and 20.8%.  Other wards with proportion over 10% are St Mary's at 18.1% and Sale Moor at 13.6% in the central area and Village at 15% in the South."
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
        alt = "Line chart showing jobseekers allowance and universal credit claims as a proportion of residents aged 16 to 64 in Trafford compared with the average of similar authorities and England from July 2021 to July 2024. All three lines follow a very similar trend, with Trafford having the lowest proportion, followed by the average of similar local authorities and then England. In July 2021 the rates were 4.4% for Trafford, 5% for the average of similar authorities and 5.4% for the national average. From there rates declined and stabilised around 3% in Trafford, 3.5% for the similar authorities average and 3.7% for England, before rising again, particularly sharply from June to July 2024. The latest rate for Trafford is 3.4% compared with 4.2% for the similar authorities average and 4.4% for England."
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
        alt = "Map showing the proportion of Claimant Count as a proportion of people aged 16 to 64 in each of Trafford's wards in April 2024. The wards in the North have high proportions with Clifford having the highest overall at 8.1% and Longford, Stretford and Gorse Hill between 5% and 6.6%. Bucklow-St Martins in the West had the third highest proportion at 6.3%. Wards with proportions over 3% are Sale Moor at 3.1% and St Mary's at 3.7% in the Central area and Village at 3.5% in the South. Timperley in the south has the lowest rate of 1% and the other southern wards of Bowdon, Hale Central and Hale Barns have between 1.1% and 1.6%. Flixton in the west also has a low claimant rate of 1.8%."
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
        alt = "Line chart showing the percentage of households in fuel poverty in Trafford compared with the average of similar authorities and England between 2014 and 2022. Whilst the lines for the average of similar authorities to Trafford and the England average show a broadly consistent trend, with the latter approximately 1 percentage point higher throughout the time period, the trend for Trafford has been irratic. Trafford's percentages have been higher, lower and in between its comparitors. Following a change in methodology in 2019, all lines show a sharp increase from the previous year with Trafford going from 10.4% to 12.8% of households in fuel poverty compared with the average of similar authorities going from 9.2% to 12.4% and England going from 10.3% to 13.4%. Since then the percentages for Trafford and the average of similar authorities have decreased at a faster rate than the national average. The latest data shows 11.4% for Trafford compared with 11.8% for similar authorities and 13.1% for England."
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
        alt = "Line chart showing the employment rate amongst those aged 16 to 64 in Trafford compared to the average of similar authorities and England between the 12 months ending March 2013 and the 12 months ending March 2024. The average for similar authorities to Trafford and England follow a broadly similar trend with the England rate approximately between 0.1 to 2 percentage points below. For the periods in 2016 to 2020 and 2024, Trafford's rate was above the average of similar local authorities and England. From 2020 to 2023 Trafford's rate was on a downward trend going below its comparators, however it has risen again at the period ending March 2024 to 78.3% compared to 75.8% for both the average of similar authorities and England."
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
         alt = "Line chart showing that a lower percentage of employees are paid at or above the real living wage in Trafford compared to the average of similar authorities and England between 2016 and 2023. Starting at 72.7% for Trafford, 76.8% for England and 77% for the average of similar authorities to Trafford, all 3 lines follow a similar trend but with Trafford below its comparators for all years except 2022 where it was in the middle at 88.7%, just behind the similar authorities average of 89% and just ahead of the national average of 87.5%. This was the peak for all 3 lines, and percentages for all have fallen slightly in 2023. The latest data shows 86.1% in Trafford behind 87.3% for the average of similar authorities and England.") +
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

# Business Births and Deaths ---------

# Load in data

business_births <- read_csv("data/economy/business_births_deaths.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(measure == "Per 1000 population", indicator == "Business Births")

business_births_cipfa_mean <- business_births %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value))

business_births_trend <- bind_rows(business_births %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), business_births_cipfa_mean)

business_deaths <- read_csv("data/economy/business_births_deaths.csv") %>%
  mutate(period = as_factor(period)) %>%
  filter(measure == "Per 1000 population", indicator == "Business Deaths")

business_deaths_cipfa_mean <- business_deaths %>%
  filter(area_code %in% c(cipfa$area_code)) %>%
  group_by(period) %>%
  summarise(value = round(mean(value, na.rm=TRUE), 1)) %>%
  mutate(area_name = "Similar Authorities average") %>%
  filter(!is.na(value))

business_deaths_trend <- bind_rows(business_deaths %>% select(area_name, period,value) %>% filter(area_name %in% c("Trafford", "England")), business_deaths_cipfa_mean)



# Plot
output$business_births_deaths_plot <- renderGirafe({
  
  if (input$business_births_deaths_selection == "Births") {
    
    gg <- 
      ggplot(
      filter(business_births_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
      #scale_x_date(date_labels = "%b %y", date_breaks = "3 month", expand = c(0.06,0.06)) +
      scale_x_discrete(breaks = business_deaths_trend %>% select(period) %>% unique() %>% filter(grepl("Q1",period)) %>% pull()) +
      labs(
        title = "Business Births",
        subtitle = NULL,
        caption = "Source: ONS",
        x = NULL,
        y = "Per 1000 population",
        colour = NULL,
        alt = "Line chart showing business births per 1000 population in Trafford compared with the average of similar authorities and England from Quarter 1 2017 to Quarter 2 2024. Q1 2017: Trafford 2.09, England 1.47, Similar Authorities average 1.30. Q2 2024: Trafford 1.39, England 1.23, Similar Authorities average 1.20. Trafford is generaly above comparators."
      ) +
      theme_x()
  } else {
    gg <- 
      ggplot(
      filter(business_deaths_trend, area_name %in% c("Trafford", "Similar Authorities average", "England")),
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
      #scale_x_date(date_labels = "%b %y", date_breaks = "3 month", expand = c(0.06,0.06)) +
      scale_x_discrete(breaks = business_deaths_trend %>% select(period) %>% unique() %>% filter(grepl("Q1",period)) %>% pull()) +
      labs(
        title = "Business Deaths",
        subtitle = NULL,
        caption = "Source: ONS",
        x = NULL,
        y = "Per 1000 population",
        colour = NULL,
        alt = "Line chart showing business deaths per 1000 population in Trafford compared with the average of similar authorities and England from Quarter 1 2017 to Quarter 2 2024. Q1 2017: Trafford 2.06, England 1.24, Similar Authorities average 1.20. Q2 2024: Trafford 1.37, England 1.13, Similar Authorities average 1.10. Trafford is generaly above comparators."
      ) +
      theme_x()
    }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_business_births_deaths_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_business_births_deaths_plot")
  
})

# Render the output in the ui object
output$business_births_deaths_box <- renderUI({
  withSpinner(
    girafeOutput("business_births_deaths_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Apprenticeship starts rate per 100,000 population ---------

# Load in data
df_apprenticeship_starts <- read_csv("data/economy/apprenticeship_starts.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value, na.rm = TRUE), 0))

# Plot
output$apprenticeship_starts_plot <- renderGirafe({
  gg <- ggplot(df_apprenticeship_starts,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, ' per 100,000</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
   # scale_x_continuous(breaks = seq(from = min(df_apprenticeship_starts$period), to = max(df_apprenticeship_starts$period), by = 1)) +
    labs(title = "Apprenticeship starts rate per 100,000 population",
         subtitle = NULL,
         caption = "Source: DfT",
         x = NULL,
         y = "per 100,000 population",
         fill = NULL,
         alt = "Line chart showing Apprenticeship starts rate per 100,000 population in Trafford compared to the average of similar authorities and England between 2017/18 and 2022/23 academic year. 2017/18: Trafford 1000, England 1075, Similar Authorities average 1208. 2022/23: Trafford 850, England 939, Similar Authorities average 1012. Trafford was below its comparators for all years except 2020/21 and the gap has increased from 2021/22 to 2022/23."
         ) +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_apprenticeship_starts_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_apprenticeship_starts_plot")
})

# Render the output in the ui object
output$apprenticeship_starts_box <- renderUI({
  withSpinner(
    girafeOutput("apprenticeship_starts_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Ratio of median house price to median gross annual earnings ---------

# Load in data
df_housing_affordability <- read_csv("data/economy/housing_affordability.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name, indicator) %>%
  summarise(value = round(mean(value, na.rm = TRUE), 1))

# Plot
output$housing_affordability_plot <- renderGirafe({
  if (input$housing_affordability_selection == "WB Trend") {
  gg <- ggplot(df_housing_affordability %>% filter(indicator == "Ratio of median house price to median gross annual workplace-based earnings"),
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, ' house price / earnings</span><br/>',
                           '<span class="plotTooltipMain">', area_name, '</span><br/>',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
     scale_x_continuous(breaks = seq(from = min(df_housing_affordability$period), to = max(df_housing_affordability$period), by = 1)) +
    labs(title = "Median house price to median earnings: Workplace-based",
         subtitle = NULL,
         caption = "Source: ONS",
         x = NULL,
         y = "house price / earnings",
         fill = NULL,
         alt = "Line chart showing workplaced-based housing affordability (median house price divided by median earnings) in Trafford compared with the average of similar authorities and England between 2016 and 2023. Trafford is above its comparators for the time period shown, and whilst they have remained generally consistent Trafford's values are on an upward trend and the gap is increasing. Trafford's values have risen from 8.6 in 2016 to 11.2 in 2023 compared with 6.5 to 7.4 for the average of similar authorities and 7.7 to 8.3 for England."
    ) +
    theme_x()
  } else {
    gg <- ggplot(df_housing_affordability %>% filter(indicator == "Ratio of median house price to median gross annual residence-based earnings"),
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = paste0('<span class="plotTooltipValue">', value, ' house price / earnings</span><br/>',
                             '<span class="plotTooltipMain">', area_name, '</span><br/>',
                             '<span class="plotTooltipPeriod">', period, '</span>')),
        shape = 21, size = 2.5, colour = "white"
      ) +
      scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
      scale_y_continuous(limits = c(0, NA)) +
       scale_x_continuous(breaks = seq(from = min(df_housing_affordability$period), to = max(df_housing_affordability$period), by = 1)) +
      labs(title = "Median house price to median earnings: Residence-based",
           subtitle = NULL,
           caption = "Source: ONS",
           x = NULL,
           y = "house price / earnings",
           fill = NULL,
           alt = "Line chart showing residence-based housing affordability (median house price divided by median earnings) in Trafford compared with the average of similar authorities and England between 2016 and 2023. Trafford has been above its comparators for 4 years out of the 8, including the last 2. In 2016 the value for Trafford was 6.8 compared to the average of similar authorities of 6.5 and 7.7 for England. Trafford's values have been on a general upward trend since then, whilst its comparators initially remained quite static. In 2021 the values for all increased sharply, peaking for the similar authorities average at 7.9 and the national average at 9.1, with Trafford just below at 8.9. However, whilst the values for its comparators have fallen since then, Trafford's value has risen to a high of 9.2 in 2023, compared with 8.3 for the national average and 7.3 for similar authorities."
      ) +
      theme_x()
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_housing_affordability_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_housing_affordability_plot")
})

# Render the output in the ui object
output$housing_affordability_box <- renderUI({
  withSpinner(
    girafeOutput("housing_affordability_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Percentage of all major development planning applications decided within 13 weeks or agreed time ---------

# Load in data
df_planning_applications_major <- read_csv("data/economy/planning_applications_major.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value, na.rm = TRUE), 0))

# Plot
output$planning_applications_major_plot <- renderGirafe({
  gg <- 
    ggplot(df_planning_applications_major,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, ' per 100,000</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    # scale_x_continuous(breaks = seq(from = min(df_planning_applications_major$period), to = max(df_planning_applications_major$period), by = 1)) +
    labs(title = "Major planning applications decided in time",
         subtitle = NULL,
         caption = "Source: MHCLG and DLUHC",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing percentage of major planning applications decided in time in Trafford compared to the average of similar authorities and England between 2022/23 Q2 and 2024/25 Q1. 2022/23 Q2: Trafford 100, England 87, Similar Authorities average 93. 2024/25 Q1: Trafford 100, England 91, Similar Authorities average 87. Trafford had 100% timeliness on all quarters except 2022/23 Q4 when 80% applications was decided on time."
    ) +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_planning_applications_major_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_planning_applications_major_plot")
})

# Render the output in the ui object
output$planning_applications_major_box <- renderUI({
  withSpinner(
    girafeOutput("planning_applications_major_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})

# Percentage of all minor development planning applications decided in time---------

# Load in data
df_planning_applications_minor <- read_csv("data/economy/planning_applications_minor.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value, na.rm = TRUE), 0))

# Plot
output$planning_applications_minor_plot <- renderGirafe({
  gg <- 
    ggplot(df_planning_applications_minor,
           aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, ' per 100,000</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    # scale_x_continuous(breaks = seq(from = min(df_planning_applications_minor$period), to = max(df_planning_applications_minor$period), by = 1)) +
    labs(title = "Minor planning applications decided in time",
         subtitle = NULL,
         caption = "Source: MHCLG and DLUHC",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing percentage of minor planning applications decided in time in Trafford compared to the average of similar authorities and England between 2021/22 Q1 and 2024/25 Q1. 2022/23 Q2: Trafford 83, England 81, Similar Authorities average 83. 2024/25 Q1: Trafford 86, England 87, Similar Authorities average 87. Trafford percentage had been mostly below its comparators from 2022/23 Q1 although the gap is clossing in the last two quarters."
    ) +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_planning_applications_minor_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_planning_applications_minor_plot")
})

# Render the output in the ui object
output$planning_applications_minor_box <- renderUI({
  withSpinner(
    girafeOutput("planning_applications_minor_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})