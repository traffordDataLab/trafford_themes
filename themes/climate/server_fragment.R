# Server code for theme: Climate Crisis

# Licensed vehicles (all vehicle types) ---------

# Load in data and create mean of similar neighbours - data for England is excluded as the counts can't be compared to individual LAs
df_licensed_vehicles <- read_csv("data/climate/licensed_vehicles.csv") %>%
  filter(area_name != "England") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average"),
         period = as.character(period)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value_all_vehicles)))

# Plot
output$licensed_vehicles_plot <- renderGirafe({
  gg <- ggplot(df_licensed_vehicles,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
               geom_line(linewidth = 1) +
               geom_point_interactive(
                 aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                                      '<span class="plotTooltipMain">', area_name, '</span><br />',
                                      '<span class="plotTooltipPeriod">', period, '</span>')),
                 shape = 21, size = 2.5, colour = "white"
               ) +
               scale_colour_manual(values = if_else(df_licensed_vehicles$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
               scale_fill_manual(values = if_else(df_licensed_vehicles$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
               scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
               labs(title = "All Licensed vehicles",
                    subtitle = NULL,
                    caption = "Source: DfT and DVLA",
                    x = NULL,
                    y = "Count",
                    fill = NULL,
                    alt = "Line chart showing the number of vehicles registered to Trafford addresses has been consistently lower compared to the average of similar authorities over the 3 year period from June 2021 to March 2024. The numbers in Trafford for each calendar quarter are consistently around 130,000. The average for similar authorities has been much higher compared to Trafford, with around 206,000 - 212,000 licensed vehicles each calendar quarter. The latest data shows 131,403 vehicles registered within Trafford compared with an average of 211,995 vehicles in similar authorities.") +
               theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_licensed_vehicles_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_licensed_vehicles_plot")
})

# Render the output in the ui object
output$licensed_vehicles_box <- renderUI({
  withSpinner(
    girafeOutput("licensed_vehicles_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Licensed vehicles (ulev) ---------

# Load in data and create percentages as well as average of similar authorities
df_licensed_ulev <- read_csv("data/climate/licensed_vehicles.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford",
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average"),
         period = as.character(period),
         value = round((value_ulev/value_all_vehicles)*100, 2)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 2))

# Plot
output$licensed_ulev_plot <- renderGirafe({
  gg <- ggplot(df_licensed_ulev,
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
    labs(title = "Proportion of Ultra Low Emission Vehicles",
         subtitle = NULL,
         caption = "Source: DfT and DVLA",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing the proportion of licensed vehicles registered to Trafford addresses which are ultra low emission vehicles has been consistently lower, and increasing at a slower rate, compared to the average for similar authorities and England across the 3 year period from June 2021 to March 2024. The latest data shows the average proportion for similar authorities was 9.15% compared with an average of 4.49% in England and 3.41% in Trafford.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_licensed_ulev_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_licensed_ulev_plot")
})

# Render the output in the ui object
output$licensed_ulev_box <- renderUI({
  withSpinner(
    girafeOutput("licensed_ulev_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Vehicle miles travelled ---------

# Load in data and create mean of similar neighbours
df_vehicle_miles <- read_csv("data/climate/vehicle_miles_travelled.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England LA average" ~ "England LA average",
                               TRUE ~ "Similar authorities average"),
         period = as.character(period)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value)))

# Plot
output$vehicle_miles_plot <- renderGirafe({
  gg <- ggplot(df_vehicle_miles,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma(accuracy = 1)(value), ' million</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England LA average" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England LA average" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
    labs(title = "Annual motor vehicle miles travelled",
         subtitle = NULL,
         caption = "Source: DfT",
         x = NULL,
         y = "Miles (millions)",
         fill = NULL,
         alt = "Line chart showing the number of miles travelled annually within Trafford between 2012 and 2023 is consistently much lower compared with the average for similar authorities and the average of all local authorities in England. Mileage in Trafford prior to 2020 was on average around 930 million miles per year whereas its comparators were on an increasing trend. Following 2020 where mileage dropped considerably due to the COVID-19 pandemic, the data for 2023 shows both Trafford (969 million miles) and the average of similar local authorities (1,414 million miles) back to pre-pandemic levels and the average of all local authorities in England (1,846 million miles) similar to that recorded in 2016.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_vehicle_miles_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_vehicle_miles_plot")
})

# Render the output in the ui object
output$vehicle_miles_box <- renderUI({
  withSpinner(
    girafeOutput("vehicle_miles_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Electric vehicle charging points ---------

# Load in data and create mean of similar neighbours
df_ev_charging_points_rate <- read_csv("data/climate/electric_vehicle_charging_points.csv") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), 1))

# Plot
output$ev_charging_points_plot <- renderGirafe({
  gg <- ggplot(df_ev_charging_points_rate,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_fill_manual(values = c("Trafford" = plot_colour_trafford, "Similar authorities average" = plot_colour_similar_authorities, "England" = plot_colour_england)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "Publicly available electric vehicle charging devices",
         subtitle = NULL,
         caption = "Source: DfT and OZEV",
         x = NULL,
         y = "Devices (per 100K)",
         fill = NULL,
         alt = "Line chart showing publicly available charging devices per 100,000 people in Trafford compared to the average of similar authorities and England between October 2021 and July 2024. The number of devices per 100,000 people in Trafford has been consistently much lower than its comparitors, however between October 2023 and January 2024 there was a significant increase raising the number in Trafford above them for the first time. The latest data shows Trafford having 85.1 devices per 100,000 population, above the average for similar authorities of 81 devices but below the national average of 97.4.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_charging_points_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_charging_points_plot")
})

# Render the output in the ui object
output$ev_charging_points_box <- renderUI({
  withSpinner(
    girafeOutput("ev_charging_points_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Household waste recycling ---------

# Load in data for percentages and tonnes separately and create mean of similar neighbours
df_household_waste_recycling_percentage <- read_csv("data/climate/household_waste_recycling.csv") %>%
  filter(measure == "Percentage") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

df_household_waste_recycling_tonnes <- read_csv("data/climate/household_waste_recycling.csv") %>%
  filter(measure == "Frequency") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$household_waste_recycling_plot <- renderGirafe({
  
  if (input$household_waste_recycling_selection == "% Trend") {
  
    gg <- ggplot(df_household_waste_recycling_percentage,
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
      labs(title = "Household waste collected and sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing the percentage of household waste collected and sent for recycling in Trafford has been higher than the average of similar authorities and England since 2011/12. Whilst the trend for its comparitors has remained broadly consistent, Trafford showed a big increase between 2012/13 (47.9%) and 2014/15 (61.9%) before going on a downward trend until 2020/21. However in 2021/22 Trafford recorded its highest percentage since 2017/18 of household waste collected and sent for recycling (58.8%) compared to the average of similar authorities (44.5%) and the average for England (42.5%).") +
      theme_x()
    
  } else {
    
    gg <- ggplot(df_household_waste_recycling_tonnes,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                             '<span class="plotTooltipMain">', area_name, '</span><br />',
                             '<span class="plotTooltipPeriod">', period, '</span>')),
        shape = 21, size = 2.5, colour = "white"
      ) +
      scale_colour_manual(values = if_else(df_household_waste_recycling_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_fill_manual(values = if_else(df_household_waste_recycling_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
      labs(title = "Household waste collected and sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Tonnes",
           fill = NULL,
           alt = "Line chart showing between 2014/15 and 2016/17 Trafford collected and sent for recycling around 50,000 tonnes of household waste, approximately 6,000 more than the average for similar authorities. However, in 2017/18 the amount collected in Trafford reduced to 45,269 tonnes, and from 2018/19 onwards the amounts have been broadly similar to its comparator. In 2021/22 Trafford collected and sent for recycing 47,469 tonnes compared to an average of 46,683 tonnes for similar authorities.") +
      theme_x()
    
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_household_waste_recycling_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_household_waste_recycling_plot")
})

# Render the output in the ui object
output$household_waste_recycling_box <- renderUI({
  withSpinner(
    girafeOutput("household_waste_recycling_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px",
  )
})


# Household waste not recycled ---------

# Load in data for percentages and tonnes separately and create mean of similar neighbours
df_household_waste_not_recycled_percentage <- read_csv("data/climate/household_waste_not_recycled.csv") %>%
  filter(measure == "Percentage") %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

df_household_waste_not_recycled_tonnes <- read_csv("data/climate/household_waste_not_recycled.csv") %>%
  filter(measure == "Frequency") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))


# Plot
output$household_waste_not_recycled_plot <- renderGirafe({
  
  if (input$household_waste_not_recycled_selection == "% Trend") {
  
    gg <- ggplot(df_household_waste_not_recycled_percentage,
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
      labs(title = "Household waste collected not sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Percentage",
           fill = NULL,
           alt = "Line chart showing that Trafford has had a lower percentage of household waste collected but not sent for recycling than the average of similar authorities and England between 2011/12 and 2020/21, with the gap narrowing between 2016/17 to 2020/21. However, in 2021/22 Trafford recorded its lowest percentage since 2017/18 of waste collected but not sent for recycling (41.2%) compared with 55.5% for the average of similar authorities and 57.5% for the England average.") +
      theme_x()
    
  } else {
  
    gg <- ggplot(df_household_waste_not_recycled_tonnes,
                 aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
      geom_line(linewidth = 1) +
      geom_point_interactive(
        aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                             '<span class="plotTooltipMain">', area_name, '</span><br />',
                             '<span class="plotTooltipPeriod">', period, '</span>')),
        shape = 21, size = 2.5, colour = "white"
      ) +
      scale_colour_manual(values = if_else(df_household_waste_not_recycled_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_fill_manual(values = if_else(df_household_waste_not_recycled_tonnes$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
      scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
      labs(title = "Household waste collected not sent for recycling",
           subtitle = NULL,
           caption = "Source: DEFRA",
           x = NULL,
           y = "Tonnes",
           fill = NULL,
           alt = "Line chart showing Trafford has had a lower tonnage of household waste that was collected but not sent for recycling compared to the average of similar authorities between 2014/15 and 2021/22. Following a sharp increase in 2020/21 for both Trafford and its comparator, Trafford's tonnage in 2021/22 (27,415) reduced by a greater amount than the average of similar authorities (41,636.9) compared to the previous year.") +
      theme_x()
  
  }
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_household_waste_not_recycled_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_household_waste_not_recycled_plot")
})

# Render the output in the ui object
output$household_waste_not_recycled_box <- renderUI({
  withSpinner(
    girafeOutput("household_waste_not_recycled_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Domestic Energy Performance Certificates ---------

# Load in data
df_epc <- read_csv("data/climate/energy_performance_certificates.csv")

# function to calculate 10-year periods of data as this is the validity period for the certificates
calc10YearProportionEPC <- function(period_from, period_to) {
  filter(df_epc, between(period, as.Date(period_from), as.Date(period_to))) %>%
    group_by(area_code, area_name) %>%
    summarise(value_certificates_lodged = sum(value_certificates_lodged),
              value_rating_A = sum(value_rating_A),
              value_rating_B = sum(value_rating_B),
              value_rating_C = sum(value_rating_C)) %>%
    # Important to convert the time period into a factor to ensure the ordering is correct in the plot
    mutate(period = as.factor(paste0(format.Date(as.Date(period_from), "%b %Y"), " - ", format.Date(as.Date(period_to), "%b %Y"))),
           value_AC = sum(value_rating_A, value_rating_B, value_rating_C),
           value = (value_AC/value_certificates_lodged)*100) %>%
    select(area_code, area_name, period, value)
}

# Get the data for the 10 year periods
df_epc <- bind_rows(calc10YearProportionEPC("2011-09-30", "2021-06-30"),
                    calc10YearProportionEPC("2011-12-31", "2021-09-30"),
                    calc10YearProportionEPC("2012-03-31", "2021-12-31"),
                    calc10YearProportionEPC("2012-06-30", "2022-03-31"),
                    calc10YearProportionEPC("2012-09-30", "2022-06-30"),
                    calc10YearProportionEPC("2012-12-31", "2022-09-30"),
                    calc10YearProportionEPC("2013-03-31", "2022-12-31"),
                    calc10YearProportionEPC("2013-06-30", "2023-03-31"),
                    calc10YearProportionEPC("2013-09-30", "2023-06-30"),
                    calc10YearProportionEPC("2013-12-31", "2023-09-30"),
                    calc10YearProportionEPC("2014-03-31", "2023-12-31"),
                    calc10YearProportionEPC("2014-06-30", "2024-03-31"))

# Create the average of similar LAs
df_epc <- df_epc %>%
  mutate(area_name = case_when(area_name == "Trafford" ~ "Trafford", 
                               area_name == "England" ~ "England",
                               TRUE ~ "Similar authorities average")) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$domestic_epc_plot <- renderGirafe({
  gg <- ggplot(df_epc,
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
    labs(title = "Domestic EPC rated A, B or C over 10 years",
         subtitle = NULL,
         caption = "Source: DLUHC",
         x = NULL,
         y = "Percentage",
         fill = NULL,
         alt = "Line chart showing that over 10 year periods Trafford has consistently lower percentages of domestic properties with Energy Performance Certificates (EPC) rated A, B or C than the average of similar authorities (approximately 10 percentage points fewer) or England (approximately 8 percentage points fewer). The latest time period available, June 2014 to March 2024 shows 38.7% of domestic properties in Trafford having EPCs with the most efficient ratings, compared to 46.0% for England and 48.7% for the average of similar authorities.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_domestic_epc_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_domestic_epc_plot")
})

# Render the output in the ui object
output$domestic_epc_box <- renderUI({
  withSpinner(
    girafeOutput("domestic_epc_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Borough wide CO2 emissions ---------

# Load in data and create mean of similar neighbours
df_borough_co2_emissions <- read_csv("data/climate/borough_wide_co2_emissions.csv") %>%
  mutate(area_name = if_else(area_name == "Trafford", "Trafford", "Similar authorities average"),
         period = as.character(period)) %>%
  group_by(period, area_name) %>%
  summarise(value = round(mean(value), digits = 1))

# Plot
output$borough_co2_emissions_plot <- renderGirafe({
  gg <- ggplot(df_borough_co2_emissions,
               aes(x = period, y = value, colour = area_name, fill = area_name, group = area_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', scales::label_comma()(value), '</span><br />',
                           '<span class="plotTooltipMain">', area_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white"
    ) +
    scale_colour_manual(values = if_else(df_borough_co2_emissions$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
    scale_fill_manual(values = if_else(df_borough_co2_emissions$area_name == "Trafford", plot_colour_trafford, plot_colour_similar_authorities)) +
    scale_y_continuous(limits = c(0, NA), labels = scales::label_comma()) +
    labs(title = expression(paste("Territorial Carbon Dioxide (", CO[2], ") emission estimates")),
         subtitle = NULL,
         caption = "Source: DESNZ",
         x = NULL,
         y = expression(paste("Kilotonnes (kt ", CO[2], "e)")),
         fill = NULL,
         alt = "Line chart showing that territorial carbon dioxide (CO2) emissions in Trafford were higher than the average for similar authorities between 2010 and 2021. Although the amount has been decreasing since 2012, the trend has been very similar to the average for similar authorities. In 2020, emissions for both Trafford and the average of similar local authorities dropped sharply, corresponding with the start of the coronavirus pandemic. However, the latest data for 2021 shows an increase again with an estimated 1,442.1 kilotonnes of CO2 emitted within the borough of Trafford compared to the similar authorities average of 1,163.4 kilotonnes.") +
    theme_x()
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_borough_co2_emissions_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_borough_co2_emissions_plot")
})

# Render the output in the ui object
output$borough_co2_emissions_box <- renderUI({
  withSpinner(
    girafeOutput("borough_co2_emissions_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Nitrogen Dioxide (NO2) Concentration ---------

# Load in data
df_no2_concentration <- read_csv("data/climate/no2_concentration.csv") %>%
  mutate(period = as.character(period))

# Plot
output$no2_concentration_plot <- renderGirafe({
  gg <- ggplot(df_no2_concentration,
               aes(x = period, y = value, colour = station_name, fill = station_name, group = station_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '</span><br />',
                           '<span class="plotTooltipMain">', station_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white",
      show.legend = FALSE
    ) +
    scale_colour_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458", "Trafford Wellacre Academy" = "#FA9209")) +
    scale_fill_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458", "Trafford Wellacre Academy" = "#FA9209")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = expression(paste("Annual mean Nitrogen Dioxide (", NO[2], ") concentration")),
         subtitle = NULL,
         caption = "Source: Trafford Council and Ricardo EE",
         x = "12 months ending",
         y = expression(paste("µg/m"^3)),
         fill = NULL,
         colour = "Location: ",
         alt = "Line chart showing the annual mean of N.O.2 readings taken between the 12 months ending June 2021 and the 12 months ending March 2023 at 3 monitoring stations within Trafford: Trafford A56, Trafford Moss Park and Trafford Wellacre Academy. Readings from Trafford A56 are the highest, followed by Trafford Moss Park and then Trafford Wellacre Academy. The latest annual mean recordings of N.O.2 per cubic metre are 19.9 microgrammes at the Trafford A56, 14.2 microgrammes at Trafford Moss Park and 10.9 microgrammes at Trafford Wellacre Academy.") +
    theme_x() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_no2_concentration_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_no2_concentration_plot")
})

# Render the output in the ui object
output$no2_concentration_box <- renderUI({
  withSpinner(
    girafeOutput("no2_concentration_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


# Particulate Matter (PM10) Concentration ---------

# Load in data
df_pm10_concentration <- read_csv("data/climate/pm10_concentration.csv") %>%
  mutate(period = as.character(period))

# Plot
output$pm10_concentration_plot <- renderGirafe({
  gg <- ggplot(df_pm10_concentration,
               aes(x = period, y = value, colour = station_name, fill = station_name, group = station_name)) +
    geom_line(linewidth = 1) +
    geom_point_interactive(
      aes(tooltip = paste0('<span class="plotTooltipValue">', value, '</span><br />',
                           '<span class="plotTooltipMain">', station_name, '</span><br />',
                           '<span class="plotTooltipPeriod">', period, '</span>')),
      shape = 21, size = 2.5, colour = "white",
      show.legend = FALSE
    ) +
    scale_colour_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458")) +
    scale_fill_manual(values = c("Trafford A56" = "#62156C", "Trafford Moss Park" = "#B63458")) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = expression(paste("Annual mean Particulate Matter (", PM[10], ") concentration")),
         subtitle = NULL,
         caption = "Source: Trafford Council and Ricardo EE",
         x = "12 months ending",
         y = expression(paste("µg/m"^3)),
         fill = NULL,
         colour = "Location: ",
         alt = "Line chart showing the annual mean of PM10 readings taken between the 12 months ending June 2021 and the 12 months ending March 2024 at 2 monitoring stations within Trafford: Trafford A56 and Trafford Moss Park. Usually the annual mean recorded at Trafford A56 is higher than that at Trafford Moss Park, however for the 4 periods following the 12 months up to March 2022 this trend reversed. The latest annual mean figures recorded of PM10 per cubic metre are 12.7 microgrammes at Trafford A56 and 10.5 microgrammes at Trafford Moss Park (the lowest recorded readings at both stations during the time period shown.") +
    theme_x() +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
  
  # Set up a custom message handler to call JS function a11yPlotSVG each time the plot is rendered, to make the plot more accessible
  observe({
    session$sendCustomMessage("a11yPlotSVG", paste0("svg_pm10_concentration_plot|", gg$labels$title, "|", get_alt_text(gg), " ", gg$labels$caption))
  })
  
  # Turn the ggplot (static image) into an interactive plot (SVG) using ggiraph
  girafe(ggobj = gg, options = lab_ggiraph_options, canvas_id = "svg_pm10_concentration_plot")
})

# Render the output in the ui object
output$pm10_concentration_box <- renderUI({
  withSpinner(
    girafeOutput("pm10_concentration_plot", height = "inherit"),
    type = 4,
    color = plot_colour_spinner,
    size = 1,
    proxy.height = "250px"
  )
})


