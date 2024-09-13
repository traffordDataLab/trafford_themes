# User Interface code for theme: Climate Crisis
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Climate Crisis",
    h2("Climate Crisis"),
    includeHTML("help.html"),
    fluidRow(
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Licensed vehicles"),
            uiOutput("licensed_vehicles_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "licensed_vehicles_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
                    includeMarkdown("data/climate/metadata/licensed_vehicles.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Licensed vehicles: ULEV"),
            uiOutput("licensed_ulev_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "licensed_ulev_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/licensed_ulev.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Vehicle miles"),
            uiOutput("vehicle_miles_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "vehicle_miles_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/vehicle_miles_travelled.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Electric vehicle charging"),
            uiOutput("ev_charging_points_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "ev_charging_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/electric_vehicle_charging_points.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Waste recycled"),
            uiOutput("household_waste_recycling_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "household_waste_recycling_selection",
                choiceNames = c("% Trend", "Wt. Trend"),
                choiceValues = c("% Trend", "Wt. Trend"),
                selected = "% Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/household_waste_recycling.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Waste not recycled"),
            uiOutput("household_waste_not_recycled_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "household_waste_not_recycled_selection",
                choiceNames = c("% Trend", "Wt. Trend"),
                choiceValues = c("% Trend", "Wt. Trend"),
                selected = "% Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/household_waste_not_recycled.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>Domestic EPC</h3>"),
            uiOutput("domestic_epc_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "domestic_epc_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/domestic_epc.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>Borough CO<sub>2</sub> emissions</h3>"),
            uiOutput("borough_co2_emissions_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "borough_co2_emissions_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/borough_co2_emissions.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>NO<sub>2</sub> concentrations</h3>"),
            uiOutput("no2_concentration_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "no2_concentration_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/no2_concentration.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>PM<sub>10</sub> concentrations</h3>"),
            uiOutput("pm10_concentration_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "pm10_concentration_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/pm10_concentration.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>Industrial emissions</h3>"),
            uiOutput("industry_emissions_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "industry_emissions_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/climate/metadata/industry_emissions.md"),
            HTML('</details>')
        )
    )
)