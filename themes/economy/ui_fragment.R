# User Interface code for theme: Economy & Homes
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Economy & Homes",
    h2("Economy & Homes"),
    includeHTML("help.html"),
    fluidRow(
        # Percentage receiving Universal Credit (UC) ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Universal Credit"),
            uiOutput("universal_credit_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "universal_credit_selection",
              choiceNames = c("Trend","Map"),
              choiceValues = c("Trend","Map"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
        HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
        includeMarkdown("data/economy/metadata/universal_credit.md"),
        HTML('</details>')
        ),
        # Claimant Count rate ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Claimant Count"),
            uiOutput("claimant_count_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "claimant_count_selection",
              choiceNames = c("Trend", "Map"),
              choiceValues = c("Trend", "Map"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/claimant_count.md"),
            HTML('</details>')
        ),
        # Reduce % of households fuel poverty levels ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Fuel poverty"),
            uiOutput("fuel_poverty_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "fuel_poverty_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/fuel_poverty.md"),
            HTML('</details>')
        ),
        # Improve overall employment rate (aged 16-64) (%) ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Employment rate"),
            uiOutput("employment_rate_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "employment_rate_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/employment_rate.md"),
            HTML('</details>')
        ),

        # Improve employees paid at/above the real living wage ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Real living wage"),
            uiOutput("real_living_wage_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "real_living_wage_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/real_living_wage.md"),
            HTML('</details>')
        ),
        # Business Births and Rates ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Business births and deaths"),
            uiOutput("business_births_deaths_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "business_births_deaths_selection",
              choiceNames = c("Births", "Deaths"),
              choiceValues = c("Births", "Deaths"),
              selected = "Births",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/business_births_deaths.md"),
            HTML('</details>')
        )
    )
)
