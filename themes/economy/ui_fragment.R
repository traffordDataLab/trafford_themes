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
        ),
        #Apprenticeships starts
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Apprenticeship starts"),
            uiOutput("apprenticeship_starts_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "apprenticeship_starts_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/apprenticeship_starts.md"),
            HTML('</details>')
        ),
        # Housing affordability ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Housing affordability"),
            uiOutput("housing_affordability_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "housing_affordability_selection",
              choiceNames = c("WB Trend", "RB Trend"),
              choiceValues = c("WB Trend", "RB Trend"),
              selected = "WB Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/housing_affordability.md"),
            HTML('</details>')
        ),
        #Major planning applications
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Major planning applications"),
            uiOutput("planning_applications_major_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "planning_applications_major_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/planning_applications_major.md"),
            HTML('</details>')
        ),
        #Minor planning applications
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Minor planning applications"),
            uiOutput("planning_applications_minor_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "planning_applications_minor_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/planning_applications_minor.md"),
            HTML('</details>')
        )
    )
)
