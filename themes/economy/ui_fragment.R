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
        # Number of people prevented from becoming homeless ---------
        #div(class = "col-sm-12 col-md-6 col-lg-4",
        
        #),
        # Improve the number of affordable housing completions ---------
        #div(class = "col-sm-12 col-md-6 col-lg-4",
        
        #),
        # Reduction in % of children in poverty ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Children in poverty"),
            uiOutput("children_poverty_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "children_poverty_selection",
              choiceNames = c("Rel. Trend", "Abs. Trend", "Rel. Map"),
              choiceValues = c("Rel. Trend", "Abs. Trend", "Rel. Map"),
              selected = "Rel. Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/children_poverty.md"),
            HTML('</details>')
            
        ),
        # Maintain the low level of 16-17 year olds who are NEET and NEET plus unknown ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("16-17 year olds NEET"),
            uiOutput("neet_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "neet_selection",
                choiceNames = c("Trend", "+NK Trend"),
                choiceValues = c("Trend", "+NK Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/neet.md"),
            HTML('</details>')
        ),
        # Improve the number of people being re-housed (from Trafford’s housing waiting list) ---------
        #div(class = "col-sm-12 col-md-6 col-lg-4",
        
        #),
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
        # Improve school readiness all children and those with a free school meal status ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("School readiness"),
            uiOutput("school_readiness_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "school_readiness_selection",
                choiceNames = c("Trend", "FSM Trend"),
                choiceValues = c("Trend", "FSM Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/school_readiness.md"),
            HTML('</details>')
        ),
        # Improve the percentage of pupils reaching the expected standard at the end of key stage 2 in reading, writing and mathematics ---------
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Expected standard KS2"),
            uiOutput("expected_standard_ks2_box", class = "indicatorContainer"),
            radioGroupButtons(
                inputId = "expected_standard_ks2_selection",
                choiceNames = c("Trend"),
                choiceValues = c("Trend"),
                selected = "Trend",
                direction = "horizontal",
                individual = FALSE,
                status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/economy/metadata/expected_standard_ks2.md"),
            HTML('</details>')
        )
    )
)
