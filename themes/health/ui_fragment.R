# User Interface code for theme: Healthy Lives
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Healthy Lives",
    h2("Healthy Lives"),
    includeHTML("help.html"),
    fluidRow(
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Overweight or obese adults"),
            uiOutput("overweight_adult_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "overweight_adult_selection",
              choiceNames = c("Trend", "Boxplot"),
              choiceValues = c("Trend", "Boxplot"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/overweight_adult.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Active adults"),
            uiOutput("active_adults_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "active_adults_selection",
              choiceNames = c("Trend", "Boxplot"),
              choiceValues = c("Trend", "Boxplot"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/active_adults.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Inactive adults"),
            uiOutput("inactive_adults_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "inactive_adults_selection",
              choiceNames = c("Trend", "Boxplot"),
              choiceValues = c("Trend", "Boxplot"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/inactive_adults.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Fairly active adults"),
            uiOutput("fairly_active_adults_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "fairly_active_adults_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/fairly_active_adults.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Preventable mortality rate"),
            uiOutput("mortality_rate_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "mortality_rate_selection",
              choiceNames = c("Trend", "Boxplot","Sex"),
              choiceValues = c("Trend", "Boxplot", "Sex"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/mortality_rate.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Healthy life expectancy"),
            uiOutput("healthy_life_expectancy_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "healthy_life_expectancy_selection",
              choiceNames = c("Sex", "Boxplot"),
              choiceValues = c("Sex", "Boxplot"),
              selected = "Sex",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/healthy_life_expectancy.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Unequal life expectancy"),
            uiOutput("inequality_life_expectancy_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "inequality_life_expectancy_selection",
              choiceNames = c("Sex", "Boxplot"),
              choiceValues = c("Sex", "Boxplot"),
              selected = "Sex",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/inequality_life_expectancy.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Smokers in manual jobs"),
            uiOutput("adults_smoking_manual_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "adults_smoking_manual_selection",
              choiceNames = c("Trend", "Boxplot"),
              choiceValues = c("Trend", "Boxplot"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/adults_smoking_manual.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            h3("Adults with depression"),
            uiOutput("adults_depression_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "adults_depression_selection",
              choiceNames = c("Trend", "Boxplot"),
              choiceValues = c("Trend", "Boxplot"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/adults_depression.md"),
            HTML('</details>')
        ),
        div(class = "col-sm-12 col-md-6 col-lg-4",
            HTML("<h3>Adults walking or cycling</h3>"),
            uiOutput("adults_walk_cycle_box", class = "indicatorContainer"),
            radioGroupButtons(
              inputId = "adults_walk_cycle_selection",
              choiceNames = c("Trend"),
              choiceValues = c("Trend"),
              selected = "Trend",
              direction = "horizontal",
              individual = FALSE,
              status = "plotButtons" # Our custom CSS class, .btn-plotButtons
            ),
            HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
            includeMarkdown("data/health/metadata/adults_walking_or_cycling.md"),
            HTML('</details>')
        )
        
    )
)
