# User Interface code for theme: Children & Families
# All ui output objects defined in server_fragment.R need to be referenced here

tabPanel(
    title = "Children & Families",
    h2("Children & Families"),
    includeHTML("help.html"),
    fluidRow(
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Obesity in 4-5 year olds"),
          uiOutput("obese_reception_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "obese_reception_selection",
            choiceNames = c("Trend", "Boxplot", "Deprivation", "Map"),
            choiceValues = c("Trend", "Boxplot", "Deprivation", "Map"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/obese_reception.md"),
          HTML('</details>')
      ),
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Obesity in 10-11 year olds"),
          uiOutput("obese_year6_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "obese_year6_selection",
            choiceNames = c("Trend", "Boxplot", "Deprivation", "Map"),
            choiceValues = c("Trend", "Boxplot", "Deprivation", "Map"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/obese_year6.md"),
          HTML('</details>')
      ),
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Active children"),
          uiOutput("active_children_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "active_children_selection",
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/active_children.md"),
          HTML('</details>')
      ),
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Children with Dental Decay"),
          uiOutput("children_dental_decay_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "children_dental_decay_selection",
            choiceNames = c("Trend", "Boxplot"),
            choiceValues = c("Trend", "Boxplot"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/children_dental_decay.md"),
          HTML('</details>')
      ),
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
          includeMarkdown("data/children/metadata/children_poverty.md"),
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
          includeMarkdown("data/children/metadata/neet.md"),
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
          includeMarkdown("data/children/metadata/school_readiness.md"),
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
          includeMarkdown("data/children/metadata/expected_standard_ks2.md"),
          HTML('</details>')
      ),
      # Percentage achieving 9-5 in English & mathematics at the end of key stage 4 (GCSE) ---------
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Grades 5 or above KS4"),
          uiOutput("grades_5_or_above_ks4_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "grades_5_or_above_ks4_selection",
            choiceNames = c("Trend"),
            choiceValues = c("Trend"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/grades_5_or_above_ks4.md"),
          HTML('</details>')
      ),
      # Average Progress 8 score ---------
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Average Progress 8 score"),
          uiOutput("progress_8_score_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "progress_8_score_selection",
            choiceNames = c("Trend"),
            choiceValues = c("Trend"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/progress_8_score.md"),
          HTML('</details>')
      ),
      # Population vaccination coverage - MMR for one dose (2 years old and 5 year old) ---------
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Vaccination MMR"),
          uiOutput("vaccination_mmr_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "vaccination_mmr_selection",
            choiceNames = c("2y Trend", "5y Trend"),
            choiceValues = c("2y Trend", "5y Trend"),
            selected = "2y Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/vaccination_mmr.md"),
          HTML('</details>')
      ),
      # Children cautioned or sentenced per 10,000 aged 10-17 ---------
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("Young people offending"),
          uiOutput("children_offending_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "children_offending_selection",
            choiceNames = c("Trend"),
            choiceValues = c("Trend"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/children_offending.md"),
          HTML('</details>')
      ),
      # Children looked after who had their annual health assessment ---------
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("CLA health assessment"),
          uiOutput("cla_health_assessment_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "cla_health_assessment_selection",
            choiceNames = c("Trend"),
            choiceValues = c("Trend"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/cla_health_assessment.md"),
          HTML('</details>')
      ),
      # Children looked after who had their teeth checked by a dentist---------
      div(class = "col-sm-12 col-md-6 col-lg-4",
          h3("CLA dental check"),
          uiOutput("cla_dental_check_box", class = "indicatorContainer"),
          radioGroupButtons(
            inputId = "cla_dental_check_selection",
            choiceNames = c("Trend"),
            choiceValues = c("Trend"),
            selected = "Trend",
            direction = "horizontal",
            individual = FALSE,
            status = "plotButtons" # Our custom CSS class, .btn-plotButtons
          ),
          HTML('<details class="furtherInfo">
                    <summary>Further information</summary>'),
          includeMarkdown("data/children/metadata/cla_dental_check.md"),
          HTML('</details>')
      )
      
    )
)
