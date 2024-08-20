# Load required packages
library(shiny)
library(bslib)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(ggiraph)
library(sf)
library(leaflet)
library(scales)
library(markdown)


# Set common plot colours, themes and options etc. used in the visualisations ---------
# NOTE: These are here to be in global scope - would've previously been placed in global.R

# ggplot2 theme
theme_x <- function () { 
  theme_minimal(base_size = 12, base_family = "Open Sans") %+replace% 
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.caption = element_text(color = "#333333", size = 10, hjust = 1, margin = margin(t = 15)),
      axis.title.x = element_text(size = 11, hjust = 1, margin = margin(t = 10)),
      axis.title.y = element_text(size = 11, angle = 90, hjust = 1, margin = margin(r = 10)),
      axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 0)),
      legend.position = "none"
    )
}

# customisation of ggiraph interactive output, building on top of theme_x() to give a common appearance to the plots
lab_ggiraph_options <- list(opts_tooltip(use_fill = FALSE, opacity = 1, css = "background-color: #e7e6e1; color: #212121; padding: 0.5em; border-radius: 0.5em;"),
                            opts_hover(css = "fill-opacity: 1; stroke:white; stroke-opacity: 1; r: 2.5pt;"),
                            opts_selection(type = "single"),
                            opts_toolbar(saveaspng = FALSE))

plot_colour_trafford = "#00445e"
plot_colour_similar_authorities = "#009590"
plot_colour_england = "#ffcb00"
plot_colour_spinner = "#bdbdbd"


# Set SASS variables for main app theme ---------
bs_global_theme(version = "4", bootswatch = NULL)
tab_theme <- bs_theme(
  bg = "#e7e6e1",
  fg = "#00142e"
)

# Setup the user interface ---------
ui <- fluidPage(
    lang = "en-GB", # set the language of the page - important for accessibility
    tags$head(
        tags$link(rel = "stylesheet", href = "css/main.css"),
        tags$link(rel = "stylesheet", href = "css/tabs.css"),
        tags$link(rel = "stylesheet", href="https://fonts.googleapis.com/css?family=Open+Sans%7CRoboto")
    ),

    HTML('<header class="themeDarkBlueBIU">
              <a href="https://www.trafford.gov.uk" aria-label="Go to Trafford Council website"><img src="images/biu_logo_white_on_transparent_large.png" alt="Trafford Council | Business Intelligence Unit" id="logoBIU"/></a>
              <h1>Trafford Themes</h1>
          </header>
          <main>'
    ),

    navbarPage(
        id = "tabs",
        title = "",
        windowTitle = "Trafford Themes", # This is the page title. Needs to be here otherwise an empty <title> is created.
        collapsible = TRUE,
        theme = tab_theme,

        tabPanel(
            # Home/landing page
            title = "Introduction",
            icon = icon("house"),
            HTML("<div id='homePageContainer'>
                  <h2>Visualising themes relating to the Council's priorities</h2>
                  <p>The <a href='https://www.trafford.gov.uk/about-your-council/strategies-plans-and-policies/Corporate-Plan.aspx' target='_blank'>corporate plan</a> describes Trafford Council's strategic vision, outcomes and priorities for the borough. The themes below relate to the priorities and other important areas of work undertaken by the council.</p>"
            ),

            fluidRow(
                column(width = 6,
                       tags$button(
                           id = "children_btn",
                           class = "btn action-button homeMenuButton",
                           span(
                               class = "fa-solid fa-circle-right",
                               role = "presentation"
                           ),
                           div(
                               h3("Children & Families"),
                               p("The best start for our children and young people, providing the right help at the right time for families.")
                           )
                       )
                ),
                column(width = 6,
                       tags$button(
                           id = "climate_btn",
                           class = "btn action-button homeMenuButton",
                           span(
                               class = "fa-solid fa-circle-right",
                               role = "presentation"
                           ),
                           div(
                               h3("Climate Crisis"),
                               p("Reducing our carbon footprint and addressing the impact of climate change in Trafford.")
                           )
                       )
                )
            ),
            fluidRow(
                column(width = 6,
                       tags$button(
                           id = "economy_btn",
                           class = "btn action-button homeMenuButton",
                           span(
                               class = "fa-solid fa-circle-right",
                               role = "presentation"
                           ),
                           div(
                               h3("Economy & Homes"),
                               p("Thriving businesses and great access to employment and good quality housing for residents.")
                           )
                       )
                ),
                column(width = 6,
                       tags$button(
                           id = "health_btn",
                           class = "btn action-button homeMenuButton",
                           span(
                               class = "fa-solid fa-circle-right",
                               role = "presentation"
                           ),
                           div(
                               h3("Healthy Lives"),
                               p("Striving to make Trafford a place where residents of all ages can be as healthy and independent as possible.")
                           )
                       )
                )
            ),

            HTML("<h3>About the dashboard</h3>
                  <p>The dashboard visualises a range of indicators relating to different themes. These show data for Trafford compared to the average of other similar Local Authorities (in terms of statistical characteristics) and also, where possible, to England. Similar Local Authorities for indicators relating to children are defined within the <a href='https://www.gov.uk/government/publications/local-authority-interactive-tool-lait' target='_blank' aria-label=\"Children's Services Statistical Neighbour Benchmarking Tool, (opens in a new window)\">Children's Services Statistical Neighbour Benchmarking Tool</a>. For all other indicators the <a href='https://www.cipfa.org/services/cipfastats/nearest-neighbour-model' target='_blank' aria-label='CIPFA nearest neighbours, (opens in new window)'>CIPFA Nearest Neighbours</a> or <a href='https://github.com/NHSDigital/ASC_LA_Peer_Groups' target='_blank'>NHS England Near Neighbour Peer Group</a> are used as appropriate.</p>
                  <p>
                      <details style='font-size: 0.85em'>
                          <summary><strong>Trafford's Children's Services Statistical Neighbours</strong></summary>
                          <ul>
                              <li>Bracknell Forest</li>
                              <li>Bromley</li>
                              <li>Buckinghamshire</li>
                              <li>Central Bedfordshire</li>
                              <li>Cheshire East</li>
                              <li>Hampshire</li>
                              <li>Hertfordshire</li>
                              <li>Solihull</li>
                              <li>Stockport</li>
                              <li>York</li>
                          </ul>
                      </details>
                      
                      <details style='font-size: 0.85em'>
                          <summary><strong>Trafford's CIPFA Nearest Neighbours</strong></summary>
                          <ul>
                              <li>Bedford</li>
                              <li>Bury</li>
                              <li>Cheshire West and Chester</li>
                              <li>Derby</li>
                              <li>Halton</li>
                              <li>Milton Keynes</li>
                              <li>North Northamptonshire</li>
                              <li>Peterborough</li>
                              <li>Solihull</li>
                              <li>South Gloucestershire</li>
                              <li>Stockport</li>
                              <li>Stockton-on-Tees</li>
                              <li>Swindon</li>
                              <li>Telford and Wrekin</li>
                              <li>Warrington</li>
                          </ul>
                      </details>
                      
                      <details style='font-size: 0.85em'>
                          <summary><strong>Trafford's NHS England Near Neighbour Peer Group</strong></summary>
                          <ul>
                              <li>Barnet</li>
                              <li>Bexley</li>
                              <li>Bracknell Forest</li>
                              <li>Bromley</li>
                              <li>Bury</li>
                              <li>Croydon</li>
                              <li>Havering</li>
                              <li>Hillingdon</li>
                              <li>Kingston upon Thames</li>
                              <li>Merton</li>
                              <li>Richmond upon Thames</li>
                              <li>Solihull</li>
                              <li>Stockport</li>
                              <li>Sutton</li>
                              <li>Windsor and Maidenhead</li>
                          </ul>
                      </details>
                  </p>
                  <p>The visualisations are interactive, displaying the values of the data presented. Some of the indicators have multiple visualisations showing different aspects of the data, which can be selected using the relevant tabs below them. Further information is also provided below each indicator, including links to download the data used in the visualisation(s) and to the original source of the data.</p>
                  </div>"
            )
        ),
        # Pull in all the ui fragments for each of the themes in the order we want the tabs to appear
        source("themes/children/ui_fragment.R", local = TRUE)$value,
        source("themes/climate/ui_fragment.R", local = TRUE)$value,
        source("themes/economy/ui_fragment.R", local = TRUE)$value,
        source("themes/health/ui_fragment.R", local = TRUE)$value

    ),

    HTML('</main>
          <footer>
              <div>Developed in <a href="https://cran.r-project.org/" target="_blank" aria-label="R, (opens in new window)">R</a> by the <a href="https://www.trafforddatalab.io">Trafford Data Lab</a> under the <a href="https://www.trafforddatalab.io/LICENSE.txt">MIT</a> licence</div>
          </footer>
         
          <script>
              /*
                  Receive call from Shiny server to make a given ggraph plot accessible.
                  Despite the fact that the SVGs produced by ggraph are navigatable by screen readers, it is not a good user experience.
                  This call handler and function add features to the SVG to improve this using the accessibility pattern:
                  <svg> + role="img" + <title> + <desc> + aria-labelledby="[ID]" (https://www.smashingmagazine.com/2021/05/accessible-svg-patterns-comparison/#pattern-11-svg-role-img-title-desc-aria-labelledby-id)
              */
              Shiny.addCustomMessageHandler("a11yPlotSVG", function(message) {
                  var a11yCallback = setInterval(function() {  // Setup a call to the update function every 500 milliseconds in case the plot does not exist yet
                      try {
                          // Split out the components of the message parameter passed to the function.
                          // These contain the id of the plot SVG we are manipulating, the title of the plot and the alt text
                          arrMsg = message.split("|")
                          svgId = arrMsg[0];
                          titleText = arrMsg[1]
                          altText = arrMsg[2];
                          
                          // Create a <title> element for the SVG containing the plot title
                          svgTitle = document.createElement("title");
                          svgTitle.setAttribute("id", svgId + "_title");
                          svgTitle.appendChild(document.createTextNode(titleText + ".")); // Add a full stop at the end (usually the titles do not have any) to add a pause between the title and the alt text when it is read aloud.
                          
                          // Create a <desc> element for the SVG containing the plot alt text
                          svgDesc = document.createElement("desc");
                          svgDesc.setAttribute("id", svgId + "_desc");
                          svgDesc.appendChild(document.createTextNode(altText));
                          
                          // Get the plot SVG DOM element, add the <title> and <desc> elements and set the required attributes
                          svg = document.getElementById(arrMsg[0]);
                          svg.appendChild(svgTitle);
                          svg.appendChild(svgDesc);
                          svg.setAttribute("role", "img");
                          svg.setAttribute("aria-labelledby", svgTitle.id + " " + svgDesc.id);
                          
                          clearInterval(a11yCallback);
                      }
                      catch(e) {
                          // An error occurred, try again in 500ms
                      }
                  }, 500);
              });
          </script>')
)

# Declare the server code to supply objects to the user interface ---------
server <- function(input, output, session) {

    # Pull in all the server fragments for each of the themes
    source("themes/children/server_fragment.R", local = TRUE)$value
    source("themes/climate/server_fragment.R", local = TRUE)$value
    source("themes/economy/server_fragment.R", local = TRUE)$value
    source("themes/health/server_fragment.R", local = TRUE)$value
    

    # Event listeners for the buttons on the "introduction" tab to select the relevant tabs
    observeEvent(input$children_btn, {
        updateTabsetPanel(session, "tabs", selected = "Children & Families")
    })
    
    observeEvent(input$climate_btn, {
        updateTabsetPanel(session, "tabs", selected = "Climate Crisis")
    })
    
    observeEvent(input$economy_btn, {
        updateTabsetPanel(session, "tabs", selected = "Economy & Homes")
    })
    
    observeEvent(input$health_btn, {
      updateTabsetPanel(session, "tabs", selected = "Healthy Lives")
    })
    
}

# Run the app
shinyApp(ui = ui, server = server)
