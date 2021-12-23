#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    navbarPage(
      title = "Pittsburgh Transit Propensity Tool",
      id = "nav",
      tabPanel(
        "Map",
        shinyjs::useShinyjs(),
        div(
            class = "outer",
            tags$head(includeCSS("inst/app/www/custom.css")),
            leaflet::leafletOutput("DisplayMap",width = "100%",height = "100%"),
            absolutePanel(
                id = "controls",
                class = "panel panel-default",
                fixed = TRUE,
                top = 60, left = "auto",
                right = 20, bottom = "auto",
                width = 330, height = "auto",
                #style = "opacity: 0.9 z-index: 1; position: absolute",
                tags$div(title = "Show/Hide Panel",
                         a(##TODO: Fix this button
                             id = "toggle_panel",
                             style = "font-size: 80%",
                             span(class = "glyphicon glyphicon-circle-arrow-up",
                                  "Hide")
                         )),
                tags$div(title = "Trip purpose",
                         selectInput("purpose", "Trip purpose:", purposes,
                                     selectize = FALSE)),
                tags$div(title = "Geography",
                         selectInput("geography","Geography:", geographies,
                                     selectize =  FALSE))
            ),
            )
      )
      
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'PTPT'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

