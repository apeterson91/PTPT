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
      theme = shinythemes::shinytheme("cosmo"),
      title = "Pittsburgh Transit Propensity Tool",
      id = "nav",
      tabPanel(
        "Map",
        shinyjs::useShinyjs(),
        mod_DisplayMap_ui("DisplayMap")
      ),
      tabPanel(
        "About",
         mod_AboutPage_ui("AboutPage")
      ),
      tabPanel(
        "Methods",
        mod_Methods_ui("MethodsPage")
               ),
      tabPanel(
        "Trip Entry",
         mod_TripEntry_ui("TripEntryPage")
               ),
      navbarMenu(
        "Analyses",
        tabPanel("Advocacy",h3("Advocacy")),
        tabPanel("Education",h3("Education"))
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
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

