#' AboutPage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_AboutPage_ui <- function(id){
  ns <- NS(id,"AboutPage")
  tagList(
           # includeHTML("inst/app/www/header.html"),
           includeHTML("inst/app/www/About.html"),
           # includeHTML("inst/app/www/footer.html")
  )
}
    
#' AboutPage Server Functions
#'
#' @noRd 
mod_AboutPage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_AboutPage_ui("AboutPage_ui_1")
    
## To be copied in the server
# mod_AboutPage_server("AboutPage_ui_1")
