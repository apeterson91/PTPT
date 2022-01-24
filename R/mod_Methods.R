#' Methods UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Methods_ui <- function(id){
  ns <- NS(id,"MethodsPage")
  tagList(
    #TODO: fix weird html issues when including header/footer
#           includeHTML("inst/app/www/header.html"),
           includeHTML("inst/app/www/Methods.html"),
#           includeHTML("inst/app/www/footer.html")
  )
}
    
#' Methods Server Functions
#'
#' @noRd 
mod_Methods_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Methods_ui("Methods_ui_1")
    
## To be copied in the server
# mod_Methods_server("Methods_ui_1")
