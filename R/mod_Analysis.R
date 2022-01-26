#' Analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Analysis Server Functions
#'
#' @noRd 
mod_Analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Analysis_ui("Analysis_ui_1")
    
## To be copied in the server
# mod_Analysis_server("Analysis_ui_1")
