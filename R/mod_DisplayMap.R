#' DisplayMap UI Function
#'
#' @description A shiny Module for presenting the PTPT display map
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DisplayMap_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}
    
#' DisplayMap Server Functions
#'
#' @noRd 
mod_DisplayMap_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_DisplayMap_ui("DisplayMap_ui_1")
    
## To be copied in the server
# mod_DisplayMap_server("DisplayMap_ui_1")
