#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

    mod_DisplayMap_server("DisplayMap")
    mod_TripEntry_server("TripEntryPage")
    mod_Advocacy_Analysis_server("AdvocacyAnalysis")
    mod_EducationAnalysis_server("EducationAnalysis")
}
