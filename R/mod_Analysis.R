#' Advocacy Analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Advocacy_Analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    navlistPanel(
      "Description",
      tabPanel("Big Picture"),
      tabPanel("Technical"),
      "Analysis",
      tabPanel("Food Accessibility",
               plotOutput(NS(id,"foodplot"))
               )
    )
  )
}
    
#' Advocacy Analysis Server Functions
#'
#' @noRd 
mod_Advocacy_Analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$foodplot <- renderPlot({
      hptt %>% 
        dplyr::select(IncomeCat,purpose,bike,car,foot) %>% 
        tidyr::gather(bike,car,foot,key = "mode",value = "duration") %>% 
        dplyr::filter(purpose == "groceries") %>% 
        ggplot2::ggplot(ggplot2::aes( duration, color = IncomeCat)) + 
        ggplot2::geom_density() + 
        ggplot2::xlab("Duration to Grocery Story (minutes)") + 
        ggplot2::facet_grid(~ mode)
    })
 
  })
}
    
## To be copied in the UI
# mod_Analysis_ui("Analysis_ui_1")
    
## To be copied in the server
# mod_Analysis_server("Analysis_ui_1")
