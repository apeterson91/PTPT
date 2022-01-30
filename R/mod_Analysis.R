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
        dplyr::filter(purpose == "groceries") %>% 
        ## TODO: redo hptt construction and join duration
        ## use here
        ggplot2::ggplot(ggplot2::aes(x = Income,
                   y = bodds)) + 
        ggplot2::geom_point()  +
        ggplot2::geom_smooth() + 
        ggplot2::xlab("Age") + 
        ggplot2::ylab("Probability of Biking")
    })
 
  })
}
    
## To be copied in the UI
# mod_Analysis_ui("Analysis_ui_1")
    
## To be copied in the server
# mod_Analysis_server("Analysis_ui_1")
