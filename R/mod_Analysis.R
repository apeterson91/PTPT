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
      tabPanel("Big Picture", includeMarkdown("inst/app/www/AnalysisPicture.md")),
      "Analysis",
      tabPanel("Propensity to Transit",
               plotOutput(NS(id,"probplot"))
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
    
    output$probplot <- renderPlot({
      hptt %>% 
        dplyr::mutate(multi_modal = ifelse(mode %in% c("bike", "foot"), 
                                                  1, 0)) %>% 
        ggplot2::ggplot(ggplot2::aes(x = Income,
                   y = multi_modal,
                   color = purpose)) + 
        binomial_smooth() + 
        ggplot2::theme_bw() + 
        ggplot2::theme(legend.title = ggplot2::element_blank()) + 
        ggplot2::ylab("Probability of Non-Car Transit") + 
        ggplot2::scale_y_continuous(labels = scales::percent) + 
        ggplot2::xlab("Income in 1000's USD")
    })
 
  })
}
    
## To be copied in the UI
# mod_Analysis_ui("Analysis_ui_1")
    
## To be copied in the server
# mod_Analysis_server("Analysis_ui_1")
