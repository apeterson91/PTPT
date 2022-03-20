#' EducationAnalysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_EducationAnalysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    navlistPanel(
      "Description",
      tabPanel("Big Picture", includeMarkdown("inst/app/www/AnalysisPicture.md")),
      "Analysis",
      tabPanel("Propensity to Transit",
               plotOutput(NS(id,"eduplot"))
               )
    )
  )
}
    
#' EducationAnalysis Server Functions
#'
#' @noRd 
mod_EducationAnalysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    output$eduplot <- renderPlot({
      hptt %>% 
        dplyr::mutate(multi_modal = ifelse(mode %in% c("bike", "foot"), 
                                                  1, 0),
                      BikeInfra = factor(ifelse(BikeInfra>0,1,0),
                                         labels = c("Doesn't know how to ride",
                                                    "Knows how to ride"))  
        ) %>% 
        dplyr::group_by(purpose) %>% 
        tidyr::nest() %>% 
        dplyr::mutate(fit = map(data, function(x) glm(multi_modal ~ BikeInfra + 
                                                        Female, data = x,
                                                      family = binomial()))) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(pars = map(fit,broom::tidy)) %>% 
        tidyr::unnest(pars) %>% 
        dplyr::filter(stringr::str_detect(term,"Bike")) %>% 
        mutate(Lower = exp(estimate - 2*std.error),
               Upper = exp(estimate + 2*std.error),
               estimate = exp(estimate)) %>% 
        ggplot2::ggplot(
          ggplot2::aes(x = purpose,
                   y = estimate,
                   color = purpose)) + 
        ggplot2::theme_bw() + 
        ggplot2::theme(legend.position = 'none') + 
        ggplot2::ylab("Odds of Non-Car Transit") + 
        ggplot2::geom_hline(yintercept = 1, linetype = 2, color = 'red') +
        ggplot2::xlab("") + 
        ggplot2::geom_pointrange(ggplot2::aes(ymin = Lower,
                                              ymax = Upper))
    })
    
  })
}
    
## To be copied in the UI
# mod_EducationAnalysis_ui("EducationAnalysis_ui_1")
    
## To be copied in the server
# mod_EducationAnalysis_server("EducationAnalysis_ui_1")
