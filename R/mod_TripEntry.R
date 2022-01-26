#' TripEntry UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_TripEntry_ui <- function(id){
  ns <- NS(id,"TripEntryPage")
  tagList(
    fluidPage(
      titlePanel("Trip Entry Form"),
      navlistPanel(
        "Preliminary Information",
        tabPanel("Data Release",
                 h3("Data Release Agreement"),
                 checkboxInput("datarelease",
                               "I agree to the terms listed in this 
                               data use and release agreement",
                               value = FALSE
                               )
                 ),
        tabPanel("FAQs",
                 includeMarkdown("inst/app/www/TripEntryAbout.md")
                 ),
        tabPanel("Demographics",
                 h3("Demographic Information"),
                 radioButtons("demosex",
                              "Biological Sex",
                              choices = c("Female","Male"),
                              inline = TRUE
                              ),
                 radioButtons("demoage",
                              "Age",
                              choices = c("18-22",
                                          "23-30",
                                          "30-40",
                                          "40-50",
                                          "50-60",
                                          "60-70",
                                          ">70"),
                              inline = TRUE
                              ),
                 radioButtons("demoedu",
                              "Highest Level of Education",
                              choices = c(
                                "High School",
                                "Some College/Technical School",
                                "Technical Certification",
                                "Bachelors Degree",
                                "Post-Graduate Education")
                              ),
                 radioButtons("demorace",
                              "Race",
                              choices = c(
                                "Asian",
                                "Black",
                                "Other",
                                "White"
                                ),
                              inline = TRUE
                              ),
                 selectInput("subjhood",
                             "What Neighborhood do you live in?",
                             choices = unique(hoods$hood),
                             selectize = TRUE,
                 )
                 ),
        "Trips and Transit",
        tabPanel("Neighborhood"),
        "Review",
        tabPanel("Review and Submit",
                 h3("Review and Submit"),
                 DT::dataTableOutput("record_tbl")
                 )
      )
    )
    )
}
  
#' TripEntry Server Functions
#'
#' @noRd 
mod_TripEntry_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ## TODO: fix the renderTable - not currently showing up
    output$record_tbl <- renderTable(
      data.frame("Sex" = input$demosex,
                 "Age" = input$demoage)
      )
  })
}
    
## To be copied in the UI
# mod_TripEntry_ui("TripEntry_ui_1")
    
## To be copied in the server
# mod_TripEntry_server("TripEntry_ui_1")
