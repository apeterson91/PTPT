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
        tabPanel("Data Use and FAQS",
                 includeMarkdown("inst/app/www/TripEntryAbout.md")
                 ),
        "Background Information",
        tabPanel("Demographics",
                 h3("Demographic Information"),
                 radioButtons(NS(id,"demosex"),
                              "Biological Sex",
                              choices = c("Female","Male"),
                              inline = TRUE,
                              selected = character(),
                              ),
                 radioButtons(NS(id,"demoage"),
                              "Age",
                              choices = c("18-22",
                                          "23-30",
                                          "30-40",
                                          "40-50",
                                          "50-60",
                                          "60-70",
                                          ">70"),
                              inline = TRUE,
                              selected = character(),
                              ),
                 radioButtons(NS(id,"demoedu"),
                              "Highest Level of Education",
                              choices = c(
                                "High School",
                                "Some College/Technical School",
                                "Technical Certification",
                                "Bachelors Degree",
                                "Post-Graduate Education"),
                              selected = character(),
                              ),
                 radioButtons(NS(id,"demorace"),
                              "Race",
                              choices = c(
                                "Asian",
                                "Black",
                                "Other",
                                "White"
                                ),
                              selected = character(),
                              inline = TRUE
                              ),
                 radioButtons(NS(id,"demoinc"),
                              stringr::str_c("Annual Gross Household Income ",
                                             "(Thousands of U.S. Dollars)"),
                              choices = c(
                                "$0-$20",
                                "$21-$40",
                                "$41-$60",
                                "$61-$100",
                                "$100-$150",
                                ">$150"),
                              selected = character()
                              ),
                 numericInput(NS(id,"demohouseholdnum"),
                              label = "Members of Household",
                              value = 1
                              ), 
                 selectInput(NS(id,"subjhood"),
                             "What Neighborhood do you live in?",
                             choices = unique(hoods$hood),
                             selectize = TRUE,
                            )
                 ),
        tabPanel("Transit Background",
                 h3("Transit Background"),
                 checkboxGroupInput(NS(id,"transitown"),
                               label = stringr::str_c("Which of the following ",
                                                      "do you have access to ",
                                                      "use in your household?"),
                               choiceNames = c("Bike",
                                               "Car",
                                               "E-bike or electric scooter"),
                               choiceValues =  c("bike",
                                                 "car",
                                                 "ebike")
                               ),
                 radioButtons(NS(id,"bikefamiliar"),
                              label = c("Do you know how to ride a bike?"),
                              choices = c("Yes","No"),
                              selected = character(),
                              ),
                 radioButtons(NS(id,"busfamiliar"),
                              label = stringr::str_c("Do you ride a Port ",
                                                     "Authority Bus with any ",
                                                     "regularity?"),
                              choices = c("Regularly: at least once / week",
                                          "Often: at least once / month",
                                          "Every now and then: 1 / 4 months",
                                          "Rarely: 1 / year",
                                          "Never"),
                              selected = character()
                              ),
                 radioButtons(NS(id,"lrailfamiliar"),
                              label = stringr::str_c("Do you ride the Port ",
                                                    "Authority Light Rail ",
                                                    "(The 'T') with any ",
                                                    "regularity?"),
                              choices = c("Regularly: at least once / week",
                                          "Often: at least once / month",
                                          "Every now and then: 1 / 4 months",
                                          "Rarely: 1 / year",
                                          "Never"),
                              selected = character()
                              )
                 ),
        "Transit Trips",
        tabPanel("Grocery",
                 selectInput(NS(id,"GroceryTripSelect"),
                             label = "Neighborhood or Street Level Info?",
                             choices = c("Neighborhood",
                                         "Street")
                             )
                 ),
        tabPanel("Commute",
                 selectInput(NS(id,"CommuteTripSelect"),
                             label = "Neighborhood or Street Level Info?",
                             choices = c("Neighborhood",
                                         "Street")
                             )
                 ),
        tabPanel("Social",
                 selectInput(NS(id,"SocialTripSelect"),
                             label = c("Neighborhood or Street Level Info?"),
                             choices = c("Neighborhood",
                                         "Street")
                             )
                 ),
        "Review",
        tabPanel("Review and Submit",
                 tableOutput(NS(id,"submit_table")),
                 textInput(NS(id,"submit_code"),label = "Submission Code"),
                 actionButton("submit",label = "Submit")
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
    df <- reactive({
      dplyr::tibble(Age = input$demoage,
             Sex = input$demosex,
             Education = input$demoedu,
             Race = input$demorace,
             Income = input$demoinc,
             HouseHold = input$householdnum,
             Hood = input$subjhood,
             `Own Car/(e)Bike?` = paste0(input$transitown,collapse = "|"),
             `Bike Familiarity` = input$bikefamiliar,
             `Bus Familiarity` = input$busfamiliar,
             `"T" Familiarity` = input$lrailfamiliar,
             )
    })
    output$submit_table <- renderTable({
      df()
    }, 
    caption = "Respondent Background Information",
    caption.placement = "top"
    )
  })
}
    
## To be copied in the UI
# mod_TripEntry_ui("TripEntry_ui_1")
    
## To be copied in the server
# mod_TripEntry_server("TripEntry_ui_1")
