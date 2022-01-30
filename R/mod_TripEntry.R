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
      ##TODO: clean up code with functions
      navlistPanel(
        "Preliminary Information",
        tabPanel("Data Use Agreement",
                 h3("Data Release Agreement"),
                 includeMarkdown("inst/app/www/DataUse.md"),
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
                                         "Street"),
                             selected = "Street"
                             ),
                 tabsetPanel(
                   id = NS(id,"GroceryTripParam"),
                   type = "hidden",
                   tabPanel("Neighborhood",
                            selectInput(NS(id,"GroceryDestHood"),
                                        label = "Grocery Store Neighborhood",
                                        choices = unique(hoods$hood),
                                        selectize = TRUE
                                        )
                            ),
                   tabPanel("Street",
                            leafletOutput(NS(id,"GroceryMap")),
                            renderDataTable(NS(id,"GroceryTable"))
                            )
                 )
                 ),
        tabPanel("Commute",
                 selectInput(NS(id,"CommuteTripSelect"),
                             label = "Neighborhood or Street Level Info?",
                             choices = c("Neighborhood",
                                         "Street")
                             ),
                 tabsetPanel(
                   id = NS(id,"CommuteTripParam"),
                   type = "hidden",
                   tabPanel("Neighborhood",
                            selectInput(NS(id,"CommuteDestHood"),
                                        label = "Grocery Store Neighborhood",
                                        choices = unique(hoods$hood),
                                        selectize = TRUE
                                        )
                            ),
                   tabPanel("Street",
                            leafletOutput(NS(id,"CommuteMap")),
                            )
                 )
                 ),
        tabPanel("Social",
                 selectInput(NS(id,"SocialTripSelect"),
                             label = c("Neighborhood or Street Level Info?"),
                             choices = c("Neighborhood",
                                         "Street")
                             ),
                 tabsetPanel(
                   id = NS(id,"SocialTripParam"),
                   type = "hidden",
                   tabPanel("Neighborhood",
                            selectInput(NS(id,"SocialDestHood"),
                                        label = "Grocery Store Neighborhood",
                                        choices = unique(hoods$hood),
                                        selectize = TRUE
                                        )
                            ),
                   tabPanel("Street",
                            leafletOutput(NS(id,"SocialMap"))
                            )
                 )
                 ),
        "Review",
        tabPanel("Review and Submit",
                 tableOutput(NS(id,"SubmitTable")),
                 textInput(NS(id,"SubmitCode"),label = "Submission Code"),
                 actionButton("SubmitBtn",label = "Submit")
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
    output$SubmitTable <- renderTable({df()}, 
            caption = "Respondent Background Information",
            caption.placement = "top"
            )
    origin_latlong <- reactive({
      if(!is.null(input$subjhood))
        out <- suppressWarnings(hoods %>% 
          dplyr::filter(hood == input$subjhood) %>% 
          sf::st_centroid() %>% 
          sf::st_coordinates())
      else
        out <- c(-79.995352,40.440936)
      return(out)
      })
    stdf <- reactive({
      hds <- hoods %>% 
        sf::st_filter(sf::st_sfc(sf::st_point(origin_latlong())) %>% 
                    sf::st_set_crs(4326),
        .predicate = sf::st_is_within_distance, 
        dist = 1000) %>% 
        dplyr::pull(hood)
      out <- streets %>% 
        dplyr::filter(hood %in% hds) %>% 
        dplyr::mutate(lid = stringr::str_c("street_",street_ix),
                      sid = stringr::str_c("selected_",street_ix))
      return(out)
    })
    # TODO: fix street segment selection - add drag and click if possible
    output$GroceryMap <- renderLeaflet({
        leaflet() %>%
           addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)) %>%
          setView(origin_latlong()[1], origin_latlong()[2], zoom = 15) %>% 
            addPolylines(color = "black",
                         fillColor = "black",
                         weight = 10,
                         opacity = 0.80,
                         group = "template",
                         stroke = TRUE,
                         options = pathOptions(clickable = TRUE),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 2),
                         layerId = ~lid,
                         data = stdf() 
            )
    })
    output$CommuteMap <- renderLeaflet({
        leaflet(hoods) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)) %>% 
          setView(origin_latlong()[1],origin_latlong()[2], zoom = 12) %>% 
            addPolygons(color = "#444444",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 0.25,
                        fillOpacity = 0.25,
                        layerId = ~hood,
                        data = hoods,
                        options = pathOptions(clickable = FALSE),
                        labelOptions = labelOptions(direction = 'auto'),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 2,
                                                            bringToFront = TRUE)
            ) %>% 
            addPolylines(color = "#444444",
                         weight = 1.3,
                         smoothFactor = 0.5,
                         opacity = 0.80,
                         layerId = ~OBJECTID,
                         options = pathOptions(clickable = T),
                         data = streets,
                         labelOptions = labelOptions(direction = 'auto'),
                         highlightOptions =
                           highlightOptions(color = "white",
                                            weight = 2,
                                            bringToFront = TRUE)
            )
    })
    output$SocialMap <- renderLeaflet({
        leaflet(hoods) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)) %>% 
          setView(origin_latlong()[1],origin_latlong()[2], zoom = 12) %>% 
            addPolygons(color = "#444444",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 0.25,
                        fillOpacity = 0.25,
                        layerId = ~hood,
                        data = hoods,
                        options = pathOptions(clickable = FALSE),
                        labelOptions = labelOptions(direction = 'auto'),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 2,
                                                            bringToFront = TRUE)
            ) %>%
            addPolylines(color = "#444444",
                         weight = 2.5, 
                         smoothFactor = 0.5,
                         opacity = 0.80,
                         layerId = ~OBJECTID,
                         options = pathOptions(clickable = T),
                         data = streets,
                         group = "template",
                         labelOptions = labelOptions(direction = 'auto'),
                         highlightOptions =
                           highlightOptions(color = "white",
                                            weight = 2,
                                            bringToFront = TRUE) 
            )
                         
    })
    observeEvent(input$GroceryTripSelect, {
      updateTabsetPanel(inputId = "GroceryTripParam",
                        selected = input$GroceryTripSelect)
    })
    observeEvent(input$CommuteTripSelect, {
      updateTabsetPanel(inputId = "CommuteTripParam",
                        selected = input$CommuteTripSelect)
    })
    observeEvent(input$SocialTripSelect, {
      updateTabsetPanel(inputId = "SocialTripParam",
                        selected = input$SocialTripSelect)
    })
    
    selected <- reactiveValues(ids = vector())
    hltstdf <- reactive({
      stdf() %>% 
        dplyr::filter(lid %in% selected$ids)
    })
    
    observeEvent(input$GroceryMap_shape_click, {
      
      click <- input$GroceryMap_shape_click
      
      proxy <- leafletProxy("GroceryMap")
      
      selected$ids <- c(selected$ids, click$id)
      
      if(click$id %in% hltstdf()$sid){
        
        removeid <- hltstdf() %>% 
          dplyr::filter(sid == click$id) %>% 
          dplyr::pull(lid)
        selected$ids <- setdiff(selected$ids,removeid)
        
        proxy %>% removeShape(layerId = click$id)
        
      } else {
        proxy %>% 
          addPolylines(data = hltstdf(),
                       fillColor = "red",
                       fillOpacity = .8,
                       weight = 15,
                       color = 'red',
                       stroke = TRUE,
                       layerId = ~sid)
      }
      
      })
      
      output$GroceryTable <- DT::renderDataTable(
        dstdf()
      )
      
      observeEvent(input$SubmitBtn,{
        session$sendCustomMessage(type = "testmessage",
                                  message = paste0("Congrats!",
                                  "This button does nothing")
        )
      })
      
      dstdf <- reactive({
       DT::datatable(
         dplyr::tibble(order_ix = 1:n(),
                       OBJECTID = selected$id) %>% 
           dplyr::left_join(streets %>% 
                              st_drop_geometry() %>% 
                              select(OBJECTID,FULL_NAME))
       )
    })
  })
}
    
## To be copied in the UI
# mod_TripEntry_ui("TripEntry_ui_1")
    
## To be copied in the server
# mod_TripEntry_server("TripEntry_ui_1")
