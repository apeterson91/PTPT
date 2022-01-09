#' DisplayMap UI Function
#'
#' @description A shiny Module for presenting the PTPT display map
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import leaflet
mod_DisplayMap_ui <- function(id){
  ns <- NS(id,"DisplayMap")
  tagList(
        div(
            class = "outer",
            tags$link(href="www/custom.css"),
            leafletOutput(ns,width = "100%",height = "100%"),
            absolutePanel(
                id = NS(id,"controls"),
                class = "panel panel-default",
                fixed = TRUE,
                top = 60, left = "auto",
                right = 20, bottom = "auto",
                width = 330, height = "auto",
                #style = "opacity: 0.9 z-index: 1; position: absolute",
                tags$div(title = "Show/Hide Panel",
                         a(
                             id = NS(id,"toggle_panel"),
                             style = "font-size: 80%",
                             span(class = "glyphicon glyphicon-circle-arrow-up",
                                  "Hide")
                         )),
                div(id = NS(id,"input_panel"),
                    tags$div(title = "Trip purpose",
                    selectInput(NS(id,"purpose"), "Trip purpose:", purposes,
                                multiple = TRUE,
                                 selectize = FALSE)),
                tags$div(title = "Geography",
                         selectInput(NS(id,"geography"),"Geography:", geographies,
                                     selectize =  FALSE)),
                tags$div(title = "Transit Mode",
                         checkboxGroupInput(NS(id,"transitmode"),"Mode",
                                            selected = "bike",
                                            choiceNames = list(icon("bicycle"),
                                                               icon("walking"),
                                                               # icon("bus"),
                                                               ## Not yet
                                                               # icon("train"),
                                                               icon("car")),
                                            choiceValues = c("bicycle",
                                                             "walk",
                                                             # "bus",
                                                             # "train",
                                                             "car"),
                                            inline = TRUE
                                            )
                         )
                )
            ),
            )
  )
}
    
#' DisplayMap Server Functions
#'
#' @noRd 
mod_DisplayMap_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$DisplayMap <- renderLeaflet({
        leaflet(hoods) %>%
          addProviderTiles(providers$Stamen.TonerLite,
                           options = providerTileOptions(noWrap = TRUE)) %>% 
        setView(-79.995352,40.440936, zoom = 12)
    })
   
    ## toggle input panel 
    shinyjs::onclick("toggle_panel", 
                     shinyjs::toggle(id = "input_panel",
                                     anim = FALSE))
    
    ## legend
    observe({
      df <- hptt %>%
        dplyr::group_by(hood) %>%
        dplyr::summarise(cycles = mean(cobs))

     leafletProxy("DisplayMap") %>%
        addLegend("topleft",
                  colors = get_color_palette(zcolorscale,4),
                  labels = get_labels(df$cycles),
                  layerId = "hood",
                  title = "% Non-Car Transit to Groceries",
                  opacity = .5)
    })

    ## Map polygons
     observe({
       proxy <- leafletProxy("DisplayMap")
       if(input$geography == "streets"){
         proxy %>% 
           clearShapes() %>% 
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
       }
     })
     
     observe({
       proxy <- leafletProxy("DisplayMap")
       if(input$geography == "neighborhoods"){
         df <- hoods %>%
           dplyr::left_join(hptt %>%
                            dplyr::group_by(hood) %>%
                            dplyr::summarise(cycles = mean(cobs)),
                            by = "hood"
         ) %>%  sf::st_as_sf()
         proxy %>% 
            clearShapes() %>% 
            addPolygons(color = "#444444",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        layerId = ~hood,
                        data = df,
                        options = pathOptions(clickable = T),
                        labelOptions = labelOptions(direction = 'auto'),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 2,
                                                            bringToFront = TRUE),
                        fillColor = ~colorQuantile("RdYlBu",cycles)(cycles)
            )
       }
     })
  })
}