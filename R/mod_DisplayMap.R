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
                ## TODO: have check so one mode of transit is always selected.
                div(id = NS(id,"input_panel"),
                    tags$div(title = "Trip purpose",
                    selectInput(NS(id,"transitpurpose"), "Trip purpose:", purposes,
                                selected = "commute",
                                multiple = TRUE,
                                 selectize = FALSE)),
                ## TODO: determine how to use street/ transit lines going 
                ## forward
                # tags$div(title = "Geography",
                #          selectInput(NS(id,"geography"),
                #                      "Geography:",
                #                      geographies,
                #                      selectize =  FALSE)),
                tags$div(title = "Transit Mode",
                         checkboxGroupInput(NS(id,"transitmode"),
                                            "Mode",
                                            selected = c("bike","foot"),
                                            choiceNames = list(icon("bicycle"),
                                                               ## Not yet
                                                               # icon("bolt"),
                                                               icon("walking"),
                                                               ## Not yet
                                                               # icon("bus"),
                                                               ## Not yet
                                                               # icon("train"),
                                                               icon("car")),
                                            choiceValues = c("bike",
                                                               ## Not yet
                                                             # "ebike",
                                                             "foot",
                                                             # "bus",
                                                             # "train",
                                                             "car"
                                                             ),
                                            inline = TRUE
                                            )
                )
            ),
            )
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
      denom <- hptt %>% 
        dplyr::filter(stringr::str_detect(purpose,
                                         paste(input$transitpurpose,
                                               collapse = "|"))) %>% 
        dplyr::group_by(origin_hood) %>% 
        dplyr::count(name = "hood_denom") %>% 
        dplyr::ungroup()
      
      df <- hptt %>%
        dplyr::filter(stringr::str_detect(mode,
                                          paste(input$transitmode,
                                                collapse = "|")),
                      stringr::str_detect(purpose,
                                          paste(input$transitpurpose,
                                                collapse = "|"))) %>%
        dplyr::distinct(origin_hood, subject_hood_id) %>%
        dplyr::left_join(denom, by = "origin_hood") %>% 
        dplyr::group_by(origin_hood) %>%
        dplyr::summarise(transit = round(100 * dplyr::n() / hood_denom , 2)) %>%
        dplyr::ungroup() %>% 
        dplyr::rename(hood = origin_hood)
     leafletProxy("DisplayMap") %>%
        addLegend("topleft",
                  colors = getColorPalette(zcolorscale,10),
                  labels = getLabels(prop_hood()$transit),
                  layerId = "hood",
                  title = "% Transit",
                  opacity = .5)
    })
    
    # hood popup
    observe({
      event <- input$DisplayMap_shape_click
      if(is.null(event))
        return()
      leafletProxy("DisplayMap") %>%
        clearPopups() %>% 
        flyTo(lng = event$lng,
              lat = event$lat,
              zoom = 15)
      showHoodPopup(prop_hood(), event$id, event$lat, event$lng)
    })

    ## Map polygons
     observe({
       proxy <- leafletProxy("DisplayMap")
       # if(input$geography == "streets"){
       #   proxy %>% 
       #     clearShapes() %>% 
       #      addPolygons(color = "#444444",
       #                  weight = 1,
       #                  smoothFactor = 0.5,
       #                  opacity = 0.25,
       #                  fillOpacity = 0.25,
       #                  layerId = ~hood,
       #                  data = hoods,
       #                  options = pathOptions(clickable = FALSE),
       #                  labelOptions = labelOptions(direction = 'auto'),
       #                  highlightOptions = highlightOptions(color = "white",
       #                                                      weight = 2,
       #                                                      bringToFront = TRUE)
       #      ) %>% 
       #      addPolylines(color = "#444444",
       #                   weight = 1.3,
       #                   smoothFactor = 0.5,
       #                   opacity = 0.80,
       #                   layerId = ~OBJECTID,
       #                   options = pathOptions(clickable = T),
       #                   data = streets,
       #                   labelOptions = labelOptions(direction = 'auto'),
       #                   highlightOptions =
       #                     highlightOptions(color = "white",
       #                                      weight = 2,
       #                                      bringToFront = TRUE)
       #      )
       # }
     })
     
     prop_hood <-  reactive({
         denom <- hptt %>% 
           dplyr::filter(stringr::str_detect(purpose,
                                             paste(input$transitpurpose,
                                                   collapse = "|"))) %>% 
           dplyr::group_by(origin_hood) %>% 
           dplyr::count(name = "hood_denom") %>% 
           dplyr::ungroup() %>% 
           dplyr::rename(hood = origin_hood)
         
         df <- hoods %>% 
           dplyr::select(hood) %>% 
           dplyr::left_join(hptt %>%
                            dplyr::rename(hood = origin_hood) %>% 
                            dplyr::filter(stringr::str_detect(mode,
                                                       paste(input$transitmode,
                                                             collapse = "|")),
                                          stringr::str_detect(purpose,
                                                paste(input$transitpurpose,
                                                      collapse = "|"))) %>%
                            dplyr::distinct(hood, subject_hood_id) %>%
                            dplyr::group_by(hood) %>%
                            dplyr::count() %>% 
                            dplyr::left_join(denom, by = "hood") %>% 
                            dplyr::summarize(transit = round(
                              100 * (n / hood_denom), 2 
                              )
                            ) %>% 
                            dplyr::ungroup(),
                            by = "hood"
           ) %>%
           dplyr::mutate(transit = tidyr::replace_na(transit, 0),
                         transit_labels = placeLabels(transit)) %>%
           sf::st_as_sf()
         
         return(df)
     })
     
     observe({
       proxy <- leafletProxy("DisplayMap")
       # if(input$geography == "neighborhoods"){
         proxy %>% 
            clearShapes() %>% 
            addPolygons(color = "#444444",
                        weight = 1,
                        smoothFactor = 0.5,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        layerId = ~hood,
                        data = prop_hood(),
                        options = pathOptions(clickable = T),
                        labelOptions = labelOptions(direction = 'auto'),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 2,
                                                            bringToFront = TRUE),
                        ## TODO what to do when breaks are not unique?
                        fillColor = ~colorFactor("RdYlBu",
                                                 transit_labels)(transit_labels)
            )
       # }
     })
  })
}