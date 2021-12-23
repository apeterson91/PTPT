#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @noRd
app_server <- function( input, output, session ) {

    output$DisplayMap <- renderLeaflet({
        isolate(
            leaflet(hoods) %>%
                addProviderTiles(providers$Stamen.TonerLite,
                                 options = providerTileOptions(noWrap = TRUE)) %>%
                addPolygons(color = "#444444",
                            weight = 1,
                            smoothFactor = 0.5,
                            opacity = 1.0,
                            fillOpacity = 0.5,
                            layerId = ~hood,
                            options = pathOptions(clickable = T),
                            labelOptions = labelOptions(direction = 'auto'),
                            highlightOptions = highlightOptions(color = "white",
                                                                weight = 2,
                                                                bringToFront = TRUE),
                            fillColor = ~colorQuantile("RdYlBu",cycles)(cycles)
                )
        ) 
    })
}
