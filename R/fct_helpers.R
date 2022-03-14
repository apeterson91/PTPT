#' Create labels for cycling levels
#' 
#' @param hoods sf object containing neighborhood level cycling simulated data
#' @details Creates quantile cycling labels for dummy cycling data
#' @return vector of character labels
purposes <- c("Commute" = "commute",
              "Groceries" = "groceries",
              "Social" = "social")

geographies <- c(
    "Neighborhoods" = "neighborhoods",
    "Streets" ="streets"
)

modes <- c(
    "Bike+" = "bike",
    "Bus" = "bus",
    "T" = "t",
    "Car" = "car"
)

showHoodPopup <- function(df, id, lat, lng){
    transit <- df %>% 
        dplyr::filter(hood == {{id}}) %>%
        dplyr::pull(transit)
    transit_string <- stringr::str_c("% Transit: ", transit)
    content <- as.character(tagList(
        tags$h4(id),
        tags$strong(transit_string)
    ))
    leafletProxy("DisplayMap") %>% 
        addPopups(lat = lat,
                  lng = lng,
                  content,
                  layerId = id)
    
}

getLabels <- function(transitprop){
    out <-c("0-1 %","2-3 %","4-6 %","7-9 %",
            "10-14 %","15-19 %","20-24 %",
            "25-29 %","30-39 %","40 %+") 
}

placeLabels <- function(transitprop){
    lbls <-c("0-1 %","2-3 %","4-6 %","7-9 %",
            "10-14 %","15-19 %","20-24 %",
            "25-29 %","30-39 %","40 %+") 
    transitprop <- round(transitprop)
    out <- dplyr::case_when(transitprop <= 1 ~ lbls[1],
              transitprop > 1 & transitprop <= 3 ~ lbls[2],
              transitprop > 3 & transitprop <= 6 ~ lbls[3],
              transitprop > 5 & transitprop <= 9 ~ lbls[4],
              transitprop > 9 & transitprop <= 14 ~ lbls[5],
              transitprop > 14 & transitprop <= 19 ~ lbls[6],
              transitprop > 19 & transitprop <= 24 ~ lbls[7],
              transitprop > 24 & transitprop <= 29 ~ lbls[8],
              transitprop > 29 & transitprop <= 39 ~ lbls[9],
              TRUE ~ lbls[10],
              )
    out <- factor(out, levels = lbls)
    return(out)
}

zcolorscale <- "RdYlBu"

getColorPalette <- function(colourscale, bins = 10){
    # Manually modify to be 'standard 10 plus one extra' for 11 levels
    if (colourscale == "RdYlBu" && bins == 11) {
        local_palette <- RColorBrewer::brewer.pal(n = 10, name = colourscale)
        extra_colour <- "#2d004b"
        local_palette <- append(local_palette, extra_colour)
    } else {
        local_palette <- RColorBrewer::brewer.pal(n = bins, name = colourscale)
    }
    # Replace #e0f3f8 with #c6dbef for colourbrewer "RdYlBu"
    if (colourscale == "RdYlBu") {
        local_palette <- gsub(pattern = "#E0F3F8", replacement = "#C6DBEF", x = local_palette)
    }
    local_palette
}
        