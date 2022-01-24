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

## TODO: make sure [0,-] is included in labels
get_labels <- function(transitprop,num_breaks = 10){
    out <- levels(
        cut(transitprop,
               include.lowest = TRUE,
                right = FALSE,
                na.rm = TRUE,
                breaks = num_breaks
            )
           )
}

zcolorscale <- "RdYlBu"

get_color_palette <- function(colourscale, bins = 10){
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
        