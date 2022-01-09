#' Create labels for cycling levels
#' 
#' @param hoods sf object containing neighborhood level cycling simulated data
#' @details Creates quantile cycling labels for dummy cycling data
#' @return vector of character labels
get_cycling_labels <- function(hoods){
    
    labels <-  levels(cut(hoods$cycles,
                          breaks = stats::quantile(hoods$cycles,probs = c(0,0.25,0.5,0.75,1)),
                          include.lowest = TRUE))
    
}

purposes <- c("Commuting" = "commute",
              "Grocery Run" = "groceries",
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

get_labels <- function(cycleprop){
    levels(cut(round(cycleprop*100),
               incude.lowest = TRUE,
               breaks = quantile(cycleprop, na.rm = TRUE, 
                                 probs = c(0,0.25,0.5,0.75,1))))
}

get_colour_palette <- function(colourscale, bins = 10){
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
        