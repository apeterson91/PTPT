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

mapGeography <- function(geography){
    return()
}