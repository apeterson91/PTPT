
get_cycling_labels <- function(hoods){
    
    labels <-  levels(cut(hoods$cycles,
                          breaks = quantile(hoods$cycles,probs = c(0,0.25,0.5,0.75,1)),
                          include.lowest = TRUE))
    
}

purposes <- c("Commuting" = "commute",
              "Grocery Run" = "groceries",
              "Social" = "social")

geographies <- c(
    "Neighborhoods" = "neighborhoods",
    "Streets" ="streets"
)