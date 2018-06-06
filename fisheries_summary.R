#' Fisheries Summary
#'
#' Takes in a table of catches of different fish species at multiple locations
#' Takes in another table of fish prices
#' Gives the most frequetly caught species at each location, the total revenue at each location, and the overall revenue
#' @param catches is a table of fish counts with rows of fish species and columns of locations
#' @param prices is a table of fish prices
#' @param plot is set to be FALSE by default
#' @author Frank Wang
#' @export

fisheries_summary <- function(catches, prices, plot=FALSE){
  # Return most frequently caught fish in each location
  rownames = rownames(catches)
  revenue = vector(mode = "numeric", length = ncol(catches))
  max_catch = rep("none", ncol(catches))

  for (j in 1:ncol(catches)) {
    a = which.max(catches[,j])
    max_catch[j] = rownames[a]
    revenue[j] = sum(prices[,2]*catches[,j])
  }
  fish_summary <- data.frame(row.names =locs$name,
                             Popular_species = max_catch,
                             Location_Revenue = revenue,
                             Total_Revenue = sum(revenue))

  if (plot) {
    lb = sprintf("The total revenue is %d dollars", sum(revenue))
    p = ggplot(data = as.data.frame(fish_summary), aes(locs$name, Location_Revenue, fill=locs$name))+geom_col()+
      labs(x = "Locations", y="revenue in dollars")+annotate("text", x=2, y=1000000, label=lb, col="red")
  }
  else p=NULL

  return(fish_summary)
}
