
#' weighted mean and weighted standard deviation
#'
#'
#' @param vector scale mean response in columns for each social group
#' @param vector percentage of each social group in the people
#' @param vector percentage of each social group in the sample
#'
#' @return the weighted mean and weighted standard deviation
#' @export
#' @examples
#' 1+1
#' group.mean <- c(10,23,15,16)
#' pop_perc <- c(0.25,0.19,0.5,0.45)
#' group_perc <- c(0.10,0.25,0.12)
#' weighted.mean2(group.mean,pop_perc,group_perc)
#'
#
weighted.mean2 <- function(group.mean, pourcent_pop, pourcent_gp) {

  # variable

  weight <- pourcent_pop/pourcent_gp
  N <- length(weight[weight != 0])

  # mean

  weighted_mean <- sum(group.mean * weight)/(sum(weight))

  # standard deviation

  weighted_standard_deviation <- sqrt(sum(weight * (group.mean - weighted_mean) ^ 2)
                                      / (((N - 1) / N) * sum(group.mean)))

  return(
    list("weighted mean" = weighted_mean,
         "weighted standard deviation " = weighted_standard_deviation
    )
  )
}


