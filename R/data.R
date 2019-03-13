#' Spot brightness of male guppies
#'
#' A dataset containing the spot brightness of male guppies along with other attributes related to the treatment they were exposed to during virtual SimBio experiments.
#'
#' @format A data frame with 10883 rows and 8 variables:
#' \describe{
#'   \item{Spot.brightness}{Numerical scale from 1-20, 20 isthe brightest.}
#'   \item{Day}{Day of experiment}
#'   \item{Substrate}{Substrate in the tank}
#'   \item{Predation}{Logical value, TRUE if guppies were exposed to predators}
#'   \item{Predator.number}{Number of predators added to tank}
#'   \item{Additional.R.hartii}{Number of *R. hartii* added to tank alongside any other predators}
#'   \item{Starting.food}{Number of food items in tank at start of experiment}
#' }
"guppies"
