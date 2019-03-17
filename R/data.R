#' # Spot brightness of male guppies
#'
#' ## guppies
#'
#' A dataset containing the spot brightness of male guppies along with other attributes related to the treatment they were exposed to during virtual SimBio experiments. Each row represents one male guppy.
#'
#' @format A data frame with 25036 objects of 5 variables.
#' \describe{
#'   \item{Spot.brightness}{Number: scale from 1-20 (dull to bright)}
#'   \item{Day}{Number: Day of experiment}
#'   \item{Substrate}{Factor: substrate in the tank}
#'   \item{Predator}{Factor: predator species added to tank}
#'   \item{Predator.number}{Number: predators added to tank}
#' }
"guppies"

#' ## guppies_extra
#'
#' Additional conditions beyond the main experiment. Each row represents one male guppy.
#'
#' @format A data frame with 2849 rows and 7 variables:
#' \describe{
#'   \item{Spot.brightness}{Number: scale from 1-20 (dull to bright)}
#'   \item{Day}{Number: Day of experiment}
#'   \item{Substrate}{Factor: substrate in the tank}
#'   \item{Predator}{Factor: predator species added to tank}
#'   \item{Predator.number}{Number: predators added to tank}
#'   \item{Additional.R.hartii}{Number: *R. hartii* added to tank alongside any other predators}
#'   \item{Starting.food}{Number: food items in tank at start of experiment}
#' }
"guppies_extra"


#' ## guppy_counts
#'
#' Guppies data converted into frequency counts, grouped by other variables. Each row represents a number of male guppies with the same spot brightness from a populations with the same conditions, on the same day of the experiment.
#'
#' @format A data frame with 3186 objects of 6 variables.
#' \describe{
#'   \item{Spot.brightness}{Number: scale from 1-20 (dull to bright)}
#'   \item{Day}{Number: Day of experiment}
#'   \item{Substrate}{Factor: substrate in the tank}
#'   \item{Predator}{Factor: predator species added to tank}
#'   \item{Predator.number}{Number: predators added to tank}
#'   \item{Count}{Number: Number of guppies recorded with this spot brightness under these conditions on this day.}
#' }
"guppies"
