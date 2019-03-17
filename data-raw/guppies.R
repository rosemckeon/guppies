# read in data from guppies.csv
guppies <- read.csv("data-raw/guppies.csv")

# Define substrate factors
guppies$Substrate <- factor(guppies$Substrate)
levels(guppies$Substrate) <- c("Sand","Vegetation","Mud")

# re-order predators for better plotting
levels(guppies$Predator)[levels(guppies$Predator)=="NULL"] <- "None"
guppies$Predator <- fct_relevel(
  guppies$Predator, "None", "R.hartii"
)

# remove redundant extra experiments
# first fill in blanks
guppies <- guppies %>% mutate(
  Starting.food = replace(
    Starting.food, # replace with the same column data
    which(is.na(Starting.food)), # but where this is true
    51 # change the value to this
  ),
  Additional.R.hartii = replace(
    Additional.R.hartii,
    which(is.na(Additional.R.hartii)),
    0
  )
)
# store the extra rows elsewhere
guppies_extra <- guppies %>%
  filter(
    Additional.R.hartii == 5 |
    Starting.food == 0
  )
# And remove them from the main data object
guppies <- guppies %>%
  filter(
    Additional.R.hartii == 0 &
    Starting.food == 51
  ) %>%
  select(
    Spot.brightness,
    Day,
    Substrate,
    Predator,
    Predator.number
  )

# convert data to frequency counts
# Spot brightnesses recoded on a sepcific day under specific conditions grouped and counted.This groups the replicates if the days recorded on happenend to be the same. OK as all populations independent of each other.
guppy_counts <- guppies %>%
  group_by(
    Spot.brightness,
    Day,
    Substrate,
    Predator,
    Predator.number
  ) %>%
  summarise(
    Count = length(Spot.brightness)
  )

# generate rda file
usethis::use_data(guppies, overwrite = T)
usethis::use_data(guppies_extra, overwrite = T)
usethis::use_data(guppy_counts, overwrite = T)
