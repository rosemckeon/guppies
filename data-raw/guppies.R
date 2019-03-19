# read in data from guppies.csv
guppies <- read.csv("data-raw/guppies.csv")

# Define substrate factors
guppies$Substrate <- factor(guppies$Substrate)
levels(guppies$Substrate) <- c("Sand","Vegetation","Mud")

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

# Refactor Predator column so we don't have aliases to cause multicolinearity. Now all the predation "None" rows will be assigned to a predator treatment and only predator number 0 will define them.
# split the dataset
no_predation <- guppies %>%
  filter(
    Predator == "NULL"
  )
predation <- guppies %>%
  filter(
    Predator != "NULL"
  )
# dish out the None's evenly to other factors
no_predation$Predator <- factor(rep(
  c("A.pulchens", "R.hartii", "C.punctata"),
  length.out = nrow(no_predation))
)
# get rid of redundant NULL factor
predation$Predator <- factor(predation$Predator)
# bind back together
guppies <- bind_rows(
  predation, no_predation
)
# re-order so R.hartii (with least effect) goes first
guppies$Predator <- fct_relevel(
  guppies$Predator, "R.hartii"
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
