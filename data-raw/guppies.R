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

# generate rda file
usethis::use_data(guppies, overwrite = T)
# load with:
# load("data/guppies.rda")
