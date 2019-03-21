#' ---
#' title: "GLM for guppies data"
#' description: "**How the guppy got his spots** (BIOU6BE)"
#' author: "**Rose McKeon** (2417024)"
#' date: "March 15th 2019"
#' ---

#' >Here goes nothing...
#'
#+ setup, echo=F, message=F, warning=F --------
# clear the workspace
rm(list=ls())
# load dependencies
source("setup.R")
library(boot)
library(knitr)
library(kableExtra)
library(tidyverse)
# make sure we have data before continuing
if(!exists("guppy_counts"))
  data("guppy_counts")

if(!exists("guppies"))
  data("guppies")

# Preamble -----------------------------
#' These data include measures of male spot brightness in guppy populations that have been exposed to different environmental conditions, such as substrate and predation.
#'
head(guppies)

#' Data is fairly balanced. Less records as predator number increases due to increased mortality.
summary(guppies$Predator)
summary(as.factor(guppies$Predator.number))

#' In every experiment spot brightness of all male guppies was recorded roughly every 250 days from 0 days to 1500.

#' ## Exploratory plots
roses_set_ggtheme()
ggplot(
  guppies,
  aes(
    x = Day,
    y = Spot.brightness,
    colour = Spot.brightness
  )
) + geom_jitter(
  width = 50,
  height = .25,
  alpha = .4
) + facet_grid(
  Predator.number ~ Predator*Substrate
)
#' Doesn't look like much is going on for substrate but Predator and Predator.number definitely have an effect.
#' We can also see that the relationship between day and spot brightness is inherently asymptote. For every factor when there is no predation we see this asymptotic curve. **A quadratic effect will need to be built into the model** to represent this in addition to any factorial effects on the linear realtionship modelled by standard parameters.

# Business ------------------------------------
#' ## Fitting the GLM
#'
fit <- glm(
  Spot.brightness ~
    Day * Predator * Predator.number * Substrate +
    I(Day^2) * Predator * Predator.number * Substrate,
  data = guppies
  # default family gaussian sufficient
)
glm.diag.plots(fit)
#' **Terrible?**
#' Lot's of points of high leverage in cooks plots. Can probably get this better with simplification as we don't expect substrate to be important. Tails are bad on QQ too.
#'
summary(fit)
#' **Extreme over dispersion.**
#' Very complex too.
#' Dispersion 4.376
#' AIC = 108042

# check for collinearity
vif(fit)
#' None are over 10 but many are over 4. Let's just keep an eye on them for now.

# save this fit for final comparison
fit_1st <- fit

# -----------------------------------------
#' ## Simplification
fit1 <- update(
  fit, ~. -Predator:Predator.number:Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal NOT VALID**
# next
fit1 <- update(
  fit, ~. -I(Day^2):Predator:Predator.number:Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal NOT VALID**
# next
fit1 <- update(
  fit, ~. -I(Day^2):Predator.number:Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -I(Day^2):Predator:Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal NOT VALID**
# next
fit1 <- update(
  fit, ~. -I(Day^2):Predator:Predator.number
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -Predator.number:Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -Predator:Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal NOT VALID**
fit1 <- update(
  fit, ~. -I(Day^2):Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -Predator:Predator.number
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -I(Day^2):Predator.number
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -I(Day^2):Predator
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -Substrate
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -Predator.number
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -Predator
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -I(Day^2)
)
anova(fit, fit1, test = "Chi")
#' **removal VALID**
fit <- fit1
# next
fit1 <- update(
  fit, ~. -Day
)
anova(fit, fit1, test = "Chi")
#' **removal NOT VALID**
# DONE

#---------------------------
#' ## The fitted model
glm.diag.plots(fit)
#' Still lots of high leverage points, but I know the data is accurate. Tails on QQ still bad - maybe worse?
summary(fit)
#' The predictor interactions left make sense in relation to the visual observations. Dispersion and AIC haven't changed, but significance of the parameters has increased across the board.
# check again for collinearity
vif(fit) # URGH (because of Day^2?)

# Predictions ----------------------------------
#' ## Predictions
#'
#' I'll need to create spot brightness predictions for every significant coefficient.
#'
# new x vals to predicts responses for
newx <- seq(
  min(guppies$Day),
  max(guppies$Day),
  .5
)

# R.hartii predictions ---------------------------
#' ### For 2 _R. hartii_ in Sand
newy <- predict(
  fit,
  newdata = list(
    # fixed substrate
    Substrate = factor(rep(
      "Sand", length(newx)
    )),
    # fixed predator
    Predator = factor(rep(
      "R.hartii", length(newx)
    )),
    # fixed predator density
    Predator.number = rep(
      2, length(newx)
    ),
    # fixed days
    Day = newx
  ),
  se = T
)
summary(newy)
#' **These predictions are crazy**

# let's store all the predictions in a useful way
newy$fit.exp <- exp(newy$fit)
# with confidence limits
newy$lower <- exp(
  newy$fit - 1.96*newy$se.fit
)
newy$upper <- exp(
  newy$fit + 1.96*newy$se.fit
)
# in a data frame
newy <- as.data.frame(newy) %>%
  select(fit.exp, lower, upper)

# ready for ggplot facets
predictions <- bind_cols(
  X = newx,
  newy,
  Predator = rep("R.hartii", nrow(newy)),
  Substrate = rep("Sand", nrow(newy)),
  Predator.number = rep(2, nrow(newy))
)

# plot...
plot_guppies <- ggplot(
  data = guppies,
  aes(
    x = Day,
    y = Spot.brightness,
    colour = Spot.brightness
  )
) + geom_jitter(
  width = 50,
  alpha = .4
) + facet_grid(
  Predator.number ~ Predator
)

plot_guppies + geom_line(
  data = predictions,
  mapping = aes(
    x = X,
    y = fit.exp,
    colour = 1
  )
)
