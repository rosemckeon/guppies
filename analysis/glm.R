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
#' These data include frequency measures of male spot brightness in guppy populations that have been exposed to different environmental conditions, such as substrate and predation.
#'
head(guppy_counts)

#' Condensing the records into counts has made the data a little less balanced, but still adequate.
summary(guppies$Predator)
summary(as.factor(guppies$Predator.number))
summary(guppy_counts$Predator)
summary(as.factor(guppy_counts$Predator.number))

#' In every experiment spot brightness of all male guppies was recorded roughly every 250 days from 0 days to 1500.

#' ## Exploratory plots
qplot(
  Day, Count, data = guppy_counts,
  geom = "point",
  colour = Spot.brightness
)
#' There is definitely an effect of predator. There are far fewer bright spots recorded for some species especially.
#'
ggplot(
  guppy_counts,
  aes(
    x = Day,
    y = Count,
    colour = Spot.brightness
  )
) + geom_jitter(
  width = 50,
  alpha = .4
) + facet_wrap(
  ~ Predator
)

#' There appears to be a similar relationship with predator number too. Though less strong. Perhaps due to records of R.hartii keeping bright spot records at higher density.
#'
ggplot(
  guppy_counts,
  aes(
    x = Day,
    y = Count,
    colour = Spot.brightness
  )
) + geom_jitter(
  width = 50,
  alpha = .4
) + facet_wrap(
  ~ Predator.number
)

ggplot(
  guppy_counts,
  aes(
    x = Day,
    y = Count,
    colour = Spot.brightness,
    size = Count
  )
) + geom_jitter(
  width = 50,
  alpha = .4
) + facet_grid(
  Predator.number ~ Predator
)

#' Not the best plot layout but we can see what's happening really nicely. How about if we look at substrate too?
ggplot(
  guppy_counts,
  aes(
    x = Day,
    y = Count,
    colour = Spot.brightness
  )
) + geom_jitter(
  width = 50,
  alpha = .4
) + facet_grid(
  Predator.number ~ Predator*Substrate
)
#' **Doesn't look like there is much going on for substrate.**
ggplot(
  guppy_counts,
  aes(
    x = Day,
    y = Spot.brightness,
    colour = Spot.brightness,
    size = Count
  )
) + geom_jitter(
  width = 50,
  height = .25,
  alpha = .4
) + facet_grid(
  Predator.number ~ Predator
)
#' We can still plot the relationship this way around too, but **I'm not sure which is better**. This way shows the spread of data and the rise and fall is easy to read. But showing count on the Y axis makes the subtle differences for R.hartii more easy to read.


# Business ------------------------------------
#' ## Fitting the GLM
#'
fit1 <- glm(
  Count ~ Day*Spot.brightness*Predator*Predator.number*Substrate,
  data = guppy_counts,
  # we're interested in frequencies/counts
  family = "poisson"
)
glm.diag.plots(fit1)
#' **Terrible**
#' Lot's of points of high leverage in cooks plots and tails well off the line for residuals. We can probably get this better with simplification as we don't expect substrate to be important.
#'
summary(fit1)
#' **Extreme over dispersion.**
#' Very complex too. Let's see if we can just take Substrate out all together as it seems not to do much. Plots didn't show a clear relationship and the significant interactions don't include substrate in the summary.
#'
fit2 <- glm(
  Count ~ Day*Spot.brightness*Predator*Predator.number,
  data = guppy_counts,
  family = "poisson"
)
glm.diag.plots(fit2) # not much better
summary(fit2) # still huge over dispersion, but more significant coeffs.
anova(fit1, fit2, test = "Chi")
#' **Removal not valid.**
#'
#' OK. We'll have to take this slower. Let's just take out the interaction, but keep the main effect.
#'
# update method not working.
fit3 <- glm(
  Count ~ Day*Spot.brightness*Predator*Predator.number+Substrate,
  data = guppy_counts,
  family = "poisson"
)
anova(fit1, fit3, test = "Chi")
#' **Removal not valid :(** Will we be able to take out anything?
#'
fit4 <- glm(
  Count ~ Day*Spot.brightness*Predator+Predator.number*Substrate,
  data = guppy_counts,
  family = "poisson"
)
anova(fit1, fit4, test = "Chi")
#'**Nope**.
fit5 <- glm(
  Count ~ Day*Spot.brightness+Predator*Predator.number*Substrate,
  data = guppy_counts,
  family = "poisson"
)
anova(fit1, fit5, test = "Chi")
#'**Nope**.
fit6 <- glm(
  Count ~ Day+Spot.brightness*Predator*Predator.number*Substrate,
  data = guppy_counts,
  family = "poisson"
)
anova(fit1, fit6, test = "Chi")
#'**Nope**.
#' > None of the interactions can be removed.
#'
#' ## Can we do something about over dispersion?
#' Let's try quasipoisson
#'

fit7 <- glm(
  Count ~ Day*Spot.brightness*Predator*Predator.number*Substrate,
  data = guppy_counts,
  # let's try and account for overdispersion
  family = "quasipoisson"
)
# dianostics
glm.diag.plots(fit7) # Still not great
summary(fit7) # overdispersion estimate = 4.069
#' **The variance is ~4 times larger than the mean.**
#'
# I was missing the comma! damn. OK let's try one more time to simplify.
fit8 <- update(fit7, ~. -Day:Substrate)
anova(fit7, fit8, test = "Chi")
#' **Removal VALID**
fit9 <- update(fit8, ~. -Spot.brightness:Substrate)
anova(fit8, fit9, test = "Chi")
#' **Removal VALID**
fit10 <- update(fit9, ~. -Predator:Substrate)
anova(fit9, fit10, test = "Chi")
#' **Removal VALID**
fit11 <- update(fit10, ~. -Predator.number:Substrate)
anova(fit10, fit11, test = "Chi")
#' **Removal VALID**
fit12 <- update(
  fit11,
  ~. -Day:Spot.brightness:Predator:Predator.number:Substrate
)
anova(fit11, fit12, test = "Chi")
#' **Removal VALID**
fit13 <- update(
  fit12,
  ~. -Spot.brightness:Predator:Predator.number:Substrate
)
anova(fit12, fit13, test = "Chi")
#' **Removal VALID**
fit14 <- update(
  fit13,
  ~. -Predator:Predator.number:Substrate
)
anova(fit13, fit14, test = "Chi")
#' **Removal VALID**
fit15 <- update(
  fit14,
  ~. -Day:Predator:Predator.number:Substrate
)
anova(fit14, fit15, test = "Chi")
#' **Removal VALID**
fit16 <- update(
  fit15,
  ~. -Day:Spot.brightness:Predator.number:Substrate
)
anova(fit15, fit16, test = "Chi")
#' **Removal VALID**
fit17 <- update(
  fit16,
  ~. -Day:Spot.brightness:Predator:Substrate
)
anova(fit16, fit17, test = "Chi")
#' **Removal VALID**
fit18 <- update(
  fit17,
  ~. -Day:Spot.brightness:Predator:Predator.number
)
anova(fit17, fit18, test = "Chi")
#' **Removal VALID**
fit19 <- update(
  fit18,
  ~. -Spot.brightness:Predator.number:Substrate
)
anova(fit18, fit19, test = "Chi")
#' **Removal VALID**
fit20 <- update(
  fit19,
  ~. -Day:Predator.number:Substrate
)
anova(fit19, fit20, test = "Chi")
#' **Removal VALID**
fit21 <- update(
  fit20,
  ~. -Spot.brightness:Predator:Substrate
)
anova(fit20, fit21, test = "Chi")
#' **Removal VALID**
fit22 <- update(
  fit21,
  ~. -Day:Predator:Substrate
)
anova(fit21, fit22, test = "Chi")
#' **Removal NOT VALID**
fit23 <- update(
  fit21,
  ~. -Day:Spot.brightness:Substrate
)
anova(fit21, fit23, test = "Chi")
#' **Removal VALID**
fit24 <- update(
  fit23,
  ~. -Spot.brightness:Predator:Predator.number
)
anova(fit23, fit24, test = "Chi")
#' **Removal VALID**
fit25 <- update(
  fit24,
  ~. -Day:Predator:Predator.number
)
anova(fit24, fit25, test = "Chi")
#' **Removal VALID**
fit26 <- update(
  fit25,
  ~. -Day:Spot.brightness:Predator.number
)
anova(fit25, fit26, test = "Chi")
#' **Removal VALID**
fit27 <- update(
  fit26,
  ~. -Day:Spot.brightness:Predator
)
anova(fit26, fit27, test = "Chi")
#' **Removal NOT VALID**
fit28 <- update(
  fit26,
  ~. -Predator:Predator.number
)
anova(fit26, fit28, test = "Chi")
#' **Removal NOT VALID**
fit29 <- update(
  fit26,
  ~. -Spot.brightness:Predator.number
)
anova(fit26, fit29, test = "Chi")
#' **Removal NOT VALID**
fit30 <- update(
  fit26,
  ~. -Day:Predator.number
)
anova(fit26, fit30, test = "Chi")
#' **Removal VALID**
#' Things are looking much simpler now, let's do a summary check and see how it's going...
summary(fit30)
#' Getting there, lots of highly significant coeffs, but still some that look removeable.
fit31 <- update(
  fit30,
  ~. -Spot.brightness:Predator
)
anova(fit30, fit31, test = "Chi")
#' **Removal NOT VALID**
fit32 <- update(
  fit30,
  ~. -Day:Predator
)
anova(fit30, fit32, test = "Chi")
#' **Removal NOT VALID**
fit33 <- update(
  fit30,
  ~. -Day:Spot.brightness
)
anova(fit30, fit33, test = "Chi")
#' **Removal VALID**
#' This just leaves the main effects which we probably can't remove. But let's make sure...
fit34 <- update(
  fit33,
  ~. -Substrate
)
anova(fit33, fit34, test = "Chi")
#' **Removal VALID**
fit35 <- update(
  fit34,
  ~. -Predator.number
)
anova(fit34, fit35, test = "Chi")
#' **Removal VALID.**
#' **Now I'm Dubious, BUT...**
#' We know there is definitely an effect for Predator number, but maybe its only when there's an interaction with Predator species? That's still in the model and preliminary plots do show that the effect of number is very different depending on the species.
summary(fit34)
#' Predator.number wasn't significant either. Let's roll with it.
summary(fit35)
#' Spot.brightness looks like the last worthwhile check
fit36 <- update(
  fit35,
  ~. -Spot.brightness
)
anova(fit35, fit36, test = "Chi")
#' **Removal VALID**
summary(fit36)
#' Let's make sure we can't take out the last few.
fit37 <- update(
  fit36,
  ~. -Predator
)
anova(fit36, fit37, test = "Chi")
#' **Removal NOT VALID**
fit38 <- update(
  fit36,
  ~. -Day
)
anova(fit36, fit38, test = "Chi")
#' **Removal VALID**
#' **Interesting. Day had a significant coeff in summary but can be removed.** It remains in some interactions so let's trust that simplest is best.
#'
#' ## The fitted model
#'
fit <- fit38
# some regex here to clear all the old fits would be nice
glm.diag.plots(fit)
#' Really still not the cleanest diagnsotics. Lot's of high leverage points, but I know all the data is accurate.
summary(fit)
#' Overdispersion reduced a little but still not great. Still no AIC score either. I've done everything I know how to now. I think that's the best I can get it.

# Predictions ----------------------------------
#' ## Predictions
#'
#' I'll need to create predictions for each predator by density to show all the relationships we could see on the initial scatter plots.
#' **Do i need to separate predictions for each spot brightness and substrate too!?**
#'
# new x vals to predicts responses for
newx <- seq(
  min(guppy_counts$Day),
  max(guppy_counts$Day),
  .5
)
# Factors to sample from
possible_spot.brightness <- 1:20
possible_substrate <- c("Sand", "Vegetation", "Mud")

# R.hartii predictions ---------------------------
#' ### For 2 _R. hartii_
newy_R.hartii_2 <- predict(
  fit,
  newdata = list(
    # random sample of spot brightness
    # Spot.brightness = sample(
    #   possible_spot.brightness,
    #   size = length(newx),
    #   replace = T
    # ),
    # random sample of substrate
    # Substrate = sample(
    #   possible_substrate,
    #   size = length(newx),
    #   replace = T
    # ),
    # fixed spot brightness
    Spot.brightness = rep(
      18, length(newx)
    ),
    # fixed substrate
    Substrate = rep(
      "Sand", length(newx)
    ),
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
# let's store all the predictions in a useful way
newy_R.hartii_2$fit.exp <- exp(newy_R.hartii_2$fit)
# with confidence limits
newy_R.hartii_2$lower <- exp(
  newy_R.hartii_2$fit - 1.96*newy_R.hartii_2$se.fit
)
newy_R.hartii_2$upper <- exp(
  newy_R.hartii_2$fit + 1.96*newy_R.hartii_2$se.fit
)
# in a data frame
newy_R.hartii_2 <- as.data.frame(newy_R.hartii_2) %>%
  select(fit.exp, lower, upper)
#' **May need to add columns for the factors to get this working in ggplot**.
#'


#' ### For 5 _R. hartii_
newy_R.hartii_5 <- predict(
  fit,
  newdata = list(
    # random sample of spot brightness
    Spot.brightness = sample(
      possible_spot.brightness,
      size = length(newx),
      replace = T
    ),
    # random sample of substrate
    Substrate = sample(
      possible_substrate,
      size = length(newx),
      replace = T
    ),
    # fixed predator
    Predator = factor(rep(
      "R.hartii", length(newx)
    )),
    # fixed predator density
    Predator.number = rep(
      5, length(newx)
    ),
    # fixed days
    Day = newx
  ),
  se = T
)
# let's store all the predictions in a useful way
newy_R.hartii_5$fit.exp <- exp(newy_R.hartii_5$fit)
# with confidence limits
newy_R.hartii_5$lower <- exp(
  newy_R.hartii_5$fit - 1.96*newy_R.hartii_5$se.fit
)
newy_R.hartii_5$upper <- exp(
  newy_R.hartii_5$fit + 1.96*newy_R.hartii_5$se.fit
)
# in a data frame
newy_R.hartii_5 <- as.data.frame(newy_R.hartii_5) %>%
  select(fit.exp, lower, upper)


#' ### For 10 _R. hartii_
newy_R.hartii_10 <- predict(
  fit,
  newdata = list(
    # random sample of spot brightness
    Spot.brightness = sample(
      possible_spot.brightness,
      size = length(newx),
      replace = T
    ),
    # random sample of substrate
    Substrate = sample(
      possible_substrate,
      size = length(newx),
      replace = T
    ),
    # fixed predator
    Predator = factor(rep(
      "R.hartii", length(newx)
    )),
    # fixed predator density
    Predator.number = rep(
      10, length(newx)
    ),
    # fixed days
    Day = newx
  ),
  se = T
)
# let's store all the predictions in a useful way
newy_R.hartii_10$fit.exp <- exp(newy_R.hartii_10$fit)
# with confidence limits
newy_R.hartii_10$lower <- exp(
  newy_R.hartii_10$fit - 1.96*newy_R.hartii_10$se.fit
)
newy_R.hartii_2$upper <- exp(
  newy_R.hartii_10$fit + 1.96*newy_R.hartii_10$se.fit
)
# in a data frame
newy_R.hartii_10 <- as.data.frame(newy_R.hartii_10) %>%
  select(fit.exp, lower, upper)

#' # A.pulchens predictions ---------------------------
#' #' ### For 2 _A. pulchens_
#' newy_A.pulchens_2 <- predict(
#'   fit,
#'   newdata = list(
#'     # random sample of spot brightness
#'     Spot.brightness = sample(
#'       possible_spot.brightness,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # random sample of substrate
#'     Substrate = sample(
#'       possible_substrate,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # fixed predator
#'     Predator = factor(rep(
#'       "A.pulchens", length(newx)
#'     )),
#'     # fixed predator density
#'     Predator.number = rep(
#'       2, length(newx)
#'     ),
#'     # fixed days
#'     Day = newx
#'   ),
#'   se = T
#' )
#' # let's store all the predictions in a useful way
#' newy_A.pulchens_2$fit.exp <- exp(newy_A.pulchens_2$fit)
#' # with confidence limits
#' newy_A.pulchens_2$lower <- exp(
#'   newy_A.pulchens_2$fit - 1.96*newy_A.pulchens_2$se.fit
#' )
#' newy_A.pulchens_2$upper <- exp(
#'   newy_A.pulchens_2$fit + 1.96*newy_A.pulchens_2$se.fit
#' )
#' # in a data frame
#' newy_A.pulchens_2 <- as.data.frame(newy_A.pulchens_2) %>%
#'   select(fit.exp, lower, upper)
#' #' **May need to add columns for the factors to get this working in ggplot**.
#' #'
#'
#'
#' #' ### For 5 _A. pulchens_
#' newy_A.pulchens_5 <- predict(
#'   fit,
#'   newdata = list(
#'     # random sample of spot brightness
#'     Spot.brightness = sample(
#'       possible_spot.brightness,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # random sample of substrate
#'     Substrate = sample(
#'       possible_substrate,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # fixed predator
#'     Predator = factor(rep(
#'       "A.pulchens", length(newx)
#'     )),
#'     # fixed predator density
#'     Predator.number = rep(
#'       5, length(newx)
#'     ),
#'     # fixed days
#'     Day = newx
#'   ),
#'   se = T
#' )
#' # let's store all the predictions in a useful way
#' newy_A.pulchens_5$fit.exp <- exp(newy_A.pulchens_5$fit)
#' # with confidence limits
#' newy_A.pulchens_5$lower <- exp(
#'   newy_A.pulchens_5$fit - 1.96*newy_A.pulchens_5$se.fit
#' )
#' newy_A.pulchens_5$upper <- exp(
#'   newy_A.pulchens_5$fit + 1.96*newy_A.pulchens_5$se.fit
#' )
#' # in a data frame
#' newy_A.pulchens_5 <- as.data.frame(newy_A.pulchens_5) %>%
#'   select(fit.exp, lower, upper)
#'
#'
#' #' ### For 10 _A. pulchens_
#' newy_A.pulchens_10 <- predict(
#'   fit,
#'   newdata = list(
#'     # random sample of spot brightness
#'     Spot.brightness = sample(
#'       possible_spot.brightness,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # random sample of substrate
#'     Substrate = sample(
#'       possible_substrate,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # fixed predator
#'     Predator = factor(rep(
#'       "A.pulchens", length(newx)
#'     )),
#'     # fixed predator density
#'     Predator.number = rep(
#'       10, length(newx)
#'     ),
#'     # fixed days
#'     Day = newx
#'   ),
#'   se = T
#' )
#' # let's store all the predictions in a useful way
#' newy_A.pulchens_10$fit.exp <- exp(newy_A.pulchens_10$fit)
#' # with confidence limits
#' newy_A.pulchens_10$lower <- exp(
#'   newy_A.pulchens_10$fit - 1.96*newy_A.pulchens_10$se.fit
#' )
#' newy_A.pulchens_2$upper <- exp(
#'   newy_A.pulchens_10$fit + 1.96*newy_A.pulchens_10$se.fit
#' )
#' # in a data frame
#' newy_A.pulchens_10 <- as.data.frame(newy_A.pulchens_10) %>%
#'   select(fit.exp, lower, upper)
#'
#'
#' # C.punctata predictions ---------------------------
#' #' ### For 2 _C. punctata_
#' newy_C.punctata_2 <- predict(
#'   fit,
#'   newdata = list(
#'     # random sample of spot brightness
#'     Spot.brightness = sample(
#'       possible_spot.brightness,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # random sample of substrate
#'     Substrate = sample(
#'       possible_substrate,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # fixed predator
#'     Predator = factor(rep(
#'       "C.punctata", length(newx)
#'     )),
#'     # fixed predator density
#'     Predator.number = rep(
#'       2, length(newx)
#'     ),
#'     # fixed days
#'     Day = newx
#'   ),
#'   se = T
#' )
#' # let's store all the predictions in a useful way
#' newy_C.punctata_2$fit.exp <- exp(newy_C.punctata_2$fit)
#' # with confidence limits
#' newy_C.punctata_2$lower <- exp(
#'   newy_C.punctata_2$fit - 1.96*newy_C.punctata_2$se.fit
#' )
#' newy_C.punctata_2$upper <- exp(
#'   newy_C.punctata_2$fit + 1.96*newy_C.punctata_2$se.fit
#' )
#' # in a data frame
#' newy_C.punctata_2 <- as.data.frame(newy_C.punctata_2) %>%
#'   select(fit.exp, lower, upper)
#' #' **May need to add columns for the factors to get this working in ggplot**.
#' #'
#'
#'
#' #' ### For 5 _C. punctata_
#' newy_C.punctata_5 <- predict(
#'   fit,
#'   newdata = list(
#'     # random sample of spot brightness
#'     Spot.brightness = sample(
#'       possible_spot.brightness,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # random sample of substrate
#'     Substrate = sample(
#'       possible_substrate,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # fixed predator
#'     Predator = factor(rep(
#'       "C.punctata", length(newx)
#'     )),
#'     # fixed predator density
#'     Predator.number = rep(
#'       5, length(newx)
#'     ),
#'     # fixed days
#'     Day = newx
#'   ),
#'   se = T
#' )
#' # let's store all the predictions in a useful way
#' newy_C.punctata_5$fit.exp <- exp(newy_C.punctata_5$fit)
#' # with confidence limits
#' newy_C.punctata_5$lower <- exp(
#'   newy_C.punctata_5$fit - 1.96*newy_C.punctata_5$se.fit
#' )
#' newy_C.punctata_5$upper <- exp(
#'   newy_C.punctata_5$fit + 1.96*newy_C.punctata_5$se.fit
#' )
#' # in a data frame
#' newy_C.punctata_5 <- as.data.frame(newy_C.punctata_5) %>%
#'   select(fit.exp, lower, upper)
#'
#'
#' #' ### For 10 _C. punctata_
#' newy_C.punctata_10 <- predict(
#'   fit,
#'   newdata = list(
#'     # random sample of spot brightness
#'     Spot.brightness = sample(
#'       possible_spot.brightness,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # random sample of substrate
#'     Substrate = sample(
#'       possible_substrate,
#'       size = length(newx),
#'       replace = T
#'     ),
#'     # fixed predator
#'     Predator = factor(rep(
#'       "C.punctata", length(newx)
#'     )),
#'     # fixed predator density
#'     Predator.number = rep(
#'       10, length(newx)
#'     ),
#'     # fixed days
#'     Day = newx
#'   ),
#'   se = T
#' )
#' # let's store all the predictions in a useful way
#' newy_C.punctata_10$fit.exp <- exp(newy_C.punctata_10$fit)
#' # with confidence limits
#' newy_C.punctata_10$lower <- exp(
#'   newy_C.punctata_10$fit - 1.96*newy_C.punctata_10$se.fit
#' )
#' newy_C.punctata_2$upper <- exp(
#'   newy_C.punctata_10$fit + 1.96*newy_C.punctata_10$se.fit
#' )
#' # in a data frame
#' newy_C.punctata_10 <- as.data.frame(newy_C.punctata_10) %>%
#'   select(fit.exp, lower, upper)

# How's it looking? -------------------------------
#' ## Plotting the fit
#'
#' Having a quick look at one of the lines in base
#'
plot(
  Count ~ Day, data = filter(guppy_counts, Predator == "R.hartii")
)

matlines(
  newx, newy_R.hartii_2,
  lty = "solid",
  lwd = 2,
  col = c(2,1,1)
)
#' **This line is for spot brightness 18, in sand when there are 2 R.hartii.** I really don't want to predict every factor separately like this.. Is it the only way?
#'
#'
ggplot(
  guppy_counts,
  aes(
    x = Day,
    y = Count,
    colour = Spot.brightness,
    size = Count
  )
) + geom_jitter(
  width = 50,
  alpha = .4
) + facet_grid(
  Predator.number ~ Predator
) + geom_line(
  data = bind_cols(
    X = newx,
    newy_R.hartii_2,
    Spot.brightness = rep(18, nrow(newy_R.hartii_2)),
    Substrate = rep("Sand", nrow(newy_R.hartii_2)),
    Predator = rep("R.hartii", nrow(newy_R.hartii_2)),
    Predator.number = rep(2, nrow(newy_R.hartii_2))
  ),
  mapping = aes(
    x = X,
    y = fit.exp,
    size = 1
  )
)
