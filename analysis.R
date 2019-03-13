#' ---
#' title: "Guppy Spot Brightness Analysis"
#' desc: "Using data collected from simulated guppy experiments on SimBio, for module BIOU6BE"
#' author: "**Rose McKeon** (2417024)"
#' date: "March 7th 2019"
#' fig_caption: yes
#' ---

#+ setup, message=FALSE, warning=FALSE ---------
# clear the workspace
#rm(list=ls())
# load dependencies
library(broom)
library(tidyverse)
library(nlstools)
library(RColorBrewer)

source("R/functions.R")
# ggplot theme settings
roses_set_ggtheme()

#' ## DATA

#+ data ----------------------------------------------
load("data/guppies.rda")
str(guppies)
summary(guppies$Substrate)
summary(guppies$Predator)

hist(guppies$Spot.brightness) # bimodal response
hist(guppies$Day)
hist(guppies$Predator.number)
hist(guppies$Additional.R.hartii)
hist(guppies$Starting.food)
# no outliers/errors

# this one takes some time!
#pairs(guppies)

#' ## Preliminary Plots
#' **Eyeballing**

#' Control tanks show guppy spot brightness increases over time when there is no predation. Neither tank substrate or starting food amount had an effect.
#'
# no predation
control <- guppies %>%
  filter(Predation == FALSE)

ggplot(
  control,
  aes(
    x = Day,
    y = Spot.brightness,
    col = Starting.food
  )
) + geom_jitter(
  width = 50,
  alpha = .1
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Substrate ~ Starting.food
) + theme(
  legend.position = "top"
)

#' When predators were added we could see that R.hartii do not control spot brightness at all - looks very much our control group. Confirms suspicions that these fish do not predate upon adult male guppies. We can also see that C.punctata seems to have the best eyesight for spotting guppy spots  controls spot brightness the most and is the only negative relationship.
#'
# predation at medium level
predation <- guppies %>%
  filter(
    Predation == TRUE &
    Predator.number == 5 &
    Additional.R.hartii == 0
  )
# make sure we compare to experiment with no predation that matches level of feeding.
control_fed_51 <- control %>%
  filter(Starting.food == 51)
# combine data for plotting
predation <- bind_rows(control_fed_51,predation)
# relevel for beter plot
predation$Predator <- fct_relevel(
  predation$Predator, "None", "R.hartii"
)
# check relevelling worked
summary(predation$Predator)
# plot
ggplot(
  predation,
  aes(
    x = Day,
    y = Spot.brightness,
    col = Predator
  )
) + geom_jitter(
  width = 50,
  alpha = .1
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  ~Predator
) + theme(
  legend.position = "none"
)

#' >Need to fit models to improve all these lines past basic regression and get coeffs.
#'
#' Now let's just look at the 2 predators which control spots. C.punctata control spots better than A.pulchens in every substrate..
#'
predators <- predation %>%
  filter(
    Predator != "None" &
    Predator != "R.hartii"
  )

ggplot(
  predators, aes(
    x = Day,
    y = Spot.brightness,
    col = Predator
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
   Substrate ~ Predator
) + theme(
  legend.position = "none"
)

#' But is there a slight difference in effect size depending on substrate?
#' It looks like C.punctata feed more effectively in the mud.
ggplot(
  predators, aes(
    x = Day,
    y = Spot.brightness,
    col = Substrate
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Predator ~ Substrate
) + theme(
  legend.position = "none"
)

#' Is this more obvious when predation is at a higher density?
#' Now it seems A.pulchens are more effective in vegetation and mud at higher numbers. While C.punctata has evened out.
predators_10 <- guppies %>%
  filter(
    Predator != "None" &
    Predator != "R.hartii" &
    Predator.number == 10
  )

ggplot(
  predators_10, aes(
    x = Day,
    y = Spot.brightness,
    col = Substrate
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Predator ~ Substrate
) + theme(
  legend.position = "none"
)

#' And what if we reduce predation?
#' Less of an effect all round.
predators_2 <- guppies %>%
  filter(
    Predator != "None" &
      Predator != "R.hartii" &
      Predator.number == 2
  )

ggplot(
  predators_2, aes(
    x = Day,
    y = Spot.brightness,
    col = Substrate
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Predator ~ Substrate
) + theme(
  legend.position = "none"
)

#' When different numbers of predators are added spot brightness is affected more or less relative to each species ability.
#'
# predation density, no excluding R.hartii
predation2 <- bind_rows(
  predators, predators_10, predators_2
)

ggplot(
  predation2, aes(
    x = Day,
    y = Spot.brightness,
    col = Predator.number
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  aes(group = Predator.number),
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Substrate ~ Predator
)

#' Does interaction between species matter?
#' It seems like addition of R. hartii makes no difference to the effects of other predators.
community <- guppies %>%
  filter(
    Predation == TRUE &
    Predator.number == 5 &
    Predator != "R.hartii"
  )

ggplot(
  community, aes(
    x = Day,
    y = Spot.brightness,
    col = Additional.R.hartii
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Predator ~ Additional.R.hartii
)

#' Is this true in every subsrate?
#' Looks like it for A.pulchens, but there may be a difference in Substrate effects for C.punctata when R.hartii are also present. **Significant?**
#'
A.pulchens <- community %>%
  filter(Predator == "A.pulchens")

ggplot(
  A.pulchens, aes(
    x = Day,
    y = Spot.brightness,
    col = Additional.R.hartii
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Substrate ~ Additional.R.hartii
)


C.punctata <- community %>%
  filter(Predator == "C.punctata")

ggplot(
  C.punctata, aes(
    x = Day,
    y = Spot.brightness,
    col = Additional.R.hartii
  )
) + geom_jitter(
  width = 50,
  alpha = 0.3
) + geom_smooth(
  method = "lm", # is this line really best fit?
  se = T # is this confidence envelope right?
) + facet_grid(
  Substrate ~ Additional.R.hartii
)

#' ## Now for some stats

#+ stats -------------------------------------------

#' ### Control
#'
fit_control <- lm(
  Spot.brightness ~ Day*Substrate*Starting.food, data = control
)
# and check diagnostics...
par(mfrow=c(2,3))
plot(fit_control)
hist(fit_control$residuals)
par(mfrow=c(1,1))

#' Fit doesn't look great. The Strong U shaped pattern in our first plot as well as high leverage points suggests **we really need to fit a curve**. The shape of our data looks asyptotic, so least-square method for custom curves is preferred.
#'
summary(fit_control)
confint(fit_control)

#' R^2^ of linear regression not great at 0.51

#' Maybe a transformation will fix this for us more easily. Our response is clearly negatively/left-skewed. Common transformations for this type of data are square-root, cube-root and log, but **transformations don't seem to help**.
#' > Or do they? Is a shorter tail better? Perhaps cube-root or log-10 are better.
#'
par(mfrow=c(2,3))
hist(control_fed_51$Spot.brightness)
hist(sqrt(control_fed_51$Spot.brightness))
hist(control_fed_51$Spot.brightness^(1/3)) # cube root
hist(log(control_fed_51$Spot.brightness))
hist(log10(control_fed_51$Spot.brightness))
par(mfrow=c(1,1))

# let's see
fit_control_cubert <-lm(Spot.brightness^(1/3) ~ Day * Substrate, data = control_fed_51)
# model diagnostics
par(mfrow=c(2,3))
plot(fit_control_cubert)
hist(fit_control_cubert$residuals)
par(mfrow=c(1,1))

fit_control_log10 <-lm(log10(Spot.brightness) ~ Day * Substrate, data = control_fed_51)
# model diagnostics
par(mfrow=c(2,3))
plot(fit_control_log10)
hist(fit_control_log10$residuals)
par(mfrow=c(1,1))



#' **Nope** Neither of these seem any better. We really need a curve.
#'
#' ### Non-linear least-square method:
#' ### Growth
# let's use R's inbuilt "self-starting" exponential function SSasymp() to find candidate starting values.
# a, b and c are used in the asymptotic growth formula for the curve fitting y = a - b * exp(-c*x)
fit_control_asymp_ss <- nls(
  Spot.brightness ~ SSasymp(Day, a, b, c),
  data = control
)
# diagnostics
plot(nlsResiduals(fit_control_asymp_ss))
summary(fit_control_asymp_ss)
#' Looks good, all parameters are significant. Should we try manual start list though?
#'
fit_control_asymp <- nls(
  Spot.brightness ~ a - b*exp(-c*Day),
  data = control,
  start = list(
    a = 20, # asymptotic val of function (plataeu)
    b = 16, # a - intercept at 0
    c = 0.0039 # -log((20-14)/16)/250)
  )
)
plot(nlsResiduals(fit_control_asymp))
summary(fit_control_asymp)

#' These look quite different. **Is this to do with my starting values?**
#'
#' ## Plotting Least Square curves
#' Note that getting confidence limits around nonlinear fits is not as straightforward conceptually or analytically because of the fact that there is more than one coefficient associated with the equation, and the estimates are correlated.
#'
roses_par()
plot(
  Spot.brightness ~ Day,
  data = control,
  bty = "l",
  xlab = "Day",
  ylab = "Spot brightness",
  type = "n",
  ylim = c(0,20)
)
points(
  jitter(Spot.brightness, 1) ~ jitter(Day, 100),
  data = filter(control, Starting.food == 0),
  col = alpha(1, .4),
  pch = roses_unicode("dot_open"),
  cex = .6
)
points(
  jitter(Spot.brightness, 1) ~ jitter(Day, 100),
  data = filter(control, Starting.food == 51),
  col = alpha(1, .2),
  pch = roses_unicode("dot_closed"),
  cex = .6
)
# setup x for predictions
newx <- seq(
  min(control$Day),
  max(control$Day),
  1
)
# predictions for whole control group
# ie: all food levels and substrates
newy <- predict(
  fit_control_asymp_ss,
  newdata = list(
    Day = newx
  )
)
lines(
  newx, newy,
  col = alpha(1, .75),
  lwd = 5
)
# predictions for separate food levels
# (IDENTICAL)
# no food
# fit_control_asymp_food0 <- nls(
#   Spot.brightness ~ SSasymp(Day, a, b, c),
#   data = filter(control, Starting.food == 0)
# )
# newy <- predict(
#   fit_control_asymp_food0,
#   newdata = list(
#     Day = newx
#   )
# )
# lines(
#   newx, newy,
#   col = brewer.pal(11,"Spectral")[2], #red
#   lwd = 2,
#   lty = "dashed"
# )
# # standard food
# fit_control_asymp_food51 <- nls(
#   Spot.brightness ~ SSasymp(Day, a, b, c),
#   data = filter(control, Starting.food == 0)
# )
# newy <- predict(
#   fit_control_asymp_food51,
#   newdata = list(
#     Day = newx
#   )
# )
# lines(
#   newx, newy,
#   col = brewer.pal(11,"Spectral")[10], #blue
#   lwd = 2,
#   lty = "dashed"
# )
# predictions for separate substrates
# sand
# fit_control_asymp_sand <- nls(
#   Spot.brightness ~ SSasymp(Day, a, b, c),
#   data = filter(control, Substrate == "Sand")
# )
# newy <- predict(
#   fit_control_asymp_sand,
#   newdata = list(
#     Day = newx
#   )
# )
# lines(
#   newx, newy,
#   col = brewer.pal(11,"Spectral")[4], #medium orange
#   lwd = 2,
#   lty = "dashed"
# )
# # vegetation
# fit_control_asymp_veg <- nls(
#   Spot.brightness ~ SSasymp(Day, a, b, c),
#   data = filter(control, Substrate == "Veg")
# )
# newy <- predict(
#   fit_control_asymp_veg,
#   newdata = list(
#     Day = newx
#   )
# )
# lines(
#   newx, newy,
#   col = brewer.pal(11,"Spectral")[9], #green
#   lwd = 2,
#   lty = "dashed"
# )
# # mud
# fit_control_asymp_mud <- nls(
#   Spot.brightness ~ SSasymp(Day, a, b, c),
#   data = filter(control, Substrate == "Mud")
# )
# newy <- predict(
#   fit_control_asymp_mud,
#   newdata = list(
#     Day = newx
#   )
# )
# lines(
#   newx, newy,
#   col = brewer.pal(11,"Spectral")[11], #purple
#   lwd = 2,
#   lty = "dashed"
# )

#' There is absolutley no difference in the curves for food levels and only a very minor difference between tanks. Sand and veg curves were identical (a touch above this line) but mud fell slightly lower at the beginning and a touch higher at the end. This line is a fair approximation of all curves.


# how about with the manual starting points?
# newy <- predict(
#   fit_control_asymp,
#   newdata = list(
#     Day = newx
#   )
# )
# lines(newx, newy, col = "red", lty = "dotted")

#' Both models make the same curve.
#' **Why are the numbers different???**
#' Can we test if the curve is the same when we look at different substrates or starting food amounts?
#'
roses_set_ggtheme()
fit_control_asymp %>%
  augment() %>%
  ggplot(
    .,
    aes(
      y = Spot.brightness,
      x = Day
    )
  ) + ylim(
    0, 21 # has to include jitter range
  ) + geom_jitter(
    pch = roses_unicode("dot_filled"),
    size = 1.5,
    width = 25,
    height = .25,
    alpha = .3
  ) + geom_smooth(
    # gam method and formula matches nls fit
    # when .fitted included in aes.
    # cannot get nls method to compute with
    # asymp formula
    method = "gam",
    formula = y ~ s(x, bs = "cs"),
    # method = "nls",
    # formula = y ~ a - b * exp(-c*x),
    # method.args = list(
    #   start = list(
    #     a = 20,
    #     b = 16,
    #     c = 0.0039
    #   )
    # ),
    se = F,
    aes(
       x = Day,
       y = .fitted
   ),
    colour = "black"
  ) + ylab(
    "Male spot brightness"
  )+ labs(
    caption = str_wrap("Increase in male guppy spot brightness over time when populations were not exposed to predation (lifespan around 120 days). Each data point represents the spot brightness of one male guppy (points are jittered around both x and y values). The curve shown has been fitted using asymptotic growth equation (y = a - b * exp(-c*x)) from all substrates and feeding levels (no differences between these factors were observed).", width = 100)
  )
