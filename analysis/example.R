# example of how to fit curve
# from Brad

# make some asymptotic data
Y <- 1 - exp(-seq(from = 0, to = 2, length = 1000));
# with deviation
Y <- Y + rnorm(n = 1000, sd = 0.1);

X <- 1:1000;
# plot the data
plot(Y ~ X)

# fit a basic model
mod1 <- glm(Y ~ X + I(X^2));

# Use the coeficients to draw the line
# X^2 coef tells us how much the line is not linear by at every step. How much it is modified.
Ymod <- coef(mod1)[1] + coef(mod1)[2] * X + coef(mod1)[3]*X*X

points(
  X, Ymod,
  type = "l",
  col = "red",
  lwd = 2
)

#-------------------------

# can i do this very simply with my data?

plot(
  jitter(Spot.brightness, .75) ~ jitter(Day, 75),
  data = filter(guppies, Predator.number == 0)
)

mod2 <- glm(
  Spot.brightness ~ Day + I(Day^2),
  data = filter(guppies, Predator.number == 0)
)

Day <- 0:1500

Ymod2 <- coef(mod2)[1] + coef(mod2)[2] * Day + coef(mod2)[3]*Day*Day


points(
  Day, Ymod2,
  type = "l",
  col = "red",
  lwd = 2
)

# YES.
# with predict?

predictions <- predict(
  mod2,
  newdata = list(
    # fixed days
    Day = Day
  ),
  se = T
)

points(
  Day, predictions$fit,
  type = "l",
  col = "green",
  lwd = 2,
  lty = "dotted"
)

# YES. same.
# Not asymptotic but looks like a good fit.

summary(mod2)

# It's a good fit too. Without any other parameters. AIC higher than my other models and dispersion lower.

# does it work on a plot for a predator?

plot(
  jitter(Spot.brightness, .75) ~ jitter(Day, 75),
  data = filter(guppies, Predator == "C.punctata")
)

mod2 <- glm(
  Spot.brightness ~ Day * Predator.number + I(Day^2) * Predator.number,
  data = filter(guppies, Predator== "C.punctata")
)

predictions <- predict(
  mod2,
  newdata = list(
    Predator.number = rep(2, length(Day)),
    Day = Day
  ),
  se = T
)

points(
  Day, predictions$fit,
  type = "l",
  col = "black",
  lwd = 2
)

predictions <- predict(
  mod2,
  newdata = list(
    Predator.number = rep(10, length(Day)),
    Day = Day
  ),
  se = T
)

points(
  Day, predictions$fit,
  type = "l",
  col = "green",
  lwd = 2
)

predictions <- predict(
  mod2,
  newdata = list(
    Predator.number = rep(5, length(Day)),
    Day = Day
  ),
  se = T
)

points(
  Day, predictions$fit,
  type = "l",
  col = "red",
  lwd = 2
)

vif(mod2)
mod3 <- update(
  mod2, ~. - Day:Predator.number
)
vif(mod3) # don't worry about squared terms

anova(mod2, mod4, test = "Chi")

predictions <- predict(
  mod3,
  newdata = list(
    Predator.number = rep(2, length(Day)),
    Day = Day
  ),
  se = T
)

points(
  Day, predictions$fit,
  type = "l",
  col = "black",
  lwd = 2,
  lty = "dotted"
)

predictions <- predict(
  mod3,
  newdata = list(
    Predator.number = rep(10, length(Day)),
    Day = Day
  ),
  se = T
)

points(
  Day, predictions$fit,
  type = "l",
  col = "green",
  lwd = 2,
  lty = "dotted"
)

predictions <- predict(
  mod3,
  newdata = list(
    Predator.number = rep(5, length(Day)),
    Day = Day
  ),
  se = T
)

points(
  Day, predictions$fit,
  type = "l",
  col = "red",
  lwd = 2,
  lty = "dotted"
)

# I don't think I should have worried about any collinearity. it's all down to squared terms. and mod2 fits the data better? mod4 has higher AIC though, but also higher dispersion. But i don't like the symmetry in either model. Curves start to ascend or descend at the edns which doesn't replicate well what is happening.

plot(
  jitter(Spot.brightness, .75) ~ jitter(Day, 75),
  data = filter(guppies, Predator.number == 0)
)

mod <- nls(
  Spot.brightness ~ SSasymp(Day, a, b, c),
  data = filter(guppies, Predator.number == 0)
)

predictions <- predict(
  mod,
  newdata = list(
    #Predator.number = rep(2, length(Day)),
    Day = Day
  ),
  se = F
)

points(
  Day, predictions,
  type = "l",
  col = "black",
  lwd = 2
)

# the way we did before
mod2 <- glm(
  Spot.brightness ~ Day + I(Day^2),
  data = filter(guppies, Predator.number == 0)
)

predictions <- predict(
  mod2,
  newdata = list(
    # fixed days
    Day = Day
  ),
  se = F
)

points(
  Day, predictions,
  type = "l",
  col = "black",
  lwd = 2,
  lty = "dotted"
)

#' NLS method better fit for mechanistics of Biology. But I can't get this to fit for predation at high levels.
#'
mod2 <- lm(
  Spot.brightness ~ Day + I(Day^2),
  data = filter(guppies, Predator.number == 0)
)

predictions <- predict(
  mod2,
  newdata = list(
    # fixed days
    Day = Day
  ),
  se = F
)

points(
  Day, predictions,
  type = "l",
  col = "red",
  lwd = 2
)

# LM fits the exact same line. lols.
# The internet was right, what is the point of glm gaussian?

plot(
  jitter(Spot.brightness, .75) ~ jitter(Day, 75),
  data = filter(
    guppies,
    Predator.number == 10 &
    Predator == "C.punctata"
  )
)

mod2 <- lm(
  Spot.brightness ~ Day + I(Day^2),
  data = filter(
    guppies,
    Predator.number == 10 &
      Predator == "C.punctata"
  )
)

predictions <- predict(
  mod2,
  newdata = list(
    # fixed days
    Day = Day
  ),
  se = F
)

points(
  Day, predictions,
  type = "l",
  col = "red",
  lwd = 2
)
