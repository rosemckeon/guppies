# make sure we have data before continuing
if(!exists("guppies"))
  data("guppies")

# subset the end of each trial
predation_density_end <- filter(
  guppies, Day > 1400
)

# fit the interaction with quadratic effect
# gives identical output to glm so keeping it simple with lm
fit_density <- lm(
  Spot.brightness ~ Predator.number * Predator + I(Predator.number^2) * Predator,
  data = predation_density_end
)
summary(fit_density)
# very good R2
fit_density1 <- update(
  fit_density, ~. -Predator.number:Predator
)
anova(fit_density, fit_density1)
# removal not valid
fit_density1 <- update(
  fit_density, ~. -I(Predator.number^2):Predator
)
anova(fit_density, fit_density1)
# removal not valid
fit_density1 <- update(
  fit_density, ~. -Predator
)
anova(fit_density, fit_density1)
# removal not valid
fit_density1 <- update(
  fit_density, ~. -Predator.number
)
anova(fit_density, fit_density1)
# removal valid
fit_density <- fit_density1

# create vals to make predictions for
density <- seq(0, 10, 0.1)

# predict response for species 1
predictions1 <- predict(
  fit_density,
  newdata = list(
    Predator = rep("R.hartii", length(density)),
    Predator.number = density
  ),
  se = T
)
predictions1 <- data.frame(
  Predator.number = density,
  Spot.brightness = predictions1$fit,
  Upper = predictions1$fit + 1.96*predictions1$se.fit,
  Lower = predictions1$fit - 1.96*predictions1$se.fit,
  Predator = rep("R.hartii", length(density))
)

# predict response for species 2
predictions2 <- predict(
  fit_density,
  newdata = list(
    Predator = rep("A.pulchens", length(density)),
    Predator.number = density
  ),
  se = T
)
predictions2 <- data.frame(
  Predator.number = density,
  Spot.brightness = predictions2$fit,
  Upper = predictions2$fit + 1.96*predictions2$se.fit,
  Lower = predictions2$fit - 1.96*predictions2$se.fit,
  Predator = rep("A.pulchens", length(density))
)

# predict response for species 3
predictions3 <- predict(
  fit_density,
  newdata = list(
    Predator = rep("C.punctata", length(density)),
    Predator.number = density
  ),
  se = T
)
predictions3 <- data.frame(
  Predator.number = density,
  Spot.brightness = predictions3$fit,
  Upper = predictions3$fit + 1.96*predictions3$se.fit,
  Lower = predictions3$fit - 1.96*predictions3$se.fit,
  Predator = rep("C.punctata", length(density))
)

# prepare predictions for ggplot
predictions <- bind_rows(
  predictions1,
  predictions2,
  predictions3
)
str(predictions)
predictions$Predator <- factor(predictions$Predator)

# plot
plot_predation_density <- ggplot(
  predation_density_end,
  aes(
    x = Predator.number,
    y = Spot.brightness,
    colour = Predator
  )
) + geom_jitter(
  pch = guppies::roses_unicode("dot_filled"),
  #size = 1.5,
  width = .1,
  height = .25,
  alpha = .5
) + facet_grid(
  ~ Predator
) + xlab(
  "Number of predators"
) + ylab(
  "Male spot brightness"
) + theme(
  legend.position = "none"
) + ylim(
  0, 21
) + scale_x_continuous(
  breaks = c(0, 5, 10)
) + scale_colour_manual(
  values = brewer.pal(5, "PuBu")[3:5]
) + geom_line(
  data = predictions
) + geom_line(
  data = predictions,
  aes(
    y = Lower
  ),
  alpha = .5,
  linetype = "dotted"
) + geom_line(
  data = predictions,
  aes(
    y = Upper
  ),
  alpha = .5,
  linetype = "dotted"
)

## older stuff
#
# # Density 10 * Substrate -----------------------------------
# # with the highest density was there an effect of substrate?
# predation_density_high_end <- predation_density_end %>%
#   filter(
#     Predator.number == 10
#   )
#
# boxplot_predation_density10 <- ggplot(
#   predation_density_high_end,
#   aes(
#     y = Spot.brightness,
#     x = Substrate,
#     fill = Substrate,
#     colour = Substrate
#   )
# ) + geom_jitter(
#   pch = guppies::roses_unicode("dot_filled"),
#   #size = 1.5,
#   width = .1,
#   height = .25,
#   alpha = .5
# ) + geom_boxplot(
#   colour = "black",
#   position = position_nudge(x = +0.3),
#   width = .3,
#   outlier.colour = NA
# ) + facet_grid(
#   ~ Predator
# ) + scale_fill_manual(
#   values = brewer.pal(4, "Spectral")[2:4]
# ) + scale_x_discrete(
#   expand = expand_scale(add = c(.3, 0.7))
# ) + ylab(
#   "Male spot brightness"
# ) + theme(
#   legend.position = "none"
# )
#
#
#
# # How do the curves change for predator density over time?
#
# # A.pulchens --------------------------------------------------
# predationA <- filter(predation_density, Predator == "A.pulchens")
#
# # create model objects for each factor we want to facet by
# # so each can have separate lines
# fit_predationA2 <- nls(
#   Spot.brightness ~ SSlogis(Day, a, b, c),
#   # better convergence than asymp
#   data = filter(predationA, Predator.number == 2)
# )
#
# fit_predationA5 <- nls(
#   Spot.brightness ~ SSlogis(Day, a, b, c),
#   # better convergence than asymp
#   data = filter(predationA, Predator.number == 5)
# )
#
# fit_predationA10 <- lm(
#   Spot.brightness ~ Day,
#   # Need to check this is best model
#   # Interaction needs including but messes up predictions
#   data = filter(predationA, Predator.number == 10)
# )
#
# # setup x vals for predictions
# newx <- seq(
#   min(predationA$Day),
#   max(predationA$Day),
#   .5
# )
#
# # get predictions from modal fit for newx
# newy2 <- predict(
#   fit_predationA2,
#   newdata = list(
#     Day = newx
#   )
# )
# newy5 <- predict(
#   fit_predationA5,
#   newdata = list(
#     Day = newx
#   )
# )
# newy10 <- predict(
#   fit_predationA10,
#   newdata = list(
#     Day = newx
#   )
# )
#
# # combine predictions into new data frame for geom_lines
# # Important that factor added for facets
# predationA_predictions <- data.frame(
#   X = c(
#     newx, newx, newx
#   ),
#   Y = c(
#     newy2, newy5, newy10
#   ),
#   Predator.number = c(
#     rep(2, length(newx)),
#     rep(5, length(newx)),
#     rep(10, length(newx))
#   )
# )
#
# # relabel factors for both data objects
# predator.number.labels <- c(
#   "2 predators",
#   "5 predators",
#   "10 predators"
# )
#
# predationA$Predator.number <- as.factor(
#   predationA$Predator.number
# )
# predationA_predictions$Predator.number <- as.factor(
#   predationA_predictions$Predator.number
# )
# control_predictions$Predator.number <- as.factor(
#   control_predictions$Predator.number
# )
#
#
# levels(predationA$Predator.number) <- predator.number.labels
# levels(predationA_predictions$Predator.number) <- predator.number.labels
# levels(control_predictions$Predator.number) <- predator.number.labels
#
# # plot raw data and fitted lines in facets
# plot_densityA <- ggplot(
#   predationA,
#   aes(
#     x = Day,
#     y = Spot.brightness,
#     colour = Predator.number
#   )
# ) + geom_jitter(
#   pch = guppies::roses_unicode("dot_filled"),
#   size = 1.5,
#   width = 25,
#   height = .25,
#   alpha = .3
# ) + geom_line(
#   aes(
#     x = X,
#     y = Y
#   ),
#   data = predationA_predictions
# ) + geom_line(
#   aes(
#     x = X,
#     y = Y
#   ),
#   data = control_predictions,
#   linetype = "dotted",
#   colour = alpha(1, .5)
# ) + scale_colour_manual(
#   values = brewer.pal(6, "Reds")[3:6]
# ) + facet_wrap(
#   ~ as.factor(Predator.number),
#   ncol = 3
# ) + theme(
#   legend.position = "none"
# ) + ylim(
#   0, 21
# ) + ylab(
#   "Male spot brightness"
# ) + scale_x_continuous(
#   breaks = c(0, 750, 1500)
# )
#
#
# # C.punctata -------------------------------------------------
# predationC <- filter(predation_density, Predator == "C.punctata")
#
# # create model objects for each factor we want to facet by
# # so each can have separate lines
# fit_predationC2 <- nls(
#   Spot.brightness ~ SSlogis(Day, a, b, c),
#   # better convergence than asymp
#   data = filter(predationC, Predator.number == 2)
# )
#
# fit_predationC5 <- lm(
#   Spot.brightness ~ Day,
#   # need to check if this is the best
#   # Interaction needed but messes up my predictions
#   data = filter(predationC, Predator.number == 5)
# )
#
# fit_predationC10 <- nls(
#   Spot.brightness ~ SSasymp(Day, a, b, c),
#   # logis doesn't fit the data this time.
#   data = filter(predationC, Predator.number == 10)
# )
#
# # setup x vals for predictions
# newx <- seq(
#   min(predationC$Day),
#   max(predationC$Day),
#   .5
# )
#
# # get predictions from modal fit for newx
# newy2 <- predict(
#   fit_predationC2,
#   newdata = list(
#     Day = newx
#   )
# )
# newy5 <- predict(
#   fit_predationC5,
#   newdata = list(
#     Day = newx
#   )
# )
# newy10 <- predict(
#   fit_predationC10,
#   newdata = list(
#     Day = newx
#   )
# )
#
# # combine predictions into new data frame for geom_lines
# # Important that factor added for facets
# predationC_predictions <- data.frame(
#   X = c(
#     newx, newx, newx
#   ),
#   Y = c(
#     newy2, newy5, newy10
#   ),
#   Predator.number = c(
#     rep(2, length(newx)),
#     rep(5, length(newx)),
#     rep(10, length(newx))
#   )
# )
#
# # relabel factors for both data objects
# predationC$Predator.number <- as.factor(
#   predationC$Predator.number
# )
# predationC_predictions$Predator.number <- as.factor(
#   predationC_predictions$Predator.number
# )
#
# levels(predationC$Predator.number) <- predator.number.labels
# levels(predationC_predictions$Predator.number) <- predator.number.labels
#
#
# # plot raw data and fitted lines in facets
# plot_densityC <- ggplot(
#     predationC,
#     aes(
#       x = Day,
#       y = Spot.brightness,
#       colour = Predator.number
#     )
#   ) + geom_jitter(
#     pch = guppies::roses_unicode("dot_filled"),
#     size = 1.5,
#     width = 25,
#     height = .25,
#     alpha = .3
#   ) + geom_line(
#     aes(
#       x = X,
#       y = Y
#     ),
#     data = predationC_predictions
#   ) + geom_line(
#     aes(
#       x = X,
#       y = Y
#     ),
#     data = control_predictions,
#     linetype = "dotted",
#     colour = alpha(1, .5)
#   ) + scale_colour_manual(
#     values = brewer.pal(6, "Reds")[4:6]
#   ) + facet_wrap(
#     ~ as.factor(Predator.number),
#     ncol = 3
#   ) + theme(
#     legend.position = "none"
#   ) + ylim(
#     0, 21
#   ) + ylab(
#     "Male spot brightness"
#   ) + scale_x_continuous(
#     breaks = c(0, 750, 1500)
#   )
#
#
