# make sure we have data before continuing
if(!exists("guppies"))
  data("guppies")

# subset the end of each trial
predation_density_end <- guppies %>%
  filter(Day > 1400)


# N for all groups at end
predation_density_N <- predation_density_end %>%
  group_by(Predator) %>% tally()

# fit a linear model
# fit_density <- lm(
#   Spot.brightness ~ Predator.number * Predator,
#   data = predation_density_end
# )
#plot(fit_density)
# looks like rel curvilinear
# summary(fit_density)

# fit the interaction with quadratic effect
# gives identical output to glm so keeping it simple with lm
fit_density <- lm(
  Spot.brightness ~ Predator.number * Predator + I(Predator.number^2) * Predator,
  data = predation_density_end
)
#summary(fit_density)
# very good R2
# fit_density1 <- update(
#   fit_density, ~. -Predator.number:Predator
# )
#anova(fit_density, fit_density1)
# removal not valid
# fit_density1 <- update(
#   fit_density, ~. -I(Predator.number^2):Predator
# )
#anova(fit_density, fit_density1)
# removal not valid
# fit_density1 <- update(
#   fit_density, ~. -Predator
# )
#anova(fit_density, fit_density1)
# removal not valid
fit_density1 <- update(
  fit_density, ~. -Predator.number
)
anova(fit_density, fit_density1)
# removal valid
fit_density <- fit_density1

#summary(fit_density)
# R2 increased :)

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
#str(predictions)
predictions$Predator <- factor(predictions$Predator)

# plot
plot_predation_density <- ggplot(
  predation_density_end,
  aes(
    x = Predator.number,
    y = Spot.brightness,
    colour = Spot.brightness
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
) + geom_line(
  data = predictions,
  color = "black"
) + geom_line(
  data = predictions,
  aes(
    y = Lower
  ),
  alpha = .5,
  linetype = "dotted",
  colour = "black"
) + geom_line(
  data = predictions,
  aes(
    y = Upper
  ),
  alpha = .5,
  linetype = "dotted",
  colour = "black"
)

# stats output
summary_predation_density <- data.frame(
  Term = c(
    "\\textit{Rivulus hartii}",
    "\\textit{Aequidens pulcher}",
    "\\textit{Crenicichla punctata}",
    "Predator number \\textsuperscript{2}",
    "Interaction between \\textit{R. hartii} and predator number",
    "Interaction between \\textit{A. pulcher} and predator number",
    "Interaction between \\textit{C. punctata} and predator number",
    "Interaction between \\textit{A. pulchens} and predator number \\textsuperscript{2}",
    "Interaction between \\textit{C. punctata} and predator number \\textsuperscript{2}"
  ),
  B = tidy(summary(fit_density)) %>% pull(estimate),
  Lower = as.data.frame(confint(fit_density)) %>% pull(1),
  Upper = as.data.frame(confint(fit_density)) %>% pull(2),
  t = tidy(summary(fit_density)) %>% pull(statistic),
  P = c(
    "\\textless\\hspace{1em}0.001",
    "\\textgreater\\hspace{1em}0.05",
    "\\textless\\hspace{1em}0.001",
    "\\textgreater\\hspace{1em}0.05",
    "\\textgreater\\hspace{1em}0.05",
    "\\textless\\hspace{1em}0.001",
    "\\textless\\hspace{1em}0.001",
    "\\textless\\hspace{1em}0.001",
    "\\textless\\hspace{1em}0.001"
  )
)
