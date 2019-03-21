# plot main effe ct of substrate (nothing)
plot_substrate <- ggplot(
  predation_end, # all groups including control
  aes(
    y = Spot.brightness,
    x = Substrate,
    fill = Spot.brightness,
    colour = Spot.brightness
  )
) + geom_violin(
  trim = F,
  scale = "count",
  width = .5,
  colour = alpha(1, .25),
  linetype = "dotted"
) + geom_jitter(
  pch = guppies::roses_unicode("dot_filled"),
  width = .05,
  height = .25,
  alpha = .5
) + stat_summary(
  fun.data = mean_cl_normal,
  color = 1,
  geom = "errorbar",
  width = .25,
  position = position_nudge(x = .25)
  # ) + scale_x_discrete(
  #   expand = expand_scale(add = c(.3, 0.7))
) + stat_summary(
  fun.data = mean_cl_normal,
  color = 1,
  geom = "pointrange",
  size = .25,
  position = position_nudge(x = .25)
) + ylab(
  "Male spot brightness"
) + theme(
  legend.position = "none",
  axis.ticks.x = element_blank(),
  axis.text.x=element_blank()
) + ylim(
  -1, 23
) + facet_grid(
  ~Substrate,
  space = "free",
  scales = "free"
)

# stastical significance?
fit_substrate <- lm(
  Spot.brightness ~ Substrate, data = predation_end
)
#summary(fit_substrate)
#summary.aov(fit_substrate)

# N for all groups at end
substrate_N <- predation_end %>%
  group_by(Substrate) %>% tally()

# plot all the interactions of substrate
plot_substrate_predation <- ggplot(
  predation_density_end,
  aes(
    y = Spot.brightness,
    x = Substrate,
    fill = Spot.brightness,
    colour = Spot.brightness
  )
) + geom_hline(
  aes(
    yintercept = control_mean
  ),
  linetype = "solid",
  size = 2,
  alpha = .1,
  colour = 1
) + geom_violin(
  trim = F,
  scale = "count",
  width = .5,
  colour = alpha(1, .25),
  linetype = "dotted"
) + geom_jitter(
  pch = guppies::roses_unicode("dot_filled"),
  width = .05,
  height = .25,
  alpha = .5
) + stat_summary(
  fun.data = mean_cl_normal,
  color = 1,
  geom = "errorbar",
  width = .25,
  position = position_nudge(x = .25)
) + stat_summary(
  fun.data = mean_cl_normal,
  color = 1,
  geom = "pointrange",
  size = .25,
  position = position_nudge(x = .25)
) + ylab(
  "Male spot brightness"
) + theme(
  legend.position = "none"
) + ylim(
  -1, 23
) + facet_grid(
  Predator.number~Predator,
  space = "free",
  scales = "free"
)

# what about as an interaction?
# fit a linear model
fit_substrate_density <- lm(
  Spot.brightness ~ Substrate * Predator.number * Predator,
  data = predation_density_end
)
#plot(fit_substrate_density)
# not quite as curvilinear looking as density alone
#summary(fit_substrate_density)
# R2 = 0.859 (lower than our curvilinear model without substrate)

# fit the interaction with quadratic effect
# gives identical output to glm so keeping it simple with lm
fit_substrate_density <- lm(
  Spot.brightness ~ Substrate * Predator.number * Predator + Substrate * I(Predator.number^2) * Predator,
  data = predation_density_end
)
summary(fit_substrate_density)
# very good R2, even better than other curvilinear model 0.911
# simplification
fit_temp <- update(
  fit_substrate_density, ~. -Substrate:Predator
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# next
fit_temp <- update(
  fit_substrate_density, ~. -Substrate:Predator.number
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# next
fit_temp <- update(
  fit_substrate_density, ~. -Substrate:I(Predator.number^2)
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# next
# fit_temp <- update(
#   fit_substrate_density, ~. -Substrate:Predator:I(Predator.number^2)
# )
# anova(fit_substrate_density, fit_temp)
# removal not valid (substrate matters!)
# fit_temp <- update(
#   fit_substrate_density, ~. -Substrate:Predator:Predator.number
# )
# anova(fit_substrate_density, fit_temp)
# removal not valid
fit_temp <- update(
  fit_substrate_density, ~. -Predator:I(Predator.number^2)
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# next
fit_temp <- update(
  fit_substrate_density, ~. -Predator:Predator.number
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# next
fit_temp <- update(
  fit_substrate_density, ~. -I(Predator.number^2)
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# next
fit_temp <- update(
  fit_substrate_density, ~. -Predator.number
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# next
# fit_temp <- update(
#   fit_substrate_density, ~. -Predator
# )
# anova(fit_substrate_density, fit_temp)
# removal not valid
fit_temp <- update(
  fit_substrate_density, ~. -Substrate
)
anova(fit_substrate_density, fit_temp)
# removal valid
fit_substrate_density <- fit_temp
# DONE
#summary(fit_substrate_density)
# R2 the same.
# Interaction of substrate with predator and predator number mostly significant for A.pulcher and C.punctata but not R.hartii

# predict responses including substrate interaction
substrate_predictions <- bind_rows(
  #predict_substrate(density, "R.hartii", "Sand"),
  #predict_substrate(density, "R.hartii", "Vegetation"),
  #predict_substrate(density, "R.hartii", "Mud"),
  predict_substrate(density, "A.pulcher", "Sand"),
  predict_substrate(density, "A.pulcher", "Vegetation"),
  predict_substrate(density, "A.pulcher", "Mud"),
  predict_substrate(density, "C.punctata", "Sand"),
  predict_substrate(density, "C.punctata", "Vegetation"),
  predict_substrate(density, "C.punctata", "Mud")
)
substrate_predictions$Predator <- factor(
  substrate_predictions$Predator
)
substrate_predictions$Substrate <- factor(
  substrate_predictions$Substrate
)
substrate_predictions$Substrate <- fct_relevel(
  substrate_predictions$Substrate,
  "Sand", "Vegetation"
)


substrate_interaction <- predation_density_end %>%
  filter(
    Predator != "R.hartii"
  )

# N for all groups at end
substrate_N <- substrate_interaction %>%
  group_by(Predator) %>% tally()

# plot
plot_substrate_interaction <- ggplot(
  substrate_interaction,
  aes(
    x = Predator.number,
    y = Spot.brightness
    #colour = Substrate
  )
) + geom_jitter(
  pch = guppies::roses_unicode("dot_filled"),
  width = .25,
  height = .25,
  alpha = .5
) + facet_grid(
  ~ Predator
) + xlab(
  "Number of predators"
) + ylab(
  "Male spot brightness"
) + ylim(
  0, 21
) + scale_x_continuous(
  breaks = c(0, 5, 10)
) + geom_line(
  data = substrate_predictions,
  aes(
    colour = Substrate
  )
) + geom_line(
  data = substrate_predictions,
  aes(
    y = Lower,
    colour = Substrate
  ),
  alpha = .5,
  linetype = "dotted"
) + geom_line(
  data = substrate_predictions,
  aes(
    y = Upper,
    colour = Substrate
  ),
  alpha = .5,
  linetype = "dotted"
) + scale_colour_manual(
  values = brewer.pal(4, "Spectral")[2:4]
) + theme(
  legend.position = "top"
)

# stats output
summary_substrate_interaction <- data.frame(
  Term = c(
    "\\textit{Rivulus hartii}",
    "\\textit{Aequidens pulcher}",
    "\\textit{Crenicichla punctata}",
    "Interaction between \\textit{R. hartii}, sand and predator number",
    "Interaction between \\textit{A. pulcher} sand and predator number",
    "Interaction between \\textit{C. punctata} sand and predator number",
    "Interaction between \\textit{R. hartii}, vegetation and predator number",
    "Interaction between \\textit{A. pulcher} vegetation and predator number",
    "Interaction between \\textit{C. punctata} vegetation and predator number",
    "Interaction between \\textit{R. hartii}, mud and predator number",
    "Interaction between \\textit{A. pulcher} mud and predator number",
    "Interaction between \\textit{C. punctata} mud and predator number",
    "Interaction between \\textit{R. hartii}, sand and predator number\\textsuperscript{2}",
    "Interaction between \\textit{A. pulcher} sand and predator number\\textsuperscript{2}",
    "Interaction between \\textit{C. punctata} sand and predator number\\textsuperscript{2}",
    "Interaction between \\textit{R. hartii}, vegetation and predator number\\textsuperscript{2}",
    "Interaction between \\textit{A. pulcher} vegetation and predator number\\textsuperscript{2}",
    "Interaction between \\textit{C. punctata} vegetation and predator number\\textsuperscript{2}",
    "Interaction between \\textit{R. hartii}, mud and predator number\\textsuperscript{2}",
    "Interaction between \\textit{A. pulcher} mud and predator number\\textsuperscript{2}",
    "Interaction between \\textit{C. punctata} mud and predator number\\textsuperscript{2}"
  ),
  B = tidy(summary(fit_substrate_density)) %>% pull(estimate),
  Lower = as.data.frame(confint(fit_substrate_density)) %>% pull(1),
  Upper = as.data.frame(confint(fit_substrate_density)) %>% pull(2),
  t = tidy(summary(fit_substrate_density)) %>% pull(statistic),
  P = c(
    "\\textless\\hspace{1em}0.001",
    "0.183",
    "\\textless\\hspace{1em}0.001",
    "0.661",
    "\\textless\\hspace{1em}0.001",
    "\\textless\\hspace{1em}0.001",
    "0.787",
    "\\textless\\hspace{1em}0.001",
    "\\textless\\hspace{1em}0.001",
    "0.211",
    "\\textless\\hspace{1em}0.001",
    "\\textless\\hspace{1em}0.001",
    "0.123",
    "\\textless\\hspace{1em}0.001",
    "\\textless\\hspace{1em}0.001",
    "0.446",
    "0.050",
    "\\textless\\hspace{1em}0.001",
    "0.673",
    "0.055",
    "\\textless\\hspace{1em}0.001"
  )
)
