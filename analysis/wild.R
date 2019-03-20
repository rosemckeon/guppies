# make sure we have data before continuing
if(!exists("guppies_wild"))
  data("guppies_wild")

# boxplot
plot_wild <- ggplot(
  guppies_wild,
  aes(
    y = Spot.brightness,
    x = Location,
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
  0, 21
) + facet_grid(
  ~Location,
  space = "free",
  scales = "free"
)

# fitting the model
fit_wild <- lm(
  Spot.brightness ~ Location, data = guppies_wild
)
# diagnostics
# par(mfrow = c(2,2))
# plot(fit_wild)
# par(mfrow = c(1,1))
#
# # summary
# # Looks like location as a function of spot brightness in the wild explains 82% of the variation.
# summary(fit_wild)
# confint(fit_wild)
# coeff estimates should match means of location subsets

wild_locations <- c(
  "Lower East\\",
  "Lower West\\",
  "Upper East\\",
  "Upper West\\"
)

wild_N <- guppies_wild %>% group_by(Location) %>% tally()

wild_obs <- data.frame(
  Location = c(wild_locations, "Meir Creek"),
  Means = c(aggregate(Spot.brightness ~ Location, mean, data = guppies_wild) %>% pull(2), 0),
  N = c(wild_N %>% pull(2), 0),
  `Other species` = c(
    "\\textit{Crenicichla punctata} (N = 6).",
    "\\textit{Rivulus hartii} (N = 5)\\newline \\textit{Aequidens pulcher} (N = 5).",
    "None present.",
    "\\textit{Rivulus hartii} (N = 5).",
    "\\textit{Rivulus hartii} (N = 5)\\newline \\textit{Crenicichla punctata} (N = 2)."
  ),
  Substrate = c(
    "Mud",
    "Vegetation",
    "Sand",
    "Sand",
    "Sand"
  )
)

summary_fit_wild <- data.frame(
  Location = wild_locations,
  B = tidy(summary(fit_wild)) %>% pull(estimate),
  Lower = as.data.frame(confint(fit_wild)) %>% pull(1),
  Upper = as.data.frame(confint(fit_wild)) %>% pull(2),
  t = tidy(summary(fit_wild)) %>% pull(statistic),
  P = rep("\\textless  0.001", 4)
)

# Anova SS shows the size of the difference between means caused by location.
#summary.aov(fit_wild)
