# make sure we have data before continuing
if(!exists("guppies"))
  data("guppies")

predation_end <- guppies %>%
  filter(
    Day > 1400 &
    Predator.number > 0
  )

# add in the control as another factor to test by anova
predation_control <- control_end

predation_control$Predator <- rep(
  factor("None"), nrow(predation_control)
)

predation_end <- bind_rows(
  predation_end,
  predation_control
)

predation_end$Predator <- fct_relevel(
  predation_end$Predator, "None", "R.hartii"
)

predation_end$Predator <- factor(predation_end$Predator)

# get predation over time
predation <- guppies %>%
  filter(
    Predator.number > 0
  )

# N for all groups at end
predation_N <- predation_end %>%
  group_by(Predator) %>% tally()

plot_predation <- ggplot(
  predation_end,
  aes(
    y = Spot.brightness,
    x = Predator,
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
  ~Predator,
  space = "free",
  scales = "free"
)

# stastical significance?
fit_predation <- lm(
  Spot.brightness ~ Predator, data = predation_end
)
#summary(fit_predation)
#summary.aov(fit_predation)

summary_predation <- data.frame(
  Predator = c(
    "None",
    "\\textit{Rivulus hartii}",
    "\\textit{Aequidens pulcher}",
    "\\textit{Crenicichla punctata}"
  ),
  B = tidy(summary(fit_predation)) %>% pull(estimate),
  Lower = as.data.frame(confint(fit_predation)) %>% pull(1),
  Upper = as.data.frame(confint(fit_predation)) %>% pull(2),
  t = tidy(summary(fit_predation)) %>% pull(statistic),
  P = c(
    "\\textless\\hspace{1em}0.001",
    "0.095",
    rep("\\textless\\hspace{1em}0.001", 2)
  )
)


boxplot_predation_substrate <- ggplot(
  predation_end,
  aes(
    y = Spot.brightness,
    x = Substrate,
    fill = Substrate,
    colour = Substrate
  )
) + geom_jitter(
  pch = guppies::roses_unicode("dot_filled"),
  width = .1,
  height = .25,
  alpha = .5
) + geom_boxplot(
  colour = "black",
  position = position_nudge(x = +0.3),
  width = .3,
  outlier.colour = NA
) + scale_fill_manual(
  values = brewer.pal(4, "Spectral")[2:4]
) + scale_x_discrete(
  expand = expand_scale(add = c(.3, 0.7))
) + ylab(
  "Male spot brightness"
) + theme(
  legend.position = "none"
) + ylim(
  0, 21
) + facet_grid(
  ~Predator
) + geom_hline(
  aes(
    yintercept = control_mean
  ),
  linetype = "dotted",
  alpha = .5,
  colour = 1
)

# plot
plot_predation_time <- ggplot(
    predation,
    aes(
      y = Spot.brightness,
      x = Day
      #col = Predator
    )
  ) + ylim(
    0, 21 # has to include jitter range
  ) + geom_jitter(
    pch = guppies::roses_unicode("dot_filled"),
    size = 1.5,
    width = 25,
    height = .25,
    alpha = .3
  ) + geom_smooth(
    aes(
      x = Day,
      y = Spot.brightness,
      colour = Predator
    ),
    # custom method allows different types of smoothing
    # for each facet panel
    method = "smooth_predation",
    se = FALSE,
    colour = "black" # why does it break without this line?
  ) + ylab(
    "Male spot brightness"
  ) + facet_wrap(
    ~ Predator
  ) + theme(
    legend.position = "none"
  ) + scale_color_manual(
    values = c(1, brewer.pal(4, "Spectral")[2:4])
  ) + scale_x_continuous(
    breaks = c(0, 750, 1500)
  )
