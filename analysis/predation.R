# make sure we have data before continuing
if(!exists("guppies"))
  data("guppies")

predation_end <- guppies %>%
  filter(
    Day > 1400 &
    Predator.number > 0
  )

predation <- guppies %>%
  filter(
    Predator.number > 0
  )

boxplot_predation <- ggplot(
  predation_end,
  aes(
    y = Spot.brightness,
    x = Predator,
    fill = Predator,
    colour = Predator
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
  values = brewer.pal(5, "PuBu")[3:5]
) + scale_colour_manual(
  values = brewer.pal(5, "PuBu")[3:5]
) + scale_x_discrete(
  expand = expand_scale(add = c(.3, 0.7))
) + ylab(
  "Male spot brightness"
) + theme(
  legend.position = "none",
  axis.ticks.x = element_blank(),
  axis.text.x=element_blank()
) + ylim(
  0, 21
) + facet_grid(
  ~Predator,
  space = "free",
  scales = "free"
) + geom_hline(
  aes(
    yintercept = control_mean
  ),
  linetype = "dotted",
  alpha = .5,
  colour = 1
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


## older stuff --------------------------

# fit the growth curve for R.hartii
# looks very similar to control
fit_R.hartii <- nls(
  Spot.brightness ~ a - b*exp(-c*Day),
  data = filter(predation, Predator == "R.hartii"),
  start = list(
    # asymptotic val of function (plataeu)
    a = 20,
    # a - intercept at 0
    b = 10,
    # steep part of curve
    # -log((a-y)/b)/x
    c = -log((20-15)/10)/250
  )
)
# fit linear regression for A.pulchens
# diagnostics look ok
# F-tests show we need to keep interaction in model
fit_A.pulchens <- lm(
  Spot.brightness ~ Day*Substrate,
  data = filter(predation, Predator == "A.pulchens")
)
fit_A.pulchens2 <- update(fit_A.pulchens, ~. - Day:Substrate)
# Perhaps slight Assymp / Sigmoidal curve would fit better
# Sigmoidal is best (tolerance lower)
fit_A.pulchens_curve <- nls(
  Spot.brightness ~ SSasymp(Day, a, b, c),
  data = filter(predation, Predator == "A.pulchens")
)
fit_A.pulchens_curve2 <- nls(
  Spot.brightness ~ SSlogis(Day, a, b, c),
  data = filter(predation, Predator == "A.pulchens")
)


# fit linear regression for C.punctata
# diagnostics look ok
# F-tests show we need to keep interaction in model
# It doesn't have great R2 but nls won't fit a curve
fit_C.punctata <- lm(
  Spot.brightness ~ Day*Substrate,
  data = filter(predation, Predator == "C.punctata")
)
fit_C.punctata2 <- update(fit_C.punctata, ~. - Day:Substrate)
# does geom_smooth lm include interaction????

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

# # let's see the difference between the end points to
# # make the effect size clear
# predation_end <- filter(predation, Day > 1400)
#
# # boxplot
# boxplot_predation <- ggplot(
#   predation_end,
#   aes(
#     y = Spot.brightness,
#     x = Substrate,
#     fill = Substrate,
#     colour = Substrate
#   )
# ) + geom_jitter(
#   pch = guppies::roses_unicode("dot_filled"),
#   width = .1,
#   height = .25,
#   alpha = .5
# ) + geom_boxplot(
#   colour = "black",
#   position = position_nudge(x = +0.3),
#   width = .3,
#   outlier.colour = NA
# ) + facet_wrap(
#   ~ Predator
# ) + ylim(
#   0, 21
# ) + scale_x_discrete(
#   expand = expand_scale(add = c(.3, 0.7))
# ) + scale_fill_manual(
#   values = brewer.pal(4, "Spectral")[2:4]
# ) + ylab(
#   "Male spot brightness"
# ) + theme(
#   legend.position = "none"
# )
