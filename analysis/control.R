# make sure we have data before continuing
if(!exists("guppies"))
  data("guppies")

# subset the data
control <- guppies %>%
  filter(Predator.number == 0)
# fit the growth curve
# better convergence than self starter
# but curve is identical
fit_control <- nls(
  Spot.brightness ~ a - b*exp(-c*Day),
  data = control,
  start = list(
    a = 20, # asymptotic val of function (plataeu)
    b = 16, # a - intercept at 0
    c = 0.0039 # -log((20-14)/16)/250)
  )
)
# setup new temp x vals for predictions
days <- seq(
  min(control$Day),
  max(control$Day),
  .5
)
# predict new temp y vals based on model fit
predictions <- predict(
  fit_control,
  newdata = list(
    Day = days
  ),
  se = F
)
# store in dataframe for use in predation plots
control_predictions <- data.frame(
  X = rep(days, 3),
  Y = rep(predictions, 3),
  Predator.number = c(
    rep(2, length(days)),
    rep(5, length(days)),
    rep(10, length(days))
  )
)

# build the plot
plot_control <- ggplot(
  control,
  aes(
    y = Spot.brightness,
    x = Day
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
  # asymp formula
  method = "nls",
  formula = as.formula('y ~ a - b * exp(-c*x)'),
  method.args = list(
    start = list(
      a = 20,
      b = 10,
      c = 0.0028
    )
  ),
  se = F,
  colour = "black"
) + ylab(
  "Male spot brightness"
)


# show no difference between control factors
control_end <- filter(control, Day > 1400)
control_mean <- mean(control_end$Spot.brightness)

# boxplot
boxplot_control <- ggplot(
  control_end,
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
  legend.position = "none",
  axis.ticks.x = element_blank(),
  axis.text.x=element_blank()
) + ylim(
  0, 21
) + facet_grid(
  ~Substrate,
  space = "free",
  scales = "free"
)




