# make sure we have data before continuing
if(!exists("guppies"))
  data("guppies")

  # subset the data
  control <- guppies %>%
    filter(Predation == FALSE)
  # fit the growth curve
  fit_control <- nls(
    Spot.brightness ~ a - b*exp(-c*Day),
    data = control,
    start = list(
      a = 20, # asymptotic val of function (plataeu)
      b = 16, # a - intercept at 0
      c = 0.0039 # -log((20-14)/16)/250)
    )
  )
  # build the plot
  plot_control <- fit_control %>% augment() %>%
    ggplot(
      .,
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
    )
