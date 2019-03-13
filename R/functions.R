#' @title Change the alpha value of colours
#' @desc Reduce the alpha value (level of transparency) of colours to identify patterns of over-plotting when displaying lots of data points.
#' @source https://magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/
#' @param col a vector of colours
#' @param alpha alpha valuses range from 0-1, 0 being fully transparent and 1 being fully opaque.
#' @return a vector of alpha colours (notice longer codes)
#' @usage
#' myColours = c(1, "steelblue", "#FFBB00", rgb(0.4, 0.2, 0.3))
#' myColoursAlpha <- rgba(myColours, alpha=0.4)
rgba <- function(col, alpha=1){
  # handle input errors
  if(missing(col)){ stop("Please provide a vector of colours.") }
  # create output
  apply(sapply(col, col2rgb)/255, 2, function(x){
    rgb(x[1], x[2], x[3], alpha=alpha)
  }
  )
}

#' @title Rose's favourite graphical params
#' @desc I'm almost always setting these - edit defaults as you see fit.
#' @param ... Accepts all the usual par() params.
#' @usage
#' roses_par()
#' roses_par(pch=18)
roses_par <- function(..., title.space = F){
  if(!title.space){
    par(
      bty = "l",
      lwd = 2,
      las = 1,
      cex = 1.2,
      mar = c(3,3,1,1),
      ...
    )
  } else {
    par(
      bty = "l",
      lwd = 2,
      las = 1,
      cex = 1.2,
      mar = c(5,4,3,1),
      ...
    )
  }
}

#' @title Rose's favourite ggplot theme settings
#' @desc Removes gridlines, adds axis lines and left aligns captions.
#' @param ... Accepts all the usual theme() params except those already set bby default.
#' @usage
#' plot_object + roses_ggtheme()
#' plot_object + roses_ggtheme(legend.position = "none")
#' plot_object + roses_ggtheme(legend.title = element_blank())
roses_set_ggtheme <- function(...){
  theme_set(theme_minimal())
  theme_update(
    plot.background = element_rect(
      colour = NA,
      fill = alpha(1, .05)
    ),
    axis.line = element_line(colour = 1),
    panel.background = element_rect(
      colour = NA,
      fill = "white"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_text(
      hjust=0, margin=margin(t=15)
    ),
    ...
  )
}

#' @title Rose's favourite unicodes
#' @desc These are all tried and tested as pch vals. Unicode characters work really nicely as pch vals with alpha transparency as they are one solid vector layer - the whole icon will be the same transparency and and will have nice high res smooth edges.
#' @param shorthand my nickname for the unicaode character
#' @return hexidecimal form of the unicode
#' @example roses_unicode("dot_open")
#' @example par(pch = roses_unicaode("dot_filled"))
roses_unicode <- function(shorthand){
  if(shorthand == "dot_open")
    code <- "25CB"

  if(shorthand == "dot_filled")
    code <- "25CF"

  return(-as.hexmode(code))
}
