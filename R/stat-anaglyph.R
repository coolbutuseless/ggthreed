

'%||%' <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Red-blue anaglyph
#'
#' Split data to create red-blue anaglyph
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @param zoffset,zscale how to offset the \code{x} coordinates based upon the
#' z Default zoffset = 0, zscale = 1
#' @param zinvert Does z get bigger as a point moves further away. default: TRUE
#' @param red,blue colours to use for the anaglyph
#' @param switch switch sides for red/blue?  default: FALSE
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(mtcars) +
#'    geom_point(aes(wt, mpg, z = disp), stat = 'anaglyph') +
#'    theme_bw()
#' }
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stat_anaglyph <- function(mapping     = NULL,
                          data        = NULL,
                          geom        = "point",
                          position    = "identity",
                          zoffset     = 0.0,
                          zscale      = 1.0,
                          zinvert     = TRUE,
                          red         = "#ff0000",
                          blue        = "#00fffb",
                          switch      = FALSE,
                          ...,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping     = mapping,
    stat        = StatAnaglyph,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      zoffset = zoffset,
      zscale  = zscale,
      zinvert = zinvert,
      red     = red,
      blue    = blue,
      switch  = switch,
      na.rm   = TRUE,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' StatAnaglyph
#'
#' @format NULL
#' @usage NULL
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StatAnaglyph <- ggproto("StatAnaglyph", Stat,

  required_aes = c("x", "z"),
  default_aes = aes(
    zoffset = 0.0,
    zscale  = 1,
    zinvert = TRUE,
    switch  = FALSE,
    red     = "#ff0000",
    blue    = "#00fffb"
  ),

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Need to set arguments here so ggplot is aware of the params
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  compute_group = function(data, scales, zoffset, zscale, switch, red, blue, zinvert) {
    data
  },

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # All the work for this Stat happens in the finish_layer
  # i.e. all the stats have been adjusted etc
  # In here we duplicate the entire dataset, ensure the groups are disjoint
  # and the colours to red/blue in the 2 datasets
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  finish_layer = function(self, data, params) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Set a 'z' var if there isn't one
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!exists('z', data)) {
      data$z <- 1
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Set a 'group' var if there isn't one
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!exists('group', data)) {
      data$group <- 1
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Ensure groups start at 1, and then find the max_group
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data      <- transform(data, group = group - min(group) + 1L)
    max_group <- max(data$group, na.rm = TRUE)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Set default values for unset params
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    params$zoffset <- params$zoffset %||% self$default_aes$zoffset
    params$zscale  <- params$zscale  %||% self$default_aes$zscale
    params$zinvert <- params$zinvert %||% self$default_aes$zinvert
    params$switch  <- params$switch  %||% self$default_aes$switch
    params$red     <- params$red     %||% self$default_aes$red
    params$blue    <- params$blue    %||% self$default_aes$blue

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If 'switch' is TRUE, swap the red and the blue
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (params$switch) {
      params$zscale <- params$zscale * -1
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate the adjustment of the x value depending on the z value
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (params$zinvert) {
      data <- transform(data, zadjust = (params$zscale / (z + params$zoffset)))
    } else {
      data <- transform(data, zadjust = (params$zscale * (z + params$zoffset)))
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Transform the data into red/blue versions
    # Adjust the x coord of the points based upon the z
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    red_data  <- transform(data, x = x - zadjust, colour = params$red)
    blue_data <- transform(data, x = x + zadjust, colour = params$blue, group = group + max_group)


    rbind(red_data, blue_data)
  }
)



if (interactive()) {
  library(ggplot2)
  library(ggthreed)

  ggplot(mtcars) +
    geom_line(aes(mpg, y = wt, z = disp), stat = 'anaglyph', switch = TRUE,
               zscale = 10, alpha = 0.5) +
    theme_bw()

}


