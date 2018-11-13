

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Clone of ggplot2:::empty()
#'
#' @param df data.frame
#'
#' @return TRUE if empty (NULL, no rows or no cols)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
empty <- function (df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Darken a hex colour by the given amount
#' Stolen from \url{https://benjaminlmoore.wordpress.com/2014/03/18/guardian-data-blog-uk-elections/}
#'
#' @param hex_colour strings e.g. "#345678"
#' @param amount fraction to darken by. default 0.15
#'
#' @return darkened hex colours
#'
#' @importFrom grDevices col2rgb rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
darken_colours <- function(hex_colour, amount = 0.15) {
  if (amount < 0 || amount > 1) {
    stop("darken_colours(): amount must be beween 0 and 1.")
  }
  return(rgb(t(col2rgb(hex_colour) * (1 - amount)), maxColorValue = 255))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' 3d pie charts in ggplot2
#'
#'
#' @inheritParams ggplot2::layer
#'
#' @param start_degrees starting angle for first pie slice (in degrees). Default: 0
#' @param tilt_degrees angle by which to tilt the pie towards the camera (in degrees). Default: 0.
#' @param height height of the pie. Default: 0.1
#' @param darken fraction by which to darken colours for the side of the pie. Default: 0.15
#' @param camera_eye location of camera eye. Default: c(0, 3, 5)
#' @param camera_look_at at what point is the camera looking. Default: c(0, 0, 0)
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @examples
#' \dontrun{
#'   ggplot(mtcars)  +
#'     geom_threedpie(aes(as.factor(cyl))) +
#'     coord_equal() +
#'     theme_minimal()
#' }
#'
#' @importFrom threed fortify.mesh3d
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_threedpie <- function(mapping        = NULL,
                           data           = NULL,
                           stat           = "count",
                           position       = "identity",
                           ...,
                           start_degrees  = 0,
                           tilt_degrees   = 0,
                           height         = 0.1,
                           darken         = 0.15,
                           camera_eye     = c(0, 3, 5),
                           camera_look_at = c(0, 0, 0),
                           na.rm          = FALSE,
                           show.legend    = NA,
                           inherit.aes    = TRUE) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set fill to be the 'x' variable unless already set by user.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!"fill" %in% names(mapping)) { mapping$fill   <- mapping$x }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Don't allow mapping colour. It is just mapped to be the same as the 'fill'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mapping$colour <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the layer
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomThreedPie,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      start_degrees  = start_degrees,
      tilt_degrees   = tilt_degrees,
      height         = height,
      darken         = darken,
      camera_eye     = camera_eye,
      camera_look_at = camera_look_at,
      na.rm          = na.rm,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a new ggproto object
#'
#' @section Calling methods in a parent:
#' To explicitly call a methods in a parent, use
#' `ggproto_parent(Parent, self)`.
#'
#' @param _class Class name to assign to the object. This is stored as the class
#'   attribute of the object. This is optional: if `NULL` (the default),
#'   no class name will be added to the object.
#' @param _inherit ggproto object to inherit from. If `NULL`, don't
#'   inherit from any object.
#' @param ... A list of members in the ggproto object.
#'
#' @format NULL
#' @usage NULL
#'
#' @import grid
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomThreedPie <- ggproto(
  "GeomThreedPie", Geom,


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Default aesthetics if none given
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  default_aes = aes(
    colour   = NA,
    fill     = NA,
    size     = 0.5,
    linetype = 1,
    alpha    = 1
  ),


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Function to handle NAs
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  handle_na = function(data, params) {
    data
  },


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Which aesthetics are absolutely REQUIRED otherwise there's an error
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  required_aes = c('x'),


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # How to draw a legend key
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  draw_key = draw_key_polygon,


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Drawing a particular panel
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  draw_panel = function(data, panel_params, coord, start_degrees, tilt_degrees,
                        height, darken, camera_eye, camera_look_at) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Enforce 'x' to be a discrete variable. This hack was the best I could do.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (all(data$group == -1)) {
      stop("geom_threedpie: 'x' must be discrete.")
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create the pie mesh3d objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    objs <- create_pie_objects(data$group, data$count,
                               start_degrees  = start_degrees,
                               tilt_degrees   = tilt_degrees,
                               height         = height,
                               camera_eye     = camera_eye,
                               camera_look_at = camera_look_at)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Convert the mesh3d objects to data.frames
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    pie     <- as.data.frame(objs$pie)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Drop the x + y from the prepared geomdata
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    summary_df <- data %>% select(-x, -y)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # But attach the rest to the polygon data.
    # Set the grouping variable to be the 'zorder' of the polygon so that
    # they get drawn in order from back to front
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygon_df <- pie %>%
      left_join(summary_df, by = 'group') %>%
      as.tbl() %>%
      mutate(group = zorder)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Darken the quads that make up the side of the pie
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polygon_df <- polygon_df %>%
      mutate(
        fill   = if_else(element_type == 4, darken_colours(fill, amount = darken), fill),
        colour = fill
      )

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add a bit of a buffer around the plotting range
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    panel_params$y.range <- extendrange(x = polygon_df$y)
    panel_params$x.range <- extendrange(x = polygon_df$x)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Calculate the new plot ratio
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    new_ratio <- diff(panel_params$y.range) / diff(panel_params$x.range)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Adjust the plot aspect ratio to match the pie chart dimensions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    coord$aspect <- function(...) {new_ratio}

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create the complete grobTree objects which make up the pie.
    # This was initially a more complicated collection of grobs.
    # Leaving in the grobTree()/nullGrob() calls for future expansion and a
    # template for other ggthreed geoms.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ggplot2:::ggname(
      "geom_threedpie",
      grobTree(
        GeomPolygon$draw_panel(polygon_df, panel_params, coord),
        grid::nullGrob()
      )
    )

  }  # end of draw_panel()
)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {
  library(grid)
  library(ggplot2)
  library(threed)
  library(ggthreed)
  library(dplyr)

  ggplot(mtcars)  +
    geom_threedpie(aes(as.factor(cyl)), tilt_degrees = -20) +
    coord_equal() +
    theme_minimal() +
    # theme(legend.position = 'none') +
    NULL


  ggplot(diamonds)  +
    geom_threedpie(aes(as.factor(cut)), tilt_degrees = 0) +
    theme_minimal() +
    facet_wrap(~clarity, labeller = label_both) +
    labs(
      title = "Distribution of Diamond Cuts by Clarity"
    ) +
    scale_fill_brewer(name = "Cut", palette = 'Dark2') +
    NULL
}

