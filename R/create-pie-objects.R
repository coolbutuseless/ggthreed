
globalVariables('label')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a set of mesh3d objects representing a pie chart
#'
#' @param labels the labels for the segments
#' @param counts the count of occurrences for each label
#' @param start_degrees starting angle for first pie slice (in degrees). Default: 0
#' @param tilt_degrees angle by which to tilt the pie towards the camera (in degrees). Default: 0
#' @param height height of the pie. Default: 0.1
#' @param camera_eye location of camera eye. Default: c(0, 3, 5)
#' @param camera_look_at at what point is the camera looking. Default: c(0, 0, 0)
#'
#' @return mesh3dlist of objects
#'
#' @import dplyr
#' @import threed
#' @importFrom utils head tail
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pie_objects <- function(labels, counts,
                               start_degrees  = 0,
                               tilt_degrees   = 0,
                               height         = 0.1,
                               camera_eye     = c(0, 3, 5),
                               camera_look_at = c(0, 0, 0)) {


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # What are all the angles that have a point on the circumference.
  # Bigger numbers mean a coarser looking pie.
  # Can't see any artefacts at 2 degrees, so going to use that as the slice
  # size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  angle <- seq(0, 359, 2)
  N     <- length(angle)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the counts into an angle cutoff
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  angle_cutoffs <- round(360 * counts/sum(counts), 0)
  groups        <- cut(angle, breaks = cumsum(c(0, angle_cutoffs)), labels = FALSE, include.lowest = TRUE)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Apply extra rotation by 'start degrees'
  # - Initial 90 rotation is to start the pie at the 12 o'clock position rather
  #   than the 3 o'clock position
  # - Reverse angle so that pie-pieces go clockwise-by-label
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  angle <- -(angle - 90 + start_degrees)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # (x, y) coordinates around circumference
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- cos(angle * pi/180)
  y <- sin(angle * pi/180)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Matrix of vertices for the faces on the top of the pie
  # - Put the centre index at the start
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertices_top <- t(unname(cbind(x, y, z = 0, w = 1)))
  vertices_top <- cbind(c(0, 0, 0, 1), vertices_top)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix of vertices for the edge of the bottom of the pie
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vertices_bot     <- vertices_top
  vertices_bot[3,] <- -abs(height)

  vertices <- cbind(vertices_top, vertices_bot)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Matrix of vertex indices for each triangular face on top
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rotr <- function(x) {c(tail(x,  1), head(x, -1))}
  rotl <- function(x) {c(tail(x, -1), head(x,  1))}

  it1 <- seq(1, N)
  it2 <- rotl(it1)

  it1 <- it1 + 1L  # offset from first vertex which is the zero point
  it2 <- it2 + 1L  # offset from first vertex which is the zero point

  it  <- t(unname(cbind(it1, it2, 1L)))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Vertex indices for quads around side of the pie
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ib1 <- seq(2, N + 1)
  ib2 <- rotl(ib1)

  ib3 <- ib1 + ncol(vertices_top)
  ib4 <- rotl(ib3)

  ib  <- t(unname(cbind(ib1, ib2, ib4, ib3)))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Properties for each element. Needs 1 row for each tri element on top and
  # each quaad along the side
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  properties <- data_frame(
    label = c(labels[groups], labels[groups]),
    group = as.integer(as.factor(label))
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a mesh3d object of the pie
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pie_obj <- list(
    vb            = vertices,
    it            = it,
    ib            = ib,
    primitivetype = 'quad',
    material      = list(),
    properties    = properties,
    texcoords     = NULL
  )

  class(pie_obj) <- c('mesh3d', 'shaped3d')


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Define where the camera is looking
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  camera_to_world <- threed::look_at_matrix(eye = camera_eye, at = camera_look_at)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # - Initial xyz space is xy in the screen plane and z coming out of the screen
  #   i.e. right-handed coordinate system.
  # - Rotate the pie from the x/y into the x/z plane i.e. laying flat.
  # - Further rotate the pie by the tilt angle, where a positive tile brings the
  #   back edge of the pie towards the camera
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tilt_radians <- tilt_degrees * pi/180
  pie_obj <- pie_obj %>%
    rotate_by(-pi/2 + tilt_radians, c(1, 0, 0)) %>%
    transform_by(invert_matrix(camera_to_world)) %>%
    perspective_projection()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create and return a mesh3dlist of objects. In this case there's only
  # one object, but for more complex 3d plot types there may be more.
  # Also I may want to add text labels to the pie slices later.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mesh3dlist(
    pie = pie_obj
  )
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Testing zone. Render a pie object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (FALSE) {

  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)

    library(threed)
  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Labels and counts to be plotted
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  labels <- letters[1:5]
  counts <- c(1, 2, 3, 1, 1)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the pie chart objects
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  objs <- create_pie_objects(labels, counts, tilt_degrees = 30)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Plot the pi
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggplot() +
    geom_polygon(data = objs$pie, aes(x, y, group = zorder, fill=label, colour=label)) +
    coord_equal() +
    theme_void()
}














