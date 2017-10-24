#' GeomTimeline
#'
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom grid gList
#' @importFrom grid linesGrob
#' @importFrom grid pointsGrob
#' @importFrom grid gpar
#'
#' @param
#'
#'
#' @description
#'
#' @example
#'
#' @export

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,

                        required_aes = c("x"),

                        default_aes = ggplot2::aes(
                          colour = "black",
                          size = 10,
                          alpha = 0.8,
                          shape = 21,
                          fill = "black"
                        ),

                        draw_key = ggplot2::draw_key_point,


                        draw_panel = function(data, panel_scales, coord) {

                          coords <- coord$transform(data, panel_scales)

                          Grob_list = grid::gList()

                          y_country_lines <- unique(coords$y)

                          Grob_line_temp <- sapply(y_country_lines, grid::linesGrob,
                                                   x = c(min(coords$x), max(coords$x)),
                                                   gp = grid::gpar(alpha = 0.4, lwd=3))

                          Grob_list = gList(Grob_list, Grob_line_temp)


                          points <- grid::pointsGrob(
                            coords$x,
                            coords$y,
                            pch = coords$shape,
                            gp = grid::gpar(col = coords$colour,
                                            fill = coords$fill,
                                            alpha = coords$alpha)
                          )

                         grid::glist(Grob_list, points)
                        }


                        )
