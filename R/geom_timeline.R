#' Visualize timeline of earthquakes
#'
#' Visualize the times at which earthquakes occur within certain countries,
#' along with the magnitude (Richter scale value) and number of deaths associated
#' with each earthquake.
#'
#' @import ggplot2 grid
#' @export
#'
#' @param mapping Set of aesthetic mappings created by `aes()`
#' @param data The data to be displayed in this layer
#' @param stat The statistical transformation to use on the data
#' @param position Position adjustment
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than combine
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed
#' @param ... other arguments passed on to `layer()`
#'
#' @section Aesthetics:
#' \describe{
#'   \item{x}{Date}
#'   \item{group}{(optional) country factor if displaying data for multiple countries;
#'      multiple countries will be shown on separate timelines}
#'   \item{size`}{(optional) size of the marker for each earthquake (e.g. magnitude)}
#'   \item{color}{(optional) color of the marker for each earthquake (e.g. number of deaths)}
#'   \item{alpha}{(optional) transparency channel for the markers}
#' }
#'
#' @examples
#' # The following example displays earthquake data for the USA from the years 2000 - 2016:
#'
#' \dontrun{
#' df %>%
#'   filter('2000-01-01' < DATE & DATE < '2017-01-01',
#'          COUNTRY == 'USA') %>%
#'   ggplot() +
#'   geom_timeline(aes(x = DATE, size=EQ_PRIMARY, color=DEATHS)) +
#'   scale_y_continuous(limits = c(0, 4)) +
#'   labs(y='', size='Richter scale value', color = '# deaths') +
#'   theme_timeline()
#' }
geom_timeline = function(mapping = NULL, data = NULL, stat = 'identity',
                          position = 'identity', show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_timeline
#' @import grid
#' @export
GeomTimeline = ggplot2::ggproto(
  'GeomTimeline', ggplot2::Geom,
  required_aes = 'x',
  default_aes = ggplot2::aes(alpha = 0.4, color = 'gray70', size=10),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(self, data, panel_scales, coord) {
    data$y = pmax(1, data$group)
    groups = split(data, factor(data$group))
    grobs = lapply(groups, function(group) self$draw_group(group, panel_scales, coord))
    grid::gTree(children = do.call('gList', grobs))
  },
  draw_group = function(self, data, panel_scales, coord) {
    coords = coord$transform(data, panel_scales)
    grid::gList(grid::linesGrob(x = grid::unit(c(0.025, 0.975), 'npc'),
                                y = grid::unit(coords$y, 'npc'),
                                gp = grid::gpar(col = 'gray',
                                                alpha = coords$alpha)),
                grid::pointsGrob(x = coords$x,
                                 y = coords$y,
                                 pch = 19,
                                 size = 3.25 * grid::unit(coords$size, 'points'),
                                 gp = grid::gpar(col = coords$colour,
                                                 alpha = coords$alpha))
    )
  }
)

#' Visualize timeline of earthquakes, labeling the location of the strongest
#'
#' Visualizes the times at which earthquakes occur within certain countries,
#' along with the magnitude (Richter scale value) and number of deaths associated
#' with each earthquake. Labels the locations of the strongest N (user specified)
#' earthquakes.
#'
#' @import ggplot2 grid
#' @export
#'
#' @inheritParams geom_timeline
#'
#' @section Aesthetics:
#' \describe{
#'   \item{label}{Character string variable with the annotation labels (e.g. LOCATION_NAME)}
#'   \item{n_max`}{Integer specifying the number of strongest earthquakes to annotate.}
#' }
#'
#' @examples
#' # The following example displays earthquake data for the USA and CHINA
#' # from the years 2000 - 2016, showing the timeline of each country on a separate,
#' # and labeling the strongest 5 earthquakes in each country.
#'
#' \dontrun{
#' df %>%
#'   filter('2000-01-01' < DATE & DATE < '2017-01-01',
#'          COUNTRY %in% c('USA', 'CHINA')) %>%
#'   ggplot() +
#'   geom_timeline_label(aes(x=DATE, group=COUNTRY, size=EQ_PRIMARY,
#'                           color=DEATHS, label=LOCATION_NAME)) +
#'   scale_y_continuous(limits = c(0, 4)) +
#'   labs(y='', size='Richter scale value', color = '# deaths') +
#'   theme_timeline()
#' }
geom_timeline_label = function(mapping = NULL, data = NULL, stat = 'identity',
                               position = 'identity', show.legend = NA,
                               na.rm = FALSE, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_timeline_label
#' @inherit GeomTimeline
#' @import dplyr grid
#' @export
GeomTimelineLabel = ggplot2::ggproto(
  'GeomTimelineLabel', GeomTimeline,
  required_aes = c('x', 'label'),
  default_aes = ggplot2::aes(alpha = 0.4, color = 'gray70', size = 10, # inhereted
                             n_max = 5),
  draw_group = function(self, data, panel_scales, coord) {
    # subset n_max datapoints
    df = data %>%
      dplyr::arrange(-size) %>%
      dplyr::slice(1:data$n_max[1])

    # transform coords for the subset only
    # (call to parent transforms the full data)
    coords = coord$transform(df, panel_scales)

    # call parent
    parent_grobs = GeomTimeline$draw_group(data, panel_scales, coord)

    grid::gList(parent_grobs,
                grid::segmentsGrob(x0 = coords$x, y0 = coords$y,
                                   x1 = coords$x, y1 = coords$y + 0.05,
                                   gp = grid::gpar(col = 'gray30',
                                                   alpha = coords$alpha)),
                grid::textGrob(label = coords$label,
                               x = coords$x, y = coords$y + 0.07,
                               rot = 45, just = 'left',
                               gp = grid::gpar(fontsize=9))
    )
  }
)

#' Custom theme for use with the earthquake timeline plots
#'
#' The theme properly formats the axes, background and gridlines.
#'
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' df %>%
#'   ggplot() +
#'   geom_timeline_label(aes(x=DATE, group=COUNTRY, size=EQ_PRIMARY,
#'                           color=DEATHS, label=LOCATION_NAME)) +
#'   scale_y_continuous(limits = c(0, 4)) +
#'   labs(y='', size='Richter scale value', color = '# deaths') +
#'   theme_timeline()
#' }
theme_timeline = function() {
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = 'bottom',
                 axis.line.x.bottom = element_line(),
                 axis.line.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())
}
