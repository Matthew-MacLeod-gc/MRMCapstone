library("lubridate")
library("dplyr")
library("tidyr")
library("stringr")
library("grid")
library("ggplot2")
library("readr")
library("leaflet")
library("magrittr")


#' Theme for an earthquake timeline
#'
#' Arranges the legend and other settings in a pleasing way for an earthquake timeline
#' created with \code{\link{geom_timeline}}.
#'
#' @return A theme for an earthquake timeline.
#'
#' @examples
#' \dontrun{
#'   cleanData %>% ggplot(aes(x = DATE, size=EQ_PRIMARY, fill = DEATHS, label = LOCATION_NAME)) + geom_timeline(alpha = 0.25, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) + guides(size = guide_legend(title = "Richter scale value"), fill = guide_colourbar(title = "# Deaths")) + theme_quakes()
#' }
#'
#'
#' @export
theme_quakes <- function() {
  # Starts with theme_minimal and then modify some parts
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # legend.key       = element_rect(fill = "white", colour = NA),
      legend.position = "bottom",
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.y = element_blank(),
      axis.title.y = element_blank(),
      axis.line.x = element_line(),
      axis.ticks.x = element_line(),

      complete = FALSE
    )
}


#' Clean earthquake location name
#'
#' Cleans up a passed location string to remove the initial country
#' name and colon, and convert to title case.
#'
#' @param locationName A location name to be cleaned.
#'
#' @return A location name in title case, without the country name.
#'
#' @examples
#' hello_world(locationName = "UNITED KINGDOM: CHANNEL ISLANDS; GAUL")
#'
#' @importFrom dplyr %>%
#'
#' @export
eq_location_clean <- function(locationName){
  name <- stringr::str_replace(locationName,".*:", "") %>% stringr::str_trim(side="left") %>% stringr::str_to_title()
}

#' Clean earthquake data
#'
#' Takes in earthquake data, converts the year/month/day columns into a unified
#' date column, ensures the latitude and longitude are numeric, and cleans
#' up the location names.
#'
#' @param df A dataframe containing earthquake information.
#'
#' @return The dataframe with cleaned date and location columns.
#'
#' @examples
#' \dontrun{cleanData <- read_tsv("signif.txt") %>% eq_clean_data()}
#'
#' @importFrom dplyr %>%
#'
#' @export
eq_clean_data <- function(df) {
  df %>% tidyr::unite(DATE, YEAR, MONTH, DAY) %>% dplyr::mutate(DATE= ymd(DATE, quiet=TRUE), LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)) %>% dplyr::mutate(LOCATION_NAME = eq_location_clean(LOCATION_NAME))

}



#' Plot earthquake data on a timeline.
#'
#' Function that implements a class to visualize earthquake data, as
#' described in function \code{\link{geom_timeline}}.
#'
#'
#' @param required_aes The aesthetics that must be specified for this visualization, including minimum and maximum years (x values).
#' @param default_aes The default values for this visualization, including a shape that takes a fill colour, and a 0.5 alpha transparency.
#' @param draw_key The function with which to draw the legend
#' @param draw_panel The function which primarily implements drawing the visualization.
#' @param setup_data Pre-cleans the data, including filtering for the minimum and maximum value.
#'
#' @return Returns a grid grob containing the earthquake timeline as a layer
#' @examples
#' \dontrun{  ggplot2::layer(geom = GeomTimeLine, mapping = mapping, data = data, stat = stat, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, nmax = nmax, xmin = xmin, xmax = xmax, ...))}
GeomTimeLine <- ggplot2::ggproto("GeomTimeLine", Geom,
                   required_aes = c("x", "xmin", "xmax"),
                   default_aes = aes(y = 0, shape = 21, size = 0.5, stroke = 1, colour = "grey20", fill = "grey20",
                                    alpha = 0.5, date = NULL),
                   draw_key = draw_key_point,
                   setup_data = function(data, params) {
                     data <- data[is.finite(data$x), ]
                     # Restrict to dates between xmin and xmax
                    data <- data[data$x >= as.numeric(as.Date(params$xmin)), ]
                     data <- data[data$x <= as.numeric(as.Date(params$xmax)), ]

                     data
                   },
                   draw_panel = function(data, panel_params, coord, xmin, xmax) {


                     ## Transform the data first
                     coords <- coord$transform(data, panel_params)

                     myLines <- grid::segmentsGrob(
                       x0 = min(coords$x), x1 = max(coords$x),
                       y0 = coords$y, y1 = coords$y,
                       gp = grid::gpar(
                         col = "grey",
                         lwd = 1 * .pt
                       )
                     )


                     myPoints <- grid::pointsGrob(
                       x = coords$x,
                       y = coords$y,
                       pch = coords$shape,
                       size = unit(coords$size, "mm"),
                       gp = grid::gpar(alpha = coords$alpha, stroke = coords$stroke,
                                       colour = coords$colour, fill = coords$fill)
                     )

                     grid::gTree(children = grid::gList(myLines, myPoints))

                   }
)

#' Create a visualization of earthquakes on a timeline.
#'
#' This function creates a visualization layer for plotting earthquakes on a timeline, with
#' their magnitude as the size of the point, and their death toll as the colour intensity.
#' Optionally, separate timelines can be created by grouping, e.g., by country.
#'
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_. Must include 'x' values as dates. Deaths can be supplied as 'fill', and intensity as 'size.'  'y' can be used for a grouping variable.
#' @param xmin The earliest date to display
#' @param xmax The latest date to display
#' @param data The data to be displayed in this layer. Must be earthquake data as specified above.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#'
#' @examples
#' \dontrun{
#'   cleanData %>% ggplot(aes(x = DATE, size=EQ_PRIMARY, fill = DEATHS, label = LOCATION_NAME)) + geom_timeline(alpha = 0.25, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) + guides(size = guide_legend(title = "Richter scale value"), fill = guide_colourbar(title = "# Deaths")) + theme_quakes()
#' }
#'
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, xmin = NULL, xmax = NULL, ...) {
  ggplot2::layer(
    geom = GeomTimeLine, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, xmin = xmin, xmax = xmax, ...)
  )
}

#' Label earthquake data on a timeline.
#'
#' Function that implements a class to label earthquake data, as
#' described in function \code{\link{geom_timeline}}.
#'
#'
#' @param required_aes The aesthetics that must be specified for this visualization, including minimum and maximum years (x values), and a maximum number of earthquakes to label.
#' @param default_aes The default values for this visualization, including a shape that takes a fill colour, and a 0.5 alpha transparency.
#' @param draw_key The function with which to draw the legend
#' @param draw_panel The function which primarily implements drawing the visualization.
#' @param setup_data Pre-cleans the data, including filtering for the minimum and maximum value.
#'
#' @return Returns a grid grob containing the earthquake labels as a layer
#' @examples
#' \dontrun{  ggplot2::layer(geom = GeomTimeLineLabel, mapping = mapping, data = data, stat = stat, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, nmax = nmax, xmin = xmin, xmax = xmax, ...))}
#'
#' @importFrom dplyr %>%
#'
GeomTimeLineLabel <- ggproto("GeomTimeLineLabel", Geom,
                        required_aes = c("x", "label", "nmax", "xmin", "xmax"),
                        default_aes = aes(nmax = 10, xmin = NULL, xmax = NULL),
                        draw_key = draw_key_point,
                        setup_data = function(data, params) {
                          data <- data[is.finite(data$x), ]
                          # Restrict to dates between xmin and xmax
                          if (!is.null(params$xmin)) data <- data[data$x >= as.numeric(as.Date(params$xmin)), ]
                          if (!is.null(params$xmax)) data <- data[data$x <= as.numeric(as.Date(params$xmax)), ]

                          # Keep only ten biggest (discarding ties)
                            data <- data %>% dplyr::arrange(desc(size)) %>% utils::head(10)
                            data
                          },
                        draw_panel = function(data, panel_params, coord, nmax) {

                          if (!("y" %in% colnames(data))) {
                            data$y <- 0
                          }

                          ## Transform the data first
                          coords <- coord$transform(data, panel_params)



                          offset <- 0.1


                          myLines <- grid::segmentsGrob(
                            x0 = coords$x,
                            y0 = coords$y,
                            x1 = coords$x,
                            y1 = coords$y + offset,
                            gp = grid::gpar(
                              col = "black",
                              lty = coords$lty,
                              lwd = coords$lwd
                            )
                          )


                          myNames <- grid::textGrob(
                            label = coords$label,
                            x = unit(coords$x, "npc"),
                            y = unit(coords$y + offset, "npc"),
                            just = c("left", "bottom"),
                            rot = 45
                          )

                          grid::gTree(children = grid::gList(myLines,myNames))

                        }
)


#' Label earthquakes on a timeline.
#'
#' This function creates a visualization layer for labelling earthquakes
#' plotted on a timeline using \code{\link{geom_timeline}}.
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_. Must include 'x' values as dates. Deaths can be supplied as 'fill', and intensity as 'size.'  'y' can be used for a grouping variable.
#' @param xmin The earliest date to display
#' @param xmax The latest date to display
#' @param data The data to be displayed in this layer. Must be earthquake data as specified above.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#'
#' @examples
#' \dontrun{
#'   cleanData %>% ggplot(aes(x = DATE, size=EQ_PRIMARY, fill = DEATHS, label = LOCATION_NAME)) + geom_timeline(alpha = 0.25, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) + geom_timeline_label(nmax = 10, xmin = ymd('2000-01-01'), xmax = ymd('2015-12-31')) + guides(size = guide_legend(title = "Richter scale value"), fill = guide_colourbar(title = "# Deaths")) + theme_quakes()
#' }
#'
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, nmax = NULL,
                          xmin = NULL, xmax = NULL, ...) {
  ggplot2::layer(
    geom = GeomTimeLineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, nmax = nmax, xmin = xmin, xmax = xmax, ...)
  )
}

#' Plot earthquakes on a leaflet map.
#'
#' Takes a data frame containing earthquake data, as well as a specified column,
#' and creates a leaflet map plotting earthquakes with size based on their intensity, and
#' a pop up label containing the information in the specified column.
#'
#' @param df The earthquake data
#' @param annot_col The name of the column to display in the pop up.
#'
#' @return A leaflet map.
#'
#' @examples
#' \dontrun{
#'   cleanData %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% eq_map(annot_col = "DATE")
#' }
#'
#' @importFrom dplyr %>%
#'
#' @export
eq_map <- function (df, annot_col) {
  leaflet::leaflet() %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = df, lng = ~ LONGITUDE, lat = ~ LATITUDE,
    radius = ~ EQ_PRIMARY, weight = 0.5, popup = ~df[[annot_col]] )
}

#' Create an informative pop up label.
#'
#' Takes a data frame containing earthquake data, and creates an informative HTML
#' label to be used on a leaflet map, containing magnitude, death toll, and location
#' information.
#'
#' @param df The earthquake data
#'
#' @return An HTML formatted text string.
#'
#' \dontrun{
#'   cleanData %>% dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% dplyr::mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function (df) {
 label <- ifelse(is.na(df$LOCATION_NAME),"",paste0("<B>Location:</B> ", df$LOCATION_NAME,"<br>"))
 label <- ifelse(is.na(df$EQ_PRIMARY),label,paste0(label,"<B>Magnitude:</B> ", df$EQ_PRIMARY,"<br>"))
 label <- ifelse(is.na(df$TOTAL_DEATHS),label,paste0(label,"<B>Total deaths:</B> ", df$TOTAL_DEATHS,"<br>"))
}

