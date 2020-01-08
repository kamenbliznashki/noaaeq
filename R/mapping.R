#' Construct `leaflet` popup text labels in html given data frame of earthquake data
#'
#' @import dplyr
#' @importFrom rlang .data
#' @export
#'
#' @param df Data frame containing cleaned earthquake data.
#' @return Character vector of the html.
#'
#' @examples
#' # return a character vector of the html code for leaflet labels,
#' # given a data frame of the cleaned data
#' df = data.frame(LOCATION_NAME='USA', EQ_PRIMARY=1, TOTAL_DEATHS=0,
#'                 stringsAsFactors=FALSE)
#' labels = eq_create_label(df)
eq_create_label = function(df) {
  df %>%
    dplyr::transmute(popup_text = ifelse(
      is.na(.data$LOCATION_NAME) | is.na(.data$EQ_PRIMARY) | is.na(.data$TOTAL_DEATHS),
      NA,
      paste('<b>Location:</b>', .data$LOCATION_NAME, '<br />',
            '<b>Magnitude:</b>', .data$EQ_PRIMARY, '<br />',
            '<b>Total deaths:</b>', .data$TOTAL_DEATHS, '<br />'))) %>%
    dplyr::pull()
}


#' Visualize signficant earthquakes
#'
#' Visualize a `leaflet` map with locations and data of signficant earthquakes,
#' based on longitude, latitude, earthquake strength and number of deaths.
#'
#' @import leaflet
#' @export
#'
#' @param df Data frame containing cleaned earthquake data.
#' @param annot_col Character string of the column name in `df` containing
#'   popup annotation text (e.g. output of eq_create_label or an existing column
#'   of the data frame `df`)
#'
#' @examples
#' # The following example loads, cleans, and displays earthquakes in Mexico
#' # occurred after the year 2000:
#' \dontrun{
#' require(readr)
#' require(dplyr)
#'
#' read_delim('earthquakes.txt', delim = '\t') %>%
#'   eq_clean_data %>%
#'   filter(COUNTRY == 'MEXICO' & DATE >= '2000-01-01') %>%
#'   mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = 'popup_text')
#' }
eq_map = function(df, annot_col = 'DATE') {
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = df,
                              radius = ~ EQ_PRIMARY,
                              lng = ~ LONGITUDE,
                              lat = ~ LATITUDE,
                              weight = 1,
                              popup = '[['(df, annot_col))

}
