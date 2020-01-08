#' Clean Location Name column data
#'
#' Cleans the LOCATION_NAME column by splitting the string at every ':' separator
#' and keeping the last substring (e.g. 'China: Shenzhen' -> 'Shenzhen').
#' Finally name is converted to title case.
#'
#' @import dplyr stringr purrr
#' @importFrom utils tail
#' @importFrom rlang .data
#'
#' @param df Data frame containing column LOCATION_NAME
#' @return Returns data frame with LOCATION_NAME variable cleaned.
eq_location_clean = function(df) {
  df %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_split(.data$LOCATION_NAME, ':') %>%
                                    purrr::map_chr(tail, n=1)) %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_squish(stringr::str_to_title(.data$LOCATION_NAME)))
}

#' Clean raw NOAA earthquake data
#'
#' Cleans raw NOAA earthquake data by:
#'   1/ conforming all variable data types
#'   2/ uniting the date information columns into a single DATE variable
#'   3/ cleaning the LOCATION_NAME information to format and keep only
#'      the specific site location excluding country, state, etc.
#'
#' @import dplyr tidyr
#' @importFrom lubridate ymd years days
#' @importFrom rlang .data
#' @export
#'
#' @param df Data frame containing raw NOAA significant earthquakes data.
#' @return Data frame containing the cleaned data as described in description.
#'
#' @examples
#' # note this example does not include all the columns in the NOAA dataset,
#' # but only the ones which eq_clean_data manipulates
#' raw_df = data.frame(FLAG_TSUNAMI='Tsu', COUNTRY='USA', STATE='',
#'                     LOCATION_NAME='USA: Pacific Islands',
#'                     YEAR=2020, MONTH=1, DAY=1, HOUR=NA, MINUTE=NA, SECOND=NA,
#'                     stringsAsFactors=FALSE)
#' clean_df = eq_clean_data(raw_df)
eq_clean_data = function(df) {
  # clean data types; all in double except 4 vars below
  out_df = df %>%
    dplyr::mutate_at(vars(-'FLAG_TSUNAMI', -'COUNTRY', -'STATE', -'LOCATION_NAME'),
              as.numeric)

  # add date column uniting the year, month, day and converting it to the Date class
  # note: lubridate converts (-) BCE years to (+);
  #       cannot also convert easily 2-, 3-, and 4-digit years all together in YYYY format
  #       so manually converting dates below
  out_df = out_df %>%
    tidyr::replace_na(list(MONTH = 1, DAY = 1)) %>%
    dplyr::mutate(DATE = ymd('0000-01-01') +
                         years(.data$YEAR) +
                         months(.data$MONTH - 1) +
                         days(.data$DAY - 1)) %>%
    dplyr::select(-'YEAR', -'MONTH', -'DAY', -'HOUR', -'MINUTE', -'SECOND')

  # clean LOCATION_NAME
  eq_location_clean(out_df)
}
