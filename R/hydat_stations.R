

#' Find HYDAT Data Stations
#'
#' @param x A character vector of a place name (geocoded by \link[prettymapr]{geocode}),
#'   or a numeric vector of length 2 (lon, lat).
#' @param year Optional range of years that must be included in the station data
#' @param limit This number of closest stations will be returned
#' @param db The hydat database object to use (you will want to use
#'   \link{hydat_load} before using these functions)
#'
#' @return A data.frame (tibble) of station information
#' @export
#'
#' @examples
#' hydat_load_test_db()
#' hydat_find_stations(c(-70.08, 46.55))
#' hydat_find_stations(c(-70.08, 46.55), year = 1988:1991)
#'
#' hydat_find_stations("Daaquam, QC")
#' hydat_find_stations("Daaquam, QC", year = 1988:1991)
#'
#'
#' @importFrom dplyr everything
#'
hydat_find_stations <- function(x, year = NULL, limit = 10, db = hydat_get_db()) {
  if(!is_hydat(db)) stop("db must be a valid src_hydat loaded using hydat_load()")

  if(is.character(x) && length(x) == 1) {
    loc <- suppressMessages(prettymapr::geocode(x, progress = "none", quiet = TRUE))[c("lon", "lat")]
  } else if(is.numeric(x) && length(x) == 2) {
    loc <- data.frame(lon = x[1], lat = x[2])
  } else {
    stop("x must be a character vector of length 1 or a numeric vector of length 2")
  }

  # calculate ranges
  YEAR_TO <- NULL; rm(YEAR_TO); YEAR_FROM <- NULL; rm(YEAR_FROM)
  year_range_stations <- dplyr::tbl(db, "STN_DATA_RANGE") %>%
    dplyr::group_by_("STATION_NUMBER") %>%
    dplyr::summarise(YEAR_FROM = min(YEAR_FROM), YEAR_TO = max(YEAR_TO))

  LONGITUDE <- NULL; rm(LONGITUDE); LATITUDE <- NULL; rm(LATITUDE)
  stations <- dplyr::tbl(db, "STATIONS") %>%
    dplyr::select_("STATION_NUMBER", "PROV_TERR_STATE_LOC", "STATION_NAME", "LATITUDE", "LONGITUDE",
                  "DRAINAGE_AREA_GROSS") %>%
    dplyr::left_join(year_range_stations, by = "STATION_NUMBER") %>%
    dplyr::collect() %>%
    dplyr::mutate(dist_from_query_km = geodist(loc$lon, loc$lat, LONGITUDE, LATITUDE) / 1000) %>%
    dplyr::arrange_("dist_from_query_km") %>%
    dplyr::select_("STATION_NUMBER", "dist_from_query_km", "STATION_NAME", FIRST_YEAR = "YEAR_FROM",
                  LAST_YEAR = "YEAR_TO", everything())

  # filter by year
  FIRST_YEAR <- NULL; rm(FIRST_YEAR); LAST_YEAR <- NULL; rm(LAST_YEAR)
  if(!is.null(year)) {
    max_year <- max(year)
    min_year <- min(year)
    stations <- stations %>%
      dplyr::filter(FIRST_YEAR <= min_year & LAST_YEAR >= max_year)
  }

  # return only limit number of rows
  stations %>% utils::head(limit)
}


#' Get Detailed Station Info
#'
#' @param stationid A station identifier, or NULL for all stations
#' @param db db The hydat database object to use (you will want to use
#'   \link{hydat_load} before using these functions)
#'
#' @return A data.frame (tibble) with one row per station
#' @export
#'
#' @examples
#' hydat_load_test_db()
#' hydat_station_info()
#' hydat_station_info("01AD012")
#' as.list(hydat_station_info("01AD012"))
#'
hydat_station_info <- function(stationid = NULL, db = hydat_get_db()) {
  if(!is_hydat(db)) stop("db must be a valid src_hydat loaded using hydat_load()")

  # calculate ranges
  YEAR_FROM <- NULL; rm(YEAR_FROM); YEAR_TO <- NULL; rm(YEAR_TO)
  year_range_stations <- dplyr::tbl(db, "STN_DATA_RANGE") %>%
    dplyr::group_by_("STATION_NUMBER") %>%
    dplyr::summarise(YEAR_FROM = min(YEAR_FROM), YEAR_TO = max(YEAR_TO))

  # join stations tables together
  stations <- dplyr::tbl(db, "STATIONS") %>%
    dplyr::left_join(dplyr::tbl(db, "REGIONAL_OFFICE_LIST"), by = "REGIONAL_OFFICE_ID") %>%
    dplyr::left_join(dplyr::tbl(db, "STN_STATUS_CODES"), by = c("HYD_STATUS" = "STATUS_CODE")) %>%
    dplyr::left_join(dplyr::tbl(db, "STN_STATUS_CODES"), by = c("SED_STATUS" = "STATUS_CODE"),
              suffix = c("_HYD", "_SED")) %>%
    dplyr::left_join(dplyr::tbl(db, "AGENCY_LIST"), by = c("CONTRIBUTOR_ID" = "AGENCY_ID")) %>%
    dplyr::left_join(dplyr::tbl(db, "AGENCY_LIST"), by = c("OPERATOR_ID" = "AGENCY_ID"),
              suffix = c("_CONTRIBUTOR", "_OPERATOR")) %>%
    dplyr::select_("STATION_NUMBER", "STATION_NAME", "PROV_TERR_STATE_LOC", "LATITUDE", "LONGITUDE",
           "DRAINAGE_AREA_GROSS", "DRAINAGE_AREA_EFFECT", "STATUS_EN_HYD", "STATUS_EN_SED",
           "REGIONAL_OFFICE_NAME_EN", "AGENCY_EN_CONTRIBUTOR", "AGENCY_EN_OPERATOR",
           "RHBN", "REAL_TIME", "DATUM_ID")

  if(!is.null(stationid)) {
    STATION_NUMBER <- NULL; rm(STATION_NUMBER)
    stations <- stations %>% dplyr::filter(STATION_NUMBER == stationid)
  }

  # join with year ranges and collect
  stations %>%
    dplyr::left_join(year_range_stations, by = "STATION_NUMBER") %>%
    dplyr::rename_(FIRST_YEAR = "YEAR_FROM", LAST_YEAR = "YEAR_TO") %>%
    dplyr::collect()
}
