
#' Retreive HYDAT Data
#'
#' @param stationid The station identifier(s) for which data should be loaded
#' @param year The year or range of years for which data should be loaded, or NULL for all
#' @param month The month or range of months for which data should be loaded
#' @param day The day or range of days for which data should be loaded
#' @param db The hydat database object to use (you will want to use
#'   \link{hydat_load} before using these functions)
#'
#' @return A data.frame (tibble, actually) of observations
#' @export
#' @rdname hydat_data
#'
#' @examples
#' hydat_load_test_db()
#' hydat_flow_monthly("01AD001")
#' hydat_flow_daily("01AD001")
#' hydat_level_monthly("01AD003")
#' hydat_level_daily("01AD003")
#' hydat_sed_monthly("01AF006")
#' hydat_sed_daily("01AF006")
#'
hydat_flow_monthly <- function(stationid, year=NULL, month=1:12, db = hydat_get_db()) {
  hydat_data_monthly("DLY_FLOWS", stationid=stationid, year=year, month=month, db = db)
}

#' @rdname hydat_data
#' @export
hydat_level_monthly <- function(stationid, year=NULL, month=1:12, db = hydat_get_db()) {
  hydat_data_monthly("DLY_LEVELS", stationid=stationid, year=year, month=month, db = db)
}

#' @rdname hydat_data
#' @export
hydat_sed_monthly <- function(stationid, year=NULL, month=1:12, db = hydat_get_db()) {
  hydat_data_monthly("SED_DLY_LOADS", stationid=stationid, year=year, month=month, db = db)
}

#' @rdname hydat_data
#' @export
hydat_flow_daily <- function(stationid, year=NULL, month=1:12, day=1:31, db = hydat_get_db()) {
  hydat_data_daily("DLY_FLOWS", "FLOW", "FLOW_SYMBOL", stationid=stationid,
                   year=year, month=month, day=day, db = db)
}

#' @rdname hydat_data
#' @export
hydat_level_daily <- function(stationid, year=NULL, month=1:12, day=1:31, db = hydat_get_db()) {
  hydat_data_daily("DLY_LEVELS", "LEVEL", "LEVEL_SYMBOL", stationid=stationid,
                   year=year, month=month, day=day, db = db)
}

#' @rdname hydat_data
#' @export
hydat_sed_daily <- function(stationid, year=NULL, month=1:12, day=1:31, db = hydat_get_db()) {
  hydat_data_daily("SED_DLY_LOADS", "LOAD", NULL, stationid=stationid,
                   year=year, month=month, day=day, db = db)
}


# ---- these functions do all the work for the above functions

hydat_data_base <- function(table, stationid, cols, year=NULL, month=1:12, db = hydat_get_db()) {

  # check db
  if(is.null(db)) stop("hydat db is not loaded. Did you forget to run `hydat_load()`?")
  if(!is_hydat(db)) stop("db must be a valid src_hydat loaded using hydat_load()")

  # hack so that easy dplyr column names can keep on keepin' on
  STATION_NUMBER <- NULL; rm(STATION_NUMBER); YEAR <- NULL; rm(YEAR)
  MONTH <- NULL; rm(MONTH)

  # get max/min year range for query
  if(is.null(year)) {
    year_min <- 1813
    year_max <- lubridate::year(Sys.Date())
    year <- year_min:year_max
  } else {
    year_min <- min(year)
    year_max <- max(year)
  }
  month_min <- min(month)
  month_max <- max(month)

  # user plyr to vectorize by stationid, since the %in% operator doesn't work for sqlite

  monthly <- plyr::adply(data.frame(STATION_NUMBER=stationid), 1, .fun=function(row) {
    # check that station exists
    df <- dplyr::tbl(db, table) %>%
      dplyr::filter(STATION_NUMBER == row$STATION_NUMBER)

    df_head <- df %>% utils::head() %>% dplyr::collect()
    if(nrow(df_head) == 0) stop("Station '", row$STATION_NUMBER, "' does not exist in table '",
                                table, "'")

    # select appropriate rows
    df <- df %>%
      dplyr::filter(MONTH >= month_min, MONTH <= month_max,
                    YEAR >= year_min, YEAR <= year_max) %>%
      dplyr::left_join(dplyr::tbl(db, "STATIONS"), by="STATION_NUMBER")

    # select requires do.call, best to do this here to keep from returning
    # too much data

    df <- do.call(dplyr::select_, c(list(df), cols)) %>%
      dplyr::arrange(YEAR, MONTH) %>%
      dplyr::collect() %>%
      dplyr::filter(MONTH %in% month, YEAR %in% year) %>%
      tibble::as_tibble()

    # return df
    df
  })
}

hydat_data_monthly <- function(table, stationid, year=NULL, month=1:12, db = hydat_get_db()) {

  # hack so that easy dplyr column names can keep on keepin' on
  STATION_NUMBER <- NULL; rm(STATION_NUMBER); YEAR <- NULL; rm(YEAR)
  MONTH <- NULL; rm(MONTH); STATION_NAME <- NULL; rm(STATION_NAME)
  MONTHLY_MEAN <- NULL; rm(MONTHLY_MEAN); MIN <- NULL; rm(MIN)
  MAX <- NULL; rm(MAX); DATE <- NULL; rm(DATE)
  MONTHLY_TOTAL <- NULL; rm(MONTHLY_TOTAL)


  # get data
  df <- hydat_data_base(table, cols=c("STATION_NUMBER", "STATION_NAME", "YEAR", "MONTH",
                                "MONTHLY_MEAN", "MONTHLY_TOTAL", "MIN", "MAX"),
                  stationid=stationid, year=year, month=month,
                  db = db)

  # rename ambiguous MIN and MAX columns, add dates, rearrange cols
  df %>%
    dplyr::mutate(DATE=suppressWarnings(lubridate::ymd(paste(YEAR, MONTH, MONTH / MONTH)))) %>%
    dplyr::select(STATION_NUMBER, STATION_NAME, YEAR, MONTH, DATE,
                  MONTHLY_MEAN, MONTHLY_TOTAL, DAILY_MIN = MIN, DAILY_MAX = MAX) %>%
    tibble::as_tibble()
}

hydat_data_daily <- function(table, colprefix, symbolprefix, stationid,
                       year=NULL, month=1:12, day=1:31, db = hydat_get_db()) {

  # hack so that easy dplyr column names can keep on keepin' on
  STATION_NUMBER <- NULL; rm(STATION_NUMBER); YEAR <- NULL; rm(YEAR)
  MONTH <- NULL; rm(MONTH); DAY <- NULL; rm(DAY); DATE <- NULL; rm(DATE)

  idcols <- c("STATION_NUMBER", "STATION_NAME", "YEAR", "MONTH")
  valuecols <- paste0(colprefix, day)
  if(!is.null(symbolprefix)) {
    symbolcols <- paste0(symbolprefix, day)
  } else {
    symbolcols <- NULL
  }
  cols <- c(idcols, valuecols, symbolcols)

  # get monthly data
  monthly <- hydat_data_base(table, stationid=stationid, year=year, month=month, cols=cols,
                             db = db)

  if(!is.null(symbolprefix)) {
    # melt using parallel.melt to keep flag data
    pmeltargs <- list(monthly, idcols, variable.name="DAY")
    pmeltargs[[colprefix]] <- valuecols
    pmeltargs[[symbolprefix]] <- symbolcols

    daily <- do.call(parallel.melt, pmeltargs)
  } else {
    # melt using reshape2::melt
    daily <- reshape2::melt(monthly, idcols, variable.name="DAY", value.name=colprefix,
                            measure.vars=valuecols)
  }

  # remove the colprefix
  daily$DAY <- as.integer(gsub(colprefix, "", as.character(daily$DAY), fixed=TRUE))

  # apply day filter and remove dates that do not exist
  daily <- daily %>%
    dplyr::mutate(DATE=lubridate::ymd(paste(YEAR, MONTH, DAY), quiet=TRUE)) %>%
    dplyr::filter(!is.na(DATE), DAY %in% day) %>%
    tibble::as_tibble()

  # return the data frame with columns ordered reasonably
  daily[c("STATION_NUMBER", "STATION_NAME", "YEAR", "MONTH", "DAY", "DATE",
          colprefix, symbolprefix)]
}
