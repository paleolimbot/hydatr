
hydat_regex <- "Hydat_sqlite3_([0-9]{8})"

#' Download the Hydat Database from Environment Canada
#'
#' Downloads the latest version of the Hydat Database from
#' \url{http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/}. This
#' package uses the zipped SQLite version of the database. The download is about
#' 200 MB, and usually takes 5-10 minutes to download.
#'
#' @param destination A filename or directory
#' @param overwrite Should the destination file be overwritten? Use NA to
#'   skip downloading a previously downloaded file (the default).
#' @param quiet If TRUE, download status messages will be suppressed.
#'
#'
#' @return The filename of the downloaded file.
#' @export
#'
#' @examples
#' \dontrun{
#' # this usually takes around 10 minutes to run
#' hydat_download()
#' }
#'
hydat_download <- function(destination = ".", overwrite = NA, quiet = FALSE) {

  source_page <- "http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/"

  . <- NULL; rm(.) # cmd hack
  hydat_fname <- source_page %>%
    curl::curl_fetch_memory() %>%
    .$content %>%
    xml2::read_html() %>%
    xml2::xml_find_all('.//a[@href]') %>%
    xml2::xml_attr("href") %>%
    stringr::str_subset(paste0(hydat_regex, ".zip$"))

  if(length(hydat_fname) == 0) stop("Could not find latest Hydat download link at ", source_page)
  if(length(hydat_fname) > 1) stop("Found more than one possible Hydat download at ", source_page)

  hydat_url <- paste0(source_page, hydat_fname)

  if(dir.exists(destination)) {
    destination_file <- file.path(destination, basename(hydat_fname))
  } else {
    destination_file <- destination
  }

  if(file.exists(destination_file)) {
    if(identical(overwrite, FALSE)) {
      stop("File ", destination_file, " already exists (use overwrite = TRUE to overwrite, ",
           "or overwrite = NA to load the existing file)")
    } else if(identical(overwrite, NA)) {
      message("Using previously downloaded version of ", hydat_fname)
      return(destination_file)
    } else if(identical(overwrite, TRUE)) {
      # do nothing
    } else {
      stop("Unrecognized overwrite option: ", overwrite)
    }
  }

  # download file
  curl::curl_download(hydat_url, destination_file, quiet = quiet)

  # return extracted filename
  return(destination_file)
}

#' List Hydat files
#'
#' List Hydat files with the specified extension.
#'
#' @param source The directory to search for Hydat files
#' @param ext The extension of the file
#' @param full.names Should full names be used?
#' @param recursive Should directories be searched recursively?
#'
#' @return A character vector of filenames
#' @export
#'
#' @examples
#' hydat_list_files()
#'
hydat_list_files <- function(source = ".", ext = ".db", full.names = TRUE, recursive = FALSE) {
  if(dir.exists(source)) {
    list.files(source, pattern = paste0(hydat_regex, ext, "$"),
               full.names = full.names, recursive = recursive)
  } else {
    stop("Directory ", source, " does not exist")
  }
}

#' Extract a Zipped Hydat Database
#'
#' A zipped version of the Hydat database can be obtained using
#' \link{hydat_download}. The extracted database is more than 1 GB in size.
#' If multiple downloaded archives exist in the source directory, the latest one is
#' used with a message.
#'
#' @param source The directory that contains the hydat database or the hydat zip file.
#' @param destination The directory that the output zip file should be unzipped to.
#' @param overwrite Should existing files be overwritten? (use NA to skip a previously
#'   extracted file).
#'
#' @return The filename of the extracted sqlite database file
#' @export
#'
#' @examples
#' \dontrun{
#' # hydat_download() takes about 10 minutes; hydat_extract() takes about 2 minutes
#' hydat_download()
#' hydat_extract()
#' }
hydat_extract <- function(source = ".", destination = NULL, overwrite = NA) {

  # destination should be the same directory as the source
  if(is.null(destination)) {
    if(dir.exists(source)) {
      destination <- source
    } else {
      destination <- dirname(source)
    }
  }

  # extract the hydat database from the downloaded file
  if(dir.exists(source)) {
    source_file <- hydat_list_files(source = source, ext = ".zip",
                                    full.names = TRUE, recursive = FALSE) %>%
      sort(decreasing = TRUE)
    if(length(source_file) == 0) {
      stop("Zero Hydat database archives were found in directory ", source)
    } else if(length(source_file) > 1) {
      message("Using the latest Hydat database archive: ", source_file[1])
    }

    # pick the first one (the latest version)
    source_file <- source_file[1]
  } else if(file.exists(source)) {
    source_file <- source
  } else {
    stop("Input ", source, " does not exist")
  }

  # find output file name
  output_file <- file.path(destination, "Hydat.sqlite3")

  # file will be renamed to Hydat_sqlite3_XXXXXXX_.db
  source_date <- stringr::str_match(basename(source_file), hydat_regex)[,2]
  if(is.na(source_date)) {
    source_date <- "00000000"
  }
  final_output_file <- file.path(destination, sprintf("Hydat_sqlite3_%s.db", source_date))

  # check output directory
  if(!dir.exists(destination)) stop("Directory ", destination, " does not exist")

  # if overwrite = NA, don't extract
  if(file.exists(final_output_file)) {
    if(identical(overwrite, FALSE)) {
      stop("Output file ", final_output_file, " already exists (use overwrite = TRUE to re-extract, or ",
           "overwrite = NA to skip extraction)")
    } else if(identical(overwrite, NA)) {
      message("Using previously extracted version of ", source_file)
      return(final_output_file)
    } else if(identical(overwrite, TRUE)) {
      # do nothing to stop extraction
    } else {
      stop("Unrecognized overwrite option: ", overwrite)
    }
  }

  # extract the archive to destination
  utils::unzip(source_file, exdir = destination, overwrite = TRUE, files = "Hydat.sqlite3")
  if(!file.exists(output_file)) stop("Unzip of ", source_file, " failed!")

  file.rename(output_file, final_output_file)

  # return the destination file
  final_output_file
}

#' Load a previously extracted Hydat Database
#'
#' This function uses \link[dplyr]{src_sqlite} to load the sqlite database
#' extracted by \link{hydat_extract} and downloaded using \link{hydat_download}.
#'
#' @param source The directory where the Hydat database was extracted, or the
#'   database file.
#' @param set Pass FALSE to skip setting the internal package reference to the loaded
#'   database (advanced).
#'
#' @return An object extending \link[dplyr]{src_sqlite}, invisibly
#' @export
#'
#' @examples
#' \dontrun{
#' # hydat_download() takes about 10 minutes; hydat_extract() takes about 2 minutes
#' hydat_download()
#' hydat_extract()
#' hydat_db <- hydat_load()
#' dplyr::src_tbls(hydat_db)
#' }
hydat_load <- function(source = ".", set = TRUE) {

  if(dir.exists(source)) {
    source_file <- hydat_list_files(source = source, ext = ".db",
                                    full.names = TRUE, recursive = FALSE) %>%
      sort(decreasing = TRUE)
    if(length(source_file) == 0) {
      stop("Zero Hydat database archives were found in directory ", source)
    } else if(length(source_file) > 1) {
      message("Using the latest Hydat database: ", source_file[1])
    }

    # pick the first one (the latest version)
    source_file <- source_file[1]
  } else if(file.exists(source)) {
    source_file <- source
  } else {
    stop("Input ", source, " does not exist")
  }

  # load using dplyr::src_sqlite
  #sqlsource <- dplyr::src_sqlite(source_file)
  sqlsource <- DBI::dbConnect(RSQLite::SQLite(), source_file)
  #class(sqlsource) <- c("src_hydat", class(sqlsource))
  # There is probably a better way to set S4 class...
  # Simply added a slot called "src_hydat" with value TRUE;
  # `is_hydat()` now checks if that slot exists.
  # See e.g. https://stackoverflow.com/questions/38265754/how-to-add-new-slot-to-already-existing-class
  attributes(sqlsource)$src_hydat <- TRUE

  # set the internal reference to the hydat database
  if(set) {
    hydat_set_db(sqlsource)
  }

  # return the src_sqlite, invisibly
  invisible(sqlsource)
}

#' Test if an object was created using hydat_load
#'
#' @param x The object to test
#'
#' @return TRUE if the object inherits src_hydat, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' hydat_db <- hydat_load()
#' is_hydat(hydat_db)
#' }
#'
is_hydat <- function(x) {
  #inherits(x, "src_hydat")
  # Update for S4 class of sqlsource: check if slot 'src_hydat' exists (and is TRUE)
  x@src_hydat
}

# setup the package state environment
hydatr_state <- new.env(parent = emptyenv()) # create internal state
hydatr_state$hydat_db <- NULL


#' Set/get the internal pointer to the Hydat database
#'
#' The functions in this package require the Hydat database to be loaded
#' using \link{hydat_load} or hydat_set_db(), or for one to be explicitly
#' passed to each function. Generally this is done using \link{hydat_load}
#' with \code{set = TRUE}.
#'
#' @param x A hydat datbase loaded using \link{hydat_load}.
#' @param set Shoult the temporary database be set as the internal
#'   database?
#'
#' @return The hydat database or the previous hydat database.
#' @export
#'
#' @examples
#' \dontrun{
#' hydat_download()
#' hydat_extract()
#' hydat_db <- hydat_load() # this calls hydat_set_db()
#' hydat_set_db(hydat_db) # this is redundant unless set = FALSE in hydat_load()
#' }
#'
hydat_set_db <- function(x) {
  if(!is_hydat(x) && !is.null(x)) stop("x must be a src_hydat created by hydat_load() or NULL")
  old_value <- hydat_get_db()
  hydatr_state$hydat_db <- x
  # return old value
  invisible(old_value)
}

#' @rdname hydat_set_db
#' @export
hydat_get_db <- function() {
  hydatr_state$hydat_db
}

#' @rdname hydat_set_db
#' @export
hydat_load_test_db <- function(set = TRUE) {
  hydat_load(system.file("Hydat_sqlite3_99999999.db", package = "hydatr"), set = set)
}

#' Low-level access to HYDAT database tables
#'
#' This function gives low-level access to the underlying HYDAT tables used by
#' other functions. Many of these tables are too large to load into memory,
#' so it is best to use dplyr to \link[dplyr]{filter} them before using
#' \link[dplyr]{collect} to read them into memory.
#'
#' @param table The name of the table. As of this writing, the list of table
#'   names that can be used includes AGENCY_LIST, ANNUAL_INSTANT_PEAKS,
#'   ANNUAL_STATISTICS, CONCENTRATION_SYMBOLS, DATA_SYMBOLS, DATA_TYPES,
#'   DATUM_LIST, DLY_FLOWS, DLY_LEVELS, MEASUREMENT_CODES, OPERATION_CODES,
#'   PEAK_CODES, PRECISION_CODES, REGIONAL_OFFICE_LIST, SAMPLE_REMARK_CODES,
#'   SED_DATA_TYPES, SED_DLY_LOADS, SED_DLY_SUSCON, SED_SAMPLES,
#'   SED_SAMPLES_PSD, SED_VERTICAL_LOCATION, SED_VERTICAL_SYMBOLS, STATIONS,
#'   STN_DATA_COLLECTION, STN_DATA_RANGE, STN_DATUM_CONVERSION,
#'   STN_DATUM_UNRELATED, STN_OPERATION_SCHEDULE, STN_REGULATION, STN_REMARKS,
#'   STN_REMARK_CODES, STN_STATUS_CODES, and VERSION
#' @param db The hydat database object to use (you will want to use
#'   \link{hydat_load} before using these functions)
#'
#' @return A tbl_sql object (use dplyr to work with these objects)
#' @export
#'
#' @examples
#' hydat_load_test_db()
#' hydat_tbl("STATIONS")
#'
hydat_tbl <- function(table, db = hydat_get_db()) {
  # check db
  if(!is_hydat(db)) stop("db must be a valid src_hydat loaded using hydat_load()")

  # return result of dplyr::tbl
  dplyr::tbl(db, table)
}

