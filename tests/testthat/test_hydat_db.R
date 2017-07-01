
context("hydat database")

# hydat_regex <- "Hydat_sqlite3_([0-9]{8})"

test_that("the page from which the download link is webscraped contains a link to the hydat db", {
  source_page <- "http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/"

  hydat_fname <- source_page %>%
    curl::curl_fetch_memory() %>%
    .$content %>%
    xml2::read_html() %>%
    xml2::xml_find_all('.//a[@href]') %>%
    xml2::xml_attr("href") %>%
    stringr::str_subset(paste0(hydat_regex, ".zip$"))

  expect_length(hydat_fname, 1)

  # check hydat url
  hydat_url <- paste0(source_page, hydat_fname)
  response <- httr::HEAD(hydat_url)
  expect_equal(response$status_code, 200) # no 404 not found error
  expect_equal(response$headers$`content-type`, "application/zip") # a zip file
  expect_gt(as.numeric(response$headers$`content-length`), 200e6) # is always over 200 MB
})

# copy test database to temp directory
tmp_directory <- tempfile()[1]
dir.create(tmp_directory)
tmp_db_location <- file.path(tmp_directory, "Hydat_sqlite3_99999999.db")
file.copy(system.file("Hydat_sqlite3_99999999.db", package = "hydatr"),
          tmp_db_location)
# check that test db was created
if(!file.exists(tmp_db_location)) stop("db could not be copied to a temporary directory")

# create test zipped version of test db
tmp_zip_dir <- tempfile()[1]
dir.create(tmp_zip_dir)
old_wd <- getwd()
setwd(tmp_zip_dir)
file.copy(tmp_db_location, "Hydat.sqlite3")
tmp_zip_arc <- file.path(tmp_directory, "Hydat_sqlite3_99999999.zip")
utils::zip(tmp_zip_arc, "Hydat.sqlite3")

# cleanup zip file
unlink(tmp_zip_dir, recursive = TRUE)
setwd(old_wd)
rm(tmp_zip_dir, old_wd)
# check that archive was created
if(!file.exists(tmp_zip_arc)) stop("test db zip file could not be created")

test_that("hydat_list_files matches files", {
  # check full names = TRUE
  expect_equal(hydat_list_files(tmp_directory, ext = ".db", full.names = TRUE),
               tmp_db_location)
  expect_equal(hydat_list_files(tmp_directory, ext = ".zip"), full.names = TRUE,
               tmp_zip_arc)

  old_wd <- getwd()
  setwd(tmp_directory)
  # check full names = FALSE
  expect_equal(hydat_list_files(".", ext = ".db"), full.names = FALSE,
               file.path(".", basename(tmp_db_location)))
  expect_equal(hydat_list_files(".", ext = ".zip"), full.names = FALSE,
               file.path(".", basename(tmp_zip_arc)))
  setwd(old_wd)
})

test_that("hydat_load works with set = TRUE and set = FALSE", {
  hydat_set_db(NULL) # clear db if one was previously loaded
  expect_null(hydat_get_db())
  expect_is(hydat_load(tmp_db_location, set = FALSE), "src_hydat")
  expect_null(hydat_get_db())
  expect_is(hydat_load(tmp_directory, set = FALSE), "src_hydat")
  expect_null(hydat_get_db())

  expect_is(hydat_load(tmp_db_location, set = TRUE), "src_hydat")
  expect_is(hydat_get_db(), "src_hydat")
  hydat_set_db(NULL)
  expect_null(hydat_get_db())
  expect_is(hydat_load(tmp_directory, set = TRUE), "src_hydat")
  expect_is(hydat_get_db(), "src_hydat")
  hydat_set_db(NULL)
})

test_that("hydat_extract works with directories and files", {
  expect_length(hydat_list_files(tmp_directory, ext = ".db"), 1)
  expect_true(file.exists(tmp_zip_arc))

  new_tmp_dir <- tempfile()[1]
  dir.create(new_tmp_dir)

  # check that source/destination parameter is respected as a directory
  expect_equal(hydat_extract(tmp_directory, destination = new_tmp_dir),
               file.path(new_tmp_dir, basename(tmp_db_location)))
  # check that overwrite parameter is respected
  expect_error(hydat_extract(tmp_directory, destination = new_tmp_dir, overwrite = FALSE),
               "Output file .*? already exists .*?")
  expect_message(hydat_extract(tmp_directory, destination = new_tmp_dir, overwrite = NA),
                 "Using previously extracted version of .*?")
  expect_silent(hydat_extract(tmp_directory, destination = new_tmp_dir, overwrite = TRUE))

  # remove tmp dir db files
  list.files(new_tmp_dir, pattern = ".db", full.names = TRUE) %>% unlink()

  # check default destination with source as a directory
  expect_message(hydat_extract(tmp_directory, destination = NULL, overwrite = NA),
                 "Using previously extracted version of .*?")

  # check that source parameter is respected as a file
  expect_equal(hydat_extract(tmp_zip_arc, destination = new_tmp_dir),
               file.path(new_tmp_dir, basename(tmp_db_location)))
  # remove tmp dir db files
  list.files(new_tmp_dir, pattern = ".db", full.names = TRUE) %>% unlink()

  # check that default destination parameter with source as file is as expected
  expect_message(hydat_extract(tmp_zip_arc, destination = NULL, overwrite = NA),
                 "Using previously extracted version of .*?")

  # check that odd named zip file doesn't cause errors
  weird_fname <- file.path(tmp_directory, "fish.weird")
  file.copy(tmp_zip_arc, weird_fname)
  expect_equal(basename(hydat_extract(weird_fname, destination = new_tmp_dir)),
               "Hydat_sqlite3_00000000.db")

  # cleanup files
  unlink(new_tmp_dir, recursive = TRUE)
})

test_that("is_hydat responds to the correct class", {
  expect_true(is_hydat(structure(1, class = "src_hydat")))
  expect_false(is_hydat(structure(1, class = "not_src_hydat")))
})

test_that("set hydat db won't set something that isn't an src_hydat", {
  expect_error(hydat_set_db(structure(1, class = "not_src_hydat")),
               "x must be a src_hydat .*?")
  expect_silent(hydat_set_db(structure(1, class = "src_hydat")))
  expect_is(hydat_get_db(), "src_hydat")
  expect_silent(hydat_set_db(NULL))
  expect_null(hydat_get_db())
})

test_that("the hydat test database gets loaded", {
  # make sure DB is null to start
  hydat_set_db(NULL)
  expect_null(hydat_get_db())

  # make sure set = FALSE is respected
  expect_is(hydat_load_test_db(set = FALSE), "src_hydat")
  expect_null(hydat_get_db())

  # make sure set = TRUE is respected
  hydat_load_test_db(set = TRUE)
  expect_is(hydat_get_db(), "src_hydat")
  expect_is(hydat_get_db(), "src_sqlite")

  # check tables
  expect_true(all(c("AGENCY_LIST", "ANNUAL_INSTANT_PEAKS", "ANNUAL_STATISTICS",
                    "CONCENTRATION_SYMBOLS", "DATA_SYMBOLS", "DATA_TYPES",
                    "DATUM_LIST", "DLY_FLOWS", "DLY_LEVELS", "MEASUREMENT_CODES",
                    "OPERATION_CODES", "PEAK_CODES", "PRECISION_CODES",
                    "REGIONAL_OFFICE_LIST", "SAMPLE_REMARK_CODES", "SED_DATA_TYPES",
                    "SED_DLY_LOADS", "SED_DLY_SUSCON", "SED_SAMPLES", "SED_SAMPLES_PSD",
                    "SED_VERTICAL_LOCATION", "SED_VERTICAL_SYMBOLS", "STATIONS",
                    "STN_DATA_COLLECTION", "STN_DATA_RANGE", "STN_DATUM_CONVERSION",
                    "STN_DATUM_UNRELATED", "STN_OPERATION_SCHEDULE", "STN_REGULATION",
                    "STN_REMARKS", "STN_REMARK_CODES", "STN_STATUS_CODES",
                    "VERSION") %in% dplyr::src_tbls(hydat_get_db())))


})

# cleanup temporary db files
unlink(tmp_directory, recursive = TRUE)
rm(tmp_directory, tmp_db_location)

