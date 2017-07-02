
context("hydat stations")

test_that("station finding works with both numeric and character input", {
  hydat_load_test_db()

  geo <- prettymapr::geocode("Wolfville NS")
  results_chr <- hydat_find_stations("Wolfville NS", limit = 10)
  expect_is(results_chr, "data.frame")
  expect_equal(nrow(results_chr), 10)

  # check that numeric input is the same with geo and character input
  expect_identical(hydat_find_stations(c(geo$lon, geo$lat), limit = 10),
                   results_chr)

  # check that invalid input is detected
  expect_error(hydat_find_stations(NULL),
               "x must be a character vector of length 1 or a numeric vector of length 2")
  expect_error(hydat_find_stations(TRUE),
               "x must be a character vector of length 1 or a numeric vector of length 2")
  expect_error(hydat_find_stations(4),
               "x must be a character vector of length 1 or a numeric vector of length 2")
  expect_error(hydat_find_stations(c("one", "two")),
              "x must be a character vector of length 1 or a numeric vector of length 2")
})

test_that("station finding fails with an inadequte db", {
  hydat_set_db(NULL)
  expect_error(hydat_find_stations(c(-64.36449, 45.09123), limit = 10),
               "db must be a valid src_hydat .*?")
  hydat_load_test_db()
  expect_silent(hydat_find_stations(c(-64.36449, 45.09123), limit = 10))
  expect_error(hydat_find_stations(c(-64.36449, 45.09123), limit = 10,
                                   db = NULL),
               "db must be a valid src_hydat .*?")
})

test_that("the limit parameter is respected", {
  hydat_load_test_db()
  expect_equal(nrow(hydat_find_stations(c(-64.36449, 45.09123), limit = 10)),
               10)
  expect_equal(nrow(hydat_find_stations(c(-64.36449, 45.09123), limit = 5)),
               5)
  expect_equal(nrow(hydat_find_stations(c(-64.36449, 45.09123), limit = 3)),
               3)
  expect_equal(nrow(hydat_find_stations(c(-64.36449, 45.09123), limit = 100)),
               100)
  expect_equal(nrow(hydat_find_stations(c(-64.36449, 45.09123), limit = 0)),
               0)
  expect_gt(nrow(hydat_find_stations(c(-64.36449, 45.09123), limit = NULL)),
            100)
})

test_that("the year filter is respected", {
  hydat_load_test_db()
  wolfville_raw <- hydat_find_stations(c(-64.36449, 45.09123), limit = NULL)
  wolfville_year <- hydat_find_stations(c(-64.36449, 45.09123), limit = NULL, year = 2000:2010)
  expect_gt(nrow(wolfville_raw), nrow(wolfville_year))
  expect_true(all(na.omit(wolfville_year$FIRST_YEAR <= 2000)))
  expect_true(all(na.omit(wolfville_year$LAST_YEAR >= 2010)))
})

test_that("column names for station_find are correct", {
  hydat_load_test_db()
  cols <- c("STATION_NUMBER", "dist_from_query_km", "STATION_NAME", "FIRST_YEAR", "LAST_YEAR")
  expect_equal(colnames(hydat_find_stations(c(-64.36449, 45.09123), limit = NULL)),
               cols)
  expect_equal(colnames(hydat_find_stations(c(-64.36449, 45.09123), limit = 10)),
               cols)
  expect_equal(colnames(hydat_find_stations(c(-64.36449, 45.09123), limit = 0)),
               cols)
})

test_that("station info returns a data frame", {
  hydat_load_test_db()
  expect_is(hydat_station_info("01AD001"), "data.frame")
  expect_is(hydat_station_info(), "data.frame")
})

test_that("station info returns the correct number of rows", {
  hydat_load_test_db()
  expect_equal(nrow(hydat_station_info("01AD001")), 1)
  expect_equal(nrow(hydat_station_info(NA)), 1)
  expect_equal(nrow(hydat_station_info(c("01AD001", NA))), 2)
  expect_equal(nrow(hydat_station_info(c("01AD001", "01AD001"))), 2)
  expect_equal(nrow(hydat_station_info(c("01AD001", "NOT_A_NUMBER"))), 2)
  expect_equal(nrow(hydat_station_info(c(NA, "NOT_A_NUMBER"))), 2)
  expect_gt(nrow(hydat_station_info()), 100)
})
