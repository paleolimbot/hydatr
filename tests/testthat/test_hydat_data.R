
context("hydat data")

test_that("data functions generate data frames", {
  # load the test database
  hydat_load_test_db()

  expect_is(hydat_flow_monthly("01AD001"), "data.frame")
  expect_is(hydat_flow_daily("01AD001"), "data.frame")
  expect_is(hydat_level_monthly("01AD003"), "data.frame")
  expect_is(hydat_level_daily("01AD003"), "data.frame")
  expect_is(hydat_sed_monthly("01AF006"), "data.frame")
  expect_is(hydat_sed_daily("01AF006"), "data.frame")
})

test_that("data functions filter by year month and day properly", {
  # load the test database
  hydat_load_test_db()

  # each data function family is tested using this generic function tester
  test_date_filtering <- function(data_monthly, data_daily, test_stationid,
                                  year_low, year_high) {
    # single year
    yr_year_low <- data_monthly(test_stationid, year = year_low)
    expect_true(all(yr_year_low$YEAR == year_low))
    yr_year_low_daily <- data_daily(test_stationid, year = year_low)
    expect_true(all(yr_year_low_daily$YEAR == year_low))

    # non-connected years
    yr_year_low_year_high <- data_monthly(test_stationid, year = c(year_low, year_high))
    expect_true(all(yr_year_low_year_high$YEAR %in% c(year_low, year_high)))
    expect_equal(max(yr_year_low_year_high$YEAR), year_high)
    expect_equal(min(yr_year_low_year_high$YEAR), year_low)
    yr_year_low_year_high_daily <- data_daily(test_stationid, year = c(year_low, year_high))
    expect_true(all(yr_year_low_year_high_daily$YEAR %in% c(year_low, year_high)))
    expect_equal(max(yr_year_low_year_high_daily$YEAR), year_high)
    expect_equal(min(yr_year_low_year_high_daily$YEAR), year_low)

    # single month
    mo_5 <- data_monthly(test_stationid, month = 5)
    expect_true(all(mo_5$MONTH == 5))
    mo_5_daily <- data_daily(test_stationid, month = 5)
    expect_true(all(mo_5_daily$MONTH == 5))

    # non-connected months
    mo_5_7 <- data_monthly(test_stationid, month = c(5, 7))
    expect_true(all(mo_5_7$MONTH %in% c(5, 7)))
    expect_equal(max(mo_5_7$MONTH), 7)
    expect_equal(min(mo_5_7$MONTH), 5)
    mo_5_7_daily <- data_daily(test_stationid, month = c(5, 7))
    expect_true(all(mo_5_7_daily$MONTH %in% c(5, 7)))
    expect_equal(max(mo_5_7_daily$MONTH), 7)
    expect_equal(min(mo_5_daily$MONTH), 5)

    # single day
    day_5 <- data_daily(test_stationid, day = 5)
    expect_true(all(day_5$DAY == 5))

    # non-connected days
    day_5_7 <- data_daily(test_stationid, day = c(5, 7))
    expect_true(all(day_5_7$DAY %in% c(5, 7)))
    expect_equal(max(day_5_7$DAY), 7)
    expect_equal(min(day_5_7$DAY), 5)

    # single year and month
    ym_year_low_5 <- data_monthly(test_stationid, year = year_low, month = 5)
    expect_equal(nrow(ym_year_low_5), 1)
    expect_equal(ym_year_low_5$YEAR, year_low)
    expect_equal(ym_year_low_5$MONTH, 5)
    ym_year_low_5_daily <- data_daily(test_stationid, year = year_low, month = 5)
    expect_equal(nrow(ym_year_low_5_daily), 31)
    expect_true(all(ym_year_low_5_daily$YEAR == year_low))
    expect_true(all(ym_year_low_5_daily$MONTH == 5))

    # non-connected year and month
    ym_nc <- data_monthly(test_stationid, year = c(year_low, year_high), month = c(5, 7))
    expect_equal(nrow(ym_nc), 4)
    expect_true(all(ym_nc$YEAR %in% c(year_low, year_high)))
    expect_true(all(ym_nc$MONTH %in% c(5, 7)))
    expect_equal(max(ym_nc$YEAR), year_high)
    expect_equal(min(ym_nc$YEAR), year_low)
    expect_equal(max(ym_nc$MONTH), 7)
    expect_equal(min(ym_nc$MONTH), 5)
    ym_nc_daily <- data_daily(test_stationid, year = c(year_low, year_high), month = c(5, 7))
    expect_true(all(ym_nc_daily$YEAR %in% c(year_low, year_high)))
    expect_true(all(ym_nc_daily$MONTH %in% c(5, 7)))
    expect_equal(max(ym_nc_daily$YEAR), year_high)
    expect_equal(min(ym_nc_daily$YEAR), year_low)
    expect_equal(max(ym_nc_daily$MONTH), 7)
    expect_equal(min(ym_nc_daily$MONTH), 5)

    # single year, month, and day
    ymd_daily <- data_daily(test_stationid, year = year_low, month = 5, day = 20)
    expect_equal(nrow(ymd_daily), 1)
    expect_equal(ymd_daily$YEAR, year_low)
    expect_equal(ymd_daily$MONTH, 5)
    expect_equal(ymd_daily$DAY, 20)

    # non-connected year month and day
    ymd_nc <- data_daily(test_stationid, year = c(year_low, year_high), month = c(5, 7), day = c(20, 23))
    expect_true(all(ymd_nc$YEAR %in% c(year_low, year_high)))
    expect_true(all(ymd_nc$MONTH %in% c(5, 7)))
    expect_equal(max(ymd_nc$YEAR), year_high)
    expect_equal(min(ymd_nc$YEAR), year_low)
    expect_equal(max(ymd_nc$MONTH), 7)
    expect_equal(min(ymd_nc$MONTH), 5)
    expect_equal(max(ymd_nc$DAY), 23)
    expect_equal(min(ymd_nc$DAY), 20)
  }

  test_date_filtering(hydat_flow_monthly, hydat_flow_daily, "01AD001", 1923, 1925)
  test_date_filtering(hydat_level_monthly, hydat_level_daily, "01AD003", 2011, 2013)
  test_date_filtering(hydat_sed_monthly, hydat_sed_daily, "01AF006", 1971, 1973)
})

test_that("multiple sites are included in data output", {
  hydat_load_test_db()

  flow_m <- hydat_flow_monthly(c("01AD001", "01AA002"))
  expect_true(setequal(flow_m$STATION_NUMBER, c("01AD001", "01AA002")))
  flow_d <- hydat_flow_daily(c("01AD001", "01AA002"))
  expect_true(setequal(flow_d$STATION_NUMBER, c("01AD001", "01AA002")))
  level_m <- hydat_level_monthly(c("01AD003", "01AD004"))
  expect_true(setequal(level_m$STATION_NUMBER, c("01AD003", "01AD004")))
  level_d <- hydat_level_daily(c("01AD003", "01AD004"))
  expect_true(setequal(level_d$STATION_NUMBER, c("01AD003", "01AD004")))
  sed_m <- hydat_sed_monthly(c("01AF006", "01AJ006"))
  expect_true(setequal(sed_m$STATION_NUMBER, c("01AF006", "01AJ006")))
  sed_d <- hydat_sed_daily(c("01AJ006", "01AF006"))
  expect_true(setequal(sed_m$STATION_NUMBER, c("01AF006", "01AJ006")))
})

test_that("station numbers that are not found generate the proper error", {
  # load the test database
  hydat_load_test_db()

  # check that errors are thrown when a station doesn't exist
  expect_error(hydat_flow_monthly("not_a_station"),
               "Station 'not_a_station' does not exist in table 'DLY_FLOWS'")
  expect_error(hydat_flow_daily("not_a_station"),
               "Station 'not_a_station' does not exist in table 'DLY_FLOWS'")
  expect_error(hydat_level_monthly("not_a_station"),
               "Station 'not_a_station' does not exist in table 'DLY_LEVELS'")
  expect_error(hydat_level_daily("not_a_station"),
               "Station 'not_a_station' does not exist in table 'DLY_LEVELS'")
  expect_error(hydat_sed_monthly("not_a_station"),
               "Station 'not_a_station' does not exist in table 'SED_DLY_LOADS'")
  expect_error(hydat_sed_daily("not_a_station"),
               "Station 'not_a_station' does not exist in table 'SED_DLY_LOADS'")

  # check that errors are not thrown when a year doesn't exist but the station does
  expect_silent(hydat_flow_monthly("01AD001", year = 1800))
  expect_silent(hydat_flow_daily("01AD001", year = 1800))
  expect_silent(hydat_level_monthly("01AD003", year = 1800))
  expect_silent(hydat_level_daily("01AD003", year = 1800))
  expect_silent(hydat_sed_monthly("01AF006", year = 1800))
  expect_silent(hydat_sed_daily("01AF006", year = 1800))
})
