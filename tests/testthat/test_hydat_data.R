
context("hydat data")

hydat_flow_daily("01DC001", year = NULL)

allain_flow <- tbl(hydat_get_db(), "DLY_FLOWS") %>%
  filter(STATION_NUMBER == "01DC001") %>%
  View()

hydat_load_test_db()
hydat_flow_monthly("01AD001")
hydat_flow_daily("01AD001")
hydat_level_monthly("01AD003")
hydat_level_daily("01AD003")
hydat_sed_monthly("01AF006")
hydat_sed_daily("01AF006")

test_that("data functions generate data frames", {
  expect_is(hydat_flow_monthly("01AD001"), "data.frame")
  expect_is(hydat_flow_daily("01AD001"), "data.frame")
  expect_is(hydat_level_monthly("01AD003"), "data.frame")
  expect_is(hydat_level_daily("01AD003"), "data.frame")
  expect_is(hydat_sed_monthly("01AF006"), "data.frame")
  expect_is(hydat_sed_daily("01AF006"), "data.frame")
})

test_that("data functions filter by year month and day properly", {

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

test_that("station numbers that are not found generate the proper error", {
  hydat_flow_monthly("not_a_station")
  hydat_flow_daily("not_a_station")
  hydat_level_monthly("not_a_station")
  hydat_level_daily("not_a_station")
  hydat_sed_monthly("not_a_station")
  hydat_sed_daily("not_a_station")
})
