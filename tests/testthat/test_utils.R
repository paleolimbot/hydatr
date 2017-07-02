
context("utility functions")

test_that("parallel.melt produces the expected data frame", {
  pocmajsum <- tibble::tribble(
    ~core,~depth,~Ca,~Ca_sd,~Ti,~Ti_sd,~V,~V_sd,
    "MAJ-1",0,1884.66666666667,452.35421224228,2369.66666666667,401.05652136991,78.33,9.23,
    "MAJ-1",1,1418,NA,2409,NA,70,NA,
    "MAJ-1",2,1550,NA,2376,NA,70,NA,
    "MAJ-1",3,1448,NA,2485,NA,64,NA,
    "MAJ-1",4,1247,NA,2414,NA,57,NA,
    "MAJ-1",5,1412.33333333333,125.894135420731,1897.33333333333,81.2916559883813,81,11.78,
    "POC-2",0,1622,508.766154534674,2038,608.303378257922,33,5.29150262212918,
    "POC-2",1,1488,NA,2016,NA,36,NA,
    "POC-2",2,2416,NA,3270,NA,79,NA,
    "POC-2",3,2253,NA,3197,NA,79,NA,
    "POC-2",4,2372,NA,3536,NA,87,NA,
    "POC-2",5,2635.33333333333,143.039621550581,3890,45.1774279923061,87,7.54
  )

  # melt automatically
  pocmajlong <- parallel.melt(pocmajsum, id.vars=c("core", "depth"),
                              value=c("Ca", "Ti", "V"),
                              sd=c("Ca_sd", "Ti_sd", "V_sd"))
  expect_that(names(pocmajlong), equals(c("core", "depth", "param", "value", "sd")))

  # melt manually
  ca <- plyr::rename(pocmajsum[c("core", "depth", "Ca", "Ca_sd")], c("Ca"="value", "Ca_sd"="sd"))
  ca$param <- "Ca"
  ti <- plyr::rename(pocmajsum[c("core", "depth", "Ti", "Ti_sd")], c("Ti"="value", "Ti_sd"="sd"))
  ti$param <- "Ti"
  v <- plyr::rename(pocmajsum[c("core", "depth", "V", "V_sd")], c("V"="value", "V_sd"="sd"))
  v$param <- "V"
  pocmajlongman <- rbind(ca, ti, v)[c("core", "depth", "param", "value", "sd")]

  expect_true(all(sapply(data.frame(pocmajlong == pocmajlongman), all, na.rm=TRUE)))
})

test_that("unnamed arguments are not allowed in parallel.melt", {
  expect_error(parallel.melt(data.rame(a=1, b=2), id.vars="a", "b"),
               "All arguments must be named")
})

test_that("geodist output approximates geosphere output", {
  cities <- tibble::tribble(
    ~query, ~lon, ~lat,
    "Halifax NS", -63.5751339, 44.6491198,
    "Windsor NS", -64.1363748, 44.9905115,
    "Wolfville NS", -64.3644922, 45.091225
  )

  src_city <- tibble::tibble(query = "Yarmouth NS", lon = -66.1156, lat = 43.83728)

  # args should be the same in either order
  dist_geodist <- geodist(src_city$lon, src_city$lat, cities$lon, cities$lat)
  dist_geodist_rev <- geodist(cities$lon, cities$lat, src_city$lon, src_city$lat)
  expect_identical(dist_geodist, dist_geodist_rev)

  # results should be equal to geosphere output
  # dist_geosphere <- geosphere::distHaversine(c(src_city$lon, src_city$lat),
  #                                            cbind(cities$lon, cities$lat))
  # expect_equal(dist_geodist, dist_geosphere)
})

