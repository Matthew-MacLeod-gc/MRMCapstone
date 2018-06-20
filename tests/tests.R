library(testthat)
expect_match(MRMCapstone::eq_location_clean("UNITED KINGDOM: CHANNEL ISLANDS; GAUL"), "Channel Islands; Gaul")


test_that{
  testDF <- data.frame(YEAR = c(2012), MONTH = c(5), DAY=c(4),
                       LOCATION_NAME="UNITED KINGDOM: CHANNEL ISLANDS; GAUL", LATITUDE=as.character(75), LONGITUDE=as.character(42), stringsAsFactors = FALSE)
  cleanDF <- MRMCapstone::eq_clean_data(testDF)

  expect_that(cleanDF$DATE, is_a("Date"))
  expect_that(cleanDF$LATITUDE, is_a("numeric"))
  expect_that(cleanDF$LONGITUDE, is_a("numeric"))
  expect_match(cleanDF$LOCATION_NAME, "Channel Islands; Gaul")
}


test_that{
  testDF <- data.frame(LOCATION_NAME="Channel Islands; Gaul", EQ_PRIMARY=8.9, TOTAL_DEATHS=52, stringsAsFactors = FALSE)

  testStr <- MRMCapstone::eq_create_label(testDF)

  expect_that(str_count(testStr, "<B>"), equals(3))

}
