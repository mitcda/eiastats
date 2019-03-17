context("EIA stats utilities")

#' Use testthat::skip() to automatically skip tests that require authentication.
#' Wrap this in a little helper function that is called at the start of every test
#' requiring authentication.
skip_if_no_auth <- function() {
  if (identical(Sys.getenv("EIA_APIKEY"), "")) {
    skip("No authentication available")
  }
}

test_that("Check eia_url() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_is(eia_url(),  "character");
  expect_match(eia_url(),  "https:\\/\\/www\\.eia\\.gov");
})


test_that("Check eia_api() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_is(eia_api(),  "character");
  expect_match(eia_api(),  "http:\\/\\/api\\.eia\\.gov");
})


test_that("Check eia_ua() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_s3_class(eia_ua(), "request")
  expect_type(eia_ua(),  "list");
  expect_match(eia_ua()$options$useragent, "https:\\/\\/github.com\\/mitcda\\/eiastats");
})


test_that("Check eia_get() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  test_str <- "search/?search_term=name&search_value='crude oil'"
  expect_is(eia_get(test_str),  "character");
  ## -- UP TO HERE --
  ## expect_type(call_api(test_str), )
  ## httr::http_type(get_return) %in% c("application/json", "text/html")
})


test_that("Check eia_search() function",
{
  ## Check eia_search returned class
  expect_s3_class(
    eia_search(name = "crude oil", simplify=FALSE),
    "data.frame");

  ## The following function call fails
  expect_s3_class(
    eia_search(name = c("brent","wti")),
    "data.frame");

  expect_s3_class(
    eia_search(name = c("brent")),
    "data.frame");
  
  expect_s3_class(
    eia_search(name = c("wti")),
    "data.frame");

  expect_s3_class(
    eia_search(last_updated = as.Date(c("2015-06-01", "2016-06-01"))),
    "data.frame")
})
