context("EIA bulk data access functions")

test_that("Check eia_bulk_url() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  expect_is(eia_bulk_url(),  "character");
  expect_match(eia_bulk_url(),  "https:\\/\\/www\\.eia\\.gov\\/opendata\\/bulkfiles\\.php");
})


test_that("Check eia_bulk_manifest() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  print("TO BE ADDED")
  ## expect_is(eia_bulk_url(),  "character");
  ## expect_match(eia_bulk_url(),  "https:\\/\\/www\\.eia\\.gov\\/opendata\\/bulkfiles\\.php");
})


test_that("Check eia_bulk_browse() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  print("TO BE ADDED")
  ## expect_is(eia_bulk_url(),  "character");
  ## expect_match(eia_bulk_url(),  "https:\\/\\/www\\.eia\\.gov\\/opendata\\/bulkfiles\\.php");
})


test_that("Check eia_bulk_search() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  print("TO BE ADDED")
  ## expect_is(eia_bulk_url(),  "character");
  ## expect_match(eia_bulk_url(),  "https:\\/\\/www\\.eia\\.gov\\/opendata\\/bulkfiles\\.php");
})


test_that("Check eia_bulk_stats() function",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  print("TO BE ADDED")
  ## expect_is(eia_bulk_url(),  "character");
  ## expect_match(eia_bulk_url(),  "https:\\/\\/www\\.eia\\.gov\\/opendata\\/bulkfiles\\.php");
})

