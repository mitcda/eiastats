context("eiastats functions")

test_that("Test eia_series_query function return",
{
  library(keyring)
  
  eia_data <- eia_series_query(c("STEO.BREPUUS.A", "STEO.BREPUUS.Q", "STEO.BREPUUS.M1",
                                 "STEO.WTIPUUS.A", "STEO.WTIPUUS.Q", "STEO.WTIPUUS.M"),
                               api_key = keyring::key_get("EIA_TESTKEY"), num = 100, end_date = "2017-06-30",
                               format = "json", simplify=FALSE)
  
  expect_s3_class(eia_data, "data.frame");

  eia_data_start_end <- eia_series_query(c("STEO.BREPUUS.Q", "STEO.WTIPUUS.Q"),
                               api_key = keyring::key_get("EIA_TESTKEY"),
                               start_date="2012-09-30", end_date = "2017-06-30")
  expect_s3_class(eia_data_start_end, "data.frame");
 

  
  geoset_id <- "INTL.53-1-TBPD.Q";
  
  eia_data <- eia_stats(c("STEO.BREPUUS.A", "STEO.BREPUUS.Q", "STEO.BREPUUS.M1",
                          "STEO.WTIPUUS.A", "STEO.WTIPUUS.Q", "STEO.WTIPUUS.M"),
                        api_key = keyring::key_get("EIA_TESTKEY"), num = 100, end_date = "2017-06-30",
                        format = "json")


  

eia_results <- eia_search(name = "crude oil", simplify=FALSE);
eia_results <- eia_search(name = c("brent","wti"));
eia_results <- eia_search(last_updated = as.Date(c("2015-06-01", "2016-06-01")))

})


test_that("Test eia_series_query function fails nicely",
{
  print("To be added")
})


test_that("eia_category_query works",
{
  eia_category <- eia_category_query(api_key=keyring::key_get("EIA_TESTKEY"), simplify=FALSE)

  eia_category <- eia_category_query(api_key=keyring::key_get("EIA_TESTKEY"))

  eia_category <- eia_category_query(category_id=711224, api_key=keyring::key_get("EIA_TESTKEY"), simplify=TRUE)
  eia_category <- eia_category_query(category_id=711225, api_key=keyring::key_get("EIA_TESTKEY"), simplify=TRUE)
  eia_category <- eia_category_query(category_id=711238, api_key=keyring::key_get("EIA_TESTKEY"), simplify=TRUE)

    
  #' The following code binds list elements of different dimensions together
  #' in a single data frame - only problem is that it doesn't automaticall
  #' include column names
  xx <- list(name = "Bogus name",
             category = "Level 1",
             unit = "million tonnes",
             data = data.frame(index = 1:10,
                               values = rnorm(n=10)))
  do.call(cbind, lapply(xx, as.data.frame))
  
  
  
eia_results <- eia_search(name = "crude oil", simplify=FALSE);
eia_results <- eia_search(name = c("brent","wti"));
eia_results <- eia_search(last_updated = as.Date(c("2015-06-01", "2016-06-01")))

})
