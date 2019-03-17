## #' # Download data from the EIA API
## #' @name eia_series_stats
## #' @title Download data from the EIA API
## #' @description This function requests and returns data from the EIA API.
## #' @param series_id Case-insensitive character vector of one or more EIA Series ID codes. The EIA
## #'   Series ID (also called source key) is a case-insensitive string consisting of letters, numbers,
## #'   dashes ("-") and periods (".") that uniquely identifies an EIA series.
## #' @param geoset_id Case-insensitive character string that uniquely identifies an EIA series.
## #' @param relation_id Case-insensitive character string that uniquely identifies an EIA series.
## #' @param regions Case-insensitive character vector of one or more EIA region codes. (Valid EIA
## #'   region codes include \href{ISO 3166}{https://en.wikipedia.org/wiki/ISO_3166-1_alpha3} country
## #'   codes or country-state codes---refer to \href{ISO
## #'   3166-2}{https://en.wikipedia.org/wiki/ISO_3166-2} Current codes to see subdivision codes for
## #'   each country.
## #' @param category_id A unique numerical ID of the EIA data category to fetch. If NULL, the API's
## #'   root category is returned.
## #' @param api_key Required. A valid API key is required and may be obtained from EIA registration
## #'   page (see details).
## #' @param start_date Optional start date filter---if supplied, returns data after the specified
## #'   start date. Argument accepts valid numeric, character, date or datetime format object. If
## #'   numeric it must be in \%Y form (i.e. four digit year).  If character string, it must be in
## #'   year-month-date (\%Y-\%m-\%d) format.
## #' @param end_date Optional end date filter---if supplied, returns data up to and including the
## #'   specified end date. Argument accepts valid numeric, character, date or datetime format
## #'   object. If numeric it must be in \%Y form (i.e. four digit year).  If character string, it must
## #'   be in year-month-date (\%Y-\%m-\%d) format.
## #' @param num Number of values to be returns, to be used in conjunction with the \code{end_date}
## #'   parameter to return the n-values from an end point. The EIA API will only accept the \code{num}
## #'   parameter in conjunction with the \code{end_date} parameter and not
## #'   \code{start_date}. If \code{num} is supplied, the \code{start_date} argument will be ignored.
## #' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
## #'   JSON formatted output. Note that it is also necessary to set \code{simplify = FALSE} to access
## #'   'xml' or 'json' formatted output.
## #' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
## #'   \code{FALSE}, function returns data in specified output \code{format}.
## #' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
## #'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
## #'   required as part of the registration process.
## #'
## #'   Note that the number of series in a single request is limited to 100.
## #'
## #' @author David Mitchell <david.pk.mitchell@@gmail.com>
## #' @export
## #' @examples
## #'   \donttest{
## #'     eia_data <- eia(c("STEO.BREPUUS.A", "STEO.BREPUUS.Q", "STEO.BREPUUS.M1",
## #'                       "STEO.WTIPUUS.A", "STEO.WTIPUUS.Q", "STEO.WTIPUUS.M"),
## #'                     api_key = "Your API Key", num = 100, end_date = "2017-06-30",
## #'                     format = "json")
## #'   }
## eia_series_stats <- function(series_id, geoset_id, relation_id, regions, api_key,
##                              start_date=NULL, end_date=NULL, num=NULL, format="json",
##                              simplify=TRUE)
## {
##   if (!tolower(format) %in% c("json","xml") & !simplify) {
##     warning(sprintf("Invalid format specified. Should be one of either 'json' or 'xml', returning 'json'."));
##     format <- "json"
##   }
##   ## Debugging code
##   DEBUG <- FALSE;
##   if (DEBUG) {
##     series_id <- c("STEO.BREPUUS.A", "STEO.BREPUUS.Q", "STEO.BREPUUS.M1",
##                    "STEO.WTIPUUS.A", "STEO.WTIPUUS.Q", "STEO.WTIPUUS.M");
##     num <- 100
##     start_date <- NULL;
##     end_date <- "2017-06-30"
##     format <- "json"
##   }
##   ### Series search string composition
##   ## a) Output format
##   series_suffix <- sprintf("&out=%s", tolower(format));
##   ## b) Number records
##   if (!is.null(num))
##     series_suffix <- sprintf("&num=%s%s", num, series_suffix)
##   ## c) End date
##   if (!is.null(end_date)) {
##     # end_date <- format(end_date, format="%Y%m%d");
##     series_suffix <- sprintf("&end=%s%s", end_date, series_suffix)
##   }
##   ## d) Start date
##   if (is.null(num) & !is.null(start_date)) {
##     # start_date <- format(start_date, format="%Y%m%d");
##     series_suffix <- sprintf("&start=%s%s", start_date, series_suffix)
##   }
##   ## e) API Key
##   series_suffix <- sprintf("&api_key=%s%s", api_key, series_suffix)
##   ## f) Series IDs - split Series ID into groups of 100 or fewer, in compliance with EIA API
##   if (!missing(series_id)) {
##     series_id <- split(series_id, ceiling(seq_along(series_id)/100))
##     ## Create EIA API query
##     series_query <- mapply(function(x, y)
##       sprintf("series/?series_id=%s%s", x, y),
##       paste(series_id, collapse=";"), series_suffix,
##       SIMPLIFY = FALSE);
##   }
## 
##   ## g) Geoset ID - split Series ID into groups of 100 or fewer, in compliance with EIA API
##   if (!missing(geoset_id)) {
##     ## Create EIA API query
##     query <- mapply(function(x, y, z)
##       sprintf("geoset/?geoset_id=%s&regions=%s%s", x, y, z),
##       paste(geoset_id, collapse=";"), paste(regions, collapse=";"), series_suffix,
##       SIMPLIFY = FALSE);
##   }
## 
##   ## h) Relation ID
##   if (!missing(relation_id)) {
##     ## Create EIA API query
##     query <- mapply(function(x, y, z)
##       sprintf("relation/?relation_id=%s&region=%s%s", x, y, z),
##       paste(relation_id, collapse=";"), paste(regions, collapse=";"), series_suffix,
##       SIMPLIFY = FALSE);
##   }
## 
##   ## Return results
##   results_json <- lapply(query, function(x) eia_get(x));
##   
##   if (!simplify) {
##     return(results_json)
##   } else {
##     tmp_results_list <- lapply(results_json,
##                                function(x) z <- jsonlite::fromJSON(x, flatten=FALSE)$series
##                                );
##     tmp_results_table <- do.call(rbind, tmp_results_list)
##     ## Extract time series elements (time_period, value) into separate list
##     tmp_results_data <- lapply(tmp_results_table$data,
##                                function(x) {
##                                  y <- unlist(strsplit(x, ","));
##                                  n <- length(y)
##                                  z <- data.frame(time_period = as.character(y[1:(n/2)]),
##                                                  value = as.numeric(y[(n/2+1):n]),
##                                                  stringsAsFactors = FALSE);
##                                });
##     ## Combine data and metadata into a 'tidy' table
##     results_list <- mapply(function(i, y)
##       data.frame(tmp_results_table[i, names(tmp_results_table)[!names(tmp_results_table) %in% "data"]],
##                  y,
##                  row.names = NULL),
##       1:nrow(tmp_results_table),
##       tmp_results_data,
##       SIMPLIFY=FALSE);
##     ## Transform to data frame and return results
##     results_table <- do.call(rbind, results_list);
##     return(results_table) 
##   }
## }


#' # Download data from the EIA API
#' @name eia_series_query
#' @title Download data by series ID from the EIA API
#' @description This function requests and returns data from the EIA API.
#' @param series_id Case-insensitive character vector of one or more EIA Series ID codes. The EIA
#'   Series ID (also called source key) is a case-insensitive string consisting of letters, numbers,
#'   dashes ("-") and periods (".") that uniquely identifies an EIA series.
#' @param api_key Required. A valid API key is required and may be obtained from EIA registration
#'   page (see details).
#' @param num Number of values to be returns, to be used in conjunction with the \code{end_date}
#'   parameter to return the n-values from an end point. The EIA API will only accept the \code{num}
#'   parameter in conjunction with the \code{end_date} parameter and not
#'   \code{start_date}. If \code{num} is supplied, the \code{start_date} argument will be ignored.
#' @param start_date Optional start date filter---if supplied, returns data after the specified
#'   start date. Argument accepts valid numeric, character, date or datetime format object. If
#'   numeric it must be in \%Y form (i.e. four digit year).  If character string, it must be in
#'   year-month-date (\%Y-\%m-\%d) format.
#' @param end_date Optional end date filter---if supplied, returns data up to and including the
#'   specified end date. Argument accepts valid numeric, character, date or datetime format
#'   object. If numeric it must be in \%Y form (i.e. four digit year).  If character string, it must
#'   be in year-month-date (\%Y-\%m-\%d) format.
#' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
#'   \code{FALSE}, function returns data in specified output \code{format}.
#' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
#'   JSON formatted output. Note that it is also necessary to set \code{simplify = FALSE} to access
#'   'xml' or 'json' formatted output.
#' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
#'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
#'   required as part of the registration process.
#'
#'   Note that the number of series in a single request is limited to 100.
#'
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   \donttest{
#'     eia_data <- eia_series_query("STEO.BREPUUS.A"),
#'                     api_key = "Your API Key", num = 100, end_date = "2017-06-30",
#'                     format = "json")
#'   }
eia_series_query <- function(series_id, api_key,
                             start_date=NULL, end_date=NULL, num=NULL,
                             format="json", simplify=TRUE)
{
  ## Debugging code
  DEBUG <- FALSE
  if (DEBUG) {
    eia_test1 <- eia_series_query(series_id=c("STEO.BREPUUS.A", "STEO.BREPUUS.Q"),
                                  api_key=keyring::key_get("EIA_TESTKEY"));
    eia_test2 <- eia_series_query(series_id=c("STEO.BREPUUS.A", "STEO.BREPUUS.Q"),
                                  api_key=keyring::key_get("EIA_TESTKEY"), simplify=FALSE,
                                  format="xml");
  }
  if (DEBUG) {
    series_id <- "STEO.BREPUUS.A"
    series_id <- c("STEO.BREPUUS.A", "STEO.BREPUUS.Q")
    series_id <- c("STEO.BREPUUS.A", "STEO.BREPUUS.Q", "STEO.BREPUUS.M1",
                   "STEO.WTIPUUS.A", "STEO.WTIPUUS.Q", "STEO.WTIPUUS.M")
    api_key <- keyring::key_get("EIA_TESTKEY")
    num <- 100
    start_date <- NULL;
    end_date <- "2017-06-30"
    format <- "json"
  }
  
  if (missing(api_key))
    stop("No api_key provided.")
  if (missing(series_id))
    stop("No series ID provided.")
  if (!tolower(format) %in% c("json","xml") & !simplify) {
    warning(sprintf("Invalid format specified. Should be one of either 'json' or 'xml', returning 'json'."));
    format <- "json"
  }
  max_series <- 100
  ### Series search string composition
  ## a) Output format
  series_suffix <- sprintf("&out=%s", tolower(format));
  ## b) Number records
  if (!is.null(num))
    series_suffix <- sprintf("&num=%s%s", num, series_suffix)
  ## c) End date
  if (!is.null(end_date)) {
    # end_date <- format(end_date, format="%Y%m%d");
    series_suffix <- sprintf("&end=%s%s", end_date, series_suffix)
  }
  ## d) Start date
  if (is.null(num) & !is.null(start_date)) {
    # start_date <- format(start_date, format="%Y%m%d");
    series_suffix <- sprintf("&start=%s%s", start_date, series_suffix)
  }
  ## e) API Key
  series_suffix <- sprintf("&api_key=%s%s", api_key, series_suffix)
  ## f) Series IDs - split Series ID into groups of 100 or fewer, in compliance with EIA API
  series_id <- split(series_id, ceiling(seq_along(series_id) / max_series))
  ## Create EIA API query
  series_query <- lapply(series_id,
                         function(x) sprintf("series/?series_id=%s%s",
                                             paste(x, collapse=";"), series_suffix));
  ## Return results
  results_json <- lapply(series_query, function(x) eia_get(x));
  
  if (!simplify) {
    ## If not 
    return(results_json)
  } else {
    ##  Return as nested data frame (tibble)
    results_list <- lapply(results_json,
                           function(x) {
                             y <- fromJSON(x)
                             z <- as_tibble(flatten(y$series, recursive = FALSE))
                             return(z)
                           });
    results_tbl <- bind_rows(results_list);

    if (DEBUG) {
      u <- jsonlite::fromJSON(results_json[[1]], flatten=TRUE)
      v <- tibble::as_tibble(u$series)
    }
    ## -- OLD STYLE --
    ## results_list <- lapply(results_json,
    ##                        function(s) {
    ##                          u <- jsonlite::fromJSON(s, simplifyVector=FALSE)
    ##                          v <- lapply(u$series,
    ##                                      function(w) {
    ##                                            ## Return data
    ##                                            x <- as.data.frame(do.call(rbind, w$data))
    ##                                            names(x) <- c("period", "value");
    ##                                            x <- transform(x,
    ##                                                           period = as.character(period),
    ##                                                           value = as.numeric(as.character(value)));
    ##                                            ## Bind metadata
    ##                                            y <- cbind(w[grep("data", names(w), invert=TRUE, value=TRUE)], x)
    ##                                            return(y)
    ##                                          })
    ##                              v <- do.call(rbind, v)
    ##                              });
    ## results_table <- do.call(rbind, results_list)
    return(results_tbl) 
  }
}


#' # Download category data from the EIA API
#' @name eia_category_query
#' @title Gets name and id for a single category, and also lists its children categories' names and
#'   ids.
#' @description This function requests and returns data from the EIA API.
#' @param category_id A unique numerical ID of the EIA data category to fetch. If NULL, the API's
#'   root category is returned.
#' @param api_key Required. A valid API key is required and may be obtained from EIA registration
#'   page (see details).
#' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
#'   \code{FALSE}, function returns data in specified output \code{format}.
#' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
#'   JSON formatted output. Note that it is also necessary to set \code{simplify = FALSE} to access
#'   'xml' or 'json' formatted output.
#' 
#' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
#'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
#'   required as part of the registration process.
#'
#'   Note that the number of series in a single request is limited to 100.
#'
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   \donttest{
#'     eia_cat <- eia_category_query(api_key = "Your API Key")
#'   }
eia_category_query <- function(category_id, api_key,#=key_get("EIA_APIKEY"),
                               format="json", simplify=TRUE)
{
  ## Debugging code
  DEBUG <- FALSE;
  if (DEBUG) {
    api_key <- keyring::key_get("EIA_TESTKEY")
    category_id <- "STEO.BREPUUS.A"
    format <- "json"
    eia_cat <- eia_category_query(category_id = "STEO.BREPUUS.A", api_key = api_key)
    eia_cat <- eia_category_query(category_id = "STEO.BREPUUS.A", api_key = api_key, simplify=FALSE)
  }
  if (missing(api_key))
    stop("No api_key provided.")
  if (!tolower(format) %in% c("json","xml") & !simplify) {
    warning(sprintf("Invalid format specified. Should be one of either 'json' or 'xml', returning 'json'."));
    format <- "json"
  }
  ### Series search string composition
  ## a) Output format
  series_suffix <- sprintf("&out=%s", tolower(format));
  ## b) Category ID
  if (!missing(category_id))
    series_suffix <- sprintf("&category_id=%s%s", tolower(category_id), series_suffix);
  ## c) API Key
  category_query <- sprintf("category/?api_key=%s%s", api_key, series_suffix)
  
  ## Return results
  results_json <- eia_get(category_query);

  if (!simplify) {
    return(results_json)
  } else {
    ## Return as nested data frame (tibble)
    results_lst <- fromJSON(eia_cat, flatten=TRUE)
    ## Remove null 'category' list elements 
    results_lst$category <- results_lst$category[lapply(results_lst$category, length) > 0]
    results_tbl <- as_tibble(results_lst$category)
    
    ## -- Hand-crafted method - return as plain data.frame --
    ## results_list <- jsonlite::fromJSON(results_json, simplifyVector=FALSE)
    ## ## Convert child categories to data.frame
    ## x <- as.data.frame(do.call(rbind, results_list$category$childcategories))
    ## names(x) <- c("category_id", "name");
    ## x <- transform(x,
    ##                category_id = as.integer(as.character(category_id)),
    ##                name = as.character(name));
    ## ## Bind metadata
    ## results_tbl <- cbind(results_list$category[grep("childcategories|childseries",
    ##                                                   names(results_list$category),
    ##                                                   invert=TRUE, value=TRUE)], x)
    ## ## NOTE: childseries is empty list for the example here and is manually excluded
    ## ##       Consider more general code to catch and correct empty elements
    ## return(results_tbl)
    return(results_tbl)
  }
}



#' # Download category data from the EIA API
#' @name eia_series_category_query
#' @title Get category names for selected EIA series ID.
#' @description This function requests and returns data from the EIA API.
#' @importFrom jsonlite fromJSON
#' @param series_id A case-sensitive string consisting of letters, numbers, dashes ("-") and
#'   periods (".") that uniquely identifies an EIA series. unique numerical ID of the EIA data
#'   category to fetch.
#' @param api_key Required. A valid API key is required and may be obtained from EIA registration
#'   page (see details).
#' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
#'   \code{FALSE}, function returns data in specified output \code{format}.
#' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
#'   JSON formatted output. Note that it is also necessary to set \code{simplify = FALSE} to access
#'   'xml' or 'json' formatted output.
#' 
#' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
#'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
#'   required as part of the registration process.
#'
#'   Note that the number of series in a single request is limited to 100.
#'
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   \donttest{
#'     eia_cat <- eia_series_category_query(series_id="ELEC.GEN.ALL-AK-99.A", api_key = "Your API Key")
#'   }
eia_series_category_query <- function(series_id, api_key, format="json", simplify=TRUE)
{
  ## Debugging code
  DEBUG <- FALSE;
  if (DEBUG) {
    series_id <- "ELEC.GEN.ALL-AK-99.A"
    api_key <- keyring::key_get("EIA_TESTKEY")
    format <- "json"
    eia_cat <- eia_series_category_query(series_id="ELEC.GEN.ALL-AK-99.A", api_key = api_key)
  }
  if (missing(series_id))
    stop("No series_id provided.")
  if (missing(api_key))
    stop("No api_key provided.")
  if (!tolower(format) %in% c("json","xml") & !simplify) {
    warning(sprintf("Invalid format specified. Should be one of either 'json' or 'xml', returning 'json'."));
    format <- "json"
  }
  ### Series search string composition
  ## a) Output format
  series_suffix <- sprintf("&out=%s", tolower(format));
  ## b) API Key
  series_suffix <- sprintf("&api_key=%s%s", api_key, series_suffix);
  ## c) Series ID
  series_category_query <- sprintf("series/categories/?series_id=%s%s", series_id, series_suffix);
  
  ## Return results
  results_json <- eia_get(series_category_query);
  
  if (!simplify) {
    return(results_json)
  } else {
    ##  Return as nested data frame (tibble)
    tmp_results_tbl <- fromJSON(results_json);
    results_tbl <- as_tibble(flatten(tmp_results_tbl$series_categories, recursive = FALSE));
    ## results_lst <- jsonlite::fromJSON(results_json, simplifyVector=FALSE)
    ## results_tbl <- do.call(rbind,
    ##                          lapply(results_lst$series_categories,
    ##                                 function(w) {
    ##                                   ## Return data frame of all series member categories
    ##                                   x <- sapply(w$categories,
    ##                                               function(v) as.data.frame(v, stringsAsFactors=FALSE),
    ##                                               USE.NAMES=TRUE, simplify=FALSE)
    ##                                   y <- do.call(rbind, x)
    ##                                   ## Append series_id
    ##                                   z <- cbind(w[grep("categories", names(w), invert=TRUE, value=TRUE)], y)
    ##                                   return(z)
    ##                                 })
    ##                          );
    return(results_tbl)
  }
}


#' # Download EIA Geoset series IDs
#' @name eia_geoset_query
#' @title Gets a set of the series belonging to a specified geoset_id.
#' @description -- TO BE COMPLETED --
#' @param geoset_id A case-sensitive string consisting of letters, numbers, dashes ("-") and
#'   periods (".") that uniquely identifies an EIA geoset.
#' @param region_ids A vector of region identifiers to be included in the geoset request.
#' @param api_key Required. A valid API key is required and may be obtained from EIA registration
#'   page (see details).
#' @param num Number of values to be returns, to be used in conjunction with the \code{end_date}
#'   parameter to return the n-values from an end point. The EIA API will only accept the \code{num}
#'   parameter in conjunction with the \code{end_date} parameter and not
#'   \code{start_date}. If \code{num} is supplied, the \code{start_date} argument will be ignored.
#' @param start_date Optional start date filter---if supplied, returns data after the specified
#'   start date. Argument accepts valid numeric, character, date or datetime format object. If
#'   numeric it must be in \%Y form (i.e. four digit year).  If character string, it must be in
#'   year-month-date (\%Y-\%m-\%d) format.
#' @param end_date Optional end date filter---if supplied, returns data up to and including the
#'   specified end date. Argument accepts valid numeric, character, date or datetime format
#'   object. If numeric it must be in \%Y form (i.e. four digit year).  If character string, it must
#'   be in year-month-date (\%Y-\%m-\%d) format.
#' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
#'   \code{FALSE}, function returns data in specified output \code{format}.
#' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
#'   JSON formatted output. Only applies if \code{simplify = FALSE}.
#' 
#' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
#'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
#'   required as part of the registration process.
#'
#'   Note that the number of series in a single request is limited to 100.
#'
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   \donttest{
#'     eia_cat <- eia_geoset_query(geoset_id="ELEC.GEN.ALL-99.M", region_ids=c("USA"),
#'                                 api_key = "Your API Key")
#'   }
if (FALSE) {
  eia_cat <- eia_geoset_query(geoset_id = "ELEC.PRICE.ALL.M", region_ids="USA",
                              api_key = keyring::key_get("EIA_TESTKEY"))
}
eia_geoset_query <- function(geoset_id, region_ids, api_key,
                            start_date=NULL, end_date=NULL, num=NULL,
                            format="json", simplify=TRUE)
  eia_geoset_relation_query(geoset_id=geoset_id, region_ids=region_ids, api_key=api_key,
                            start_date=start_date, end_date=end_date, num=num,
                            format=format, simplify=simplify)


#' # Download EIA Relation series IDs
#' @name eia_relation_query
#' @title Gets a set of the series belonging to a specified relation_id.
#' @description -- TO BE COMPLETED --
#' @param relation_id A case-sensitive string consisting of letters, numbers, dashes ("-") and
#'   periods (".") that uniquely identifies an EIA geoset.
#' @param region_ids A character string specifying a single region identifier.
#' @param api_key Required. A valid API key is required and may be obtained from EIA registration
#'   page (see details).
#' @param num Number of values to be returns, to be used in conjunction with the \code{end_date}
#'   parameter to return the n-values from an end point. The EIA API will only accept the \code{num}
#'   parameter in conjunction with the \code{end_date} parameter and not
#'   \code{start_date}. If \code{num} is supplied, the \code{start_date} argument will be ignored.
#' @param start_date Optional start date filter---if supplied, returns data after the specified
#'   start date. Argument accepts valid numeric, character, date or datetime format object. If
#'   numeric it must be in \%Y form (i.e. four digit year).  If character string, it must be in
#'   year-month-date (\%Y-\%m-\%d) format.
#' @param end_date Optional end date filter---if supplied, returns data up to and including the
#'   specified end date. Argument accepts valid numeric, character, date or datetime format
#'   object. If numeric it must be in \%Y form (i.e. four digit year).  If character string, it must
#'   be in year-month-date (\%Y-\%m-\%d) format.
#' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
#'   \code{FALSE}, function returns data in specified output \code{format}.
#' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
#'   JSON formatted output. Only applies if \code{simplify = FALSE}.
#' 
#' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
#'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
#'   required as part of the registration process.
#'
#'   Note that the number of series in a single request is limited to 100.
#'
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   \donttest{
#'     eia_cat <- eia_geoset_query(relation_id = "ELEC.PRICE.ALL.M|ENDSEC", region_ids="USA",
#'                                 api_key = "Your API Key")
#'   }
if (FALSE) {
  eia_cat <- eia_relation_query(relation_id = "ELEC.PRICE.ALL.M|ENDSEC", region_ids="USA",
                                api_key = keyring::key_get("EIA_TESTKEY"))
}
eia_relation_query <- function(relation_id, region_ids, api_key,
                               start_date=NULL, end_date=NULL, num=NULL,
                               format="json", simplify=TRUE)
  eia_geoset_relation_query(relation_id=relation_id, region_ids=region_ids, api_key=api_key,
                            start_date=start_date, end_date=end_date, num=num,
                            format=format, simplify=simplify)


#' # Download EIA geoset/relation series IDs
#' @name eia_geoset_relation_query
#' @title Gets a set of the series belonging to a specified relation_id.
#' @description -- TO BE COMPLETED --
#' @param geoset_id A case-sensitive string consisting of letters, numbers, dashes ("-") and periods
#'   (".") that uniquely identifies an EIA geoset.
#' @param relation_id A case-sensitive string consisting of letters, numbers, dashes ("-") and
#'   periods (".") that uniquely identifies an EIA geoset.
#' @param region_ids A character string specifying a single region identifier (relation query) or
#'   character vector of one or more region identifiers (geoset query) to be included in the API
#'   request.
#' @param api_key Required. A valid API key is required and may be obtained from EIA registration
#'   page (see details).
#' @param num Number of values to be returns, to be used in conjunction with the \code{end_date}
#'   parameter to return the n-values from an end point. The EIA API will only accept the \code{num}
#'   parameter in conjunction with the \code{end_date} parameter and not
#'   \code{start_date}. If \code{num} is supplied, the \code{start_date} argument will be ignored.
#' @param start_date Optional start date filter---if supplied, returns data after the specified
#'   start date. Argument accepts valid numeric, character, date or datetime format object. If
#'   numeric it must be in \%Y form (i.e. four digit year).  If character string, it must be in
#'   year-month-date (\%Y-\%m-\%d) format.
#' @param end_date Optional end date filter---if supplied, returns data up to and including the
#'   specified end date. Argument accepts valid numeric, character, date or datetime format
#'   object. If numeric it must be in \%Y form (i.e. four digit year).  If character string, it must
#'   be in year-month-date (\%Y-\%m-\%d) format.
#' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
#'   \code{FALSE}, function returns data in specified output \code{format}.
#' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
#'   JSON formatted output. Only applies if \code{simplify = FALSE}.
#' 
#' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
#'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
#'   required as part of the registration process.
#'
#'   Note that the number of series in a single request is limited to 100.
#'
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
## #' @examples
## #'   \donttest{
## #'     eia_cat <- eia_geoset_query(geoset_id="ELEC.GEN.ALL-99.M", region_ids=c("USA"),
## #'                                 api_key = "Your API Key")
## #'   }
#' @keywords internal
eia_geoset_relation_query <- function(geoset_id, relation_id, region_ids, api_key,
                                      start_date=NULL, end_date=NULL, num=NULL,
                                      format="json", simplify=TRUE)
{
  ## Debugging code
  DEBUG <- FALSE;
  if (DEBUG) {
    geoset_id <- "ELEC.GEN.ALL-99.M"
    relation_id <- "ELEC.PRICE.ALL.M|ENDSEC"
    region_ids <- c("USA")
    api_key <- keyring::key_get("EIA_TESTKEY")
    format <- "json"
    eia_res <- eia_geoset_query(geoset_id=geoset_id, region_ids=region_ids, api_key = api_key)#, simplify=FALSE)
  }
  if ((missing(geoset_id) & missing(relation_id)) |
      (!missing(geoset_id) & !missing(relation_id)))
    stop("One and only one of geoset_id or relation_id must be specified.")
  if (missing(region_ids))
    stop("No region_ids provided.")
  if (missing(api_key))
    stop("No api_key provided.")
  if (!tolower(format) %in% c("json","xml") & !simplify) {
    warning(sprintf("Invalid format specified. Should be one of either 'json' or 'xml', returning 'json'."));
    format <- "json"
  }
  ### Series search string composition
  ## a) Output format
  query_suffix <- sprintf("&out=%s", tolower(format));
  ## b) End date
  if (!is.null(end_date)) {
    # end_date <- format(end_date, format="%Y%m%d");
    query_suffix <- sprintf("&end=%s%s", end_date, query_suffix)
  }
  ## c) Number records
  if (!is.null(num))
    query_suffix <- sprintf("&num=%s%s", num, query_suffix)
  ## d) Start date
  if (is.null(num) & !is.null(start_date)) {
    # start_date <- format(start_date, format="%Y%m%d");
    query_suffix <- sprintf("&start=%s%s", start_date, query_suffix)
  }
  ## e) API Key
  query_suffix <- sprintf("&api_key=%s%s", api_key, query_suffix)
  ## f) Region IDs - split Series ID into groups of 100 or fewer, in compliance with EIA API
  # query_suffix <- sprintf("&region=%s%s", paste(region_ids, collapse=","), query_suffix)
  ## g) Check if Create EIA API query
  if (!missing(geoset_id)) {
    query_suffix <- sprintf("&regions=%s%s", paste(region_ids, collapse=","), query_suffix)
    query <- sprintf("geoset/?geoset_id=%s%s", geoset_id, query_suffix);
    query_name <- "geoset"
    }
  if (!missing(relation_id)) {
    query_suffix <- sprintf("&region=%s%s", paste(region_ids, collapse=","), query_suffix)
    query <- sprintf("relation/?relation_id=%s%s", relation_id, query_suffix);
    query_name <- "relation"
  }
  ## Query EIA API
  results_json <- eia_get(query);
  
  if (!simplify) {
    return(results_json)
  } else {
    ## -- CONSIDER TRANSFORMING OUTPUT TO tibble
    ## tmp_results_tbl <- fromJSON(results_json, flatten=T);
    ## results_tbl <- as_tibble(flatten(tmp_results_tbl[[query_name]], recursive = TRUE));
    results_lst <- jsonlite::fromJSON(results_json, simplifyVector=FALSE)
    ## Convert results to table
    series_tables <- lapply(results_lst[[query_name]]$series, ## results_list$geoset$series,
                            function(x) {
                              ## Return data
                              y <- as.data.frame(do.call(rbind, x$data))
                              names(y) <- c("period", "value");
                              y <- transform(y,
                                             period = as.character(period),
                                             value = as.numeric(as.character(value)));
                              ## Bind metadata
                              ## -- First replace NULL values with NA
                              idx_null <- sapply(x[grep("data", names(x), invert=TRUE)],
                                                 is.null)
                              x[idx_null] <- NA_character_;
                              z <- cbind(x[grep("data", names(x), invert=TRUE)], y)
                            })
    ## -- Replace NULL values with NA
    idx_null <- sapply(results_lst[[query_name]][grep("series", names(results_lst[[query_name]]),
                                                      invert=TRUE)],
                       is.null)
    results_lst[[query_name]][idx_null] <- NA_character_;
    results_tbl <- cbind(results_lst[[query_name]][grep("series", names(results_lst[[query_name]]),
                                                        invert=TRUE)],
                         do.call(rbind, series_tables));
    results_tbl <- do.call(rbind, series_tables)
    rownames(results_tbl) <- 1:nrow(results_tbl);
    return(results_tbl) 
  }
}


## #' # Download EIA Relation series IDs
## #' @name eia_relation_query
## #' @title Gets a set of the series belonging to a specified relation_id.
## #' @description -- TO BE COMPLETED --
## #' @param geoset_id A case-sensitive string consisting of letters, numbers, dashes ("-") and
## #'   periods (".") that uniquely identifies an EIA geoset.
## #' @param region_ids A vector of region identifiers to be included in the geoset request.
## #' @param api_key Required. A valid API key is required and may be obtained from EIA registration
## #'   page (see details).
## #' @param num Number of values to be returns, to be used in conjunction with the \code{end_date}
## #'   parameter to return the n-values from an end point. The EIA API will only accept the \code{num}
## #'   parameter in conjunction with the \code{end_date} parameter and not
## #'   \code{start_date}. If \code{num} is supplied, the \code{start_date} argument will be ignored.
## #' @param start_date Optional start date filter---if supplied, returns data after the specified
## #'   start date. Argument accepts valid numeric, character, date or datetime format object. If
## #'   numeric it must be in \%Y form (i.e. four digit year).  If character string, it must be in
## #'   year-month-date (\%Y-\%m-\%d) format.
## #' @param end_date Optional end date filter---if supplied, returns data up to and including the
## #'   specified end date. Argument accepts valid numeric, character, date or datetime format
## #'   object. If numeric it must be in \%Y form (i.e. four digit year).  If character string, it must
## #'   be in year-month-date (\%Y-\%m-\%d) format.
## #' @param simplify Logical. If \code{TRUE}, the function returns data in a data frame. If
## #'   \code{FALSE}, function returns data in specified output \code{format}.
## #' @param format API return format. Valid values are 'xml' or 'json'. Default is for API to return
## #'   JSON formatted output. Only applies if \code{simplify = FALSE}.
## #' 
## #' @details Users of the EIA API are required to obtain an API Key from the EIA Open Data
## #'   registration page (\url{https://www.eia.gov/opendata/register.php}).  A valid email address is
## #'   required as part of the registration process.
## #'
## #'   Note that the number of series in a single request is limited to 100.
## #'
## #' @author David Mitchell <david.pk.mitchell@@gmail.com>
## #' @export
## #' @examples
## #'   \donttest{
## #'     eia_cat <- eia_geoset_query(geoset_id="ELEC.GEN.ALL-99.M", region_ids=c("USA"),
## #'                                 api_key = "Your API Key")
## #'   }
## eia_geoset_query <- function(geoset_id, region_ids, api_key,
##                              start_date=NULL, end_date=NULL, num=NULL,
##                              format="json", simplify=TRUE)
## {
##   ## Debugging code
##   DEBUG <- FALSE;
##   if (DEBUG) {
##     geoset_id <- "ELEC.GEN.ALL-99.M"
##     region_ids <- c("USA")
##     api_key <- keyring::key_get("EIA_TESTKEY")
##     format <- "json"
##     eia_res <- eia_geoset_query(geoset_id=geoset_id, region_ids=region_ids, api_key = api_key) #, simplify=FALSE)
##   }
##   if (missing(geoset_id))
##     stop("No geoset_id provided.")
##   if (missing(region_ids))
##     stop("No region_ids provided.")
##   if (missing(api_key))
##     stop("No api_key provided.")
##   if (!tolower(format) %in% c("json","xml") & !simplify) {
##     warning(sprintf("Invalid format specified. Should be one of either 'json' or 'xml', returning 'json'."));
##     format <- "json"
##   }
##   ### Series search string composition
##   ## a) Output format
##   geoset_suffix <- sprintf("&out=%s", tolower(format));
##   ## b) End date
##   if (!is.null(end_date)) {
##     # end_date <- format(end_date, format="%Y%m%d");
##     geoset_suffix <- sprintf("&end=%s%s", end_date, geoset_suffix)
##   }
##   ## c) Number records
##   if (!is.null(num))
##     geoset_suffix <- sprintf("&num=%s%s", num, geoset_suffix)
##   ## d) Start date
##   if (is.null(num) & !is.null(start_date)) {
##     # start_date <- format(start_date, format="%Y%m%d");
##     geoset_suffix <- sprintf("&start=%s%s", start_date, geoset_suffix)
##   }
##   ## e) API Key
##   geoset_suffix <- sprintf("&api_key=%s%s", api_key, geoset_suffix)
##   ## ## f) Region IDs - split Series ID into groups of 100 or fewer, in compliance with EIA API
##   ## series_id <- split(series_id, ceiling(seq_along(series_id) / max_series))
##   ## f) Region IDs - split Series ID into groups of 100 or fewer, in compliance with EIA API
##   geoset_suffix <- sprintf("&regions=%s%s", paste(region_ids, collapse=","), geoset_suffix)
##   ## g) Create EIA API query
##   geoset_query <- sprintf("geoset/?geoset_id=%s%s", geoset_id, geoset_suffix);

##   ## Query EIA API
##   results_json <- eia_get(geoset_query);
  
##   if (!simplify) {
##     return(results_json)
##   } else {
##     results_list <- jsonlite::fromJSON(results_json, simplifyVector=FALSE)
##     ## Convert results to table
##     series_tables <- lapply(results_list$geoset$series,
##                            function(x) {
##                              ## Return data
##                              y <- as.data.frame(do.call(rbind, x$data))
##                              names(y) <- c("period", "value");
##                              y <- transform(y,
##                                             period = as.character(period),
##                                             value = as.numeric(as.character(value)));
##                              ## Bind metadata
##                              ## -- First replace NULL values with NA
##                              idx_null <- sapply(x[grep("data", names(x), invert=TRUE)],
##                                                 is.null)
##                              x[idx_null] <- NA_character_;
##                              z <- cbind(x[grep("data", names(x), invert=TRUE)], y)
##                            })
##     ## -- Replace NULL values with NA
##     ## idx_null <- sapply(results_list$geoset[grep("data", names(results_list$geoset), invert=TRUE)],
##     ##                   is.null)
##     ## results_list$geoset[idx_null] <- NA_character_;
##     ## results_table <- cbind(results_list$geoset[grep("series", names(results_list$geoset), invert=TRUE)],
##     ##                        do.call(rbind, series_tables));
##     results_table <- do.call(rbind, series_tables)
##     rownames(results_table) <- 1:nrow(results_table);
##     return(results_table) 
##   }
## }



