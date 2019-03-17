#' url chunks to be used in API calls
#' @name eia_api
#' @title EIA URL
#' @description Function specifying the EIA API url, used inside other functions in this package
#' @return string specifying the EIA API base url.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
eia_api <- function()
  base_url <- "http://api.eia.gov"


#' url chunks to be used in API calls
#' @name eia_url
#' @title EIA URL
#' @description Function specifying the EIA url, used inside other functions in this package
#' @return string specifying the EIA base url.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
eia_url <- function()
  base_url <- "https://www.eia.gov/"


#' url chunks to be used in API calls
#' @name eia_ua
#' @title EIA user agent
#' @description Function specifying the EIA API user agent, used inside other functions in this package
#' @importFrom httr user_agent
#' @return string specifying the EIA user agent.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
eia_ua <- function()
  user_agent("https://github.com/mitcda/eiastats")


#' Call the EIA API and return result
#' @name eia_get
#' @title Call EIA API and return results
#' @description Function that calls the EIA API and returns results
#' @importFrom httr build_url content GET http_type http_error http_status http_type modify_url parse_url
#' @param api_query EIA API format query string
#' @param ... other arguments to httr::build_url.
#' @return json contents of page information
#' @export
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
eia_get <- function(api_query)
{
  DEBUG <- FALSE
  if (DEBUG) {
    api_query <- "search/?search_term=name&search_value='crude oil'"
  }
  
  url_string <- file.path(eia_api(), api_query)
  url_list <- parse_url(url_string)
  ## Get URL
  resp <- GET(url_list, eia_ua());

  ## Check for errors
  if (http_error(resp)) {
    error_status <- http_status(resp)
    
    stop(sprintf("EIA API request failed for indicator %s\nmessage: %s\ncategory: %s\nreason: %s \nurl: %s",
                 indicator,
                 error_status$message,
                 error_status$category,
                 error_status$reason,
                 url_string),
         call. = FALSE)
  }
  ## Check format
  if (!http_type(resp) %in% c("application/json", "text/html")) {
    stop("API call executed successfully, but did not return expected json/html format", call. = FALSE)
  }

  results_json <- content(resp, as = "text")
  return(results_json)
}


#' EIA search function
#' @name eia_search
#' @title EIA API search data query
#' @description Function to search the EIA data catalogue
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate days
#' @param ... any valid EIA API search argument. Valid search arguments are: \code{series_id},
#'   \code{name} and \code{last_updated}. The function will return results for every search string
#'   and/or every search argument where multiple strings or multiple search arguments supplied---see
#'   \link{Examples} for relevant examples.
#' @param page_num default page number (default: NULL).
#' @param rows_per_page number of rows per page (default: NULL).
#' @return Returns the Series IDs and details as an R data frame.
#' @details Search terms provided to arguments \code{series_id} and \code{name} must be
#'   \code{character} objects and \code{last_updated} searches must be date type objects
#'   (i.e. either \code{Date}, \code{chron} or \code{POSIXt}) specifying a date range to check. Note
#'   that if only one date value is provided, the query will search for all series updated on that
#'   date.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   ## Series ID search
#'   eia_search(series_id = "PET.MB")
#'
#'   ## Keyword search
#'   eia_search(name = "crude oil");
#' 
#'   ## Date search
#'   eia_search(last_updated = as.Date(c("2015-01-01", "2015-01-02")))
eia_search <- function(..., page_num=NULL, rows_per_page=NULL, simplify=TRUE) {
  args <- list(...);
  if (missing(args))
    stop("No search term provided.")

  DEBUG <- FALSE;
  if (DEBUG) {
     args <- list(name = "crude oil");
  }

  ## Check search terms are valid - stop if any invalid names specified
  arg_names <- names(args);
  if (any(!arg_names %in% c("series_id", "name", "last_updated")))
    stop(sprintf("Unused search term(s): %s, in search.",
                 arg_names[!arg_names %in% c("series_id", "name", "last_updated")]));
  
  ## Check last_updated search term are date/time class objects
  if (any(arg_names %in% "last_updated")) {
    ## Stop if last_updated not date/datetime object
    if ( !class(search$last_updated) %in% c("Date", "chron", "POSIXt") )
      stop(sprintf("Last updated search must be of class: Date, chron or POSIXt."))
    ## Convert last_updated to appropriately formatted datetime string
    if (length(args$last_updated) == 1) {
      args <- transform(args,
                        last_updated = c(last_updated, last_updated + days(1)));
    }
    args$last_updated <- sprintf("[%sTO%s]",
                                 format(min(args$last_updated[1:2]), "%Y-%m-%dT%TZ"),
                                 format(max(args$last_updated[1:2]), "%Y-%m-%dT%TZ"));
  }
  
  ## Parse search terms and set names
  args <- unlist(sapply(args, c), recursive=FALSE);
  names(args) <- sub("\\d+", "", names(args));

  param_strings <- mapply(function(x, y)
    sprintf("search/?search_term=%s&search_value=%s", x,
            ifelse(x  == "last_updated", y, shQuote(y))),
    names(args), args)

  if (!is.null(rows_per_page))
    param_strings <- sprintf("%s&%s=%d", param_strings, "rows_per_page", rows_per_page)

  if (!is.null(page_num))
    param_strings <- sprintf("%s&%s=%d", param_strings, "page_num", page_num)
  ## Return results
  results_json <- lapply(param_strings,
                         function(x) {
                           y <- eia_get(x);
                           z <- jsonlite::fromJSON(y, flatten=TRUE)
                           w <- as.data.frame(sapply(z$response$docs, as.character, USE.NAMES=FALSE));
                           names(w) <- unlist(strsplit(z$responseHeader$params$fl, ","))
                           return(w)
                         })
  results_json <- do.call(cbind, results_json);
  names(results_json) <- sub("\\w+\\.(.+)", "\\1", names(results_json));
  return(results_json);
}
