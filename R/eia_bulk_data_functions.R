#' EIA bulk file download page
#' @name eia_bulk_url
#' @title EIA Bulk Download URL
#' @description Function specifying the EIA url, used inside other functions in this package
#' @importFrom httr build_url http_error parse_url 
#' @param path EIA bulk download page path.
#' @return string specifying the EIA bulk download url.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
eia_bulk_url <- function(path="opendata/bulkfiles.php") {
  url_lst <- parse_url(eia_url())
  url_lst$path <- path
  url <- build_url(url_lst)
  ## Check URL valid
  if (http_error(GET(url)))
    stop(sprintf("URL (%s) cannot be downloaded", url))
  ## Return results
  return(url)
}


#' Get EIA bulk file manifest
#' @name eia_bulk_manifest
#' @title EIA Bulk Download URL
#' @description Function to download the list of bulk EIA files
#' @importFrom rvest follow_link html_attr html_nodes html_session html_text
#' @importFrom httr GET http_error status_code progress
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @param path EIA bulk download page path
#' @return string specifying the EIA bulk download url.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @keywords internal
eia_bulk_manifest <- function(url=eia_bulk_url()) {
  suppressWarnings(s <- html_session(url));
  ## Get path to 'Past & Future Releases' page
  paths <- html_nodes(s, "a");
  paths <- paths[grepl("manifest\\.txt", paths)];
  path <- html_attr(paths, "href");

  resp <- GET(path, eia_ua(), progress());
  if (http_error(resp)) {
    stop(
      sprintf(
        "EIA bulk file manifest file request failed (Error code: %s)\nInvalid URL: %s",
        status_code(resp),
        path
      ),
      call. = FALSE
    )
  }
  ## Check content-type is compliant
  if (!http_type(resp) %in% c("text/plain")) {
    stop("EIA bulk file manifext file is not plain text.", call. = FALSE)
  }
  ## Extract results
  series_list <- fromJSON(content(resp, as="text"));
  results_list <- lapply(series_list$dataset,
                          function(s) 
                            x <- as.data.frame(s));
  results_table <- suppressWarnings(bind_rows(results_list));
  return(results_table);
}


#' Browse EIA bulk dataset cachelist
#' @name eia_bulk_browse
#' @title EIA browse bulk dataset cache
#' @description Function to browse selected fields of the bulk dataset cachelist
#' @param fields Case-insensitive list of EIA bulk dataset field names. Includes \code{category_id},
#'   \code{data_set}, \code{name} and \code{description} by default.
#' @param update_cache Logical expression, if FALSE (default), use the cached list of available RBA
#'   tables (\code{rba_cachelist}), if TRUE, update the list of available datasets.
#' @return data frame of selected
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @details Other available field names include: \code{category_id}, \code{identifier},
#'   \code{title}, \code{keyword}, \code{publisher}, \code{person}, \code{mbox}, \code{accessLevel},
#'   \code{accessLevelComment}, \code{accessURL}, \code{webService}, \code{format}, \code{temporal},
#'   \code{modified}, and \code{spatial} (see \code{\link{eia_bulk_cachelist}}).
#' @export
#' @examples
#'   \donttest{
#'     eia_bulk_browse()
#'     eia_bulk_browse(fields=c("category_id", "data_set", "name"))
#'   }
eia_bulk_browse <- function(fields=c("category_id", "data_set", "name", "description"),
                            update_cache=FALSE) {
  ## Update metadata
  if (update_cache) {
    eia_cache <- eia_bulk_manifest();
  } else {
    eia_cache <- eiastats::eia_bulk_cachelist;
  }
  ## Check EIA fields
  bool_fields <- tolower(fields) %in% tolower(names(eia_cache))
  if (any(!bool_fields))
    stop(sprintf("Field names: %s not in cache",
                 paste(fields[!tolower(fields) %in% tolower(names(eia_cache))], sep=", ")))
  ## Return list of matching EIA bulk datasets
  z <- eia_cache[, match(tolower(fields), tolower(names(eia_cache)))]
  return(z);
}


#' Browse EIA bulk dataset manifest
#' @name eia_bulk_search
#' @title EIA bulk data search
#' @description Function to list bulk EIA datasets matching the specified pattern
#' @param pattern Character string or regular expression to be matched.
#' @param fields Character vector of column names through which to search. By default, the function
#'   searches 'data_set' -- dataset identifier, 'name' -- series name, 'description' -- series
#'   description and 'keyword'.
#' @param ignore.case Case sensitive pattern match or not.
#' @param update_cache Logical expression, if FALSE (default), use the cached list of available RBA
#'   tables (\code{rba_cachelist}), if TRUE, update the list of available datasets.
#' @return data frame of selected
#' @details Other available field names include: \code{category_id}, \code{identifier},
#'   \code{title}, \code{keyword}, \code{publisher}, \code{person}, \code{mbox}, \code{accessLevel},
#'   \code{accessLevelComment}, \code{accessURL}, \code{webService}, \code{format}, \code{temporal},
#'   \code{modified}, and \code{spatial} (see \code{\link{eia_bulk_cachelist}}).
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   \donttest{
#'     eia_bulk_search(pattern = "energy outlook 2019")
#'     eia_bulk_search(pattern = "annual energy outlook")
#'     eia_bulk_search(pattern = "international energy outlook")
#'   }
eia_bulk_search <- function(pattern, fields=c("data_set", "name", "description", "keyword"),
                            ignore.case=TRUE, update_cache=FALSE) {
  ## If not pattern specified, then stop
  if (missing(pattern))
    stop("No pattern specified.");
  ## Update metadata
  if (update_cache) {
    eia_cache <- eia_bulk_manifest();
  } else {
    eia_cache <- eiastats::eia_bulk_cachelist;
  }
  if (any(!fields %in% names(eia_cache)))
    stop(sprintf("Field names: %s not in cache", fields[!fields %in% names(eia_cache)]))
  ## Return list of matching EIA bulk datasets
  match_index <- sapply(fields,
                        function(field) grep(pattern, eia_cache[, field], ignore.case=ignore.case));
  match_index <- sort(unique(unlist(match_index)));
  z <- eia_cache[match_index, unique(c("category_id", fields))];
  return(z);
}


## EIA API bulk data download function
#' @name eia_bulk_data
#' @title Bulk download data from the EIA API
#' @description This function accesses EIA bulk files. (The EIA bulk download facility does not
#'   require a registered user API key.)
#' @importFrom httr content GET http_type http_error http_status progress status_code write_disk
#' @importFrom jsonlite flatten stream_in
#' @importFrom tibble as_tibble
#' @param dataset EIA bulk download data set identifier. This argument will accept a regular
#'   expression search string and download all matching bulk download data sets. If
#'   \code{dataset}="All", all bulk download data files will be downloaded and returned in an R data
#'   frame.
#' @param simplify Logical. If \code{TRUE}, the function returns EIA data in data frame objects. If
#'   \code{FALSE}, function returns raw JSON formatted source.
#' @param data_only Logical. If \code{TRUE} (the default), returns only EIA series data, in a data
#'   frame.  If \code{FALSE} it returns data and category classification information. (Presently
#'   only applies to simplified data.)
#' @param update_cache Logical expression, if FALSE (default), use the cached list of available
#'   ABS.Stat datasets, if TRUE, update the list of available datasets.
#' @return Returns either a data frame with EIA bulk series data or a list containing EIA bulk
#'   series data and associatioed catalogue information.
#' @author David Mitchell <david.pk.mitchell@@gmail.com>
#' @export
#' @examples
#'   \donttest{
#'     ## Fetch Annual Energy Outlook 2019 bulk data
#'     eia_ieo <- eia_bulk_data(dataset="AEO.2019");
#'   }
eia_bulk_data <- function(dataset, simplify=TRUE, data_only=TRUE, update_cache=FALSE) {
  ## Check dataset present and valid 
  if (missing(dataset))
    stop("No dataset supplied.");
  ## Return metadata
  if (update_cache) {
    cache <- eia_bulk_manifest();
  } else {
    cache <- eiastats::eia_bulk_cachelist;
  }
  if (!dataset %in% cache$identifier)
    stop(sprintf("%s not a valid EIA bulk dataset.", dataset));
  
  data_url <- cache$accessURL[dataset == cache$identifier]
  local_filename <- basename(data_url)
  
  ## Download data
  cat(sprintf("Downloading: %s", local_filename));
  resp <- GET(data_url,
              write_disk(file.path(tempdir(), local_filename), overwrite=TRUE),
              eia_ua(), progress());

  ## Validate file download
  if (http_error(resp)) {
    stop(
      sprintf(
        "EIA bulk file request failed (Error code: %s)\nInvalid URL: %s", 
        status_code(resp),
        data_url
      ),
      call. = FALSE
    )
  }
  ## Check content-type is compliant
  if (!http_type(resp) %in% c("application/zip", "application/x-zip")) {
    stop("EIA bulk file request did not return Zip file", call. = FALSE)
  }
  ## Extract data file:
  filename <- unzip(file.path(tempdir(), local_filename), list=TRUE)$Name;
  unzip(file.path(tempdir(), local_filename), exdir=tempdir(), filename);

  ## Read raw JSON
  if (DEBUG)
    x_json <- readLines(file(file.path("data-raw", #tempdir(), "AEO2019.txt"
                                       filename)));
  x_json <- readLines(file(file.path(tempdir(), filename)));
 
  if (!simplify) {
    return(x_json)
  } else {
    ## Import and return raw stream-JSON
    x_json_type <- sapply(x_json,
                          function(x)
                            sub("^\\{\\\"(\\w+)\\\":.+$", "\\1", x),
                          USE.NAMES=FALSE);
    ## Note that EIA includes both data 'series' and 'catalogue' tables in
    ## its JSON-formatted bulk data sets. These are streamed in separately
    ## with `jsonlite::stream_in`, and then re-formatted into separate nested
    ## data frames.
    x_list <- lapply(unique(x_json_type),
                     function(x) {
                       ## Separate series data and catalogue information in separate JSON files ..
                       tfile <- tempfile()
                       writeLines(x_json[x_json_type == x], con=tfile)
                       ## .. then process separately.
                       conn <- file(tfile, "rt")
                       cat(sprintf("Importing EIA bulk data (type: %s) ...\n",
                                   sub("(\\w+)_id", "\\1", x)))
                       z <- stream_in(conn); ## , simplifyDataFrame=TRUE
                       close(conn);
                       ## Useful references on flattening and tidying nested JSON data structures:
                       ## https://blog.exploratory.io/working-with-json-data-in-very-simple-way-ad7ebcc0bb89
                       z <- as_tibble(flatten(z)) ## , .name_repair = "unique"
                     });
    if (DEBUG) {
      con_in <- file(file.path("data-raw", "AEOtest.txt"), open = "rt")
      z <- as_tibble(jsonlite::flatten(stream_in(con_in), recursive = FALSE))
      close(con_in)
    }

    names(x_list) <- sub("(\\w+)_id", "\\1", unique(x_json_type))
    if (data_only) {
      return(x_list$series)
    } else {
      return(x_list)
    }
  }
}
               
