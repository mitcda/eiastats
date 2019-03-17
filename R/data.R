### EIA data sets

#' @name eia_bulk_cachelist
#' @title Cached list of bulk data sets available from the EIA
#' @description This data is a cached set of the EIA bulk manifest file
#'   (<http://api.eia.gov/bulk/manifest.txt>) produced by the \code{\link{eia_bulk_manifest}}
#'   function. By default functions \code{\link{eia_bulk_search}}, \code{\link{eia_bulk_browse}} and
#'   \code{\link{eia_bulk_data}} use this dataset data if the \code{update_cache} parameter is
#'   \code{TRUE}.
#' 
#' @format A data frame containing 19 columns:
#' \itemize{
#'   \item \code{last_updated} date dataset last updated
#'   \item \code{category_id} EIA dataset category identifier
#'   \item \code{name} EIA dataset name
#'   \item \code{data_set} EIA dataset identifier
#'   \item \code{identifier} EIA dataset identifier
#'   \item \code{title} EIA datset name
#'   \item \code{description} Long form description of dataset
#'   \item \code{keyword} List of dataset keywords
#'   \item \code{publisher} Publisher (U.S. Energy Information Administration)
#'   \item \code{person} Responsible person
#'   \item \code{mbox} Corresponding e-mail address
#'   \item \code{accessLevel} Dataset access level (public)
#'   \item \code{accessLevelComment} Dataset access level comment
#'   \item \code{accessURL} Dataset URL
#'   \item \code{webService} EIA web address
#'   \item \code{format} Available dataset formats
#'   \item \code{temporal} Dataset frequency
#'   \item \code{modified} Dataset modification date
#'   \item \code{spatial} Spatial data availability (e.g. country, state, city,
#'     mine/plant/refinery level).
#' }
"eia_bulk_cachelist"

