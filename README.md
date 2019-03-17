# eiastats

## Introduction

This package provides functions to search and download data from the United
States' Energy Information Administration's (EIA) API (see
http://www.eia.gov/beta/api/).  There are currently over a million unique time
series available through the API.  To use the package you'll need a *free* API
key from here: http://www.eia.gov/beta/api/register.cfm.

This package extends on the functions in the
[https://cran.r-project.org/web/packages/EIAdata/index.html](EIAdata)
package.^[The functionality in this package mirrors the functionality in the
[https://cran.r-project.org/web/packages/wbstats/index.html](wbstats) package.]
Details of the EIA API are available at:
https://www.eia.gov/opendata/commands.php.

The package has four principal functions, `eia_search`, `eia_get`,
`eia` and `eia_bulk`.
