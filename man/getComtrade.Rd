% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getComtrade.R
\name{getComtrade}
\alias{getComtrade}
\title{Download data from Comtrade}
\usage{
getComtrade(region, partner, time = "now", freq = "A", type = "C",
  classification = "HS", tradeflow = "all", cc = "TOTAL", fmt = "json",
  maxrec = 100000)
}
\arguments{
\item{region}{the reporting region/country}

\item{partner}{the partner region}

\item{time}{the time period as a string. Can be a year, i.e. "2013" or a month in a year, i.e. "201301", depending on whether freq is "A" or "M"}

\item{freq}{frequency, can be either "A" for annual data or "M" for monthly data}

\item{type}{type of trade flow. Currently only "C" for commodity. In the future maybe also "S" for Services.}

\item{classification}{revision of the HS classification that is used. Default is "HS" which corresponds to "HS, as reported", but can also be "H0", "H1", ..., "ST" (SITC classification, as reported), "S1", ... or "BEC" (Broad Economic Categories). The homepage has more information on this.}

\item{tradeflow}{the tradeflow, can be "1" for imports, "2" for exports or "all" for both (default)}

\item{cc}{the classification code for the commodity, such as "0101" for live animals, according to the HS classification. Special codes are "TOTAL" for the total trade between reporter and partner, "ALL" for all commodities or "AG1" to "AG6" for detailed codes at specific level.}

\item{fmt}{the format, either "csv" for a .CSV download or "json" for a JSON download. JSON also provides a validation list, which contains any errors and query information.}

\item{maxrec}{the maximum number of records, default = 100000.}
}
\value{
a list with a validation list (if fmt = "JSON" was specified) and a data.frame which contains the data
}
\description{
Download trade data from the Comtrade API, provided by the United Nations.
}
\details{
Downloads the data and returns it in usable format, see \code{See also} for the API-homepage
}
\examples{
dta <- getComtrade(region = "40", partner = "688")
}
\seealso{
\link{http://comtrade.un.org/data/doc/api/}
}

