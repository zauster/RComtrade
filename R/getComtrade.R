#' Download data from Comtrade
#'
#' Download trade data from the Comtrade API, provided by the United Nations.
#'
#' @param region the reporting region/country
#' @param partner the partner region
#' @param time the time period as a string. Can be a year, i.e. "2013" or a month in a year, i.e. "201301", depending on whether freq is "A" or "M"
#' @param freq frequency, can be either "A" for annual data or "M" for monthly data
#' @param type type of trade flow. Currently only "C" for commodity. In the future maybe also "S" for Services.
#' @param classification revision of the HS classification that is used. Default is "HS" which corresponds to "HS, as reported", but can also be "H0", "H1", ..., "ST" (SITC classification, as reported), "S1", ... or "BEC" (Broad Economic Categories). The homepage has more information on this.
#' @param tradeflow the tradeflow, can be "1" for imports, "2" for exports or "all" for both (default)
#' @param cc the classification code for the commodity, such as "0101" for live animals, according to the HS classification. Special codes are "TOTAL" for the total trade between reporter and partner, "ALL" for all commodities or "AG1" to "AG6" for detailed codes at specific level.
#' @param fmt the format, either "csv" for a .CSV download or "json" for a JSON download. JSON also provides a validation list, which contains any errors and query information.
#' @param maxrec the maximum number of records, default = 100000.
#'
#' @details Downloads the data and returns it in usable format, see \code{See also} for the API-homepage
#' @return a list with a validation list (if fmt = "JSON" was specified) and a data.frame which contains the data
#' @seealso \link{http://comtrade.un.org/data/doc/api/}
#' @examples dta <- getComtrade(region = "40", partner = "688")
#' @export

getComtrade <- function(region,                # reporting region
                        partner,               # partner region
                        time = "now",          # time period
                        freq = "A",    # frequency
                        type = "C",    # type of trade (c=commodities)
                        classification = "HS", # classification
                        tradeflow = "all",     # trade flow
                        cc = "TOTAL",          # classification code
                        fmt = "json",          # format (json or csv)
                        maxrec = 100000)
{
    string <- paste("http://comtrade.un.org/api/get?",
                    "max=", maxrec, "&", #maximum no. of records returned
                    "type=", type, "&", #type of trade (c=commodities)
                    "freq=", freq, "&", #frequency
                    "px=", classification, "&", #classification
                    "ps=", time, "&", #time period
                    "r=", region, "&", #reporting area
                    "p=", partner, "&", #partner country
                    "rg=", tradeflow, "&", #trade flow
                    "cc=", cc, "&", #classification code
                    "fmt=", fmt,        #Format
                    sep = ""
                    )

    if(fmt == "csv") {
        dta <- read.csv(string, header = TRUE)
    } else {
        if(fmt == "json" ) {

            ## download the file
            raw.data <- fromJSON(file = string)
            datalst <- raw.data$dataset

            
            if(length(datalst) > 0) {
                var.names <- names(datalst[[1]])
                
                datalst <- as.data.frame(t(sapply(datalst, rbind)))
                data <- as.data.frame(sapply(datalst, clearCol))
                colnames(data) <- var.names
            }

            ## get the validation
            validation <- unlist(raw.data$validation, recursive = TRUE)
            
            dta <- list(validation = validation,
                        data = data)
        } else {
            message("Use either 'json' or 'csv'. Exiting. ")
        }   
    }
    return(dta)
}
