#' Get Comtrade dictionary
#'
#' Get a dictionary for a parameter
#'
#' @param parameter which parameter do you want the dictionary for? Specify "HS", "H1" etc for the product classifications (see link below)
#' @details This function downloads the data dictionary from the UN Comtrade API. See link below.
#' @return the dictionary as a data.frame
#' @export

getComtradeDict <- function(parameter = "region")
{
    string <- switch(parameter,
                     region = "http://comtrade.un.org/data/cache/reporterAreas.json",
                     partner = "http://comtrade.un.org/data/cache/partnerAreas.json",
                     HS = "http://comtrade.un.org/data/cache/classificationHS.json",
                     H1 = "http://comtrade.un.org/data/cache/classificationH1.json",
                     H2 = "http://comtrade.un.org/data/cache/classificationH2.json",
                     H3 = "http://comtrade.un.org/data/cache/classificationH3.json",
                     H4 = "http://comtrade.un.org/data/cache/classificationH4.json",
                     ST = "http://comtrade.un.org/data/cache/classificationST.json",
                     S1 = "http://comtrade.un.org/data/cache/classificationS1.json",
                     S2 = "http://comtrade.un.org/data/cache/classificationS2.json",
                     S3 = "http://comtrade.un.org/data/cache/classificationS3.json",
                     S4 = "http://comtrade.un.org/data/cache/classificationHS.json",
                     BEC = "http://comtrade.un.org/data/cache/classificationBEC.json",
                     tradeflow = "http://comtrade.un.org/data/cache/tradeRegimes.json")

    dict <- fromJSON(file = string)
    dict <- as.data.frame(t(sapply(dict[["results"]], unlist)))

    return(dict)
}
