#' Clear column
#'
#' Clear column from NULL-values and unlist it
clearCol <- function(lst) {
    lst[sapply(lst, is.null)] <- NA
    unlist(lst)
}
