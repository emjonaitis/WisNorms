#' Clean up missing values in Wisconsin WRAP/ADRC data
#'
#' This function takes in a column of raw Wisconsin WRAP/ADRC data and returns a column with missing value codes replaced as NA.
#' @param x Vector containing untransformed Wisconsin WRAP/ADRC data.
#' @param raw.name Name of the original variable. Should be among the following: anartraw, ANIMALS, AnimTotRaw, bnttot, bnttot30, BOSTON, CRAFTDRE, CRAFTURS, drraw, flucfl, LOGIMEM, MEMUNITS, MINTTOTS, ORRT.Unc_Std, readstn, TRAILA, TRAILB, trla, trlb, tTotal, ttotal, UDSVERTN, waisrtot, wmsrar, wmsrar2
#' @param source Data source. Should be one of the following: ADRC, WRAP
#' @return Vector containing cleaned Wisconsin WRAP/ADRC data.
#' @seealso \code{\link{wis_harm}}
#' @keywords Wisconsin, WRAP, ADRC
#' @export

convert_missing <- function(x, raw.name, source) {
  # if (is.null(raw.name) | !is.character(raw.name) | !(raw.name %in% missval$variable)) {
  #   stop("Unrecognized raw.name value. See the help file for a list of permitted names.")
  # }
  if (!(source %in% c("ADRC","WRAP"))) {
    stop("source must be either 'ADRC' or 'WRAP'.")
  }
  replace <- unlist(strsplit(missval$missval[missval$variable==raw.name & missval$Data.Source==source], split=", "))
  newval <- ifelse(grepl(pattern="missing", x=x, fixed=FALSE)==TRUE, NA,
                   ifelse(length(replace)==0, x,
                          ifelse(max(mapply(FUN=grepl, pattern=replace, x=x, fixed=TRUE))==TRUE, NA, x)))
  newval.n <- tryCatch(as.numeric(newval),
                       warning=function(w) {message(paste("We found a problem.\nVariable:",
                                                          raw.name,
                                                          "\nData source:",source,
                                                          "\nInput value:",x,
                                                          "\nNew value:",newval,
                                                          "\nMissing values:",
                                                          paste(replace, collapse=",")))})
  if (is.null(newval.n)) {
    newval.out <- NA
  } else {newval.out <- newval.n}
  newval.out
}
