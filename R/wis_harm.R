#' Convert raw to crosswalked scores in Wisconsin WRAP/ADRC
#'
#' This function takes in a column of raw Wisconsin WRAP/ADRC data and returns the crosswalked version
#' @param data Vector containing untransformed Wisconsin WRAP/ADRC data.
#' @param raw.names Names of the original variables. Should be among the following: anartraw, ANIMALS, AnimTotRaw, bnttot, bnttot30, BOSTON, CRAFTDRE, CRAFTURS, drraw, flucfl, LOGIMEM, MEMUNITS, MINTTOTS, ORRT.Unc_Std, readstn, TRAILA, TRAILB, trla, trlb, tTotal, ttotal, UDSVERTN, waisrtot, wmsrar, wmsrar2
#' @param id Column containing subject ID. Defaults to Reggieid.
#' @param source Column containing data source. Defaults to Data.Source.
#' @param visno Column containing visit number. Defaults to Visit_Number.
#' @return Data frame containing crosswalked Wisconsin WRAP/ADRC data.
#' @keywords Wisconsin, ADRC, WRAP, crosswalk, harmonization
#' @importFrom magrittr "%>%"
#' @importFrom dplyr ungroup group_by select filter summarize mutate rename arrange recode
#' @importFrom tidyr gather spread
#' @seealso \code{\link{wis_est}}
#' @export
#' @examples

wis_harm <- function(data, raw.names, id="Reggieid", source="Data.Source", visno="Visit_Number") {
  # if (is.null(raw.names) | !is.character(raw.names) | !(min(raw.names %in% missval$variable)==TRUE)) {
  #   stop("Must provide a character vector naming the raw neuropsych variables. See the help file for a list of permitted raw names.")
  # }
  if (is.grouped_df(data)) {
    data <- ungroup(data)
  }
  # Renaming key columns to temporary values for ease of programming
  innames <- c(id, source, visno)
  names(innames) <- c("id.tmp","source.tmp","visno.tmp")
  data.use       <- select(data,
                           id, source, visno, raw.names) %>%
                    rename(., !!!innames)
  source.list <- sort(unique(data.use$source.tmp[data.use$source.tmp != ""]))
  for (s in 1:length(source.list)) {
    if (!(source.list[s] %in% c("ADRC","WRAP"))) {
      stop("Currently supported values of source for this module: ADRC, WRAP")
    }
  }
  data.covs      <- select(data, -raw.names)
  data.nomiss.long <- group_by(data.use, id.tmp, source.tmp, visno.tmp) %>%
                      gather(key="variable", value="value", raw.names) %>%
                      ungroup() %>%
                      mutate(variable.xw = paste(variable, "xw", sep="."),
                             value.nomiss = mapply(FUN=convert_missing, x=value, raw.name=variable, source=source.tmp))
  data.nomiss.wide <- select(data.nomiss.long,
                                    id.tmp, source.tmp, visno.tmp, variable, value.nomiss) %>%
                      group_by(id.tmp, source.tmp, visno.tmp) %>%
                      spread(key="variable", value="value.nomiss", fill=NA) %>%
                      ungroup()
  # this is the problem: there is no target value matching animtotraw because it is raw
  data.xw          <- mutate(data.nomiss.long,
                             value.recode = mapply(FUN=recode_values, x=value.nomiss, raw.name=variable, source=source.tmp)) %>%
                      merge(target, by="variable") %>%
                      arrange(id.tmp, source.tmp, visno.tmp, target, rank) %>%
                      group_by(id.tmp, source.tmp, visno.tmp, target) %>%
                      filter(!is.na(value.recode)) %>%
                      summarize(value.xw = first(value.recode)) %>%
                      ungroup() %>%
                      select(id.tmp, source.tmp, visno.tmp, target, value.xw) %>%
                      spread(key="target", value="value.xw", fill=NA)
  outnames   <- c("id.tmp","source.tmp","visno.tmp")
  names(outnames) <- c(id, source, visno)
  data.out    <- merge(data.nomiss.wide, data.xw, by=c("id.tmp","source.tmp","visno.tmp"), all.x=TRUE) %>%
                 rename(., !!!outnames) %>%
                 merge(data.covs, all.x=TRUE)
  return(data.out)
}
