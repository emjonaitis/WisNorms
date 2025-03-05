#' Compute crosswalked composite scores in Wisconsin WRAP/ADRC
#'
#' This function takes in a column of raw Wisconsin WRAP/ADRC data and returns the crosswalked version
#' @param data Vector containing untransformed Wisconsin WRAP/ADRC data.
#' @param id Column containing subject ID. Defaults to Reggieid.
#' @param source Column containing data source. Defaults to Data.Source.
#' @param visno Column containing visit number. Defaults to Visit_Number.
#' @return Data frame containing Wisconsin WRAP/ADRC data augmented with composites.
#' @keywords Wisconsin, ADRC, WRAP, crosswalk, harmonization
#' @importFrom magrittr "%>%"
#' @importFrom dplyr ungroup group_by select filter summarize mutate rename arrange recode
#' @importFrom tidyr gather spread
#' @seealso \code{\link{wis_est}, \link{wis_harm}}
#' @export

wis_composites <- function(data, id, source, visno) {
  # if (is.null(raw.names) | !is.character(raw.names) | !(min(raw.names %in% missval$variable)==TRUE)) {
  #   stop("Must provide a character vector naming the raw neuropsych variables. See the help file for a list of permitted raw names.")
  # }
  if (is.grouped_df(data)) {
    data <- ungroup(data)
  }
  raw.names        <- compscale$variable[compscale$variable %in% colnames(data)]
  comp.names       <- compscale2$variable[compscale2$variable %in% colnames(data)]

  pacc3.trlb.use   <- as.logical(min(c("trlb.xw","lm_del.xw","ttotal") %in% raw.names))
  pacc4.trlb.use   <- as.logical(min(c("trlb.xw","lm_del.xw","ttotal","mmse.xw") %in% raw.names))
  pacc3.cfl.use    <- as.logical(min(c("lm_del.xw","ttotal","cfl.xw") %in% raw.names))
  pacc4.cfl.trlb.use <- as.logical(min(c("trlb.xw","lm_del.xw","ttotal","cfl.xw") %in% raw.names))
  pacc3.an.use   <- as.logical(min(c("animtotraw","lm_del.xw","ttotal") %in% raw.names))
  pacc3.wrap.use   <- as.logical(min(c("lm_del.xw","ttotal","waisrtot") %in% raw.names))
  pacc4.wrap.use   <- as.logical(min(c("lm_del.xw","ttotal","waisrtot","mmse.xw") %in% raw.names))
  pacc5.wrap.use   <- as.logical(min(c("lm_del.xw","ttotal","waisrtot","mmse.xw","animtotraw") %in% raw.names))
  theo.mem.use     <- as.logical(min(c("ttotal","drraw","lm_imm.xw","lm_del.xw") %in% raw.names))
  # Need a better way to handle this. This is producing totally NA data
  if (max(pacc3.trlb.use, pacc4.trlb.use, pacc3.cfl.use, pacc4.cfl.trlb.use, pacc3.an.use, pacc3.wrap.use, pacc4.wrap.use, pacc5.wrap.use, theo.mem.use)==FALSE) {
    warning("No composites computed.")
  }
  # Renaming key columns to temporary values for ease of programming
  innames <- c(id, source, visno)
  names(innames) <- c("id.tmp","source.tmp","visno.tmp")

  data.covs      <- select(data, -raw.names)

  data.use       <- select(data,
                           id, source, visno, raw.names) %>%
                    rename(., !!!innames)
  source.list <- sort(unique(data.use$source.tmp[data.use$source.tmp != ""]))
  for (s in 1:length(source.list)) {
    if (!(source.list[s] %in% c("ADRC","WRAP"))) {
      stop("Currently supported values of source for this module: ADRC, WRAP")
    }
  }
  data.raw         <- select(data.use, id.tmp, source.tmp, visno.tmp, raw.names)
  data.long        <- group_by(data.raw, id.tmp, source.tmp, visno.tmp) %>%
                      gather(key="variable", value="value", raw.names, na.rm=FALSE) %>%
                      merge(compscale) %>%
                      ungroup() %>%
                      mutate(z.value = ifelse(variable=="trlb.xw",
                                              -1*(value - mean)/sd,
                                              (value - mean)/sd),
                             z.value.wrap = ifelse(variable=="trlb.xw",
                                                   -1*(value - mean.wrap)/sd.wrap,
                                                   (value - mean.wrap)/sd.wrap))
  data.comp        <- group_by(data.long, id.tmp, source.tmp, visno.tmp) %>%
                      summarize(pacc3.trlb.xw = if (pacc3.trlb.use==TRUE)
                                  { sum(z.value[variable %in% c("ttotal","lm_del.xw","trlb.xw")])/3 } else {NA},
                                pacc4.trlb.xw = if (pacc4.trlb.use==TRUE)
                                  { sum(z.value[variable %in% c("ttotal","lm_del.xw","trlb.xw","mmse.xw")])/4 } else { NA },
                                pacc3.cfl.xw = if (pacc3.cfl.use==TRUE)
                                  { sum(z.value[variable %in% c("ttotal","lm_del.xw","cfl.xw")])/3 } else { NA },
                                pacc4.cfl.trlb.xw = if(pacc4.cfl.trlb.use==TRUE)
                                  { sum(z.value[variable %in% c("ttotal","lm_del.xw","cfl.xw","trlb.xw")])/4 } else { NA },
                                pacc3.an.xw = if (pacc3.an.use==TRUE)
                                { sum(z.value[variable %in% c("ttotal","lm_del.xw","animtotraw")])/3 } else {NA},
                                pacc3.wrap = if (first(source.tmp)=="WRAP" & pacc3.wrap.use==TRUE)
                                  { sum(z.value.wrap[source.tmp=="WRAP" & variable %in% c("ttotal","lm_del.xw","waisrtot")])/3 } else { NA },
                                pacc4.wrap = if (first(source.tmp)=="WRAP" & pacc4.wrap.use==TRUE)
                                  { sum(z.value.wrap[source.tmp=="WRAP" & variable %in% c("ttotal","lm_del.xw","waisrtot","mmse.xw")])/4 }
                                    else { NA },
                                pacc5.wrap = if (first(source.tmp)=="WRAP" & pacc5.wrap.use==TRUE)
                                  { sum(z.value.wrap[source.tmp=="WRAP" &
                                                       variable %in% c("ttotal","lm_del.xw","waisrtot","mmse.xw","animtotraw")])/5 }
                                    else { NA },
                                theo.mem.xw = if (theo.mem.use==TRUE)
                                  { sum(z.value[variable %in% c("ttotal","drraw","lm_imm.xw","lm_del.xw")]/4) } else { NA } ) %>%
                      ungroup()

  data.comp.scaled <- group_by(data.comp, id.tmp, source.tmp, visno.tmp) %>%
                      gather(key="variable", value="value", pacc3.trlb.xw:theo.mem.xw, na.rm=TRUE) %>%
                      ungroup() %>%
                      merge(select(compscale2, variable=compvar, grandmean=mean, grandsd=sd)) %>%
                      mutate(value=as.numeric(value),
                             z.value = (value - grandmean)/grandsd,
                             sca.value = (15*z.value) + 100)
  data.comp.sca1   <- group_by(data.comp.scaled,
                               id.tmp, source.tmp, visno.tmp) %>%
                      select(variable, z.value) %>%
                      spread(key="variable", value="z.value", fill=NA) %>%
                      ungroup()
  colnames(data.comp.sca1)[4:ncol(data.comp.sca1)] <- paste(colnames(data.comp.sca1)[4:ncol(data.comp.sca1)], "z", sep=".")
  data.comp.sca2   <- group_by(data.comp.scaled,
                               id.tmp, source.tmp, visno.tmp) %>%
                      select(variable, sca.value) %>%
                      spread(key="variable", value="sca.value", fill=NA) %>%
                      ungroup()
  colnames(data.comp.sca2)[4:ncol(data.comp.sca2)] <- paste(colnames(data.comp.sca2)[4:ncol(data.comp.sca2)], "sca", sep=".")
  data.rawcomp     <- merge(data.comp, data.comp.sca1, by=c("id.tmp","source.tmp","visno.tmp")) %>%
                      merge(data.comp.sca2, by=c("id.tmp","source.tmp","visno.tmp")) %>%
                      merge(data.raw, ., by=c("id.tmp","source.tmp","visno.tmp"), all.x=TRUE)

  outnames   <- c("id.tmp","source.tmp","visno.tmp")
  names(outnames) <- c(id, source, visno)
  data.out   <- rename(data.rawcomp, !!!outnames) %>%
                merge(data.covs, .)
  return(data.out)
}
