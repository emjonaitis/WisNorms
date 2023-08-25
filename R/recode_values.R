#' Recode a single Wisconsin WRAP/ADRC data value
#'
#' This function takes in a column of raw Wisconsin WRAP/ADRC data and returns a column of crosswalked Wisconsin WRAP/ADRC data. It is intended as an internal function.
#' @param x Vector containing untransformed UP data.
#' @param raw.name Name of the original variable. Should be among the following: anartraw, ANIMALS, AnimTotRaw, bnttot, bnttot30, BOSTON, CRAFTDRE, CRAFTURS, drraw, flucfl, LOGIMEM, MEMUNITS, MINTTOTS, ORRT.Unc_Std, readstn, TRAILA, TRAILB, trla, trlb, tTotal, ttotal, UDSVERTN, waisrtot, wmsrar, wmsrar2
#' @param source Data source. Should be one of the following: ADRC, WRAP
#' @return Vector containing recoded Wisconsin WRAP/ADRC data.
#' @keywords Wisconsin, WRAP, ADRC
#' @export
#' @importFrom dplyr recode
#' @examples
#' @seealso \code{\link{wis_harm}}

recode_values <- function(x, raw.name, source=Data.Source) {
  # if (is.null(raw.name) | !is.character(raw.name) | !(raw.name %in% missval$variable)) {
  #   stop("Unrecognized raw.name value. See the help file for a list of permitted names.")
  # }
  if (!(source %in% c("ADRC","WRAP"))) {
    stop("source must be either 'ADRC' or 'WRAP'.")
  }
  # Recoding vectors
  # Names contain the STARTING value
  # The vector values contain the TARGET value in the new scale
  # check bnt recoding!!! seems like we should be recoding 60 to 30 instead of the opposite.

   # Updated 2019-10-16 to fix mapping in upper parts of the scale (skipping 19 and 23)
  mint_to_bnt <- c(1,2,3,4,5,6,6,7,7,8,8,9,9,10,11,11,12:18,20:22,24:30)
  names(mint_to_bnt) <- c(0:32)
  craft_to_lm_imm <- c(0,0,1,2,2,3:23)
  names(craft_to_lm_imm) <- c(0:25)
  craft_to_lm_del <- c(0,1,3,3,4,5,5,6,7,8,8,9:16,18:24)
  names(craft_to_lm_del) <- c(0:25)
  moca_to_mmse <- c(6,9,10,11,12,12,13,14,15,15,16:27,28,28,29,29,29,30,30,30,30)
  names(moca_to_mmse) <- c(0:30)
  dspf_to_dspf <- c(3,3,3,5,6,7,7,8,10:12,13,15)
  names(dspf_to_dspf) <- c(0:12)
  nspf_to_dspf <- c(3,3,5,6,7,7,8:16)
  names(nspf_to_dspf) <- c(0:14)
  dspb_to_dspb <- c(0:10,12,14)
  names(dspb_to_dspb) <- c(0:12)
  nspb_to_dspb <- c(0:14)
  names(nspb_to_dspb) <- c(0:14)
  wrat_to_readcat <- c(rep(0,45),rep(1,10),rep(2,10),rep(3,25))
  names(wrat_to_readcat) <- c(45:134)
  orrt_to_readcat <- c(rep(0,52),rep(1,5),rep(2,7),rep(3,137))
  names(orrt_to_readcat) <- c(50:250)
  anart_to_readcat <- c(rep(0,25),rep(1,10),rep(2,6),rep(3,30))
  names(anart_to_readcat) <- c(0:70)
  # Nested if statements matching the input to the output
  newval <- ifelse(is.na(x),
               NA,
               ifelse(source=="WRAP" & !(raw.name %in% c("readstn", "MINTTOTS")),
                 x,
                 ifelse(raw.name %in% c("BOSTON","bnttot30"),
                    2*x,
                    ifelse(raw.name=="MINTTOTS",
                       2*recode(x, !!!mint_to_bnt),
                       ifelse(raw.name=="CRAFTURS",
                          recode(x, !!!craft_to_lm_imm),
                          ifelse(raw.name=="CRAFTDRE",
                             recode(x, !!!craft_to_lm_del),
                             ifelse(raw.name=="UDSVERTN",
                                1.5*x,
                                ifelse(raw.name=="MOCATOTS",
                                   recode(x, !!!moca_to_mmse),
                                   ifelse(raw.name=="DIGIF",
                                      recode(x, !!!dspf_to_dspf),
                                      ifelse(raw.name=="DIGFORCT",
                                         recode(x, !!!nspf_to_dspf),
                                         ifelse(raw.name=="DIGIB",
                                            recode(x, !!!dspb_to_dspb),
                                            ifelse(raw.name=="DIGBACCT",
                                               recode(x, !!!nspb_to_dspb),
                                               ifelse(raw.name=="readstn",
                                                  recode(x, !!!wrat_to_readcat),
                                                  ifelse(raw.name=="ORRT.Unc_Std",
                                                    recode(x, !!!orrt_to_readcat),
                                                    ifelse(raw.name=="anartraw",
                                                      recode(x, !!!anart_to_readcat),
                                                      x)))))))))))))))
  newval
}
