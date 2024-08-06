#' Estimate unconditional and conditional percentiles using an rrq() object
#'
#' This function takes in a neuropsych dataset and a quantile regression object and computes the best-fit unconditional and conditional percentiles for each observation against a reference dataset. By default, the quantile regression object is a model of the specified variable based on Wisconsin WRAP and ADRC data from Freeze 17. The results are returned as a data frame.
#' @param data Data frame containing neuropsych scores and covariates.
#' @param variable.list Vector containing names of variables to evaluate. Norms are available for the following (crosswalked) outcomes: animtotraw, bnt.xw, cfl.xw, drraw, dspf.xw, dspb.xw, lm_del.xw, lm_imm.xw, PACC3.cfl.xw.sca, PACC3.trlb.xw.sca, PACC4.cfl.trlb.xw.sca, PACC4.trlb.xw.sca, PACC3.wrap.sca, PACC4.wrap.sca, PACC5.wrap.sca, theo.mem.xw.sca, trla, trlb.xw, ttotal.
#' @param model.unc Quantile regression model object used to predict unconditional percentile. Default model included in package built using Freeze 17 data.
#' @param model.c Quantile regression model object used to predict conditional percentile. Default model included in package built using Freeze 17 data.
#' @param raw Boolean variable indicating whether the data are raw data. If set to TRUE (the default), the values in \code{raw.name} will be crosswalked to the variable specified in \code{variable} using crosswalks developed by Rebecca Langhough Koscik.
#' @param raw.names Name of columns in \code{data} containing original data values to be transformed to \code{variable}.
#' @param id Name of column containing patient ID.
#' @param source Name of column containing data source (WRAP or ADRC).
#' @param age Name of column containing age.
#' @param visno Name of column containing visit number.
#' @param gender Name of column containing gender.
#' @param ed Name of column containing years of education.
#' @return Dataframe containing values best.q.unc and best.q.c corresponding to the best-fit unconditional and conditional percentiles, respectively.
#' @keywords Wisconsin, ADRC, WRAP, conditional norms
#' @importFrom magrittr "%>%"
#' @importFrom dplyr ungroup group_by select filter summarize mutate rename arrange recode
#' @importFrom tidyr gather spread
#' @seealso \code{\link{wis_harm}}
#' @export

wis_est <- function(data, variable.list=NULL, model.unc=NULL, model.c=NULL,
                   raw=TRUE, raw.names=NULL, id="subjid", source="Data.Source", age="age", visno="Visit_Number", gender="gender",
                   ed="Education.Years") {
  # Renaming key columns to temporary values for ease of programming
  innames <- c(id, source, age, visno, gender, ed)
  names(innames) <- c("id.tmp","source.tmp","age.tmp","visno.tmp","gender.tmp","ed.tmp")
  redata         <- rename(data, !!!innames)
  if (!is.numeric(redata$ed.tmp)) {
    stop("Variable ed must be numeric.")
  } else {
    redata$ed.ba.f <- factor(ifelse(redata$ed.tmp < 16, "No BA", "BA"),
                             levels=c("No BA", "BA"),
                             ordered=TRUE)
    contrasts(redata$ed.ba.f) = contr.treatment(n=c(1:0), base=1) # Comparison group: Those with a BA
  }
  gender.list <- sort(unique(redata$gender.tmp[redata$gender.tmp != ""]))
  for (g in 1:length(gender.list)) {
    if (!(gender.list[g] %in% c("Female","Male"))) {
      stop("Currently supported values of gender for this module: Female, Male.")
    }
    if (g==length(gender.list)) {
      redata$gender.f <- factor(redata$gender.tmp,
                                  levels=c("Female","Male"),
                                  ordered=TRUE)
      contrasts(redata$gender.f) = contr.treatment(n=c(1:0), base=1) # Comparison group: Women
    }
  }
  if (raw==TRUE) {
    if (is.null(raw.names)) {
      target_columns <- which(colnames(redata) %in% unique(target$variable) |
                                colnames(redata) %in% unique(missval$variable))
      colnames(redata)[target_columns] <- tolower(colnames(redata)[target_columns])
      raw.names <- colnames(redata)[target_columns]
      message(paste("Identified cognitive variables:", paste(raw.names, collapse=", ")))
    } 
    harmdata <- wis_harm(redata, raw.names=raw.names, id="id.tmp", source="source.tmp", visno="visno.tmp")
    harm.names <- colnames(harmdata)
    pacc3.trlb.use   <- as.logical(min(c("trlb.xw","lm_del.xw","ttotal") %in% harm.names))
    pacc4.trlb.use   <- as.logical(min(c("trlb.xw","lm_del.xw","ttotal","mmse.xw") %in% harm.names))
    pacc3.cfl.use    <- as.logical(min(c("lm_del.xw","ttotal","cfl.xw") %in% harm.names))
    pacc4.cfl.trlb.use <- as.logical(min(c("trlb.xw","lm_del.xw","ttotal","cfl.xw") %in% harm.names))
    pacc3.wrap.use   <- as.logical(min(c("lm_del.xw","ttotal","waisrtot") %in% harm.names))
    pacc4.wrap.use   <- as.logical(min(c("lm_del.xw","ttotal","waisrtot","mmse.xw") %in% harm.names))
    pacc5.wrap.use   <- as.logical(min(c("lm_del.xw","ttotal","waisrtot","mmse.xw","animtotraw") %in% harm.names))
    theo.mem.use     <- as.logical(min(c("ttotal","drraw","lm_imm.xw","lm_del.xw") %in% harm.names))
    composites.use   <- as.logical(max(pacc3.trlb.use, pacc4.trlb.use, pacc3.cfl.use, pacc4.cfl.trlb.use,
                                       pacc3.wrap.use, pacc4.wrap.use, pacc5.wrap.use, theo.mem.use))
    if (composites.use) {
      usedata  <- wis_composites(harmdata, id="id.tmp", source="source.tmp", visno="visno.tmp")
    } else {
      usedata <- harmdata
    }
  }
  else {
    usedata <- redata
  }
    base <- usedata %>%
          #filter(!is.na(readcat.xw)) %>%
          group_by(id.tmp) %>%
          arrange(visno.tmp) %>%
          summarize(base.readcat.f = factor(first(readcat.xw, na_rm=TRUE), levels=c(3,2,1,0), ordered=TRUE)) %>%
          ungroup()
  contrasts(base$base.readcat.f) <- contr.treatment(n=c(3:0), base=1) # Comparison group: Highest reading category
  usedata <- merge(usedata, base) %>%
             mutate(c.age = age.tmp - 58.9)

  # Need to get a list of all the variables we're trying to create models for, so we can look up the model objects.
  # If variable list isn't specified, assume it is the intersection of the columns in usedata and the variables in target
  # (excluding the reading variable).
  if (is.null(variable.list)) {
    variable.list <- colnames(usedata)[colnames(usedata) %in% unique(target$target) & colnames(usedata) != "readcat.xw"]
  }
  nvar <- length(variable.list)
  temp.unc.bysub.full.df <- data.frame()
  for (i in 1:nvar) {
    this.var <- variable.list[i]
    excl.var <- variable.list[-i]
    # Need to compute practice independently for each variable, since it can vary.
    this.data  <- select(usedata, -excl.var) %>%
                  rename(value=this.var) %>%
                  filter(!is.na(value)) %>%
                  group_by(id.tmp) %>%
                  arrange(id.tmp, visno.tmp) %>%
                  ungroup() %>%
                  mutate(age.L = poly(c.age, degree=3, raw=TRUE)[,1],
                         age.Q = poly(c.age, degree=3, raw=TRUE)[,2],
                         age.C = poly(c.age, degree=3, raw=TRUE)[,3])
    # Selecting the first visit with nonmissing data for this variable
    this.sum   <- group_by(this.data, id.tmp) %>%
                  summarize(base.value = first(value),
                            base.age = first(c.age),
                            base.vis = first(visno.tmp)) %>%
                  ungroup()
    # Merging it with the main dataset for this variable and using base.vis to calculate practice
    this.data  <- merge(this.data, this.sum, by.x="id.tmp", by.y="id.tmp") %>%
                  mutate(practice = visno.tmp-base.vis)
    # Getting conditional model object
    if (!is.null(model.c)) {
      usemodel.c <- model.c
    } else {
      usemodel.c <- paste0(this.var, ".rrq.c")
    }
    usemodel.c.obj <- get(usemodel.c)
    # Checking conditional model terms
    terms.c <- attr(usemodel.c.obj$terms, "term.labels")
    nterms.c <- length(terms.c)
    for (j in 1:nterms.c) {
      if (!(terms.c[j] %in% colnames(this.data))) {
        stop("Error: Variable ", terms.c[j], " required for conditional rrq model not found in input dataset ", data, ".")
      }
    }
    # Predicting best-fit conditional percentile for the second observation onward
    temp.rrq.c.pred <- Qtools::predict.rrq(get(usemodel.c), newdata=filter(this.data, c.age > base.age))
    colnames(temp.rrq.c.pred) <- substr(colnames(temp.rrq.c.pred), 7, 10)
    # This section pulls out the predicted quantiles for each level of tau; reshapes the data into long form (one tau per row);
    # computes the difference between the predicted quantile and the actual observed value; and selects the quantile (and tau)
    # having the minimum absolute difference.
    temp.rrq.c.bysub  <- if (length(usemodel.c.obj$tau)==50) {
                            filter(this.data, c.age > base.age) %>% # Keep only the second observation onward for each person
                            select(id.tmp, visno.tmp, source.tmp, value, age.L, age.Q, age.C, practice) %>%
                            cbind(temp.rrq.c.pred) %>%
                            group_by(id.tmp, visno.tmp, source.tmp, age.L, age.Q, age.C, practice) %>%
                            tidyr::gather(key="tau", value="quantile", `0.01`:`0.50`) %>%
                            arrange(id.tmp, visno.tmp, tau) %>%
                            mutate(absdiff = abs(quantile-value)) %>%
                            arrange(id.tmp, visno.tmp, quantile) %>%
                            summarize(mindiff = min(absdiff),
                                      best.q.c = mean(as.numeric(tau[abs(absdiff-mindiff)<.00001]))) %>%
                            select(-mindiff) %>%
                            ungroup()
                          } else {
                            filter(this.data, c.age > base.age) %>% # Keep only the second observation onward for each person
                            select(id.tmp, visno.tmp, source.tmp, value, age.L, age.Q, age.C, practice) %>%
                            cbind(temp.rrq.c.pred) %>%
                            group_by(id.tmp, visno.tmp, source.tmp, age.L, age.Q, age.C, practice) %>%
                            gather(key="tau", value="quantile", `0.01`:`0.99`) %>%
                            arrange(id.tmp, visno.tmp, tau) %>%
                            mutate(absdiff = abs(quantile-value)) %>%
                            arrange(id.tmp, visno.tmp, quantile) %>%
                            summarize(mindiff = min(absdiff),
                                      best.q.c = mean(as.numeric(tau[abs(absdiff-mindiff)<.00001]))) %>%
                            select(-mindiff) %>%
                            ungroup()
                          }
    colnames(temp.rrq.c.bysub) <- c("id.tmp", "visno.tmp", "source.tmp", "age.L", "age.Q", "age.C", "practice", "best.q.c")
    # Getting unconditional model object
    if (!is.null(model.unc)) {
      usemodel.unc <- model.unc
    } else {
      usemodel.unc <- paste0(this.var, ".rrq.unc")
    }
    usemodel.unc.obj <- get(usemodel.unc)
    # Checking unconditional model terms
    terms.unc <- attr(usemodel.unc.obj$terms, "term.labels")
    nterms.unc <- length(terms.unc)
    for (j in 1:nterms.unc) {
      if (!(terms.unc[j] %in% colnames(this.data))) {
        stop("Error: Variable ", terms.unc[j], " required for unconditional rrq model not found in input dataset ", data, ".")
      }
    }
    # Predicting best-fit unconditional centile for all observations
    temp.rrq.unc.pred <- Qtools::predict.rrq(get(usemodel.unc), newdata=this.data)
    colnames(temp.rrq.unc.pred) <- substr(colnames(temp.rrq.unc.pred), 7, 10)
    # This section pulls out the predicted quantiles for each level of tau; reshapes the data into long form (one tau per row);
    # computes the difference between the predicted quantile and the actual observed value; and selects the quantile (and tau)
    # having the minimum absolute difference.
    if (length(usemodel.unc.obj$tau)==50) {
      temp.rrq.unc.bysub.full   <- select(this.data,
                                          id.tmp, visno.tmp, source.tmp, value, Summary.Diagnosis,
                                          gender.f, ed.ba.f, base.readcat.f, age.L, age.Q, age.C, practice) %>%
                                          cbind(temp.rrq.unc.pred) %>%
                                          group_by(id.tmp, visno.tmp, source.tmp,
                                                   gender.f, ed.ba.f, base.readcat.f, age.L, age.Q, age.C, practice) %>%
                                          gather(key="tau", value="quantile", `0.01`:`0.50`) %>%
                                          arrange(id.tmp, visno.tmp, tau) %>%
                                          mutate(diff = abs(quantile-value),
                                                 var=this.var,
                                                 this.formula=deparse(usemodel.unc.obj$formula)) %>%
                                          arrange(id.tmp, visno.tmp, quantile) %>%
                                          ungroup()
      temp.rrq.unc.bysub        <- group_by(temp.rrq.unc.bysub.full,
                                            id.tmp, visno.tmp, source.tmp,
                                            gender.f, ed.ba.f, base.readcat.f, age.L, age.Q, age.C, practice) %>%
                                   summarize(variable=this.var,
                                             value=first(value),
                                             Summary.Diagnosis=first(Summary.Diagnosis),
                                             mindiff = min(diff),
                                             best.q.unc = mean(as.numeric(tau[abs(diff-mindiff)<.00001]))) %>%
                                             select(-mindiff) %>%
                                             ungroup()
    } else {
      temp.rrq.unc.bysub.full   <- select(this.data,
                                          id.tmp, visno.tmp, source.tmp, value, Summary.Diagnosis,
                                          gender.f, ed.ba.f, base.readcat.f, age.L, age.Q, age.C, practice) %>%
                                    cbind(temp.rrq.unc.pred) %>%
                                    group_by(id.tmp, visno.tmp, source.tmp,
                                             gender.f, ed.ba.f, base.readcat.f, age.L, age.Q, age.C, practice) %>%
                                    gather(key="tau", value="quantile", `0.01`:`0.99`) %>%
                                    arrange(id.tmp, visno.tmp, tau) %>%
                                    mutate(diff = abs(quantile-value),
                                           var=this.var,
                                           this.formula=deparse(usemodel.unc.obj$formula)) %>%
                                    arrange(id.tmp, visno.tmp, quantile) %>%
                                    ungroup()
      temp.rrq.unc.bysub         <- group_by(temp.rrq.unc.bysub.full,
                                             id.tmp, visno.tmp, source.tmp,
                                             gender.f, ed.ba.f, base.readcat.f, age.L, age.Q, age.C, practice) %>%
                                    summarize(variable=this.var,
                                              value=first(value),
                                              Summary.Diagnosis=first(Summary.Diagnosis),
                                              mindiff = min(diff),
                                              best.q.unc = mean(as.numeric(tau[abs(diff-mindiff)<.00001]))) %>%
                                    select(-mindiff) %>%
                                    ungroup()
    }
    colnames(temp.rrq.unc.bysub) <- c("id.tmp", "visno.tmp", "source.tmp", "gender.f", "ed.ba.f", "base.readcat.f",
                                      "age.L", "age.Q", "age.C", "practice", "variable", "value", "Summary.Diagnosis", "best.q.unc")
    pred.out <- merge(temp.rrq.c.bysub, temp.rrq.unc.bysub, all=TRUE)
    if (i==1) {
      outdata <- pred.out
    } else {
      outdata <- rbind(outdata, pred.out)
    }
    #usedata  <- rbind(usedata, pred.out)
  }
  outnames   <- c("id.tmp","source.tmp","visno.tmp")
  names(outnames) <- c("id","source","visno")
  outdata   <- rename(outdata, !!!outnames) %>%
               select(id, source, visno, variable, value, best.q.c, best.q.unc,
                      age.L, age.Q, age.C, gender.f, ed.ba.f, base.readcat.f, practice, Summary.Diagnosis) %>%
               mutate(best.q.c = if_else(variable %in% c("trla","trlb.xw"), 1-best.q.c, best.q.c),
                      best.q.unc = if_else(variable %in% c("trla","trlb.xw"), 1-best.q.unc, best.q.unc),
                      best.q.c = if_else(is.na(base.readcat.f), NA, best.q.c),
                      best.q.unc = if_else(is.na(base.readcat.f), NA, best.q.unc)) %>%
               filter(!is.na(visno)) %>%
               arrange(variable, id, visno)
  return(outdata)
}
