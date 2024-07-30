#' Convert raw to crosswalked scores in Wisconsin WRAP/ADRC
#'
#' This function takes in an output dataset matching the output of wis_est, and a variable or variable list and
#' subject id, and outputs a plot. This is a parallel version of the Shiny app in runExample() that can be used
#' at the console. If given a path, this function will output the plot as a .png file. Otherwise, it will output
#' the plot to the current session.
#' @param data Vector containing the output of a wis_est call.
#' @param var Names of the original variables. Should be among the following: memory, execfn, language, global, animtotraw ttotal, drraw, cfl.xw, waisrtot, lm_imm.xw, lm_del.xw, theo.mem.xw.sca, pacc3.an.xw.sca , pacc3.wrap.sca, pacc4.wrap.sca, pacc5.wrap.sca, trla, trlb.xw
#' @param sub Column containing subject ID. Defaults to Reggieid.
#' @param vislabel Logical indicating if visit labels should be displayed on plot.
#' @param biomarker_list List of biomarker data sets.
#' @param ownData Logical indicating if using your own data or simulated data.
#' @param path File path in which to store output plot. The plot filename will be created based on the sub and var parameters.
#' @param width Width of output png file.
#' @param height Height of output png file.
#' @return Plot containing individual longitudinal trajectories for selected WisNorms variables.
#' @keywords Wisconsin, ADRC, WRAP, crosswalk, harmonization
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @importFrom tidyr gather spread
#' @importFrom stringr str_pad
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggnewscale new_scale
#' @seealso \code{\link{wis_est}}
#' @export

wis_plot <- function(data, var, sub, vislabel=TRUE, biomarker_list=NULL, ownData=TRUE, path=NULL, width=15, height=10) {

  message("Entering my wis_plot function.")
  meanage  <- 58.9
  
  varnames <- c("ttotal", "drraw", "lm_imm.xw", "lm_del.xw", "theo.mem.xw.sca",
                "bnt.xw", "animtotraw", "cfl.xw",
                "trla", "trlb.xw", "waisrtot",
                "pacc3.cfl.xw.sca", "pacc3.an.xw.sca", "pacc3.wrap.sca", "pacc4.wrap.sca", "pacc5.wrap.sca")
  varlabels <- c("AVLT Learning", "AVLT Delayed", "Logical Memory IA (XW)",
                 "Logical Memory IIA (XW)", "Memory Composite (XW)",
                 "Boston Naming (XW)", "Animal Naming", "Letter Fluency (CFL) (XW)",
                 "TMT-A", "TMT-B", "Digit Symbol",
                 "PACC3-CFL", "PACC3-AN", "PACC3-DS", "PACC4-DS", "PACC5-DS")
  testmax.vec <- c(75, 15, 25, 25, 200, 60, 100, 250, 150, 400, 100, 200, 200, 200, 200, 200)
  names(testmax.vec) <- varnames
  
  var.in <- var
  if (var.in=="memory"){
    var <- c("ttotal", "lm_imm.xw", "lm_del.xw", "theo.mem.xw.sca", "drraw")
  } else if (var.in=="execfn") {
    var <- c("waisrtot", "trla", "trlb.xw")
  } else if (var.in=="language") {
    var <- c("animtotraw","cfl.xw")
  } else if (var.in=="global") {
    var <- c("pacc3.cfl.xw.sca", "pacc3.an.xw.sca", "pacc3.wrap.sca", "pacc4.wrap.sca", "pacc5.wrap.sca")
  } else {
    var <- var.in
  }
  
  v   <- length(var)

  inid  <- str_pad(sub, 4, side="left", pad="0")

  df <- data %>%
        mutate(variable.f = factor(variable,
                                   levels=varnames,
                                   labels=varlabels))
  if (class(df$id)=="integer") { inid <- as.integer(inid) }
  

  # Pulling biomarker datasets out of input list and filtering to id=inid
  if (is.null(biomarker_list)) {
    message("No biomarker list input.")
    mh <- FALSE
    pib <- FALSE
    csf <- FALSE
    ptau <- FALSE
    mk <- FALSE
    amp <- FALSE
  } else {
    message("Checking through availability of biomarker datasets")
    
    if (is.null(biomarker_list$mh)) {
      message("No mh dataset")
      mh <- FALSE
    } else if (nrow(biomarker_list$mh) > 0) {
      message("I see an mh dataset")
      df.mh <- biomarker_list$mh %>% dplyr::filter(id==inid)
      mh <- TRUE
    } else { 
      mh <- FALSE 
    }
    
    if (is.null(biomarker_list$pib)) {
      message("No pib dataset")
      pib <- FALSE
    } else if (nrow(biomarker_list$pib) > 0) {
      message("I see a pib dataset")
      df.pib <- biomarker_list$pib %>% dplyr::filter(id==inid)
      pib <- TRUE
    } else { 
      pib <- FALSE 
    }
    
    if (is.null(biomarker_list$csf)) {
      message("No csf dataset")
      csf <- FALSE
    } else if (nrow(biomarker_list$csf) > 0) {
      message("I see a csf dataset")
      df.csf <- biomarker_list$csf %>% 
        dplyr::filter(id==inid) %>%
        rename(age_csf=age) %>%
        mutate(pTau_Abeta42_bin = ifelse(is.na(pTau_Abeta42_bin), NA, pTau_Abeta42_bin))
      csf <- TRUE
    } else { 
      csf <- FALSE 
    }
    
    if (is.null(biomarker_list$ptau)) {
      message("No ptau dataset")
      ptau <- FALSE
    } else if (nrow(biomarker_list$ptau) > 0) {
      message("I see a ptau dataset")
      
      if(ownData==T){
      df.ptau <- biomarker_list$ptau %>%
        mutate(id = gsub("WRAP", "", enumber)) %>%
        dplyr::filter(id==inid) %>%
        mutate(mean_conc=as.numeric(mean_conc),
               ptau_bin = dplyr::case_when(mean_conc<=0.4 ~ 1,
                                    mean_conc>0.4 & mean_conc<0.63 ~ 2,
                                    mean_conc >= 0.63 ~ 3),
               age_ptau=age_at_appointment) %>%
        select(id, age_ptau, mean_conc, ptau_bin)
      } else {
        df.ptau <- biomarker_list$ptau %>%
          filter(id==inid) %>%
          select(id, age_ptau=age, mean_conc, ptau_bin)
      }
      
      ptau <- TRUE
    } else { 
      ptau <- FALSE 
    }
    
  
    if (is.null(biomarker_list$mk)) {
      message("No mk dataset")
      mk <- FALSE
    } else if (nrow(biomarker_list$mk) > 0) {
      message("I see a mk dataset")
      df.mk <- biomarker_list$mk %>% 
        dplyr::filter(id==inid) %>%
        mutate(mk_bin_combined = dplyr::case_when(!is.na(mk_vr_bin) ~ mk_vr_bin,
                                           is.na(mk_vr_bin) & !is.na(mk_MTL_bin) ~ mk_MTL_bin,
        ),
        mk_bin_total = dplyr::case_when(is.na(mk_bin_combined) ~ NA,
                                 mk_bin_combined %in% c("SUVR MTL-", "T-") ~ 1,
                                 mk_bin_combined %in% c("T+/MTL only") ~ 2,
                                 mk_bin_combined %in% c("SUVR MTL+", "T+/MTL & Neo") ~ 3)
        ) %>%
        select(id, age_mk=age, mk_MTL_bin,mk_vr_bin, mk_bin_total)
      mk <- TRUE
    } else { 
      mk <- FALSE 
    }
    
    if (is.null(biomarker_list$amp)) {
      message("No amp dataset")
      amp <- FALSE
    } else if (nrow(biomarker_list$amp) > 0) {
      message("I see a amp dataset")
      
      if(ownData==TRUE) {
        df.amp <- biomarker_list$amp %>%
        mutate(id = gsub("WRAP", "", Name)) %>%
        dplyr::filter(id==inid) %>%
        mutate(amp_bin = dplyr::case_when(Result %in% c("Detected-1") ~ 2,
                                   Result %in% c("Not Detected") ~ 1,
                                   Result %in% c("QNS", "Indeterminate", "Detected-2") ~ NA),
               age=dplyr::case_when(shareable_age_at_appointment == ">90" ~ 90,
                             TRUE ~ as.numeric(shareable_age_at_appointment))) %>%
        select(id, age_amp=age, Result, amp_bin)
      } else{
        df.amp <- biomarker_list$amp %>% 
          filter(id==inid) %>%
          mutate(amp_bin = case_when(Result %in% c("Detected-1") ~ 2,
                                     Result %in% c("Not Detected") ~ 1,
                                     Result %in% c("QNS", "Indeterminate", "Detected-2") ~ NA)) %>%
          select(id, age_amp=age, Result, amp_bin)
      }
      amp <- TRUE
    } else { 
      amp <- FALSE 
      } 
  }

  # Restrict to desired variables
  # On full set, set testmin
  message("Selected variable of interest.")
  this.df <- dplyr::filter(df,
                    variable %in% var) %>%
    group_by(id) %>% arrange(visno, .by_group=T) %>%
    mutate(cur_age = dplyr::last(age, na_rm=T)) %>% ungroup()
  
  limits  <- group_by(this.df, variable, variable.f) %>%
    summarize(testmin = pmin(min(value, na.rm=TRUE), 0),
              testmax = first(testmax.vec[variable]),
              minage = ifelse(first(variable)=="animtotraw", 45, 40),
              maxage = 85) %>%
    ungroup() %>%
    data.frame()
  
  this.df <- mutate(this.df,
                    sign = factor(sign(this.df$best.q.c - 0.5),
                                  levels=c(-1,1), labels=c("Low","High")),
                    alpha = ifelse(is.na(this.df$best.q.c), NA,
                                   I(((abs(this.df$best.q.c - 0.5) * 100)^4)/(50^4)))) %>%
             dplyr::filter(id==inid) %>%
             mutate(age=age.L+meanage)

  if (nrow(this.df)==0) {
    outplot  <- ggplot() + annotate(geom="text", x=0, y=0, label="No data", colour="red", size=5) +
      theme(axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_rect(colour="white",fill="white"))
  } else {
    message("Beginning plot")
    nobs    <- group_by(this.df, variable) %>%
      summarize(nobs=n(),
                singleobs = (nobs==1)) %>%
      ungroup()
    this.df <- merge(this.df, nobs)
    limits  <- merge(limits, nobs)
    alphalim<- group_by(this.df, variable) %>%
      summarize(alphamin = ifelse(length(na.omit(alpha))==0,
                                  NA, min(alpha, na.rm=TRUE)),
                alphamax = ifelse(length(na.omit(alpha))==0,
                                  NA, max(alpha, na.rm=TRUE))) %>%
      ungroup()
    limits  <- merge(limits, alphalim)
    message("unccoefs.rds loading.")
    this.unccoefs <- dplyr::filter(unccoefs.rds,
                            name %in% var)
    message("Success.")
    nlines.df<-group_by(this.unccoefs, name) %>%
      select(name, nlines) %>%
      rename(variable=name) %>%
      dplyr::slice(1) %>%
      ungroup()
    limits <-  merge(nlines.df, limits)
    
    demos   <- arrange(this.df, visno) %>%
      summarize(source=first(source),
                last_dx=dplyr::last(Summary.Diagnosis),
                gender.f=first(gender.f),
                ed.ba.f=first(ed.ba.f),
                base.readcat.f=first(base.readcat.f),
                base.readcat.lab = recode(base.readcat.f, `0`="Q1",`1`="Q2",`2`="Q3",`3`="Q4"))
    # Note 2023-09-25: Need to modify this to create some kind of intercept default
    #   for use when we are missing demographics. We want to still be able to plot raw scores even when
    #   we cannot plot lines.
    int.levels<-data.frame(Intercept=1,
                           age.L=-1*meanage,
                           gender.f0=as.numeric(demos$gender.f=="Male"),
                           ed.ba.f0=as.numeric(demos$ed.ba=="BA"),
                           base.readcat.f0=as.numeric(demos$base.readcat.f==0),
                           base.readcat.f1=as.numeric(demos$base.readcat.f==1),
                           base.readcat.f2=as.numeric(demos$base.readcat.f==2)) %>%
      gather(key="coef", value="int.levels")
    component<- data.frame(coef=c("age.L","age.Q","age.C"))
    
    coef.long<- group_by(this.unccoefs, name, coef) %>%
      gather(key="tau", value="value", starts_with("tau")) %>%
      ungroup()
    
    intercepts.long<- merge(coef.long, int.levels) %>%
      mutate(value.add = int.levels*value) %>%
      group_by(name, tau) %>%
      arrange(name, tau, coef) %>%
      summarize(intercept = sum(value.add)) %>%
      na.exclude() %>%
      ungroup()
    
    intercepts<-spread(intercepts.long,
                       key="tau", value="intercept", fill=0) %>%
      ungroup() %>%
      data.frame()
    rownames(intercepts) <- intercepts$name
    intercepts<-select(intercepts, -name)
    
    int.miss<-  ((nrow(na.exclude(intercepts.long))==0) | (all(is.na(this.df$best.q.c)) & all(is.na(this.df$best.q.unc))))
    
    slopes.raw <- expand.grid(unique(coef.long$name),
                              unique(coef.long$tau),
                              component$coef,
                              stringsAsFactors=FALSE) %>%
      rename(name=Var1, tau=Var2, coef=Var3) %>%
      data.frame() %>%
      merge(coef.long, all.x=TRUE) %>%
      arrange(tau, coef, name)
    slopes     <- array(slopes.raw$value, dim=c(length(var),3,9),
                        dimnames=list(unique(slopes.raw$name),
                                      unique(slopes.raw$coef),
                                      unique(slopes.raw$tau)))
    slopes[is.na(slopes)] <- 0  # Fill NA values with 0 (indicates unused coef)
    myfun <- function(x, v, t) { intercepts[v, t] +
        slopes[v, "age.L", t]*(x) +
        slopes[v, "age.Q", t]*(x-meanage)^2 +
        slopes[v, "age.C", t]*(x-meanage)^3 }
    my.xlim <- c(35,90)
    
    limits  <- group_by(limits, variable) %>%
      mutate(highfun = max(myfun(x=c(minage:90), v=variable, t=nlines), na.rm=TRUE),
             highobs = max(this.df$value[this.df$variable==variable]),
             lowfun = min(myfun(x=c(minage:90), v=variable, t=1), na.rm=TRUE),
             lowobs = min(this.df$value[this.df$variable==variable])) %>%
      ungroup() %>%
      data.frame
    ## Obtain limits of plottable values for each tau for each outcome
    ## (Reason: Some functions predict out of range values. Don't want to plot)
    
    if (int.miss==FALSE) {
      fun.xlim   <- group_by(limits, variable) %>%
        dplyr::filter(variable %in% rownames(intercepts)) %>%
        do(expand.grid(x=c(.$minage:.$maxage), v=.$variable, t=c(1:.$nlines),
                       stringsAsFactors=FALSE)) %>%
        ungroup() %>%
        group_by(x,v,t) %>%
        mutate(y=myfun(x,v,t)) %>%
        ungroup() %>%
        merge(select(limits, variable, testmin, testmax)) %>%
        group_by(v, t) %>%
        summarize(ll=ifelse(length(na.omit(y))==0, NA,
                            min(x[y>testmin & y<testmax])),
                  ul=ifelse(length(na.omit(y))==0, NA,
                            max(x[y>testmin & y<testmax]))) %>%
        ungroup() %>%
        arrange(v, t) %>%
        data.frame()
      
      myfun.labs <- group_by(limits, variable) %>%
        dplyr::filter(variable %in% rownames(intercepts)) %>%
        tidyr::expand(t=seq(1:nlines)) %>%
        ungroup() %>%
        merge(select(limits, variable, variable.f)) %>%
        group_by(variable, t) %>%
        mutate(x=87,
               y=as.numeric(t(myfun(x, variable, t))),
               lab.raw=substr(colnames(this.unccoefs)[3+t], 9, 10),
               lab = ifelse(variable %in% c("trla","trlb.xw"),
                            sprintf("%02.0f", round(100 - as.numeric(lab.raw), 1)),
                            lab.raw)) %>%
        merge(fun.xlim, by.x=c("variable","t"), by.y=c("v","t")) %>%
        ungroup()
      myfun.ymin <- group_by(myfun.labs, variable) %>%
        arrange(y) %>%
        summarize(lab.ymin=first(y), lab.ymax=dplyr::last(y)) %>%
        ungroup()
    }
    
    limits <- limits %>%
      mutate(highpoint = pmax(highobs, highfun, na.rm=TRUE),
             lowpoint = pmax(1, pmin(lowobs, lowfun, na.rm=TRUE)),
             my.ymin = ifelse(int.miss==TRUE | !(variable %in% rownames(intercepts)),
                              testmin - pmax(1, 0.1*highpoint),
                              lowpoint - pmax(1, 0.1*highpoint)),
             my.ymax = ifelse(int.miss==TRUE | !(variable %in% rownames(intercepts)),
                              testmax + pmax(1, 0.1*highpoint),
                              highpoint + pmax(1, 0.1*highpoint))) %>%
      ungroup() %>%
      mutate(ncol = length(unique(variable)))
    
    if (int.miss==FALSE) {
      limits<-  merge(limits, myfun.ymin, all.x=TRUE) %>%
        mutate(y.range = pmax(my.ymax, lab.ymax, na.rm=TRUE) -
                 pmin(my.ymin, lab.ymin, na.rm=TRUE))
      
      unclines.df<- group_by(myfun.labs, variable, t) %>%
        select(variable, t, ll, ul) %>%
        tidyr::expand(x=seq(ll, ul)) %>%
        ungroup() %>%
        group_by(x, variable, t) %>%
        mutate(y=myfun(x, variable, t)) %>%
        ungroup() %>%
        merge(select(limits, variable, variable.f))
    }else {
      limits    <-  mutate(limits,
                           y.range = my.ymax - my.ymin)
    }
    
    ## Start plot creation
    message("Start Plot creation.")
    outplot  <- ggplot(this.df, aes(x=age)) +
      scale_x_continuous(limits=c(35,90))
    
    if (int.miss==FALSE) {
      this.df <- merge(this.df,
                       select(limits, variable, y.range, my.ymin, my.ymax, lab.ymin, lab.ymax, ncol)) %>%
        mutate(annohts = 1.05 * pmin(my.ymin, lab.ymin, na.rm=TRUE),
               annonudge = 0.05*y.range,
               annosize = 5 - pmin(ncol, 2))
    } else {
      this.df <- merge(this.df,
                       select(limits, variable, y.range, my.ymin, my.ymax, ncol)) %>%
        mutate(annohts = 1.05 * my.ymin,
               annonudge = 0.05*y.range,
               annosize = 5 - pmin(ncol, 2))
    }
    
    annonudge.vec <- na.omit(this.df$annonudge)
    names(annonudge.vec) <- unique(this.df$variable)
    
    if (vislabel==TRUE) {
      ## Adding facet_wrap and facetted_pos_scales here for differing scales to work. 
      outplot<- outplot +
        geom_text_repel(data=this.df,
                        aes(x=age, y=0,
                            label=paste0("V", visno)),
                        hjust=0.5, colour="blue",
                        direction="x",
                        force=1.5,
                        segment.colour=NA,
                        size=this.df$annosize[1])+
        facet_wrap(~variable.f, 
                   ncol=pmin(limits$ncol[1],3),
                   scales="free_y")
    } else{
      outplot<- outplot +
        facet_wrap(~variable.f, 
                   ncol=pmin(limits$ncol[1],3),
                   scales="free_y")
    }
    
    ## Adding in scale limits so no y values display over test limits due to biomarker symbol
    if(var.in=="memory"){
      breaks_list<- list(
        if("ttotal" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0, 75, 25), limits=c(0,NA))
        }, 
        if("drraw" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0, 15, 5), limits=c(0,NA))
        }, 
        if("lm_imm.xw" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0, 25, 5), limits=c(0,NA))
        }, 
        if("lm_del.xw" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0, 25, 5), limits=c(0,NA))
        }, 
        if("theo.mem.xw.sca" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="Memory Composite (XW)")$my.ymax), 40), limits=c(0,NA))
        })
      breaks_list<- breaks_list[!sapply(breaks_list,is.null)]
      
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = breaks_list)
      
    } else if(var.in=="execfn"){
      
      breaks_list<- list(
        if("trla" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="TMT-A")$my.ymax), 25), limits=c(0, NA))
        },
        if("trlb.xw" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="TMT-B")$my.ymax), 50), limits=c(0, NA))
        }, 
        if("waisrtot" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0, 100, 25), limits=c(0, NA))
        })
      
      breaks_list<- breaks_list[!sapply(breaks_list,is.null)]
      
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = breaks_list)
      
      
    } else if (var.in=="language") {
      
      breaks_list<- list(
        if("animtotraw" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="Animal Naming")$my.ymax), 20), limits=c(0, NA))
        },
        if("cfl.xw" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="Letter Fluency (CFL) (XW)")$my.ymax), 20), limits=c(0, NA))
        })
      
      breaks_list<- breaks_list[!sapply(breaks_list,is.null)]
      
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = breaks_list)
      
    } else if (var.in=="global") {
      
      breaks_list<- list(
        if("pacc3.cfl.xw.sca" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="PACC3-CFL")$my.ymax), 50), limits=c(0, NA))
        },
        if("pacc3.an.xw.sca" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="PACC3-AN")$my.ymax), 50), limits=c(0, NA))
        },
        if("pacc3.wrap.sca" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="PACC3-DS")$my.ymax), 50), limits=c(0, NA))
        },
        if("pacc4.wrap.sca" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="PACC4-DS")$my.ymax), 50), limits=c(0, NA))
        },
        if("pacc5.wrap.sca" %in% this.df$variable){
          scale_y_continuous(breaks=seq(0,ceiling(dplyr::filter(limits, variable.f=="PACC5-DS")$my.ymax), 50), limits=c(0, NA))
        })
      
      breaks_list<- breaks_list[!sapply(breaks_list,is.null)]
      
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = breaks_list)
      
    } else if (var.in %in% c("drraw", "lm_imm.xw", "lm_del.xw")) {
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = list(
          scale_y_continuous(breaks=seq(0,ceiling(limits$my.ymax), 5), limits=c(0, NA))
        ))
      
    } else if (var.in %in% c("animtotraw", "cfl.xw", "ttotal")) {
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = list(
          scale_y_continuous(breaks=seq(0,ceiling(limits$my.ymax), 20), limits=c(0, NA))
        ))
      
    } else if (var.in %in% c("trla", "waisrtot")) {
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = list(
          scale_y_continuous(breaks=seq(0,ceiling(limits$my.ymax), 25), limits=c(0, NA))
        ))
      
    } else if (var.in == "theo.mem.xw.sca") {
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = list(
          scale_y_continuous(breaks=seq(0,ceiling(limits$my.ymax), 40), limits=c(0, NA))
        ))
      
    } else if (var.in %in% c("trlb.xw", "pacc3.cfl.xw.sca", "pacc3.an.xw.sca", "pacc3.wrap.sca", "pacc4.wrap.sca", "pacc5.wrap.sca")) {
      outplot<- outplot+
        ggh4x::facetted_pos_scales(y = list(
          scale_y_continuous(breaks=seq(0,ceiling(limits$my.ymax), 50), limits=c(0, NA))
        ))
      
    }
    
    message("Looking for biomarkers.")
    if (!is.null(biomarker_list)) {
      
      if (pib) { 
        df.pib_lim<-  merge(this.df, df.pib %>% rename(age_pib=age), all=T) %>%
        group_by(variable, visno, age_pib) %>% dplyr::slice(1) %>% ungroup() %>%
        add_row(pib_trunc=1.10, age_pib=-1, variable.f=limits$variable.f[1]) %>%
        add_row(pib_trunc=1.19, age_pib=-1, variable.f=limits$variable.f[1]) 
        }

      if (csf) { 
        df.csf_lim<-  merge(this.df, df.csf, all=T) %>%
        group_by(variable, visno, age_csf) %>% dplyr::slice(1) %>% ungroup() %>%
        add_row(pTau_Abeta42_bin=0, age_csf=-1, variable.f=limits$variable.f[1]) %>%
        add_row(pTau_Abeta42_bin=1, age_csf=-1, variable.f=limits$variable.f[1]) 
        }
      
      if (ptau) { 
        df.ptau_lim<- merge(this.df, df.ptau, all=T) %>% 
        group_by(variable, visno, age_ptau) %>% dplyr::slice(1) %>% ungroup() %>%
        add_row(ptau_bin=1, age_ptau=-1, variable.f=limits$variable.f[1]) %>%
        add_row(ptau_bin=2, age_ptau=-1, variable.f=limits$variable.f[1]) %>%
        add_row(ptau_bin=3, age_ptau=-1, variable.f=limits$variable.f[1])
      }
      
      if (mk) { 
        df.mk_lim<-   merge(this.df, df.mk, all=T) %>% 
        group_by(variable, visno, age_mk) %>% dplyr::slice(1) %>% ungroup() %>%
        add_row(mk_bin_total=1, age_mk=-1, variable.f=limits$variable.f[1]) %>%
        add_row(mk_bin_total=2, age_mk=-1, variable.f=limits$variable.f[1]) %>%
        add_row(mk_bin_total=3, age_mk=-1, variable.f=limits$variable.f[1]) 
        }
      
      if (amp) { 
        df.amp_lim<-  merge(this.df, df.amp, all=T) %>% 
        group_by(variable, visno, age_amp) %>% dplyr::slice(1) %>% ungroup() %>%
        add_row(amp_bin=1, age_amp=-1, variable.f=limits$variable.f[1]) %>%
        add_row(amp_bin=2, age_amp=-1, variable.f=limits$variable.f[1]) %>%
        add_row(amp_bin=NA, age_amp=-1, variable.f=limits$variable.f[1]) 
        }
      
      
      ## Combine plots
      shapes <- c("pTau/AB42" = "square", "pTau217" = "triangle", "MK6240" = "diamond", "aSyn" = "circle cross", "PiB/NAV" = "circle")
      
      if(csf==TRUE) {
        ## pTau/AB42
        outplot<- outplot+
          guides(colour="none")+
          new_scale("colour")+
          geom_point(data=df.csf_lim, aes(x=age_csf, y=my.ymin + 1.15*(my.ymax-my.ymin), 
                                          colour=pTau_Abeta42_bin, shape="pTau/AB42"), size=4)+
          scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=0.5, na.value="black",
                                 breaks=c(0, 0.5, 1),
                                 labels=c("-", "","+"),
                                 name="Biomarker")
      }
      if(ptau==TRUE) { 
        ## pTau217
        outplot<- outplot+
          guides(colour="none")+
          new_scale("colour")+
          geom_point(data=df.ptau_lim, aes(x=age_ptau, y=my.ymin + 1.25*(my.ymax-my.ymin), 
                                           colour=ptau_bin, shape="pTau217"), size=4)+
          scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=2, na.value="black",
                                 breaks=c(1,2,3),
                                 labels=c("-", "","+"),
                                 name="Biomarker")
      }
      if (mk==TRUE) {
        ## MK6240
        outplot<- outplot+
          guides(colour="none")+
          new_scale("colour")+
          geom_point(data=df.mk_lim, aes(x=age_mk, y= my.ymin + 1.35*(my.ymax-my.ymin), 
                                         colour=mk_bin_total, shape="MK6240"), size=4)+
          scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=2, na.value="black",
                                 breaks=c(1, 2, 3),
                                 labels=c("-","","+"),
                                 name="Biomarker")
      }
      if (amp==TRUE) {
        ## Amprion
        outplot<- outplot+
          guides(colour="none")+
          new_scale("colour")+
          geom_point(data=df.amp_lim, aes(x=age_amp, y= my.ymin + 1.45*(my.ymax-my.ymin), 
                                          colour=amp_bin, shape="aSyn"), size=4)+
          scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.5, na.value="black",
                                 breaks=c(1,1.5,2),
                                 labels=c("-", "","+"),
                                 name="Biomarker")
      }
      outplot <- outplot + scale_shape_manual(values = shapes, 
                                              breaks=c("aSyn", "MK6240", "pTau217", "pTau/AB42", "PiB/NAV"),
                                              guide=guide_legend(order=3)) +
        labs(shape=NULL)
      
      ## PiB
      if (nrow(df.pib)==0) {
        if(csf==TRUE|ptau==TRUE|mk==TRUE|amp==TRUE){
          outplot<- outplot +
            guides(colour="none")+
            new_scale("colour")+
            geom_point(data=df.pib_lim, aes(x=age_pib, y=my.ymin + 1.05*(my.ymax-my.ymin), 
                                            colour=pib_trunc, shape="PiB/NAV"),  size=4)+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.15, na.value="black",
                                   breaks=c(1.10, 1.15, 1.19),
                                   labels=c("-", "", "+"),
                                   name="Biomarker")+
            scale_shape_manual(values = shapes, 
                               breaks=c("aSyn", "MK6240", "pTau217", "pTau/AB42", "PiB/NAV"),
                               guide=guide_legend(order=3))+
            labs(shape=NULL)
          
        }else {
          outplot<- outplot
        }
        
      } else {
        outplot<- outplot + guides(colour="none")
        
        limits<-  df.pib %>% arrange(age) %>%
          dplyr::slice(n()) %>%
          mutate(my.pibmin=pmin(eaoa_1p19_gbtm, eaoa_1p19_sila, na.rm=TRUE),
                 my.pibmax=pmax(eaoa_1p19_gbtm, eaoa_1p19_sila, na.rm=TRUE),
                 my.pibmean=if_else(is.na(eaoa_1p19_mean) | is.na(eaoa_1p19_sila),
                                    my.pibmin, eaoa_1p19_mean),
                 my.pibrange = if_else(is.na(eaoa_1p19_mean) | is.na(eaoa_1p19_sila),
                                       0, eaoa_1p19_range)) %>%
          select(my.pibmin, my.pibmax, my.pibrange, my.pibmean) %>%
          merge(limits, ., all.x=TRUE)
        
        if (all(is.na(limits$my.pibmin))) {
          outplot <-  
            outplot +
            new_scale("colour")+
            geom_point(data=df.pib_lim, aes(x=age_pib, y=my.ymin + 1.05*(my.ymax-my.ymin), 
                                            colour=pib_trunc, shape="PiB/NAV"),  size=4)+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.15, na.value="black",
                                   breaks=c(1.10, 1.15, 1.19),
                                   labels=c("-", "", "+"),
                                   name="Biomarker")+
            scale_shape_manual(values = shapes, 
                               breaks=c("aSyn", "MK6240", "pTau217", "pTau/AB42", "PiB/NAV"),
                               guide=guide_legend(order=3))+
            labs(shape=NULL)
          
          
        } else if (limits$my.pibrange[1] > 1) {
          my.spacing <- ifelse(v>3, 0.03, 0.02)
          
          outplot <- 
            outplot +
            new_scale("colour")+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.15, na.value="black",
                                   breaks=c(1.10, 1.15, 1.19),
                                   labels=c("-", "", "+"),
                                   name="Biomarker") +
            geom_point(data=df.pib_lim, aes(x=age_pib, y=my.ymin + 1.05*(my.ymax-my.ymin), 
                                            colour=pib_trunc, shape="PiB/NAV"),  size=4)+
            geom_rect_pattern(data=dplyr::slice(df.pib, 1),
                              aes(xmin=limits$my.pibmin[1],
                                  xmax=limits$my.pibmax[1],
                                  ymin=-Inf,
                                  ymax=Inf,
                                  pattern_fill="Amyloid EAOA"),
                              pattern="circle",
                              fill=NA,
                              colour="white",
                              pattern_colour="red",
                              pattern_density=0.1,
                              pattern_spacing=0.02,
                              pattern_alpha=1,
                              pattern_size=1)+
            scale_pattern_fill_manual(values=c("Amyloid EAOA"="red"), 
                                      guide=guide_legend(order=1, 
                                                         override.aes=list(pattern_colour="red", fill="white", 
                                                                           pattern_alpha=1, pattern_size=0.4,
                                                                           pattern_spacing=0.01, pattern_density=0.3))) +
            labs(pattern_fill=NULL, shape=NULL)+
            scale_shape_manual(values = shapes, 
                               breaks=c("aSyn", "MK6240", "pTau217", "pTau/AB42", "PiB/NAV"),
                               guide=guide_legend(order=3))
          
        }else {
          
          outplot <- outplot +
            new_scale("colour")+
            geom_vline(aes(xintercept=limits$my.pibmean[1], colour="Amyloid EAOA"), linetype="twodash", linewidth=1.25)+
            scale_color_manual(values="red", guide=guide_legend(order=1))+
            labs(colour=NULL)+
            new_scale("colour")+
            geom_point(data=df.pib_lim, aes(x=age_pib, y=my.ymin + 1.05*(my.ymax-my.ymin), 
                                            colour=pib_trunc, shape="PiB/NAV"),  size=4)+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.15, na.value="black",
                                   breaks=c(1.10, 1.15, 1.19),
                                   labels=c("-", "", "+"),
                                   name="Biomarker")+
            scale_shape_manual(values = shapes, 
                               breaks=c("aSyn", "MK6240", "pTau217", "pTau/AB42", "PiB/NAV"),
                               guide=guide_legend(order=3))+
            labs(shape=NULL)
        }
      }
    }
    message("Biomarkers complete.")
    
    if (int.miss==FALSE) {
      outplot<- outplot +
        geom_path(data=unclines.df,
                  aes(x=x, y=y, group=t),
                  colour="grey40", linetype="dashed") +
        geom_text(aes(x=x, y=y, label=lab),
                  data=myfun.labs, colour="grey40", hjust=0,
                  check_overlap=TRUE, size=2.5)
    }
    
    # Below: dummy data to set y limits
    # Need to add ghost rows to show both High and Low arrows in legend:
    message("Ghost rows for arrow legend.")
    if(all(is.na(this.df$sign)) |
       all(this.df$sign=="High" | is.na(this.df$sign)) |
       all(this.df$sign=="Low" | is.na(this.df$sign)) ) {
      
      this.df2<- this.df %>%
        mutate(no_percentile = ifelse(is.na(best.q.c) & is.na(best.q.unc) & !singleobs %in% c(T), T, NA)) %>%
        add_row(sign="High", variable.f=this.df$variable.f[1]) %>%
        add_row(sign="Low", variable.f=this.df$variable.f[1]) 
      
      outplot <-  outplot +
        new_scale("shape")+
        geom_point(data=limits, x=50, aes(y=my.ymin), alpha=0) +
        geom_point(data=limits, x=50, aes(y=my.ymax), alpha=0) +
        geom_line(aes(y=value, group=id),
                  data=this.df2, linewidth=1.5, show.legend=FALSE) +
        geom_point(data=this.df2, 
                   aes(y=value, shape=sign, alpha=alpha), size=4, stroke=2) +
        scale_shape_manual(limits=c("High","Low"), values=c(2,6), 
                           na.translate=FALSE, guide = guide_legend(order = 99)) +
        scale_alpha_continuous(range=c(0,1), limits=c(0,1), guide="none")
      
      if(any(this.df$singleobs)==TRUE){
        
        ## Adding for those that have single observations and no percentiles
        if(any(this.df2$no_percentile) %in% c(TRUE)){
          outplot <-  outplot +
            geom_point(data=dplyr::filter(this.df, singleobs==TRUE),
                       aes(y=value),
                       shape=4, size=4, stroke=2, show.legend=F) +
            geom_point(data=dplyr::filter(this.df2, no_percentile==TRUE & !singleobs %in% c(TRUE)),
                       aes(y=value),
                       shape=4, size=4, stroke=2, show.legend=F) +
            labs(x="Age",
                 y="Outcome value",
                 shape="Conditional performance",
                 pattern_fill="") +
            theme_bw() +
            theme(legend.position="bottom", legend.direction = "horizontal",
                  legend.box="vertical")
        } else{
          outplot <-  outplot +
            geom_point(data=dplyr::filter(this.df, singleobs==TRUE),
                       aes(y=value),
                       shape=4, size=4, stroke=2, show.legend=F) +
            labs(x="Age",
                 y="Outcome value",
                 shape="Conditional performance",
                 pattern_fill="") +
            theme_bw() +
            theme(legend.position="bottom", legend.direction = "horizontal",
                  legend.box="vertical")
        }
        
      } else if(any(this.df2$no_percentile) %in% c(TRUE)){
        outplot <-  outplot +
          geom_point(data=dplyr::filter(this.df2, no_percentile==TRUE),
                     aes(y=value),
                     shape=4, size=4, stroke=2, show.legend=F) +
          labs(x="Age",
               y="Outcome value",
               shape="Conditional performance",
               pattern_fill="") +
          theme_bw() +
          theme(legend.position="bottom", legend.direction = "horizontal",
                legend.box="vertical")
      } else {
        outplot <- outplot +
          labs(x="Age",
               y="Outcome value",
               shape="Conditional performance",
               pattern_fill="") +
          theme_bw() +
          theme(legend.position="bottom", legend.direction = "horizontal",
                legend.box="vertical")
      }
      
      
    } else{
      outplot  <- outplot +
        new_scale("shape")+
        geom_point(data=limits, x=50, aes(y=my.ymin), alpha=0) +
        geom_point(data=limits, x=50, aes(y=my.ymax), alpha=0) +
        geom_line(aes(y=value, group=id),
                  data=this.df, linewidth=1.5, show.legend=FALSE) +
        geom_point(aes(y=value, shape=sign, alpha=alpha), size=4, stroke=2) +
        scale_shape_manual(limits=c("High","Low"), values=c(2,6),
                           na.translate=FALSE, guide = guide_legend(order = 99)) +
        scale_alpha_continuous(range=c(0,1), limits=c(0,1), guide="none") +
        labs(x="Age",
             y="Outcome value",
             shape="Conditional performance",
             pattern_fill="") +
        theme_bw() +
        theme(legend.position="bottom", legend.direction = "horizontal",
              legend.box="vertical")
    }
    
    if (mh==TRUE) {
      df.mh      <- dplyr::filter(df.mh, age > my.xlim[1] & age < my.xlim[2])
      if (nrow(df.mh)==0) {
        mh <- FALSE
      } else {
        # Plot only events in the age range
        df.mh.sum  <- arrange(df.mh, age) %>%
          mutate(event = ifelse(is.na(event), "Unknown event", event),
                 event.f = factor(event,
                                  levels=unique(event[order(age)])),
                 age_lag = lag(age),
                 new_group = ifelse(is.na(age_lag) | (age - age_lag) > 2,
                                    TRUE, FALSE))
        
        df.mh.sum<- df.mh.sum %>%
          do({group_starts = which(.$new_group==TRUE)
          group_reps = ifelse(!is.na(lead(group_starts)),
                              lead(group_starts) - group_starts,
                              length(.$new_group) - group_starts + 1)
          groups = as.vector(unlist(mapply(rep,
                                           x=LETTERS[c(1:length(group_starts))],
                                           times=group_reps)))
          
          df.out <- data.frame(., groups)
          colnames(df.out)[length(df.out)] <- "groups"
          df.out }) %>%
          arrange(age) %>%
          group_by(groups) %>%
          summarize(age = first(age),
                    event.paste = paste(event, collapse=";\n"),
                    event.paste.f = factor(event.paste,
                                           levels=event.paste[order(groups)])) %>%
          ungroup() %>%
          merge(select(limits, variable.f, my.ymin, my.ymax, ncol)) %>%
          mutate(label_height = my.ymin + 0.95*(my.ymax-my.ymin),
                 label_size = 6 - pmin(ncol, 4),
                 label_xnudge = -1*pmin(ncol, 2),
                 age_xnudge = age + label_xnudge)
        letterlabels <- as.numeric(sapply(unique(df.mh.sum$groups), utf8ToInt))
        letterlabels2<- unique(df.mh.sum$groups)
        
        outplot <- outplot + 
          geom_vline(data=df.mh,
                     aes(xintercept=age),
                     linetype=3, linewidth=1, colour="purple") +
          new_scale("shape") +
          geom_point(data=df.mh.sum,
                     aes(x=age_xnudge, y=label_height,
                         shape=event.paste.f),
                     size=5,colour="purple", stroke=3) +
          guides(size="none")+
          scale_shape_manual(breaks=levels(df.mh.sum$event.paste.f),
                             values=letterlabels2,
                             name="Medical events", guide = guide_legend(order = 99, override.aes = list(size = 5)))+
          theme(legend.direction="horizontal")
      }
    }
    
    ## Plot formatting and add current age
    outplot<- outplot +
      theme(text=element_text(size=18),
            legend.text=element_text(size=14))+
      new_scale("linetype")+
      geom_vline(aes(xintercept=this.df$cur_age, linetype="Current Age"), color = "darkgreen",alpha=0.8, linewidth=1)+
      scale_linetype_manual(values=c("Current Age" = "dotted"), 
                            guide=guide_legend(order=99, override.aes = list(alpha=1)))+
      labs(linetype=NULL)
  }
  message("Returning plot.")
  if (!is.null(path)) { 
    plotname <- paste(paste("wisplot", sub, var.in, sep="_"), "png", sep=".")
    ggsave(plotname, outplot, path=path, width=width, height=height, units="in")
  } else { outplot }
}
