# app.R

library(shiny)
library(ggplot2)
library(ggnewscale)
library(ggrepel)
library(ggpattern)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(patchwork)

unccoefs <- readRDS("unccoefs.rds")
sample_df <- readRDS("./data/sample_df.rds")
sample_mh <- readRDS("./data/sample_mh.rds")
sample_pib <- readRDS("./data/sample_pib.rds")
sample_csf <- readRDS("./data/sample_csf.rds")
sample_amp<-  readRDS("./data/sample_amp.rds")
sample_mk<-  readRDS("./data/sample_mk.rds")
sample_ptau<-  readRDS("./data/sample_ptau.rds")
meanage  <- 58.9


ui <- fluidPage(

  # App title ----
  titlePanel("Visualizing Wisconsin Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width=3,fluid=T, 

      checkboxInput("ownData",
                    "Import my own data file",
                    value=TRUE),

      conditionalPanel(
        condition = "input.ownData==1",

        fileInput("file1", "Choose NP CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        fileInput("file2", "Choose PiB CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        fileInput("file3", "Choose CSF CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        fileInput("file4", "Choose pTau217 CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        fileInput("file5", "Choose MK6240 CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
      
        fileInput("file6", "Choose aSyn CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        fileInput("file7", "Choose medical history CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))),

      selectInput("id", "Participant ID:", ""),

      selectInput("variable", "Variable:",
                  c("All Memory" = "memory",
                    "All Executive Function" = "execfn",
                    "All Language" = "language",
                    "All Global" = "global",
                    "Animal Naming" = "animtotraw",
                    "AVLT Total" = "ttotal",
                    "AVLT Delayed Recall" = "drraw",
                    "C,F,L" = "cfl.xw",
                    "Digit Symbol" = "waisrtot",
                    "Logical Memory IA" = "lm_imm.xw",
                    "Logical Memory IIA" = "lm_del.xw",
                    "Memory Composite" = "theo.mem.xw.sca",
                    "PACC3-CFL" = "pacc3.cfl.xw.sca",
                    "PACC3-AN" = "pacc3.an.xw.sca",
                    "PACC3-WRAP" = "pacc3.wrap.sca",
                    "PACC4-WRAP" = "pacc4.wrap.sca",
                    "PACC5-WRAP" = "pacc5.wrap.sca",
                    "TMT-A" = "trla",
                    "TMT-B" = "trlb.xw")),

      checkboxGroupInput("options",
                         "Annotation options",
                         choices = list("Dynamic axis" = 1,
                                        "Annotate with visit labels" = 2,
                                        "Annotate with medical history" = 3,
                                        "Annotate with biomarkers" = 4
                                        ),
                         selected = c(1, 2)),

      # Main panel for displaying outputs ----

      submitButton("Submit")
    ),

    # Only show this panel if the plot type is a histogram
    mainPanel(width=9,fluid=T,
                    # Output: Tabset w/ plot, summary, and table ----
                    h2(textOutput("rrqSubject")),
                    h3(column(width=6, htmlOutput("rrqHeader1")),
                      column(width=6, htmlOutput("rrqHeader2"))),
                    h3(column(width=12, htmlOutput("rrqHeader3"))),
                    tabsetPanel(type = "tabs",
                                tabPanel("Plot", plotOutput("rrqPlot", width="100%")),
                                tabPanel("Table", tableOutput("rrqTable")),
                                tabPanel("Biomarkers", tableOutput("biotable"))) 
    )
  )
)

# increased file size
options(shiny.maxRequestSize=100*1024^2)

server <- function(input, output, session) {

  dataInput <- reactive({

    if (input$ownData==FALSE) {
      df <- read.csv("sample data/sample_df.csv") %>%
        rename(ed.ba = ed.ba.f,
               gender = gender.f,
               base.readcat = base.readcat.f) %>%
        mutate(id = str_pad(id, 4, side="left", pad="0"),
               ed.ba.f = factor(ed.ba,
                                levels=c("No BA", "BA"),
                                ordered=TRUE),
               gender.f = factor(gender,
                                 levels=c("Female","Male"),
                                 ordered=TRUE),
               base.readcat.f = factor(base.readcat,
                                       levels=c(3,2,1,0),
                                       ordered=TRUE),
               age = age.L+meanage)
      contrasts(df$ed.ba.f) = contr.treatment(n=c(1:0), base=1) # Comparison group: Those with a BA
      contrasts(df$gender.f) = contr.treatment(n=c(1:0), base=1) # Comparison group: Women
      contrasts(df$base.readcat.f) <- contr.treatment(n=c(3:0), base=1) # Comparison group: Highest reading category
      df
    } else if (input$ownData==TRUE) {
      req(input$file1)

      # This was computed by hand in the UPnorm testing file
      df <- read.csv(input$file1$datapath, stringsAsFactors=FALSE) %>%
        rename(ed.ba = ed.ba.f,
               gender = gender.f,
               base.readcat = base.readcat.f) %>%
        mutate(id = str_pad(id, 4, side="left", pad="0"),
               ed.ba.f = factor(ed.ba,
                                levels=c("No BA", "BA"),
                                ordered=TRUE),
               gender.f = factor(gender,
                                 levels=c("Female","Male"),
                                 ordered=TRUE),
               base.readcat.f = factor(base.readcat,
                                       levels=c(3,2,1,0),
                                       ordered=TRUE),
               age = age.L+meanage)
      contrasts(df$ed.ba.f) = contr.treatment(n=c(1:0), base=1) # Comparison group: Those with a BA
      contrasts(df$gender.f) = contr.treatment(n=c(1:0), base=1) # Comparison group: Women
      contrasts(df$base.readcat.f) <- contr.treatment(n=c(3:0), base=1) # Comparison group: Highest reading category
      df
    }
  })

  subList = reactive({
    df <- dataInput()
    df %>% select(id) %>% unique()
  })

  observeEvent(input$sample, {
    cat("I have my own data:", input$ownData)
  })

  observe({
    updateSelectInput(session, "id", choices=subList())
  })
  
  ownData<- reactive({
    if(input$ownData==FALSE){
      paste0(FALSE)
    } else{
      paste0(TRUE)
    }
  })

  pibInput <- reactive({
    if (input$ownData==FALSE) {
      read.csv("sample data/sample_pib.csv")
    } else {
      req(input$file2)
      read.csv(input$file2$datapath, stringsAsFactors=FALSE)
    }
  })

  csfInput <- reactive({
    if (input$ownData==FALSE) {
      read.csv("sample data/sample_csf.csv")
    } else {
      req(input$file3)
      read.csv(input$file3$datapath, stringsAsFactors=FALSE)
    }
  })
  
  ptauInput <- reactive({
    if (input$ownData==FALSE) {
      read.csv("sample data/sample_ptau.csv")
    } else {
      req(input$file4)
      read.csv(input$file4$datapath, stringsAsFactors=FALSE)
    }
  })
  
  mkInput <- reactive({
    if (input$ownData==FALSE) {
      read.csv("sample data/sample_mk.csv")
    } else {
      req(input$file5)
      read.csv(input$file5$datapath, stringsAsFactors=FALSE)
    }
  })
  
  ampInput <- reactive({
    if (input$ownData==FALSE) {
      read.csv("sample data/sample_amp.csv")
    } else {
      req(input$file6)
      read.csv(input$file6$datapath, stringsAsFactors=FALSE)
    }
  })

  mhInput <- reactive({
    if (input$ownData==FALSE) {
      read.csv("sample data/sample_mh.csv")
    } else {
      req(input$file7)
      read.csv(input$file7$datapath, stringsAsFactors=FALSE)
    }
  })

  output$rrqPlot <- renderPlot(height=900,{
    req(input$id, input$variable)
    message("~~~~~~~~~~~~~~~~~~~~~BEGIN PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    dyn.axis <- as.logical(max(grepl("1", input$options)))
    vislabel <- as.logical(max(grepl("2", input$options)))
    mh <- as.logical(max(grepl("3", input$options)))
    biomarkers <- as.logical(max(grepl("4", input$options)))
    
    ownData<- ownData()

    varnames <- c("ttotal", "drraw", "lm_imm.xw", "lm_del.xw", "theo.mem.xw.sca",
                  "bnt.xw", "animtotraw", "cfl.xw",
                  "trla", "trlb.xw", "waisrtot",
                  "pacc3.cfl.xw.sca", "pacc3.an.xw.sca", "pacc3.wrap.sca", "pacc4.wrap.sca", "pacc5.wrap.sca")
    varlabels <- c("AVLT Learning", "AVLT Delayed", "Logical Memory IA (XW)",
                   "Logical Memory IIA (XW)", "Memory Composite (XW)",
                   "Boston Naming (XW)", "Animal Naming", "Letter Fluency (CFL) (XW)",
                   "TMT-A", "TMT-B", "Digit Symbol",
                   "PACC3-CFL", "PACC3-AN", "PACC3-DS", "PACC4-DS", "PACC5-DS")
    testmax.vec <- c(75, 15, 25, 25, Inf, 60, Inf, Inf, 150, 300, Inf, Inf, Inf, Inf, Inf, Inf)
    names(testmax.vec) <- varnames

    var.in <- input$variable
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
    
    message(paste("Made it past initial import; I see", v, "variables."))

    inid  <- str_pad(input$id, 4, side="left", pad="0")
 
    df <- dataInput() %>%
          mutate(variable.f = factor(variable,
                                     levels=varnames,
                                     labels=varlabels))
    
    df.mh <- mhInput()
    
    if(biomarkers==TRUE) {
      df.pib <- pibInput()
      df.csf <- csfInput()
      df.ptau <- ptauInput()
      df.mk <- mkInput()
      df.amp <- ampInput()
    }
    
    # Restrict to desired variables
    # On full set, set testmin
    this.df <- filter(df,
                      variable %in% var) %>%
               group_by(id) %>% arrange(visno, .by_group=T) %>%
               mutate(cur_age = last(age, na_rm=T)) %>% ungroup()

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
                                     I(((abs(this.df$best.q.c - 0.5) * 100)^4)/(50^4))))

    # Now restrict to single subject
    if(biomarkers==TRUE) {
      
      df.pib <- filter(df.pib,id==inid) 
      message(paste("Number of PiB observations for this subject:", nrow(df.pib)))
      
      if(nrow(df.pib)==0){
        pib<- FALSE
      }else{
        pib<- TRUE
      }
      
      df.csf <- filter(df.csf, id==inid) %>%
                rename(age_csf=age) %>%
                mutate(pTau_Abeta42_bin = ifelse(is.na(pTau_Abeta42_bin), NA, pTau_Abeta42_bin))
      message(paste("Number of CSF observations for this subject:", nrow(df.csf)))
      
      if(nrow(df.csf)==0){
        csf<- FALSE
      }else{
        csf<- TRUE
      }
      
      if (ownData==TRUE){
        df.ptau <- df.ptau %>%
                   mutate(id = gsub("WRAP", "", enumber)) %>%
                   filter(id==inid) %>%
                   mutate(mean_conc=as.numeric(mean_conc),
                          ptau_bin = case_when(mean_conc<=0.4 ~ 1,
                                               mean_conc>0.4 & mean_conc<0.63 ~ 2,
                                               mean_conc >= 0.63 ~ 3),
                          age_ptau=age_at_appointment) %>%
                   select(id, age_ptau, mean_conc, ptau_bin)
      } else {
        df.ptau <- df.ptau %>%
                   filter(id==inid) %>%
                   select(id, age_ptau=age, mean_conc, ptau_bin)
      }
      message(paste("Number of pTau217 observations for this subject:", nrow(df.ptau)))
      
      if(nrow(df.ptau)==0){
        ptau<- FALSE
      }else{
        ptau<- TRUE
      }
      
      df.mk <- filter(df.mk, id==inid) %>%
               mutate(mk_bin_combined = case_when(!is.na(mk_vr_bin) ~ mk_vr_bin,
                                                  is.na(mk_vr_bin) & !is.na(mk_MTL_bin) ~ mk_MTL_bin,
                                                  ),
                      mk_bin_total = case_when(is.na(mk_bin_combined) ~ NA,
                                               mk_bin_combined %in% c("SUVR MTL-", "T-") ~ 1,
                                               mk_bin_combined %in% c("T+/MTL only") ~ 2,
                                               mk_bin_combined %in% c("SUVR MTL+", "T+/MTL & Neo") ~ 3)
                      ) %>%
               select(id, age_mk=age, mk_MTL_bin,mk_vr_bin, mk_bin_total)
      message(paste("Number of MK6240 observations for this subject:", nrow(df.mk)))
      
      if(nrow(df.mk)==0){
        mk<- FALSE
      }else{
        mk<- TRUE
      }
      
      if(ownData==TRUE){
        df.amp <- df.amp %>%
                  mutate(id = gsub("WRAP", "", Name)) %>%
                  filter(id==inid) %>%
                  mutate(amp_bin = case_when(Result %in% c("Detected-1", "Detected-2") ~ 2,
                                             Result %in% c("Not Detected") ~ 1,
                                             Result %in% c("QNS", "Indeterminate") ~ NA),
                         age=case_when(shareable_age_at_appointment == ">90" ~ 90,
                                       TRUE ~ as.numeric(shareable_age_at_appointment))) %>%
                  select(id, age_amp=age, Result, amp_bin)
      }else{
        df.amp <- df.amp %>% 
                  filter(id==inid) %>%
                  mutate(amp_bin = case_when(Result %in% c("Detected-1", "Detected-2") ~ 2,
                                             Result %in% c("Not Detected") ~ 1,
                                             Result %in% c("QNS", "Indeterminate") ~ NA)) %>%
                  select(id, age_amp=age, Result, amp_bin)
      }
      
      message(paste("Number of Amprion observations for this subject:", nrow(df.amp)))
      
      if(nrow(df.amp)==0){
        amp<- FALSE
      }else{
        amp<- TRUE
      }
    }
  
    if(mh==TRUE) {
      df.mh <- filter(df.mh, id==inid)
      message(paste("Number of hospitalization events for this subject:", nrow(df.mh)))
    }
    
    message(paste("ID is", inid))
    
    this.df <- filter(this.df, id==inid) %>%
               mutate(age=age.L+meanage)
    message(paste("Number of observations across all variables for this subject:", nrow(this.df)))
    
    
    if (nrow(this.df)==0) {
      outplot  <- ggplot() + annotate(geom="text", x=0, y=0, label="No data", colour="red", size=5) +
                  theme(axis.text=element_blank(),
                        axis.ticks=element_blank(),
                        axis.title=element_blank(),
                        panel.background=element_rect(colour="white",fill="white"))
    } else {
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
      this.unccoefs <- filter(unccoefs,
                              name %in% var)
      nlines.df<-group_by(this.unccoefs, name) %>%
                 select(name, nlines) %>%
                 rename(variable=name) %>%
                 slice(1) %>%
                 ungroup()
      limits <-  merge(nlines.df, limits)

      demos   <- arrange(this.df, visno) %>%
                 summarize(source=first(source),
                           last_dx=last(Summary.Diagnosis),
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
      
      int.miss<-  (nrow(na.exclude(intercepts.long))==0)

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
                      filter(variable %in% rownames(intercepts)) %>%
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
                      filter(variable %in% rownames(intercepts)) %>%
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
                      summarize(lab.ymin=first(y), lab.ymax=last(y)) %>%
                      ungroup()
      }

      if (dyn.axis==TRUE) {
        message("Dynamic axis on.")
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
      } else {
        message("Dynamic axis off.")
        limits <- limits %>%
                  mutate(highpoint = pmax(highobs, highfun, na.rm=TRUE),
                         lowpoint = pmax(1, testmin+1),
                         testmax = ifelse(is.infinite(testmax), highpoint, testmax),
                         my.ymin = lowpoint - pmax(1, 0.1*highpoint),
                         my.ymax = testmax + pmax(1, 0.1*highpoint)) %>%
                  ungroup() %>%
                  mutate(ncol = length(unique(variable)))
      }

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
      outplot  <- ggplot(this.df, aes(x=age)) +
        scale_x_continuous(limits=c(35,90))
      
      if (vislabel==TRUE) {
        padding <- ifelse(dyn.axis==TRUE, 20, 16)
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
        outplot<- outplot +
                  geom_text_repel(data=this.df,
                                  aes(x=age, y=annohts,
                                      label=paste0("V", visno)),
                                  hjust=0.5, colour="blue",
                                  direction="x",
                                  force=1.5,
                                  segment.colour=NA,
                                  size=this.df$annosize[1])
      }
        
      if(biomarkers==TRUE){
        
        df.pib_lim<-  merge(this.df, df.pib %>% rename(age_pib=age), all=T) %>%
                      group_by(variable, visno, age_pib) %>% slice(1) %>% ungroup() %>%
                      add_row(pib_trunc=1.10, age_pib=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(pib_trunc=1.19, age_pib=-1, variable.f=limits$variable.f[1])
        
        df.csf_lim<-  merge(this.df, df.csf, all=T) %>%
                      group_by(variable, visno, age_csf) %>% slice(1) %>% ungroup() %>%
                      add_row(pTau_Abeta42_bin=0, age_csf=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(pTau_Abeta42_bin=1, age_csf=-1, variable.f=limits$variable.f[1])
        
        df.ptau_lim<- merge(this.df, df.ptau, all=T) %>% 
                      group_by(variable, visno, age_ptau) %>% slice(1) %>% ungroup() %>%
                      add_row(ptau_bin=1, age_ptau=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(ptau_bin=2, age_ptau=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(ptau_bin=3, age_ptau=-1, variable.f=limits$variable.f[1])
        
        df.mk_lim<-   merge(this.df, df.mk, all=T) %>% 
                      group_by(variable, visno, age_mk) %>% slice(1) %>% ungroup() %>%
                      add_row(mk_bin_total=1, age_mk=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(mk_bin_total=2, age_mk=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(mk_bin_total=3, age_mk=-1, variable.f=limits$variable.f[1])
        
        df.amp_lim<-  merge(this.df, df.amp, all=T) %>% 
                      group_by(variable, visno, age_amp) %>% slice(1) %>% ungroup() %>%
                      add_row(amp_bin=1, age_amp=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(amp_bin=2, age_amp=-1, variable.f=limits$variable.f[1]) %>%
                      add_row(amp_bin=NA, age_amp=-1, variable.f=limits$variable.f[1])
        
        
        ## Combine plots
        shapes <- c("pTau/AB42" = "square", "pTau217" = "triangle", "MK6240" = "diamond", "aSyn" = "circle cross", "PiB/NAV" = "circle")
        
        if(csf==TRUE|ptau==TRUE|mk==TRUE|amp==TRUE){
          outplot<- outplot+
            ## pTau/AB42
            geom_point(data=df.csf_lim, aes(x=age_csf, y=my.ymin + 1.15*(my.ymax-my.ymin), 
                                            colour=pTau_Abeta42_bin, shape="pTau/AB42"), size=4)+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=0.5, na.value="black",
                                   breaks=c(0, 0.5, 1),
                                   labels=c("-", "","+"),
                                   name="Biomarker")+
            ## pTau217
            guides(colour="none")+
            new_scale("colour")+
            geom_point(data=df.ptau_lim, aes(x=age_ptau, y=my.ymin + 1.25*(my.ymax-my.ymin), 
                                             colour=ptau_bin, shape="pTau217"), size=4)+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=2, na.value="black",
                                   breaks=c(1,2,3),
                                   labels=c("-", "","+"),
                                   name="Biomarker")+
            ## MK6240
            guides(colour="none")+
            new_scale("colour")+
            geom_point(data=df.mk_lim, aes(x=age_mk, y= my.ymin + 1.35*(my.ymax-my.ymin), 
                                           colour=mk_bin_total, shape="MK6240"), size=4)+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=2, na.value="black",
                                   breaks=c(1, 2, 3),
                                   labels=c("-","","+"),
                                   name="Biomarker")+
            ## Amprion
            guides(colour="none")+
            new_scale("colour")+
            geom_point(data=df.amp_lim, aes(x=age_amp, y= my.ymin + 1.45*(my.ymax-my.ymin), 
                                            colour=amp_bin, shape="aSyn"), size=4)+
            scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.5, na.value="black",
                                   breaks=c(1,1.5,2),
                                   labels=c("-", "","+"),
                                   name="Biomarker")+
            scale_shape_manual(values = shapes, 
                               breaks=c("aSyn", "MK6240", "pTau217", "pTau/AB42", "PiB/NAV"),
                               guide=guide_legend(order=3))+
            labs(shape=NULL)
        }
        
        ## PiB
        if (nrow(df.pib)==0) {
          # Two rows are ghost rows for scaling purposes
          message("No PiB observed for this participant")
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
                    slice(n()) %>%
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
              geom_rect_pattern(data=slice(df.pib, 1),
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
                    geom_point(data=filter(this.df, singleobs==TRUE),
                               aes(y=value),
                               shape=4, size=4, stroke=2, show.legend=FALSE) +
                    labs(x="Age",
                         y="Outcome value",
                         shape="Conditional performance",
                         pattern_fill="") +
                    theme_bw() +
                    theme(legend.position="bottom", legend.direction = "horizontal",
                          legend.box="vertical")
      
      
      if (mh==TRUE) {
        df.mh      <- filter(df.mh, age > my.xlim[1] & age < my.xlim[2])
        if (nrow(df.mh)==0) {
          message("No medical history events observed for this participant within the relevant age range.")
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
                  facet_wrap(~variable.f, 
                             ncol=pmin(limits$ncol[1],3),
                             scales="free_y")+
                  theme(text=element_text(size=18),
                        legend.text=element_text(size=14))+
                  new_scale("linetype")+
                  geom_vline(aes(xintercept=this.df$cur_age, linetype="Current Age"), color = "darkgreen",alpha=0.8, size=1)+
                  scale_linetype_manual(values=c("Current Age" = "dotted"), 
                                        guide=guide_legend(order=99, override.aes = list(alpha=1)))+
                  labs(linetype=NULL)
    }
    
    outplot
    
  })

  output$rrqTable <- renderTable({
    req(input$id, input$variable)
    
    var.in <- input$variable
    
    if (var.in=="memory")
    {
      var <- c("ttotal", "drraw", "lm_imm.xw", "lm_del.xw", "theo.mem.xw.sca")
    } else if (var.in=="execfn") {
      var <- c("waisrtot", "trla", "trlb.xw")
    } else if (var.in=="language") {
      var <- c("animtotraw","cfl.xw")
    } else if (var.in=="global") {
      var <- c("pacc3.wrap.sca", "pacc4.wrap.sca", "pacc5.wrap.sca")
    } else {
      var <- var.in
    }
    
    v   <- length(var)

    varnames <- c("ttotal", "drraw", "lm_imm.xw", "lm_del.xw", "theo.mem.xw.sca",
                  "bnt.xw", "animtotraw", "cfl.xw",
                  "trla", "trlb.xw", "waisrtot",
                  "pacc3.wrap.sca", "pacc4.wrap.sca", "pacc5.wrap.sca")
    varlabels <- c("AVLT Learning", "AVLT Delayed", "Logical Memory IA (XW)",
                   "Logical Memory IIA (XW)", "Memory Composite (XW)",
                   "Boston Naming (XW)", "Animal Naming", "Letter Fluency (CFL) (XW)",
                   "TMT-A", "TMT-B", "Digit Symbol",
                   "PACC3", "PACC4", "PACC5")

    inid  <- str_pad(input$id, 4, side="left", pad="0")

    df <- dataInput() %>%
          mutate(variable.f = factor(variable,
                                     levels=varnames,
                                     labels=varlabels))
    df <- filter(df,
                 variable %in% var & id==inid) %>%
          mutate(best.q.unc.print=sprintf("%2.0f",100*best.q.unc),
                 best.q.c.print=sprintf("%2.0f",100*best.q.c)) %>%
          select(variable.f, visno, age, value, best.q.unc.print, best.q.c.print) %>%
          arrange(variable.f, visno)
    
    colnames(df) <- c("Test", "Visit", "Age", "Raw score",
                      "Uncond'l %ile", "Cond'l %ile")
    
    df
  },
  striped=TRUE,
  digits=1,
  align="c")

  
  output$biotable<- renderTable({
    req(input$id)
    
    biomarkers <- as.logical(max(grepl("4", input$options)))
    inid  <- str_pad(input$id, 4, side="left", pad="0")
    ownData<- ownData()
    
    if(biomarkers==TRUE){
      df.amp<-  ampInput()
      df.mk<-   mkInput()
      df.ptau<- ptauInput()
      df.csf<-  csfInput()
      df.pib<-  pibInput()
      
      df.pib <- filter(df.pib,id==inid) %>%
                mutate(pib_index = round(pib_index, 3),
                       pib_trunc = round(pib_trunc, 3))
  
      df.csf <- filter(df.csf, id==inid) %>%
                rename(age_csf=age) %>%
                mutate(pTau_Abeta42_bin = ifelse(is.na(pTau_Abeta42_bin), NA, pTau_Abeta42_bin))
      
      if (ownData==TRUE){
        df.ptau<- df.ptau %>%
                  mutate(id = gsub("WRAP", "", enumber)) %>%
                  filter(id==inid) %>%
                  mutate(mean_conc=as.numeric(mean_conc),
                         ptau_bin = case_when(mean_conc<=0.4 ~ 1,
                                              mean_conc>0.4 & mean_conc<0.63 ~ 2,
                                              mean_conc >= 0.63 ~ 3),
                         mean_conc = round(as.numeric(mean_conc), 3))%>%
                  select(id, age_ptau=age_at_appointment, mean_conc, ptau_bin)
      } else {
        df.ptau<- df.ptau %>%
                  filter(id==inid) %>%
                  mutate(mean_conc = round(as.numeric(mean_conc), 3)) %>%
                  select(id, age_ptau=age, mean_conc, ptau_bin)
      }
      
      df.mk <- filter(df.mk, id==inid) %>%
               mutate(mk_bin_combined = case_when(!is.na(mk_vr_bin) ~ mk_vr_bin,
                                                  is.na(mk_vr_bin) & !is.na(mk_MTL_bin) ~ mk_MTL_bin),
                      mk_bin_total = case_when(is.na(mk_bin_combined) ~ NA,
                                               mk_bin_combined %in% c("SUVR MTL-", "T-") ~ 1,
                                               mk_bin_combined %in% c("T+/MTL only") ~ 2,
                                               mk_bin_combined %in% c("SUVR MTL+", "T+/MTL & Neo") ~ 3)) %>%
               select(id, age_mk=age, mk_MTL_bin,mk_vr_bin, mk_bin_total)
      
      if(ownData==TRUE){
        df.amp<-  df.amp %>%
                  mutate(id = gsub("WRAP", "", Name)) %>%
                  filter(id==inid) %>%
                  mutate(amp_bin = case_when(Result %in% c("Detected-1", "Detected-2") ~ 2,
                                             Result %in% c("Not Detected") ~ 1,
                                             Result %in% c("QNS", "Indeterminate") ~ NA),
                         age=case_when(shareable_age_at_appointment == ">90" ~ 90,
                                       TRUE ~ as.numeric(shareable_age_at_appointment)))%>%
                  select(id, age_amp=age, Result, amp_bin)
      }else{
        df.amp<-  df.amp %>% 
                  filter(id==inid) %>%
                  mutate(amp_bin = case_when(Result %in% c("Detected-1", "Detected-2") ~ 2,
                                             Result %in% c("Not Detected") ~ 1,
                                             Result %in% c("QNS", "Indeterminate") ~ NA)) %>%
                  select(id, age_amp=age, Result, amp_bin)
      }
      
      df.amp.merge<-  df.amp %>%
                      arrange(age_amp) %>%
                      group_by(age_amp) %>% slice(1) %>% ungroup() %>%
                      select(-amp_bin) %>%
                      mutate(Biomarker="aSyn") %>%
                      pivot_longer(cols=c(Result),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_amp, Variable, Value)
      
      df.mk.merge<-   df.mk %>%
                      arrange(age_mk) %>%
                      group_by(age_mk) %>% slice(1) %>% ungroup() %>%
                      select(-mk_bin_total) %>%
                      mutate(Biomarker="MK6240") %>%
                      pivot_longer(cols=c(mk_MTL_bin,mk_vr_bin),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_mk, Variable, Value)
      
      df.ptau.merge<- df.ptau %>%
                      arrange(age_ptau) %>%
                      group_by(age_ptau) %>% slice(1) %>% ungroup() %>%
                      select(-ptau_bin) %>%
                      mutate(Biomarker="pTau217") %>%
                      pivot_longer(cols=c(mean_conc),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_ptau, Variable, Value)
      
      df.csf.merge<-  df.csf %>%
                      arrange(age_csf) %>%
                      group_by(age_csf) %>% slice(1) %>% ungroup() %>%
                      mutate(Biomarker="pTau/AB42") %>%
                      pivot_longer(cols=c(pTau_Abeta42_bin),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_csf, Variable, Value)
      
      df.pib.merge<-  df.pib %>%
                      arrange(age) %>%
                      group_by(age) %>% slice(1) %>% ungroup() %>%
                      select(id, age, pib_index, amyloid_vr) %>%
                      mutate(Biomarker="PiB/NAV") %>%
                      pivot_longer(cols=c(pib_index, amyloid_vr),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age, Variable, Value)
      
      df<-  rbind(df.amp.merge, df.mk.merge) %>%
            rbind(., df.ptau.merge) %>%
            rbind(., df.csf.merge) %>%
            rbind(., df.pib.merge)
    } else if(biomarkers==FALSE){
      df<- data.frame(id=inid, Biomarker=NA, age=NA, Variable=NA, Value=NA)
    }
    
    df
    
    },
    striped=TRUE,
    digits=1,
    align="c")
  
  output$rrqSubject <- renderText({
    req(input$id)

    inid  <- str_pad(input$id, 4, side="left", pad="0")
    df <- dataInput()
    
    df <- filter(df,id==inid) %>%
          arrange(visno)
    
    ids<- summarize(df,id=first(id))
    
    label0=c(paste0("Participant ID: ", inid))
    
    HTML(label0)
  })

  output$rrqHeader1 <- renderUI({
    req(input$id)
    inid  <- str_pad(input$id, 4, side="left", pad="0")
    
    df <- dataInput()
    df <- filter(df,id==inid) %>%
          arrange(visno)
    
    demos   <- summarize(df,
                         source=first(source),
                         gender.f=first(gender.f))
    
    label1=c(paste(paste("Cohort:", demos$source),
                  paste("Sex:", demos$gender.f),
                  sep='<br />'))
    
    HTML(label1)
  })

  output$rrqHeader2 <- renderUI({
    req(input$id)

    inid  <- str_pad(input$id, 4, side="left", pad="0")
    df <- dataInput()
    
    df <- filter(df, id==inid) %>%
          arrange(visno)
    demos   <- summarize(df,
                         ed.ba.f=first(ed.ba.f),
                         base.readcat.f=first(base.readcat.f),
                         base.readcat.lab = recode(base.readcat.f, `0`="Q1",`1`="Q2",`2`="Q3",`3`="Q4"))
    
    label2=c(paste(paste("Education:", demos$ed.ba.f),
                  paste("Baseline Reading:",demos$base.readcat.lab),
                  sep='<br />'))
    
    HTML(label2)
  })

  output$rrqHeader3 <- renderUI({
    req(input$id)

    inid  <- str_pad(input$id, 4, side="left", pad="0")
    df <- dataInput()
    
    df <- filter(df, id==inid) %>%
          arrange(visno)
    if (length(na.omit(df$Summary.Diagnosis))>0) {
      df <- filter(df, !is.na(Summary.Diagnosis))
    }
    demos   <- summarize(df,
                         last_dx=last(Summary.Diagnosis))
    
    label3=paste("Last dx:",demos$last_dx)
    
    HTML(label3)
  })
}

shinyApp(ui = ui, server = server)
