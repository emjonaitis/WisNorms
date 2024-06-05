# app.R

# Last modified 2021-12-20.
# To-do items:
#  - Add a footer explaining what the dotted region is.
#  - Update the package.
#  - If it works, install on 10504 and 11523 and 12862.
#  - Merge current git branch with old branch.
#  - Upload to gitlab.

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

unccoefs <- readRDS("./data/unccoefs.rds")
sample_df <- readRDS("./data/sample_df.rds")
sample_mh <- readRDS("./data/sample_mh.rds")
sample_pib <- readRDS("./data/sample_pib.rds")
sample_csf <- readRDS("./data/sample_csf.rds")
meanage  <- 58.9


ui <- fluidPage(

  # App title ----
  titlePanel("Visualizing Wisconsin Data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

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

        fileInput("file4", "Choose medical history CSV file",
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
                                        "Annotate with PiB measurements" = 3,
                                        "Annotate with CSF measurements" = 4,
                                        "Annotate with medical history" = 5),
                         selected = c(1, 2)),


      # fluidRow(
      #   column(width = 7, tableOutput("wsf")),
      #   column(width = 5, tableOutput("wbf")))

      # Main panel for displaying outputs ----

      submitButton("Submit")
    ),

    # Only show this panel if the plot type is a histogram
    mainPanel(      # Output: Tabset w/ plot, summary, and table ----
                    h2(textOutput("rrqSubject")),
                    h3(column(width=6, htmlOutput("rrqHeader1")),
                      column(width=6, htmlOutput("rrqHeader2"))),
                    h3(column(width=12, htmlOutput("rrqHeader3"))),
                    tabsetPanel(type = "tabs",
                                tabPanel("Plot", plotOutput("rrqPlot")),
                                #tabPanel("Summary", verbatimTextOutput("summary")),
                                tabPanel("Table", tableOutput("rrqTable"))) # need to create this
    )
  )
)

options(shiny.maxRequestSize=100**1024^2)

server <- function(input, output, session) {

  dataInput <- reactive({

    if (input$ownData==FALSE) {
      df <- sample_df %>%
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

  pibInput <- reactive({
    if (input$ownData==FALSE) {
      df.pib <- sample_pib
      df.pib
    } else {
      req(input$file2)

      # This was computed by hand in the UPnorm testing file
      df.pib <- read.csv(input$file2$datapath, stringsAsFactors=FALSE)
      df.pib
    }
  })

  csfInput <- reactive({
    if (input$ownData==FALSE) {
      df.csf <- sample_csf
      df.csf
    } else {
      req(input$file3)

      # This was computed by hand in the UPnorm testing file
      df.csf <- read.csv(input$file3$datapath, stringsAsFactors=FALSE)
      df.csf
    }
  })

  mhInput <- reactive({
    if (input$ownData==FALSE) {
      df.mh <- sample_mh
      df.mh
    } else {
      req(input$file4)

      # This was computed by hand in the UPnorm testing file
      df.mh <- read.csv(input$file4$datapath, stringsAsFactors=FALSE)
      df.mh
    }
  })

  output$rrqPlot <- renderPlot({
    req(input$id, input$variable)
    message("~~~~~~~~~~~~~~~~~~~~~BEGIN PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    dyn.axis <- as.logical(max(grepl("1", input$options)))
    vislabel <- as.logical(max(grepl("2", input$options)))
    pib <- as.logical(max(grepl("3", input$options)))
    csf <- as.logical(max(grepl("4", input$options)))
    mh <- as.logical(max(grepl("5", input$options)))

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
    if (var.in=="memory")
      {
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
    if(pib==TRUE) {
      df.pib <- pibInput() %>%
                mutate(pib_trunc = ifelse(pib_index<1.1, 1.1,
                                          ifelse(pib_index>1.19, 1.19, pib_index)))
    }

    if(csf==TRUE) {
      df.csf <- csfInput()
    }

    if(mh==TRUE) {
      df.mh <- mhInput()
    }
    # Restrict to desired variables
    # On full set, set testmin
    this.df <- filter(df,
                      variable %in% var)

    limits  <- group_by(this.df, variable, variable.f) %>%
               summarize(testmin = pmin(min(value, na.rm=TRUE), 0),
                         testmax = first(testmax.vec[variable]),
                         minage = if(first(variable)=="animtotraw") {45} else {40},
                         maxage = 85) %>%
               ungroup() %>%
               data.frame()

    this.df <- mutate(this.df,
                      sign = factor(sign(this.df$best.q.c - 0.5),
                                    levels=c(-1,1), labels=c("Low","High")),
                      alpha = ifelse(is.na(this.df$best.q.c), NA,
                                     I(((abs(this.df$best.q.c - 0.5) * 100)^4)/(50^4))))

    # Now restrict to single subject
    if(pib==TRUE) {
      ghostrows <- data.frame(id=rep(inid, 2),
                              pib_trunc=c(1.1,1.19),
                              age=c(-1,-1))
      df.pib <- filter(df.pib,
                      id==inid) %>%
                add_row(pib_trunc=1.10, age=-1) %>%
                add_row(pib_trunc=1.19, age=-1)
      message(paste("Number of PiB observations for this subject:", nrow(df.pib)-2))
    }

    if(csf==TRUE) {
      df.csf <- filter(df.csf,
                       id==inid) %>%
                mutate(pTau_Abeta42_bin = ifelse(is.na(pTau_Abeta42_bin), NA,
                                                       ifelse(pTau_Abeta42_bin==TRUE, 1.19, 1.10))) %>%
                add_row(pTau_Abeta42_bin=1.10, age=-1) %>%
                add_row(pTau_Abeta42_bin=1.19, age=-1)
      message(paste("Number of CSF observations for this subject:", nrow(df.csf)-2))
    }

    if(mh==TRUE) {
      df.mh <- filter(df.mh,
                      id==inid)
      message(paste("Number of hospitalization events for this subject:", nrow(df.mh)))
    }
    message(paste("ID is", inid))
    this.df <- filter(this.df,
                      id==inid) %>%
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
      alphalim <- group_by(this.df, variable) %>%
        summarize(alphamin = ifelse(length(na.omit(alpha))==0,
                                    NA, min(alpha, na.rm=TRUE)),
                  alphamax = ifelse(length(na.omit(alpha))==0,
                                    NA, max(alpha, na.rm=TRUE))) %>%
        ungroup()
      limits   <- merge(limits, alphalim)
      this.unccoefs <- filter(unccoefs,
                              name %in% var)
      nlines.df  <- group_by(this.unccoefs, name) %>%
        select(name, nlines) %>%
        rename(variable=name) %>%
        slice(1) %>%
        ungroup()
      limits <- merge(nlines.df, limits)

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
      int.levels <- data.frame(Intercept=1,
                               age.L=-1*meanage,
                               gender.f0=as.numeric(demos$gender.f=="Male"),
                               ed.ba.f0=as.numeric(demos$ed.ba=="BA"),
                               base.readcat.f0=as.numeric(demos$base.readcat.f==0),
                               base.readcat.f1=as.numeric(demos$base.readcat.f==1),
                               base.readcat.f2=as.numeric(demos$base.readcat.f==2)) %>%
        gather(key="coef", value="int.levels")
      component   <- data.frame(coef=c("age.L","age.Q","age.C"))

      coef.long   <- group_by(this.unccoefs, name, coef) %>%
        gather(key="tau", value="value", starts_with("tau")) %>%
        ungroup()
      intercepts.long   <- merge(coef.long, int.levels) %>%
        mutate(value.add = int.levels*value) %>%
        group_by(name, tau) %>%
        arrange(name, tau, coef) %>%
        summarize(intercept = sum(value.add)) %>%
        na.exclude() %>%
        ungroup()

      intercepts <- spread(intercepts.long,
                           key="tau", value="intercept", fill=0) %>%
        ungroup() %>%
        data.frame()
      rownames(intercepts) <- intercepts$name
      intercepts <- select(intercepts, -name)
      int.miss   <- (nrow(na.exclude(intercepts.long))==0)

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
        myfun.ymin  <- group_by(myfun.labs, variable) %>%
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
      # my.ylim   <- select(limits, my.ymin, my.ymax) %>%
      #              data.frame()
      # rownames(my.ylim) <- limits$variable

      if (int.miss==FALSE) {
        limits      <- merge(limits, myfun.ymin, all.x=TRUE) %>%
          mutate(y.range = pmax(my.ymax, lab.ymax, na.rm=TRUE) -
                   pmin(my.ymin, lab.ymin, na.rm=TRUE))
        unclines.df <- group_by(myfun.labs, variable, t) %>%
          select(variable, t, ll, ul) %>%
          tidyr::expand(x=seq(ll, ul)) %>%
          ungroup() %>%
          group_by(x, variable, t) %>%
          mutate(y=myfun(x, variable, t)) %>%
          ungroup() %>%
          merge(select(limits, variable, variable.f))
      }
      else {
        limits      <- mutate(limits,
                              y.range = my.ymax - my.ymin)
        
      }
      outplot  <- ggplot(this.df, aes(x=age)) +
        scale_x_continuous(limits=c(35,90))
      # extend xlim for purposes of labeling
      # scale_y_continuous(limits=c(limits$my.ymin[1],
      #                             limits$my.ymax[1])) +

      # Last edited 2021-12-21
      # Current question: How can I get a linetype scale to display that
      # will include EAOA and PiB for PiB data, and CSF for CSF data,
      # and all three if both are present? Do I test the binaries again after these
      # layers are done, and add the scale at that point?

      # initializing line scale
      my.linescale <- NULL

      if (csf==TRUE) {
        if (nrow(df.csf)<=2) {
          # Two rows are ghost rows for scaling purposes
          message("No CSF observed for this participant")
          csf <- FALSE
          # Setting the csf value to FALSE in this case, so it doesn't continue trying to plot CSF stuff
        } else {
          my.linescale <- c(my.linescale, "pTau/AB42"=5)
          outplot <- outplot +
            geom_vline(data=df.csf, aes(xintercept=age, colour=pTau_Abeta42_bin, linetype="pTau/AB42"))
        }
      }

      if (pib==TRUE) {
        message("pib==TRUE")
        if (nrow(df.pib)<=2) {
          # Two rows are ghost rows for scaling purposes
          message("No PiB observed for this participant")
          pib <- FALSE
          # Setting the pib value to FALSE in this case, so it doesn't continue trying to plot PiB stuff
        } else {
          limits      <- df.pib %>%
            slice(1) %>%
            mutate(my.pibmin=pmin(eaoa_1p19_gbtm, eaoa_1p19_sila, na.rm=TRUE),
                   my.pibmax=pmax(eaoa_1p19_gbtm, eaoa_1p19_sila, na.rm=TRUE),
                   my.pibmean=if_else(is.na(eaoa_1p19_mean) | is.na(eaoa_1p19_sila),
                                      my.pibmin, eaoa_1p19_mean),
                   my.pibrange = if_else(is.na(eaoa_1p19_mean) | is.na(eaoa_1p19_sila),
                                         0, eaoa_1p19_range)) %>%
            select(my.pibmin, my.pibmax, my.pibrange, my.pibmean) %>%
            merge(limits, ., all.x=TRUE)
          if (all(is.na(limits$my.pibmin))) {
            my.spacing <- ifelse(v>3, 0.03, 0.02)
            my.linescale <- c(my.linescale, "PiB"=1)
            outplot <- outplot +
                       geom_vline(data=df.pib, aes(xintercept=age, colour=pib_trunc, linetype="PiB")) +
                       scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.15)
          } else if (limits$my.pibrange[1] > 1) {
            my.spacing <- ifelse(v>3, 0.03, 0.02)
            my.linescale <- c(my.linescale, "PiB"=1)
            my.patternscale <- c("EAOA"="orangered")
            outplot <- outplot +
              scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.15) +
              geom_rect_pattern(data=slice(df.pib, 1),
                                aes(xmin=limits$my.pibmin[1],
                                    xmax=limits$my.pibmax[1],
                                    ymin=-Inf,
                                    ymax=Inf,
                                    pattern_fill="EAOA"),
                                pattern="circle",
                                fill="white",
                                colour="white",
                                #pattern_fill="orangered",
                                #pattern_colour="orangered",
                                pattern_density=0.3,
                                pattern_spacing=my.spacing,
                                pattern_alpha=0.4) +
              geom_vline(data=df.pib, aes(xintercept=age, colour=pib_trunc, linetype="PiB"))
          }
          else {
            # previously had mapped this to size. let's try it this way
            my.linescale <- c(my.linescale, "PiB"=1, "EAOA"=3)
            outplot <- outplot +
              geom_vline(data=df.pib, aes(xintercept=age, colour=pib_trunc, linetype="PiB")) +
              geom_vline(aes(xintercept=limits$my.pibmean[1], linetype="EAOA"), colour="orangered")
          }
        }
      }



      if (int.miss==FALSE) {
        outplot <- outplot +
          geom_path(data=unclines.df,
                    aes(x=x, y=y, group=t),
                    colour="grey40", linetype="dashed") +
          geom_text(aes(x=x, y=y, label=lab),
                    data=myfun.labs, colour="grey40", hjust=0,
                    check_overlap=TRUE, size=2.5)
      }
      # Below: dummy data to set y limits
      outplot  <- outplot +
        geom_point(data=limits, x=50, aes(y=my.ymin), alpha=0) +
        geom_point(data=limits, x=50, aes(y=my.ymax), alpha=0) +
        geom_line(aes(y=value, group=id), #, linetype=id
                  data=this.df, linewidth=1.5, show.legend=FALSE) +
        geom_point(aes(y=value, shape=sign, alpha=alpha), size=4, stroke=2) +
        scale_shape_manual(limits=c("High","Low"), values=c(2,6),
                           na.translate=FALSE) +
        scale_alpha_continuous(range=c(0,1), limits=c(0,1)) +
        geom_point(data=filter(this.df, singleobs==TRUE),
                   aes(y=value),
                   shape=4, size=4, stroke=2, show.legend=FALSE) +
        labs(x="Age",
             y="Outcome value",
             shape="Conditional performance",
             colour="Amyloid") +
        guides(alpha="none") +
        theme_bw() +
        theme(legend.position="bottom")

      # this is new
      # In order to put the guides in a nice order, we have to do the guide manipulation in a later
      #   step. This means testing the presence of pib and csf files and observations again.
      if (pib==TRUE | csf==TRUE) {
        outplot  <- outplot +
          scale_colour_gradient2(low="blue", mid="yellow", high="red", midpoint=1.15,
                                 breaks=c(1.10, 1.15, 1.19),
                                 labels=c(expression("A"*beta*"-"), "",
                                          expression("A"*beta*"+"))) +
          scale_linetype_manual(values=my.linescale) +
          guides(colour_gradient2=guide_legend(order=1),
                 linetype=guide_legend(order=2, override.aes=list(linewidth=0.7, colour="orangered"))) +
          labs(linetype="Biomarker")
        if (pib==TRUE) {
          if (limits$my.pibrange[1] > 1) {
            outplot  <- outplot +
              scale_pattern_fill_manual(values=my.patternscale) +
              guides(pattern_fill=guide_legend(order=3, override.aes=list(pattern_alpha=1, pattern_size=0.4, pattern_spacing=0.01,
                                                                          pattern_colour="orangered"))) +
              labs(pattern_fill=NULL)
          }
        }
      }

      # > colnames(pib.out)
      # [1] "Reggieid"                    "subjid"                      "id"                          "age"
      # [5] "pib_index"                   "pib_bin"                     "eaoa_ref_age"                "eaoa_ref_DVR"
      # [9] "eaoa_ref_above1p19"          "eaoa_1p19_gbtm"              "eaoa_1p19_sila"              "eaoa_1p19_sila_extrapolated"
      # [13] "eaoa_1p19_sila_truncated"    "eaoa_1p16_sila"              "eaoa_1p16_gbtm"



      if (mh==TRUE) {
        df.mh      <- filter(df.mh,
                             age > my.xlim[1] & age < my.xlim[2])
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

          df.mh.sum <- df.mh.sum %>%
            do({group_starts = which(.$new_group==TRUE)
            group_reps = ifelse(!is.na(lead(group_starts)),
                                lead(group_starts) - group_starts,
                                length(.$new_group) - group_starts + 1)
            groups = as.vector(unlist(mapply(rep,
                                             x=LETTERS[c(1:length(group_starts))],
                                             times=group_reps)))
            # colnames(groups)[1] <- "groups"
            # if (length(unique(groups))==1)
            # { colnames(groups) = "groups" }
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
          outplot <- outplot + geom_vline(data=df.mh,
                                          aes(xintercept=age),
                                          linetype=3, linewidth=1, colour="purple") +
            new_scale("shape") +
            geom_point(data=df.mh.sum,
                       aes(x=age_xnudge, y=label_height, size=label_size,
                           shape=event.paste.f),
                       colour="purple") +
            guides(size="none",
                   shape=guide_legend(order=4, override.aes = list(size = 5))) +
            scale_shape_manual(breaks=levels(df.mh.sum$event.paste.f),
                               values=letterlabels,
                               name="Medical events") +
            theme(legend.direction="vertical")
        }
      }
      if (vislabel==TRUE) {
        padding <- ifelse(dyn.axis==TRUE, 20, 16)
        if (int.miss==FALSE) {
          this.df <- merge(this.df,
                           select(limits, variable, y.range, my.ymin, lab.ymin, ncol)) %>%
            mutate(annohts = 1.05 * pmin(my.ymin, lab.ymin, na.rm=TRUE),
                   annonudge = 0.05*y.range,
                   annosize = 5 - pmin(ncol, 2))
        } else {
          this.df <- merge(this.df,
                           select(limits, variable, y.range, my.ymin, ncol)) %>%
            mutate(annohts = 1.05 * my.ymin,
                   annonudge = 0.05*y.range,
                   annosize = 5 - pmin(ncol, 2))
        }
        annonudge.vec <- na.omit(this.df$annonudge)
        names(annonudge.vec) <- unique(this.df$variable)
        outplot <- outplot +
          geom_text_repel(data=this.df,
                          aes(x=age, y=annohts,
                              label=paste0("V", visno)),
                          hjust=0.5, colour="blue",
                          direction="x",
                          force=1.5,
                          segment.colour=NA,
                          size=this.df$annosize[1],
                          show.legend=FALSE)
      }
      outplot <- outplot +
        facet_wrap(~variable.f, ncol=pmin(limits$ncol[1],3),
                   scales="free_y")
    }
    outplot +
      theme(text=element_text(size=18))
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
  align=c("lccccc"))

  output$rrqSubject <- renderText({
    req(input$id)

    # var.in <- input$variable
    # if (var.in=="memory")
    # {
    #   var <- c("ttotal", "drraw", "lm_imm.xw", "lm_del.xw", "theo.mem.xw.sca")
    # } else if (var.in=="execfn") {
    #   var <- c("waisrtot", "trla", "trlb.xw")
    # } else if (var.in=="language") {
    #   var <- c("animtotraw","cfl.xw")
    # } else if (var.in=="global") {
    #   var <- c("PACC3-WRAP", "PACC4-WRAP", "PACC5-WRAP")
    # } else {
    #   var <- var.in
    # }
    #

    inid  <- str_pad(input$id, 4, side="left", pad="0")
    df <- dataInput()
    df <- filter(df,
                 id==inid) %>%
          arrange(visno)
    ids   <- summarize(df,
                       id=first(id))
    label0=c(paste0("Participant ID: ", inid))
    HTML(label0)
    # df <- dataInput()
    # df <- filter(df,
    #              variable %in% var & id==inid)
    # id=unique(df$id)
    # id
  })

  output$rrqHeader1 <- renderUI({
    req(input$id)
    inid  <- str_pad(input$id, 4, side="left", pad="0")
    df <- dataInput()
    df <- filter(df,
                 id==inid) %>%
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
    df <- filter(df,
                 id==inid) %>%
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
    df <- filter(df,
                 id==inid) %>%
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
