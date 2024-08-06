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

# modified for testing
unccoefs <- readRDS("./data/unccoefs.rds")
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
    sidebarPanel(width=3, fluid=T, 

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

        fileInput("file2", "Choose Biomarkers CSV file",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        fileInput("file3", "Choose Medical History CSV file",
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
                         choices = list("Annotate with visit labels" = 2,
                                        "Annotate with medical history" = 3,
                                        "Annotate with biomarkers" = 4
                                        ),
                         selected = c(2)),

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
  
  ownData<- reactive({
    if(input$ownData==FALSE){
      paste0(FALSE)
    } else{
      paste0(TRUE)
    }
  })
  
  pibInput <- reactive({
    if(input$ownData==FALSE){
      sample_pib}
    })
  csfInput <- reactive({
    if(input$ownData==FALSE){
      sample_csf}
  })
  ptauInput <- reactive({
    if(input$ownData==FALSE){
      sample_ptau}
  })
  mkInput <- reactive({
    if(input$ownData==FALSE){
      sample_mk}
  })
  ampInput <- reactive({
    if(input$ownData==FALSE){
      sample_amp}
  })
  biomarkerInput<- reactive({
    if(input$ownData==TRUE){
      req(input$file2)
      read.csv(input$file2$datapath, stringsAsFactors=FALSE)
      }
    })

  mhInput <- reactive({
    if (input$ownData==FALSE) {
      sample_mh
    } else {
      req(input$file3)
      read.csv(input$file3$datapath, stringsAsFactors=FALSE)
    }
  })

  output$rrqPlot <- renderPlot(height=800,{
    
    req(input$id, input$variable)
    message("~~~~~~~~~~~~~~~~~~~~~BEGIN PLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    if(!is.null(input$options)){
      vislabel<- as.logical(max(grepl("2", input$options)))
      mh<- as.logical(max(grepl("3", input$options)))
      biomarkers<- as.logical(max(grepl("4", input$options)))
    } else {
      vislabel<- FALSE
      mh<- FALSE
      biomarkers<- FALSE
    }
    
    message(paste("mh:",mh,"biomarkers:",biomarkers))
    
    ownData<- ownData()
    data <- dataInput()
    var.in <- input$variable
    inid <- input$id

    mh_list <- list()
    if (mh==TRUE) {
      mh_list$mh <- mhInput()
    }
    
    biomarker_list <- list()
    if (biomarkers==TRUE & ownData==FALSE) {
      biomarker_list$pib <- pibInput()
      biomarker_list$csf <- csfInput()
      biomarker_list$ptau <- ptauInput()
      biomarker_list$mk <- mkInput()
      biomarker_list$amp <- ampInput()
    } else if (biomarkers==TRUE & ownData==TRUE){
      biomarker_list$pib<- biomarkerInput() %>% filter(biomarker=="PiB/NAV")
      biomarker_list$csf <- biomarkerInput() %>% filter(biomarker=="pTau/AB42")
      biomarker_list$ptau <- biomarkerInput() %>% filter(biomarker=="pTau217")
      biomarker_list$mk <- biomarkerInput() %>% filter(biomarker=="MK6240")
      biomarker_list$amp <- biomarkerInput() %>% filter(biomarker=="aSyn")
    }
    
    message(paste("Got through the inputs. My biomarker list:", paste(names(biomarker_list), collapse=", ")))
    
      if (length(biomarker_list)>0) {
        if(length(mh_list)>0) {
          outplot <- wis_plot(data=data, var=var.in, sub=inid, vislabel=vislabel, biomarker_list=biomarker_list, mh_list=mh_list, ownData=ownData)
        } else {
          outplot <- wis_plot(data=data, var=var.in, sub=inid, vislabel=vislabel, biomarker_list=biomarker_list, ownData=ownData)
        }
      } else {
        if(length(mh_list)>0) {
          outplot <- wis_plot(data=data, var=var.in, sub=inid, vislabel=vislabel, mh_list=mh_list, ownData=ownData)
        } else {
          outplot <- wis_plot(data=data, var=var.in, sub=inid, vislabel=vislabel, ownData=ownData)
        }
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
      if (ownData==FALSE) {
        df.pib <- pibInput()
        df.csf <- csfInput()
        df.ptau <- ptauInput()
        df.mk <- mkInput()
        df.amp <- ampInput()
      } else if (ownData==TRUE){
        df.pib<- biomarkerInput() %>% filter(biomarker=="PiB/NAV")
        df.csf <- biomarkerInput() %>% filter(biomarker=="pTau/AB42")
        df.ptau <- biomarkerInput() %>% filter(biomarker=="pTau217")
        df.mk <- biomarkerInput() %>% filter(biomarker=="MK6240")
        df.amp <- biomarkerInput() %>% filter(biomarker=="aSyn")
      }
      
      df.pib <- df.pib %>%
                filter(id==inid) %>%
                mutate(pib_index = round(pib_index, 3),
                       pib_trunc = round(pib_trunc, 3))
  
      df.csf <- df.csf %>%
                filter(id==inid) %>%
                mutate(pTau_Abeta42_bin = ifelse(is.na(pTau_Abeta42_bin), NA, pTau_Abeta42_bin)) %>%
                select(id, age_csf=age, pTau_Abeta42_bin)
      
      df.ptau<- df.ptau %>%
                filter(id==inid) %>%
                mutate(mean_conc = round(as.numeric(mean_conc), 3)) %>%
                select(id, age_ptau=age, mean_conc, ptau_bin)
      
      df.mk <- df.mk %>%
               filter(id==inid) %>%
               mutate(mk_bin_combined = case_when(!is.na(mk_vr_bin) ~ mk_vr_bin,
                                                  is.na(mk_vr_bin) & !is.na(mk_MTL_bin) ~ mk_MTL_bin),
                      mk_bin_total = case_when(is.na(mk_bin_combined) ~ NA,
                                               mk_bin_combined %in% c("SUVR MTL-", "T-") ~ 1,
                                               mk_bin_combined %in% c("T+/MTL only") ~ 2,
                                               mk_bin_combined %in% c("SUVR MTL+", "T+/MTL & Neo") ~ 3)) %>%
               select(id, age_mk=age, mk_MTL_bin, mk_vr_bin, mk_bin_total)
      
      if(ownData==TRUE){
        df.amp<-  df.amp %>%
                  filter(id==inid) %>%
                  select(id, age_amp=age, Result, amp_bin)
      }else{
        df.amp<-  df.amp %>% 
                  filter(id==inid) %>%
                  mutate(amp_bin = case_when(Result %in% c("Detected-1") ~ 2,
                                             Result %in% c("Not Detected") ~ 1,
                                             Result %in% c("QNS", "Indeterminate", "Detected-2") ~ NA)) %>%
                  select(id, age_amp=age, Result, amp_bin)
      }
      
      df.amp.merge<-  df.amp %>%
                      arrange(age_amp) %>%
                      group_by(age_amp) %>% slice(1) %>% ungroup() %>%
                      select(id, age_amp, aSyn=Result) %>%
                      mutate(Biomarker="aSyn") %>%
                      pivot_longer(cols=c(aSyn),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_amp, Variable, Value)
      
      df.mk.merge<-   df.mk %>%
                      arrange(age_mk) %>%
                      group_by(age_mk) %>% slice(1) %>% ungroup() %>%
                      select(id, age_mk, MK_SUVR_MTL=mk_MTL_bin, MK_VR=mk_vr_bin) %>%
                      mutate(Biomarker="MK6240") %>%
                      pivot_longer(cols=c(MK_SUVR_MTL,MK_VR),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_mk, Variable, Value)
      
      df.ptau.merge<- df.ptau %>%
                      arrange(age_ptau) %>%
                      group_by(age_ptau) %>% slice(1) %>% ungroup() %>%
                      select(id, age_ptau, pTau217=mean_conc) %>%
                      mutate(Biomarker="pTau217") %>%
                      pivot_longer(cols=c(pTau217),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_ptau, Variable, Value)
      
      df.csf.merge<-  df.csf %>%
                      arrange(age_csf) %>%
                      group_by(age_csf) %>% slice(1) %>% ungroup() %>%
                      select(id, age_csf, pTau181_Abeta42=pTau_Abeta42_bin) %>%
                      mutate(Biomarker="pTau181/AB42",
                             pTau181_Abeta42 = case_when(pTau181_Abeta42==1~ "A+", 
                                                          pTau181_Abeta42==0 ~ "A-")) %>%
                      pivot_longer(cols=c(pTau181_Abeta42),
                                   names_to = "Variable",
                                   values_to="Value") %>%
                      select(id, Biomarker, age=age_csf, Variable, Value)
      if(ownData==TRUE){
      df.pib.merge<-  df.pib %>%
                      arrange(age) %>%
                      group_by(age) %>% slice(1) %>% ungroup() %>%
                      select(id, age, PiB_DVR=pib_index, Amyloid_VR=amyloid_vr) %>%
                        mutate(Biomarker="PiB/NAV") %>%
                        pivot_longer(cols=c(PiB_DVR, Amyloid_VR),
                                     names_to = "Variable",
                                     values_to="Value") %>%
                        select(id, Biomarker, age, Variable, Value)
        } else{
        df.pib.merge<- df.pib %>%
                      arrange(age) %>%
                      group_by(age) %>% slice(1) %>% ungroup() %>%
                      select(id, age, pib_trunc) %>%
                      select(id, age, PiB_trunc=pib_trunc) %>%
                        mutate(Biomarker="PiB/NAV") %>%
                        pivot_longer(cols=c(PiB_trunc),
                                     names_to = "Variable",
                                     values_to="Value") %>%
                        select(id, Biomarker, age, Variable, Value)
        }
      
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
