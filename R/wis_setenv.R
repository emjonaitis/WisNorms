# Setting necessary environment variables for internal use.
# Should need to rerun only when new outcomes are added.

library(pryr)

e <- new.env()
assign("target", read.csv("Z:/sandbox/target.csv", stringsAsFactors=FALSE), envir=e)
assign("missval", read.csv("Z:/sandbox/missval.csv", stringsAsFactors=FALSE), envir=e)
assign("compscale", read.csv("Z:/sandbox/composite_scaling.csv", stringsAsFactors=FALSE), envir=e)
# Naming is weird here; compscale3 just includes an additional mean/sd for pacc3.an.xw,
#   but I wanted to leave it called compscale2 for compatibility with code elsewhere.
assign("compscale2", read.csv("Z:/sandbox/composite_scaling3.csv", stringsAsFactors=FALSE), envir=e)
assign("sample_df", read.csv("P:/data/sample_df.csv", stringsAsFactors=FALSE), envir=e)
assign("sample_mh", read.csv("P:/data/sample_mh.csv", stringsAsFactors=FALSE), envir=e)
assign("sample_pib", read.csv("P:/data/sample_pib.csv", stringsAsFactors=FALSE), envir=e)
assign("sample_csf", read.csv("P:/data/sample_csf.csv", stringsAsFactors=FALSE), envir=e)

f <- list.files(path="P:/model-2023/")
for (i in 1:length(f)) {
  name <- gsub(".rda","",f[i])
  message(paste("Starting",name))
  filename <- paste0("P:/model-2023/", f[i])
  assign(name, readRDS(filename), envir=e)
  message(paste("Read",name))
  message(paste("ls:",paste(ls(), collapse=", ")))
  message(paste("ls(e):", paste(ls(e), collapse=", ")))
  size.now <- object_size(e)
  message(paste("e contains",size.now,"MB"))
  message(paste("mem_used is", mem_used()))
  gc()
}

save(list=ls(envir=e), envir=e, file="R/sysdata.rda")
saveRDS(e$unccoefs.rds, file="inst/shiny-examples/WisNormPlot/data/unccoefs.rds")
saveRDS(e$sample_df, file="inst/shiny-examples/WisNormPlot/data/sample_df.rds")
saveRDS(e$sample_mh,file="inst/shiny-examples/WisNormPlot/data/sample_mh.rds")
saveRDS(e$sample_pib, file="inst/shiny-examples/WisNormPlot/data/sample_pib.rds")
saveRDS(e$sample_csf, file="inst/shiny-examples/WisNormPlot/data/sample_csf.rds")
