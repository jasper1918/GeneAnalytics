source("http://bioconductor.org/biocLite.R")

if(!require(abind))   { biocLite("abind")}
if(!require(corrplot))  { biocLite("corrplot")}
if(!require(gage))   { biocLite("gage")}
if(!require(genefu))   { biocLite("genefu")}
if(!require(ggplot2))   { biocLite("ggplot2")}
if(!require(gplots))   { biocLite("gplots")}
if(!require(hgu133a.db))     { biocLite("hgu133a.db")  }
if(!require(igraph))   { biocLite("igraph")}
if(!require(RColorBrewer))      biocLite("RColorBrewer")
if(!require(reshape))   { biocLite("reshape")}
if(!require(rmeta))   { biocLite("rmeta")}
if(!require(RSQLite))   { biocLite("RSQLite")}
if(!require(survcomp))   { biocLite("survcomp")}
if(!require(survival))   { biocLite("survival")}

if(!require(jetset))   { cat("Install Jetset")}