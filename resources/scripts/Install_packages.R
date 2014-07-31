source("http://bioconductor.org/biocLite.R")

if(!require(abind))   { biocLite("abind",lib='/usr/local/lib/R/site-library')}
if(!require(corrplot))  { biocLite("corrplot",lib='/usr/local/lib/R/site-library')}
if(!require(gage))   { biocLite("gage",lib='/usr/local/lib/R/site-library')}
if(!require(genefu))   { biocLite("genefu",lib='/usr/local/lib/R/site-library')}
if(!require(ggplot2))   { biocLite("ggplot2",lib='/usr/local/lib/R/site-library')}
if(!require(gplots))   { biocLite("gplots",lib='/usr/local/lib/R/site-library')}
if(!require(hgu133a.db))     { biocLite("hgu133a.db",lib='/usr/local/lib/R/site-library') }
if(!require(igraph))   { biocLite("igraph",lib='/usr/local/lib/R/site-library')}
if(!require(RColorBrewer))      {biocLite("RColorBrewer",lib='/usr/local/lib/R/site-library')}
if(!require(reshape))   { biocLite("reshape",lib='/usr/local/lib/R/site-library')}
if(!require(rmeta))   { biocLite("rmeta",lib='/usr/local/lib/R/site-library')}
if(!require(RSQLite))   { biocLite("RSQLite",lib='/usr/local/lib/R/site-library')}
if(!require(survcomp))   { biocLite("survcomp",lib='/usr/local/lib/R/site-library')}
if(!require(survival))   { biocLite("survival",lib='/usr/local/lib/R/site-library')}

if(!require(jetset))   { cat("Installing Jetset")
install.packages("~/mygit/gene_analytics/resources/scripts/jetset_2.14.0.tar.gz", repos = NULL, lib='/usr/local/lib/R/site-library', type = "source")
}