#packages to install for all scripts to work
source("http://bioconductor.org/biocLite.R")
biocLite(c("hgu133a.db","ggplot2","survival","RSQLite","survcomp",
         "rmeta","abind","genefu","WGCNA","corrplot","Biobase", "gage"))
#install jetset
from source

install.packages("dynamicTreeCut")
install.packages("WGCNA")

#fix crappy "important message" from WGCNA using modified WGCNA
http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/index.html
  download source and delete text in zzz.R in R
  package as tar -zcvf WGCNA_m.tar.gz WGCNA-1
install.packages("WGCNA_m.tar.gz",repos = NULL, type="source")
#remove WGCNA, not much of a speed gain afterall.
#test lines



