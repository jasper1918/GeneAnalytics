##expression from perl
args <- commandArgs(trailingOnly = TRUE)
myid<-args[1]
myidtype<-args[2]
mysubtype<-args[3]
mydir<-args[4]
#cat ("found the R-perl file")
source("../resources/scripts/BRCADB_2013_plotexprsfxn_portal.R")
plotexpr(id=myid, idtype=myidtype, subtype=mysubtype, results_dir=mydir)
