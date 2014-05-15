##corr from perl
args <- commandArgs(trailingOnly = TRUE)
myid<-args[1]
myidtype<-args[2]
mysubtype<-args[3]
myenrich<-args[4]
mydir<-args[5]
source("../resources/scripts/BRCADB_2013_corrfxn_portal.R")
docorr(id=myid, idtype=myidtype, subtype=mysubtype, enrich=myenrich, results_dir=mydir)