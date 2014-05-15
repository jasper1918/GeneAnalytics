##survival from perl
args <- commandArgs(trailingOnly = TRUE)
myid<-args[1]
myidtype<-args[2]
mymonths<-args[3]
mysubtype<-args[4]
mysplit<-args[5]
mytam<-args[6]
mychemo<-args[7]
mysurv<-args[8]
mydir<-args[9]

source("../resources/scripts/BRCADB_2013_plotkmfxn_portal.R")
plotkm(id=myid, idtype=myidtype, months=mymonths, subtype=mysubtype, chemo=mychemo, tam=mytam, split=mysplit, surv_type=mysurv, results_dir=mydir)