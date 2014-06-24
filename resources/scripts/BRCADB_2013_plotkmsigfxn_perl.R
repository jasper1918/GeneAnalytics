#signature survival from perl
args <- commandArgs(trailingOnly = TRUE)
mysigfile<-args[1]
mysigname<-args[2]
myidtype<-args[3]
mymonths<-args[4]
mysubtype<-args[5]
mysplit<-args[6]
mytam<-args[7]
mychemo<-args[8]
mysurv<-args[9]
mydir<-args[10]

source("../resources/scripts/BRCADB_2013_plotkmsigfxn_portal.R")
plotkmsig(mysigfile=mysigfile, idtype=myidtype, signame=mysigname, months=mymonths, subtype=mysubtype, chemo=mychemo, tam=mytam, split=mysplit, surv_type=mysurv, results_dir=mydir)