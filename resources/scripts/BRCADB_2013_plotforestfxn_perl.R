##forest plots from perl
args <- commandArgs(trailingOnly = TRUE)
myforestfile<-args[1]
myforestname<-args[2]
myidtype<-args[3]
mysubtype<-args[4]
mymetric<-args[5]
mytam<-args[6]
mychemo<-args[7]
mysurv<-args[8]
mydir<-args[9]

source("../resources/scripts/BRCADB_2013_plotforestfxn2_portal.R")
plotforest(mysigfile=myforestfile, signame=myforestname, idtype=myidtype, metric=mymetric,chemo=mychemo, tam=mytam, subtype=mysubtype,surv_type=mysurv, results_dir=mydir)