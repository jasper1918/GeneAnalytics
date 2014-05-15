#heatmapfrom perl
args <- commandArgs(trailingOnly = TRUE)
myheatmapfile<-args[1]
myheatmapname<-args[2]
myidtype<-args[3]
mysubtype<-args[4]
mydir<-args[5]

#cat("Found the R-perl bridge")
source("../resources/scripts/BRCADB_2013_plotheatmapfxn_portal.R")
plotheatmap(mysigfile=myheatmapfile, signame=myheatmapname, idtype=myidtype, subtype=mysubtype, results_dir=mydir)