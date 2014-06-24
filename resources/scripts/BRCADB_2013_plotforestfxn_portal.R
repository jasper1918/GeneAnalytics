plotforest<-function(mysigfile, signame, idtype=c("symbol", "probe"), metric=c("hr", "ci"),chemo=c("any","yes", "no"), tam=c("any","yes","no","compare"), subtype=c("basal", "luma", "lumb", "her2", "normal"),surv_type=c("rfs","dmfs","combo"), results_dir){
  
  #validation
  idtype <- match.arg(idtype)
  metric<-match.arg(metric)
  chemo <- match.arg(chemo)
  tam <- match.arg(tam)
  subtype <- match.arg(subtype)
  surv_type<-match.arg(surv_type)
  #get file
  
  mysigfileloc<-paste("../htdocs/uploads","/", mysigfile, sep="")
  mysigfiledn<-read.table(mysigfileloc, sep="\t", header=F)
  
  id<-unique(as.character(mysigfiledn[,1]))
  id<-gsub("\\s","", id)

  if (exists("mysigfiledn") & (idtype=="symbol")){
    sigsymbols<-id
  } else if (exists("mysigfiledn") & (idtype=="probe")){
    sigprobes<-id
  } else {stop("Please define id and idtype.")
  }
 
  #get identifiers, validate, inform if missing
  if (!exists("sigsymbols") & exists("sigprobes")){
    library(hgu133a.db)
    sigsymbols_map = unlist(mget(sigprobes, hgu133aSYMBOL,ifnotfound=NA))
    sigpmissing<-subset(sigsymbols_map,is.na(sigsymbols_map))
    
    if ( length(sigpmissing) > 0 ) {
      cat ( " These probes could not be mapped:", paste(shQuote(names(sigpmissing)), collapse=", "))
    }
    sigann<-subset(sigsymbols_map,!is.na(sigsymbols_map))
    myidann<-data.frame(names(sigann), sigann)
  }
  if (!exists("sigprobes") & exists("sigsymbols")){
    library(jetset)
    sigsymbols<- toupper(sigsymbols)
    sigprobes_map<-(jmap('hgu133a', symbol = sigsymbols))
    sigsmissing<-subset(sigprobes_map,is.na(sigprobes_map))
    
    if ( length(sigsmissing) > 0 ) {
      cat ( " These probes could not be mapped:", paste(shQuote(names(sigsmissing)), collapse=", "))
    }
    sigann<-subset(sigprobes_map,!is.na(sigprobes_map))
    myidann<-data.frame(sigann, names(sigann))
  }
 
  colnames(myidann)<-c("Probe", "Symbol")
  rownames(myidann)<-myidann$Probe
 
  ###load data using sql
  library(survcomp)
  library(Biobase)
  library(rmeta)
  library(RSQLite)
  library(genefu)

  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname="../resources/external/BRCADB_final_2013.sqlite")
  dbListTables(con)
 
  probesql<-paste("'",as.character(myidann[,1]),"'",collapse=", ",sep="")
  myquery<-paste("SELECT * FROM BRCADB WHERE row_names IN (",probesql,")" ,sep="")
  gene<-dbGetQuery(con,myquery)
  gene<-data.frame(gene, row.names=1)
  gene<-as.data.frame(gene)
  
  ##subset treatments from clinical data
  clin<-read.table("../resources/data/BRCADB_2013_Clinical.txt", sep="\t", header=T) 
  
  if(exists("chemo")){
    
    if(chemo=="yes"){
      clin<-subset(clin,AnyChemo %in% c("YES"))
      }
    if(chemo=="no"){
      clin<-subset(clin,AnyChemo %in% c("NO"))
      }
  }
  
  if(exists("tam")){
    
    if(tam=="yes"){
      clin<-subset(clin,Tamoxifen %in% c("YES"))
      }
    if(tam=="no"){
      clin<-subset(clin,Tamoxifen %in% c("NO"))
      }
  }
 
  #get index of survival type
  if (surv_type=='rfs') {
    mysurv_event=15
    mysurv_time=16
    mysurv_label="RFS"
  }
  if (surv_type=='dmfs') {
    mysurv_event=17
    mysurv_time=18
    mysurv_label="DMFS"
  }
  if (surv_type=='combo') {
    mysurv_event=23
    mysurv_time=24
    mysurv_label="RFS/DMFS"
  }
  
  clin<-subset(clin, clin[,mysurv_event] %in% c("0","1"))
  clin<-subset(clin, clin[,mysurv_time]>1)
  clin<-subset(clin, tolower(clin$PAM50)==subtype)
  #subset subtype here
  
  myset<-merge( clin, t(gene), by.x=2, by.y=0)
  rownames(myset)<-myset[,1]
  
  ###get ready to plot-------
  basedir<-results_dir
  mydir<-paste(basedir,"/", signame, "-","forestplot",sep="")
  dir.create(mydir)
  mydir2<-paste(mydir, "/", sep="")
  setwd(mydir2)

  write.table(clin, paste(signame,"_",metric,"_", subtype,"_data.txt", sep=""), sep="\t", col.names=NA)
  ##calculate
  rescale <- function(x, na.rm=FALSE, q=0.05) {
    ma <- quantile(x, probs=1-(q/2), na.rm=na.rm)
    mi <- quantile(x, probs=q/2, na.rm=na.rm)
    x <- (x - mi) / (ma - mi)
    return((x - 0.5) * 2)
  }
  
###calc metrics and plot
  if (metric=="hr"){
  hratio<- t(apply(X=rescale(t(myset[,30:ncol(myset)]) , q=0.05, na.rm=TRUE), MARGIN=1, function(x, y, z) {
    tt <- hazard.ratio(x=x, surv.time=y, surv.event=z, na.rm=TRUE);
    return(c("hratio"=tt$hazard.ratio, "hratio.se"=tt$se, "lower"=tt$lower, "upper"=tt$upper)); },
                   y=myset[,mysurv_time], z=myset[,mysurv_event]))
  
  mydata<-merge(myidann, hratio, by=0)
  mydata<-mydata[,-1]
  rownames(mydata)<-mydata[,1]
  mydata<-mydata[,-1]
  
  mymean<-2
  mylower<-4
  myupper<-5

  mydata_ordered<-mydata[ order(-mydata[,mymean]), ]
  ordered_names<-as.character(mydata_ordered$Symbol)
  
  myspace <- " "
  mybigspace <- "    "
  labeltext <- cbind(c("Gene Symbol",ordered_names),c(rep(mybigspace,length(ordered_names)+1))) ###need one more bigspace than genes
  bs <- rep(.75, nrow(labeltext))###this changes the square of the box data point
  r.mean <- c(NA,log2(mydata_ordered[,mymean]))
  r.lower <- c(NA,log2(mydata_ordered[,mylower])) 
  r.upper <- c(NA,log2(mydata_ordered[,myupper]))
  
  mylow=floor(min(log2(mydata_ordered[,mymean+2]))*2)/2; myhigh=ceiling(max(log2(mydata_ordered[,mymean+3]))*2)/2; 
  mystep=1; myzero=0; mywidth=5; mytitle="Log2 Hazard Ratio"
  
  }
  
  if (metric=="ci"){
  cindex<- t(apply(X=rescale(t(myset[,30:ncol(myset)]) , q=0.05, na.rm=TRUE), MARGIN=1, function(x, y, z) {
    tt <- concordance.index(x=x, surv.time=y, surv.event=z, na.rm=TRUE);
    return(c("cindex"=tt$c.index, "cindex.se"=tt$se, "lower"=tt$lower, "upper"=tt$upper)); },
                   y=myset[,mysurv_time], z=myset[,mysurv_event]))
  
  mydata<-merge(myidann, cindex, by=0)
  mydata<-mydata[,-1]
  rownames(mydata)<-mydata[,1]
  mydata<-mydata[,-1]
  
  mymean<-2
  mylower<-4
  myupper<-5
  
  mydata_ordered<-mydata[ order(-mydata[,mymean]), ]
  ordered_names<-as.character(mydata_ordered$Symbol)
 
  myspace <- " "
  mybigspace <- "    "
  labeltext <- cbind(c("Gene Symbol",ordered_names),c(rep(mybigspace,length(ordered_names)+1))) ###need one more bigspace than genes
  bs <- rep(.75, nrow(labeltext))###this changes the square of the box data point
  r.mean <- c(NA,(mydata_ordered[,mymean]))
  r.lower <- c(NA,(mydata_ordered[,mylower])) 
  r.upper <- c(NA,(mydata_ordered[,myupper]))
  
  mylow=floor(min((mydata_ordered[,mymean+2]))*20)/20; myhigh=ceiling(max((mydata_ordered[,mymean+3]))*20)/20;
  mystep=.05; myzero=.5; mywidth=5; mytitle="Concordance Index"
  }
  pdf(file=paste(signame,"_",metric,"_", subtype,"_ForestPlot.pdf", sep=""),width=8, height=10.5)
  forestplot.surv(labeltext=labeltext, mean=r.mean, lower=r.lower, upper=r.upper, zero=myzero,
                  align=c("l"), graphwidth=unit(mywidth, "inches"), x.ticks=seq(mylow,myhigh,mystep), xlab=paste( mytitle, myspace, sep=""),
                  col=meta.colors(box="royalblue", line="darkblue", zero="darkred"), box.size=bs, clip=c(mylow,myhigh),margin=c(5,5))
  graphics.off() 
}