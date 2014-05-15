plotkmsig<-function(mysigfiledn,idtype=c("symbol", "probe"),signame,months,subtype=c("PAM50", "MOD1", "MOD2", "ALL"), chemo=c("any","yes", "no"), tam=c("any","yes","no","compare"),split=c("median","tertile", "quartile"),surv_type=c("rfs","dmfs","combo"),results_dir){

  #validation
  idtype <- match.arg(idtype)
  chemo <- match.arg(chemo)
  tam <- match.arg(tam)
  subtype <- match.arg(subtype)
  chemo <- match.arg(chemo)
  tam <- match.arg(tam)
  split<-match.arg(split)
  surv_type<-match.arg(surv_type)
  months<-as.numeric(months)
  
  #get file
  mysigfileloc<-paste("../htdocs/uploads","/", mysigfile, sep="")
  mysigfiledn<-read.table(mysigfileloc, sep="\t", header=F)
  
  id<-as.character(mysigfiledn[,1])
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
      cat ( " These symbols could not be mapped:", paste(shQuote(names(sigsmissing)), collapse=", "))
    }
    sigann<-subset(sigprobes_map,!is.na(sigprobes_map))
    myidann<-data.frame(sigann, names(sigann))
  }
  
  ###load data using from sql
  clin<-read.table("../resources/data/BRCADB_2013_Clinical.txt", sep="\t", header=T) 
  
  library(ggplot2)
  library(survival)
  library(RSQLite)
  library(rmeta)
  
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname="../resources/external/BRCADB_final_2013.sqlite")
  dbListTables(con)
  probesql<-paste("'",as.character(myidann[,1]),"'",collapse=", ",sep="")
  myquery<-paste("SELECT * FROM BRCADB WHERE row_names IN (",probesql,")" ,sep="")
  gene<-dbGetQuery(con,myquery)
  gene<-data.frame(gene, row.names=1)
  gene<-as.data.frame(gene)
  
  ###genefu to get sigscores
  library("genefu")
  
  ###get sigfile
  if (idtype=="symbol"){
    mysigfiledn[,1]<-toupper(mysigfiledn[,1])
    newsigfile<-merge(mysigfiledn, myidann, by.x=1, by.y=2)
    newsigfile<-unique(newsigfile)
    rownames(newsigfile)<-newsigfile[,3]
    colnames(newsigfile)<-c("symbol", "coefficient", "probe")
    newsigfile$EntrezGene.ID<-"ent"
  }
  ###if probes
  if( idtype=="probe"){
    newsigfile<-merge(mysigfiledn, myidann, by.x=1, by.y=1)
    newsigfile<-unique(newsigfile)
    rownames(newsigfile)<-newsigfile[,1]
    colnames(newsigfile)<-c("probe", "coefficient", "symbol")
    newsigfile$EntrezGene.ID<-"ent"
  }
  
  
  SigScores<-function (data, annot, do.mapping = FALSE, mapping, verbose = FALSE) {
    res <- sig.score(x = SigScores.gl, data = data, annot = annot, 
                     do.mapping = do.mapping, mapping = mapping, signed = TRUE, 
                     verbose = verbose)$score
    return(res) }
  
  SigScores.gl<-newsigfile
  myscores<-SigScores(data=t(gene), annot=dummy, do.mapping=FALSE, verbose=TRUE)
  
  myset<-merge((myscores), clin, by.x=0, by.y=2)
  
  ##subset treatments
  if(exists("chemo")){
    
    if(chemo=="yes"){
      survcases<-subset(clin,AnyChemo %in% c("YES"))
      myset<-myset[myset[,1] %in% survcases$GSM,]}
    if(chemo=="no"){
      survcases<-subset(clin,AnyChemo %in% c("NO"))
      myset<-myset[myset[,1] %in% survcases$GSM,]}
    if(chemo=="compare"){
      survcases<-subset(clin,AnyChemo %in% c("YES", "NO"))
      myset<-myset[myset[,1] %in% survcases$GSM,]}
  }
  
  if(exists("tam")){
    
    if(tam=="yes"){
      survcases<-subset(clin,Tamoxifen %in% c("YES"))
      myset<-myset[myset[,1] %in% survcases$GSM,]}
    if(tam=="no"){
      survcases<-subset(clin,Tamoxifen %in% c("NO"))
      myset<-myset[myset[,1] %in% survcases$GSM,]}
    if(tam=="compare"){
      survcases<-subset(clin,Tamoxifen %in% c("YES", "NO"))
      myset<-myset[myset[,1] %in% survcases$GSM,]}
  }

  colnames(myset)[2]<-signame
  
  ###get ready to plot-------
  basedir<-results_dir
  mydir<-paste(basedir,"/", signame, "-","sig_survival",sep="")
  dir.create(mydir)
  mydir2<-paste(mydir, "/", sep="")
  setwd(mydir2)
  
  mysetkm<-NULL
  mycoxkm<-NULL
  mycoxphkm<-NULL
  myfitkm<-NULL
  
  if (subtype=='PAM50') {setlistkm<-levels(clin[,3])
                         myrow<-4 }#pam50
  if (subtype=='MOD1') {setlistkm<-levels(clin[,4])
                        myrow<-5 }# mod1
  if (subtype=='MOD2') {setlistkm<-levels(clin[,5])
                        myrow<-6 }#mod2
  
  if (split=='median') {
    mysplit=2
    mykeep<-c('1','2')}
  if (split=='tertile') {
    mysplit=3
    mykeep <- c('1','3')}
  if (split=='quartile') {
    mysplit=4
    mykeep<-c('1','4')}
  
  #get index of survival type
  if (surv_type=='rfs') {
    mysurv_event=16
    mysurv_time=17
    mysurv_label="RFS"
  }
  if (surv_type=='dmfs') {
    mysurv_event=18
    mysurv_time=19
    mysurv_label="DMFS"
  }
  if (surv_type=='combo') {
    mysurv_event=24
    mysurv_time=25
    mysurv_label="RFS/DMFS"
  }
  
  myset<-subset(myset,myset[,mysurv_event] %in% c("0","1"))
  myset<-subset(myset,myset[,mysurv_time] >1)
  
  subtypename<-gsub("/", "_", setlistkm)
  subtypename<-gsub("\\+", "pos", subtypename)
  subtypename<-gsub("\\-", "neg", subtypename)
  subtypename<-gsub(" ", "-", subtypename)

  write.table(myset, paste("all-myset.txt", sep=""), sep="\t", col.names=NA)
  
  ###Plot overall-----------
  medinf<-NULL
  mytext<-NULL
  myinf<-NULL
  mysetk<-myset
  mysetkm<-NULL
  dim(mysetk)
  mysetk <- within(mysetk, datasplit <- as.integer(cut(mysetk[,2], quantile(mysetk[,2], probs=0:mysplit/mysplit), include.lowest=TRUE)))
  mysetkm <- subset(mysetk, datasplit %in% mykeep)
  write.table(mysetkm, paste( paste("Overall", "-myset.txt",sep=""), sep=""), sep="\t", col.names=NA)
  
  mycoxkm<-Surv(mysetkm[,mysurv_time], mysetkm[,mysurv_event])~mysetkm$datasplit
  mycoxphkm<-coxph(Surv(mysetkm[,mysurv_time], mysetkm[,mysurv_event])~mysetkm$datasplit)
  beta <- coef(mycoxphkm) 
  HR <- signif(exp(beta), digits=3) 
  se <- sqrt(diag(vcov(mycoxphkm))) 
  HRse <- HR * se 
  myfitkm<- survfit(mycoxkm, data = mysetkm)
  pdf(file=paste(signame,"_", subtype,"_", chemo,"_", tam,"_",split,"_","Overall","_Survival.pdf", sep=""),width=10.5, height=8)
  par(fig=c(0,1,0,1),oma=c(1,1,1,1), mar=c(5, 5, 4, 2))
  plot(myfitkm, main=paste(signame, ":", subtype ,"-","Overall"),cex.main=1.5, cex.axis=1.5, cex.lab= 1.7 ,col=c("blue", "red"), lty=1, lwd=2.5, xlab= "Months", ylab= mysurv_label,xlim=c(0,months))
  legend("topright",c("Low", "High"), col=c("blue", "red"), lty=1, lwd=3, title="gene-set scores", ncol=2, cex=1.3, x.intersp=.5, y.intersp=.8)
  test<-survdiff(mycoxkm)
  p.value <- signif(1-pchisq(test$chi,df=1),digits=3)
  msurv<-with(mysetkm,Surv(mysetkm[,mysurv_time], mysetkm[,mysurv_event]))
  val<-msurv[!is.na(msurv)]
  nval<-dim(val)[1]
  text(months/4,.05, paste("pval=",p.value,","," HR=",HR,",", " N=",nval),cex=1.5)
  rug(mysetkm[,mysurv_time][mysetkm[,mysurv_event]==0])
  #medinf<-summary(myfitkm)$table
  #rownames(medinf)<-c(paste(symbol,"-Low", sep=""), paste(symbol,"-High", sep=""))
  #medinf<-as.matrix(medinf)
  #myinf<-medinf[,c(1,4,5,6,7)]
  #colnames(myinf)<-c("Samples","Events", "median", "0.95LCL", "0.95UCL")
  dev.off()
  
  ###forestplots--
  myGSEIDS<-as.character(unique(mysetk$GSE))
  
  rescale <- function(x, na.rm=FALSE, q=0.05) {
    ma <- quantile(x, probs=1-(q/2), na.rm=na.rm)
    mi <- quantile(x, probs=q/2, na.rm=na.rm)
    x <- (x - mi) / (ma - mi)
    return((x - 0.5) * 2)
  }
  
  myeset<-list()
  hratio<-list()
  myexprs<-NULL
  mysetpts<-NULL
  for ( j in 1: length(myGSEIDS)){
    myeset[[j]] <- subset(mysetk,GSE==myGSEIDS[j] )
    myexprs[[j]]<- myeset[[j]][,c(2,2)]
    rownames(myexprs[[j]])<-myeset[[j]][,1]
    colnames(myexprs[[j]])<-c("1","2")
    
    hratio[[j]]<- t(apply(X=rescale(t(myexprs[[j]]) , q=0.05, na.rm=TRUE), MARGIN=1, function(x, y, z) {
      tt <- concordance.index(x=x, surv.time=y, surv.event=z, method="noether",na.rm=TRUE);
      return(c("cindex"=tt$c.index, "cindex.se"=tt$se, "lower"=tt$lower, "upper"=tt$upper)); },
                          y=myeset[[j]][,mysurv_time], z=myeset[[j]][,mysurv_event]))
    mysetpts[j]<-dim(myeset[[j]])[[1]]
  }
  
  ###get data into usable format
  library(abind)
  j<-abind(hratio,along=1)
  mysetindex<-rep(myGSEIDS, each=2)
  mygeneindex<-rep(1:2,times=length(myGSEIDS))
  k<-data.frame(j,row.names=paste(rownames(j),mysetindex, sep="_"))
  k$set<-mysetindex
  k$genes<-rownames(j)
  myhrdf<-subset(k, genes=="1")
  myhrdf$no<-mysetpts
  myhrdf<-na.omit(myhrdf)
  
  ###forestplot
  myspace <- " "
  mybigspace <- "    "
  labeltext <- cbind(c("Dataset",paste(myhrdf$set, ", N=", myhrdf$no, sep="")),c(rep(mybigspace,length(myhrdf$set)+1)))
  bs <- rep(0.5, nrow(labeltext))   
  r.mean <- c(NA,(myhrdf$cindex))
  r.lower <- c(NA,(myhrdf$lower))
  r.upper <- c(NA,(myhrdf$upper))
  
  pdf(file=paste(signame,"_", subtype,"_", chemo,"_", tam,"_",split,"_",surv_type, "_", "Overall","_cindex.pdf", sep=""),width=10.5, height=8)
  forestplot.surv(labeltext=labeltext, mean=r.mean, lower=r.lower, upper=r.upper, zero=.5, 
                  align=c("l"), graphwidth=unit(1.5, "inches"), x.ticks=seq(0,1,0.25), xlab=paste( "Concordance Index", myspace, sep=""),
                  col=meta.colors(box="royalblue", line="darkblue", zero="darkred"), box.size=bs ) #clip=c(0,1)
  dev.off()
  
  ###plot by subtype ---------
  mysetk<-NULL
  mysetkm<-NULL
  mycoxkm<-NULL
  mycoxphkm<-NULL
  myfitkm<-NULL
  
  for(i in 1:length(setlistkm)){
    #cat("Doing=:", setlistkm[i], "\n")
    medinf<-NULL
    mytext<-NULL
    myinf<-NULL
    mysetk[[i]]<-myset
    dim(mysetk[[i]])
    mysetk[[i]]<-subset(mysetk[[i]], mysetk[[i]][,myrow]==setlistkm[i])
    mysetk[[i]] <- within(mysetk[[i]], datasplit <- as.integer(cut(mysetk[[i]][,2], quantile(mysetk[[i]][,2], probs=0:mysplit/mysplit), include.lowest=TRUE)))
    mysetkm[[i]] <- subset(mysetk[[i]], datasplit %in% mykeep)
    
    ###need a failsafe if not enough records
    table(mysetkm[[i]]$MYSURV_Event,mysetkm[[i]]$datasplit)
    write.table(mysetkm[[i]], paste( paste(subtypename[i], "-myset.txt",sep=""), sep=""), sep="\t", col.names=NA)
    
    if(tam == "compare"){
      mycoxkm[[i]]<-Surv(mysetkm[[i]][,mysurv_time], mysetkm[[i]][,mysurv_event])~mysetkm[[i]]$datasplit+mysetkm[[i]]$Tamoxifen 
      myfitkm[[i]]<- survfit(mycoxkm[[i]], data = mysetkm[[i]])
      pdf(file=paste(signame,"_", subtype,"_", chemo,"_", tam,"_",split,"_",surv_type, "_",subtypename[i],"_Survival.pdf", sep=""),width=10.5, height=8)
      par(fig=c(0,1,0,1),oma=c(1,1,1,1), mar=c(5, 5, 4, 2))
      plot(myfitkm[[i]], main=paste(signame, ":", subtype ,"-",setlistkm[i]),cex.main=1.5, cex.axis=1.5, cex.lab= 1.7 ,col=c("blue", "forestgreen", "red", "purple"), lty=1, lwd=2.5, xlab= "Months", ylab= mysurv_label,xlim=c(0,months))
      legend("topright",c("Low", "Low-Tam", "High", "High-Tam"), col=c("blue", "forestgreen", "red", "purple"), lty=1, lwd=3, title="gene-set scores", ncol=2, cex=1.3, x.intersp=.5, y.intersp=.8)
      test<-survdiff(mycoxkm[[i]])
      p.value <- signif(1-pchisq(test$chi,df=1),digits=3)
      msurv<-with(mysetkm[[i]],Surv(mysetkm[[i]][,mysurv_time], mysetkm[[i]][,mysurv_event]))
      val<-msurv[!is.na(msurv)]
      nval<-dim(val)[1]
      text(months/3,.05, paste("pval=",p.value,",", " N=",nval),cex=1.5)
      rug(mysetkm[[i]][,mysurv_time][mysetkm[[i]][,mysurv_event]==0])
      #medinf<-summary(myfitkm[[i]])$table
      #rownames(medinf)<-c(paste(symbol,"-Low", sep=""), paste(symbol,"-High", sep=""))
      #medinf<-as.matrix(medinf)
      #myinf<-medinf[,c(1,4,5,6,7)]
      #colnames(myinf)<-c("Samples","Events", "median", "0.95LCL", "0.95UCL")
      dev.off()
    }
    
    if(tam != "compare"){
      mycoxkm[[i]]<-Surv(mysetkm[[i]][,mysurv_time], mysetkm[[i]][,mysurv_event])~mysetkm[[i]]$datasplit
      mycoxphkm[[i]]<-coxph(Surv(mysetkm[[i]][,mysurv_time], mysetkm[[i]][,mysurv_event])~mysetkm[[i]]$datasplit)
      beta <- coef(mycoxphkm[[i]]) 
      HR <- signif(exp(beta), digits=3) 
      se <- sqrt(diag(vcov(mycoxphkm[[i]]))) 
      HRse <- HR * se 
      myfitkm[[i]]<- survfit(mycoxkm[[i]], data = mysetkm[[i]])
      pdf(file=paste(signame,"_", subtype,"_", chemo,"_", tam,"_",split,"_",surv_type, "_",subtypename[i],"_Survival.pdf", sep=""),width=10.5, height=8)
      par(fig=c(0,1,0,1),oma=c(1,1,1,1), mar=c(5, 5, 4, 2))
      plot(myfitkm[[i]], main=paste(signame, ":", subtype ,"-",setlistkm[i]),cex.main=1.5, cex.axis=1.5, cex.lab= 1.7 ,col=c("blue", "red"), lty=1, lwd=2.5, xlab= "Months", ylab= mysurv_label,xlim=c(0,months))
      legend("topright",c("Low", "High"), col=c("blue", "red"), lty=1, lwd=3, title="gene-set scores", ncol=2, cex=1.3, x.intersp=.5, y.intersp=.8)
      test<-survdiff(mycoxkm[[i]])
      p.value <- signif(1-pchisq(test$chi,df=1),digits=3)
      msurv<-with(mysetkm[[i]],Surv(mysetkm[[i]][,mysurv_time], mysetkm[[i]][,mysurv_event]))
      val<-msurv[!is.na(msurv)]
      nval<-dim(val)[1]
      text(months/4,.05, paste("pval=",p.value,","," HR=",HR,",", " N=",nval),cex=1.5)
      rug(mysetkm[[i]][,mysurv_time][mysetkm[[i]][,mysurv_event]==0])
      #medinf<-summary(myfitkm[[i]])$table
      #rownames(medinf)<-c(paste(symbol,"-Low", sep=""), paste(symbol,"-High", sep=""))
      #medinf<-as.matrix(medinf)
      #myinf<-medinf[,c(1,4,5,6,7)]
      #colnames(myinf)<-c("Samples","Events", "median", "0.95LCL", "0.95UCL")
      dev.off()
    }
    
    ###forestplots---
    myGSEdf<-data.frame(table(mysetkm[[i]]$MYSURV_Event,mysetkm[[i]]$GSE))
    myGSEdf<-subset(myGSEdf, myGSEdf[,1]==1 & myGSEdf[,3] >= 1)
    myGSEIDS<-as.character(unique(myGSEdf[,2]))
    
    rescale <- function(x, na.rm=FALSE, q=0.05) {
      ma <- quantile(x, probs=1-(q/2), na.rm=na.rm)
      mi <- quantile(x, probs=q/2, na.rm=na.rm)
      x <- (x - mi) / (ma - mi)
      return((x - 0.5) * 2)
    }
    
    myeset<-list()
    hratio<-list()
    myexprs<-NULL
    mysetpts<-NULL
    for ( j in 1: length(myGSEIDS)){
      myeset[[j]] <- subset(mysetk[[i]],GSE==myGSEIDS[j] )
      myexprs[[j]]<- myeset[[j]][,c(2,2)]
      rownames(myexprs[[j]])<-myeset[[j]][,1]
      colnames(myexprs[[j]])<-c("1","2")
      
      hratio[[j]]<- t(apply(X=rescale(t(myexprs[[j]]) , q=0.05, na.rm=TRUE), MARGIN=1, function(x, y, z) {
        tt <- concordance.index(x=x, surv.time=y, surv.event=z, method="noether",na.rm=TRUE);
        return(c("cindex"=tt$c.index, "cindex.se"=tt$se, "lower"=tt$lower, "upper"=tt$upper)); },
                            y=myeset[[j]][,mysurv_time], z=myeset[[j]][,mysurv_event]))
      mysetpts[j]<-dim(myeset[[j]])[[1]]
    }
    
    ###get data into usable format
    library(abind)
    j<-abind(hratio,along=1)
    mysetindex<-rep(myGSEIDS, each=2)
    mygeneindex<-rep(1:2,times=length(myGSEIDS))
    k<-data.frame(j,row.names=paste(rownames(j),mysetindex, sep="_"))
    k$set<-mysetindex
    k$genes<-rownames(j)
    myhrdf<-subset(k, genes=="1")
    myhrdf$no<-mysetpts
    myhrdf<-na.omit(myhrdf)
    
    ###forestplot
    if(dim(myhrdf)[[1]]>1){
      myspace <- " "
      mybigspace <- "    "
      labeltext <- cbind(c("Dataset",paste(myhrdf$set, ", N=", myhrdf$no, sep="")),c(rep(mybigspace,length(myhrdf$set)+1)))
      bs <- rep(0.5, nrow(labeltext))   
      r.mean <- c(NA,(myhrdf$cindex))
      r.lower <- c(NA,(myhrdf$lower))
      r.upper <- c(NA,(myhrdf$upper))
      
      pdf(file=paste(signame,"_", subtype,"_", chemo,"_", tam,"_",split,"_",surv_type,"_",subtypename[i],"_cindex.pdf", sep=""),width=10.5, height=8)
      forestplot.surv(labeltext=labeltext, mean=r.mean, lower=r.lower, upper=r.upper, zero=.5, 
                      align=c("l"), graphwidth=unit(1.5, "inches"), x.ticks=seq(0,1,0.25), xlab=paste( "Concordance Index", myspace, sep=""),
                      col=meta.colors(box="royalblue", line="darkblue", zero="darkred"), box.size=bs ) #clip=c(0,1)
      dev.off()
    }
  }
  ###plotexpresssion-------------
  basekm<- ggplot(myset, aes(myset[,myrow], myset[,2]), environment=environment())
  basekmbox<-basekm +geom_boxplot(aes(fill = factor(myset[,myrow])), alpha=1)+scale_colour_brewer(palette="Set1")+scale_fill_hue(c=150, l=45)
  myplotkmbox<- basekmbox+ labs(fill= "",title="", x= "", y= "Log2 Expression")
  fname<-paste(signame,"_", subtype,"_", chemo,"_", tam,"_",split,"_Expression.pdf",sep="")
  #print(myplotkmbox)
  ggsave(plot=myplotkmbox,filename=fname, dpi=320, width=12, height=10)

}