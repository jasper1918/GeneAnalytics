plotheatmap<-function(mysigfile, signame, idtype=c("symbol", "probe"), subtype=c("PAM50", "MOD1", "MOD2"), results_dir){
  #Function to correlate gene expression. 
  #Jeff S Jasper, jasper1918@gmail.com

  #validate args
  idtype <- match.arg(idtype)
  subtype <- match.arg(subtype)

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
    require(hgu133a.db)
    sigsymbols_map = unlist(mget(sigprobes, hgu133aSYMBOL,ifnotfound=NA))
    sigpmissing<-subset(sigsymbols_map,is.na(sigsymbols_map))
    
    if ( length(sigpmissing) > 0 ) {
      cat ( " These probes could not be mapped:", paste(shQuote(names(sigpmissing)), collapse=", "))
    }
    sigann<-subset(sigsymbols_map,!is.na(sigsymbols_map))
    myidann<-data.frame(names(sigann), sigann)
  }
  if (!exists("sigprobes") & exists("sigsymbols")){
    require(jetset)
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
  myidann<-myidann[order(myidann$Probe),]
  
  #load data using sql
  require(RSQLite)
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname="../resources/external/BRCADB_final_2013.sqlite")
  dbListTables(con)
  probesql<-paste("'",myidann$Probe,"'",collapse=", ",sep="")
  myquery<-paste("SELECT * FROM BRCADB WHERE row_names IN (",probesql,") ORDER BY row_names" ,sep="")
  gene<-dbGetQuery(con,myquery)
  genenames<-gene[1,]
  gene<-gene[-1]
  gene<-as.data.frame(t(gene))
  colnames(gene)<-myidann$Probe
  
  clin<-read.table("../resources/data/BRCADB_2013_Clinical.txt", sep="\t", header=T) 
  rownames(clin)<-clin[,2]
  if (subtype=='PAM50') {setlistkm<-levels(clin[,3])
                         myrow<-3 }#pam50
  if (subtype=='MOD1') {setlistkm<-levels(clin[,4])
                        myrow<-4 }# mod1
  if (subtype=='MOD2') {setlistkm<-levels(clin[,5])
                        myrow<-5 }#mod2
  clinset<-clin[,myrow]
  clindf<-data.frame(rownames(clin),clinset)
  colnames(clindf)<-c("GSM","Subtype")
  myset<-merge(clindf, gene, by.x=1, by.y=0)
  
  #get ready to plot-------
  basedir<-results_dir
  mydir<-paste(basedir,"/", signame, "-","heatmap",sep="")
  dir.create(mydir)
  mydir2<-paste(mydir, "/", sep="")
  setwd(mydir2)
  
  #reshape data for heatmap
  require(ggplot2)
  require(reshape)
  mdata <- melt(myset, id=c("GSM","Subtype"),variable_name="probe")
  cdata <- cast(mdata, probe~Subtype, mean)
  cadata<-merge(myidann, cdata, by.x=1, by.y=1)
  mynames<-make.names(cadata[,2], unique=T)
  rownames(cadata)<-mynames
  cadata<-as.matrix(cadata[,-c(1:2)])
  gdata<- melt(cdata, id=c("probe"))
  adata<-merge(myidann, gdata, by.x=1, by.y=1)
  
  #ggplot heatmap
  col1<- colorRampPalette(c("ivory","yellow", "red", "black"))
  base <- (ggplot(adata, aes(Subtype, Symbol)) + geom_tile(aes(fill = value),color = "black") 
           + scale_fill_gradientn(colours=col1(200)))
  myheatmap<- base + theme_bw() + theme(axis.text  = element_text(size = rel(1.5)),axis.text.y = element_text(size=rel(.8)))+labs(title = "",x = "", y="") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  fname<-paste(signame,"_",subtype, "_Heatmap_ggplot.pdf",sep="")
  ggsave(plot=myheatmap,filename=fname, dpi=320, width=8, height=12)
  
  #alternate heatmap
  require(gplots)
  require(RColorBrewer)
  
  mydistman= function(x) dist(x,method = 'manhattan')
  col1<- colorRampPalette(c("ivory","yellow",  "red", "black"))
  #my.breaks <- c(seq(4, 6, length.out=51),seq(6, 7, length.out=50),seq(7, 8, length.out=50),seq(8,9, length.out=50),seq(12,14, length.out=50))
  pdf(file=paste(signame,"_", subtype,"_Heatmap_clustered.pdf", sep=""),width=10, height=12)
  rmheat<-heatmap.2(cadata, trace="none", dendrogram="both",col=col1(250),Rowv=TRUE, Colv=TRUE, main="", symbreaks=F, scale= "none",margins=c(15,10), cexRow=1, cexCol=1.5, labRow=row.names(cadata),distfun=mydistman,density.info="none", keysize=1)
  graphics.off() 
}
