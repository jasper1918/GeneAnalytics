
plotexpr<-function(id,idtype=c("symbol", "probe"),subtype=c("PAM50", "MOD1", "MOD2", "ALL"),results_dir){
  ###validation and data prep----
  idtype <- match.arg(idtype)
  subtype <- match.arg(subtype)
  
  if (exists("id") & (idtype=="symbol")){
    symbol<-id
  } else if (exists("id") & (idtype=="probe")){probe<-id
  } else {stop("Please define id and idtype.")
  }
  
  #get identifiers
  if (!exists("symbol") & exists("probe")){
    library(hgu133a.db)
    symbol = unlist(mget(probe, hgu133aSYMBOL))
    symbol<-symbol[[1]]}
  if (!exists("probe") & exists("symbol")){
    library(jetset)
    symbol
    symbol<- toupper(symbol)
    probe<-jmap('hgu133a', symbol = symbol)[[1]]}

  ###load data using from sql
  clin<-read.table("../resources/data/BRCADB_2013_Clinical.txt", sep="\t", header=T) 
  rownames(clin)<-clin[,2]
  
  library(ggplot2)
  library(RSQLite)

  
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, dbname="../resources/external/BRCADB_final_2013.sqlite")
  dbListTables(con)
  myquery<-paste("SELECT * FROM BRCADB WHERE row_names = '",probe,"'" ,sep="")
  gene<-dbGetQuery(con,myquery)
  gene<-gene[-1]
  gene<-as.data.frame(t(gene))
  
  myset<-merge(gene,clin, by=0)
  colnames(myset)[2]<-probe
  
  ###get ready to plot-------
  basedir<-results_dir
  name<-paste(symbol, "-", probe, sep="")
  mydir<-paste(basedir,"/", id, "-","Expression",sep="")
  dir.create(mydir)
  mydir2<-paste(mydir, "/", sep="")
  setwd(mydir2)

  
  if (subtype=='PAM50') {setlistkm<-levels(clin[,3])
                         myrow<-5 }#pam50
  if (subtype=='MOD1') {setlistkm<-levels(clin[,4])
                        myrow<-6 }# mod1
  if (subtype=='MOD2') {setlistkm<-levels(clin[,5])
                        myrow<-7 }#mod2
  
  
  subtypename<-gsub("/", "_", setlistkm)
  subtypename<-gsub("\\+", "pos", subtypename)
  subtypename<-gsub("\\-", "neg", subtypename)
  subtypename<-gsub(" ", "-", subtypename)

  ###plotexpression-------------
  fname<-paste(symbol,"_",subtype, "_Expression.pdf",sep="")
  basekm<- ggplot(myset, aes(myset[,myrow], myset[,2]), environment=environment())
  basekmbox<-basekm +geom_boxplot(aes(fill = factor(myset[,myrow])), alpha=1)+scale_colour_brewer(palette="Set1")+scale_fill_hue(c=150, l=45)
  myplotkmbox<- basekmbox+ labs(fill= "",title="", x= "", y= "Log2 Expression")
  ggsave(plot=myplotkmbox,filename=fname, dpi=320, width=12, height=10)
}
