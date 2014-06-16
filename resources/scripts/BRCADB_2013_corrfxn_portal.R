
docorr<-function(id,idtype=c("symbol", "probe"),subtype=c("PAM50", "MOD1", "MOD2"),enrich=c("yes","no"),results_dir) {
	#Function to correlate gene expression. 
	#Jeff S Jasper, jasper1918@gmail.com

  load("../resources/external/BRCADB_final_2013_Data.RData")
  clin<-read.table("../resources/data/BRCADB_2013_Clinical.txt", sep="\t", header=T)

  #validate args
  idtype <- match.arg(idtype)
  subtype <- match.arg(subtype)
  
  if (exists("id") & (idtype=="symbol")){
    symbol<-id
  } else if (exists("id") & (idtype=="probe")){probe<-id
  } else {stop("Please define id and idtype.")
  }
  
  #get identifiers
  if (!exists("symbol") & exists("probe")){
    require(hgu133a.db)
    symbol = unlist(mget(probe, hgu133aSYMBOL))
    symbol<-symbol[[1]]}
  if (!exists("probe") & exists("symbol")){
    require(jetset)
    symbol
    symbol<- toupper(symbol)
    probe<-jmap('hgu133a', symbol = symbol)[[1]]
  }
  
  brcadbdata<-as.data.frame(t(exprs(myesetann)))
  basedir<-results_dir
  name<-paste(symbol, "-", probe, sep="")
  mydir<-paste(basedir,"/", id, "-","corr",sep="")
  dir.create(mydir)
  mydir2<-paste(mydir, "/", sep="")
  setwd(mydir2)
  
  if (subtype=='PAM50') {setlist<-levels(clin[,3])
                         myrow<-3 }#pam50
  if (subtype=='MOD1') {setlist<-levels(clin[,4])
                        myrow<-4 }# mod1
  if (subtype=='MOD2') {setlist<-levels(clin[,5])
                        myrow<-5 }#mod2
  
  #overall corr
  cat("Doing:", "Overall","\n")
  overallcorr<-cor(brcadbdata[,1:ncol(brcadbdata)],brcadbdata[,probe],use='p')

  #subtype corr
  spdata<-NULL
  for(i in 1:length(setlist) ){
    cat(",", setlist[i],"\n")
    mycases<-NULL
    esetgene<-NULL
    exprsgene<-NULL
    mycases <- clin[ which(clin[,myrow]==setlist[i]) , ]
    subeset<-myesetann[,as.character(mycases$GSM)]
    subexprs<-t(exprs(subeset))
    spdata[[i]]<-cor(subexprs[,1:ncol(subexprs)],subexprs[,probe],use='p')
  }
  
  #Merge all subtypes
  require(abind)
  subtypecorr<-abind(spdata)
  
  #merge all to one, make col names
  allcorr<-cbind(overallcorr, subtypecorr)
  probesub<-paste(setlist, "_", symbol, sep="")
  colnames(allcorr)<-c(paste("Overall_", symbol, sep=""), probesub)
  
  ##get annot
  require(hgu133a.db)
  Probe_ID <- featureNames(myesetann)
  Gene_Symbol<- unlist(mget(Probe_ID,hgu133aSYMBOL))
  #rs <- unlist(mget(Probe_ID,hgu133aREFSEQ)) #not one to one
  Entrez <- unlist(mget(Probe_ID,hgu133aENTREZID))
  Description<- unlist(mget(Probe_ID,hgu133aGENENAME))
  myannot<-cbind(Probe_ID, Entrez, Gene_Symbol, Description)
  
  #merge annot, write to file
  allcorr_annot<-merge(myannot, allcorr, by=0)
  rownames(allcorr_annot)<-allcorr_annot[,1]
  filename2<-paste(symbol,"_", subtype,"_correlation.txt", sep="")
  write.table(allcorr_annot, filename2, sep="\t", col.names=NA)

  ###get top of each end
  overall_ranked<-overallcorr[order(-overallcorr[,1]),] ##sorts overall
  spup<-head(overall_ranked,n=50)
  spdn<-tail(overall_ranked,n=50)
  sptot<-c(spup, spdn)
  sptot<-as.matrix(sptot)
  
  #plot overall each end
  cat(", Plotting Correlation","\n")
  require(corrplot)
  sptotdata<-t(exprs(myesetann[rownames(sptot),]))
  sptotdatacorr<-cor(sptotdata,method="spearman")
  myrowsym<- unlist(mget(rownames(sptotdatacorr),hgu133aSYMBOL))
  rownames(sptotdatacorr)<-make.names(myrowsym, unique=T)
  mycolsym<- unlist(mget(colnames(sptotdatacorr),hgu133aSYMBOL))
  colnames(sptotdatacorr)<-make.names(mycolsym, unique=T)
  col3 <- colorRampPalette(c("midnightblue", "white", "red"))
  pdf(file=paste(symbol,"-",subtype,"_OverallCorrplot.pdf", sep=""),width=10.5, height=8)
  corrplot(sptotdatacorr, is.corr = T, method = "square",order="hclust", hclust.method="ward",addrect=6, col=col3(20), tl.col="black",, tl.cex=.4,rect.lwd=3)
  graphics.off()
  
  #plot top 
  spupdata<-t(exprs(myesetann[names(spup),]))
  spupdatacorr<-cor(spupdata,method="spearman")
  myrowsym<- unlist(mget(rownames(spupdatacorr),hgu133aSYMBOL))
  rownames(spupdatacorr)<-make.names(myrowsym, unique=T)
  mycolsym<- unlist(mget(colnames(spupdatacorr),hgu133aSYMBOL))
  colnames(spupdatacorr)<-make.names(mycolsym, unique=T)
  col3 <- colorRampPalette(c("midnightblue", "white", "red"))
  pdf(file=paste(symbol,"-",subtype,"_TopUpCorrplot.pdf", sep=""),width=10.5, height=8)
  corrplot(spupdatacorr, is.corr = T, method = "square",order="hclust", hclust.method="ward",addrect=6, col=col3(30), tl.col="black",, tl.cex=.9,rect.lwd=3)
  graphics.off()
  
  #plot bottom
  spdndata<-t(exprs(myesetann[names(spdn),]))
  spdndatacorr<-cor(spdndata,method="spearman")
  myrowsym<- unlist(mget(rownames(spdndatacorr),hgu133aSYMBOL))
  rownames(spdndatacorr)<-make.names(myrowsym, unique=T)
  mycolsym<- unlist(mget(colnames(spdndatacorr),hgu133aSYMBOL))
  colnames(spdndatacorr)<-make.names(mycolsym, unique=T)
  col3 <- colorRampPalette(c("red", "white", "midnightblue"))
  pdf(file=paste(symbol,"-",subtype,"_TopDnCorrplot.pdf", sep=""),width=10.5, height=8)
  corrplot(spdndatacorr, is.corr = T, method = "square",order="hclust", hclust.method="ward",addrect=6, col=col3(30), tl.col="black",, tl.cex=.9,rect.lwd=3)
  graphics.off()
  
  #plot network graph
  cat(", Plotting Network","\n")
    subcor<-allcorr_annot[,c(4,6)]
    avgcor<-aggregate(subcor[,2], by=list(subcor[,1]), FUN=mean)
    rownames(avgcor)<-avgcor[,1]
    sort_avgcor <-avgcor[order(-avgcor[,2]), ]
    top_cor<-sort_avgcor[1:50, 1:2]
    top_cor$from<-symbol
    final_cor<-as.data.frame(top_cor[,c(3,1,2)])
    
    colnames(final_cor)<-c("From", "To", "strength")
    unique.genes<-data.frame(name=unique(final_cor[,2]))
    
    require(igraph)
    final_cor$strength=final_cor$strength * 10
    g <- graph.data.frame(final_cor, directed=TRUE, vertices=unique.genes)
    
    
    require(RColorBrewer)
    set.seed(1919)
    the.layout = layout.fruchterman.reingold(g, weights=E(g)$strength, coolexp=3, area=vcount(g)^2.3, repulserad=vcount(g)^3)[match(V(g)$name, V(g)$name),]
    #the.layout = layout.kamada.kawai(g)
    v.colors = brewer.pal(9,"YlOrRd") #YlGnBu or YlOrRd
    v.col = v.colors[cut(E(g)$strength,c(1,3,quantile(E(g)$strength,probs=seq(.3,1,length=7))),labels=F)][match(V(g)$name, V(g)$name)]
    
    l.size <-2 * E(g)$strength / max(E(g)$strength)+.2
    v.size <-25 * E(g)$strength / max(E(g)$strength)+10
    
    h<-simplify(g,remove.multiple=F,remove.loops=T)
    par(mar=c(0,0,0,0))
    
    pdf(file=paste(symbol,"-",subtype,"_NetworkPlot_Overall.pdf", sep=""),width=19, height=19)
    plot(h, layout=the.layout, vertex.size=v.size, vertex.color=v.col, vertex.frame.color=NA, edge.color="grey40",
         vertex.label=V(h)$name, vertex.label.font=2, vertex.label.color="black",
         edge.width=E(h)$strength, edge.arrow.size=.2, vertex.label.cex=l.size)
    graphics.off()
  
  #do enrichment c2 GSEA
    cat(", Doing Enrichment","\n")
    require(gage)
    load("../resources/data/GSEA_C2_list.R.RData")
  
    mydata<-allcorr_annot[,c(3,6)]
    mydata<-na.omit(mydata)
    cordata<-mydata[,2]
    names(cordata)<-mydata[,1]
    corr_up <- names(cordata)[cordata > 0.4]
    corr_dn<-names(cordata)[cordata < -0.4]
    
    c2_gage <- gage(cordata, gsets = c2.eg, ref = NULL, samp = NULL,rank.test=T, same.dir=T)
    c2_gage$greater[1:20,]
    c2.sigsets_up <- subset(c2_gage$greater, c2_gage$greater[, "q.val"] < .05 )
    c2.sigsets_up
    write.table(c2.sigsets_up, paste(symbol,"-",subtype,"_GSEA_C2_Up.txt", sep=""), sep="\t", col.names=NA)
    c2.sigsets_dn <- subset(c2_gage$less, c2_gage$less[, "q.val"] < .05 )
    c2.sigsets_dn
    write.table(c2.sigsets_dn, paste(symbol,"-",subtype,"_GSEA_C2_Dn.txt", sep=""), sep="\t", col.names=NA)
  
  #plot graphs for subtyes
  for(i in 1:length(setlist) ){
    cat(", Plotting Network","\n")
    subcor<-allcorr_annot[,c(4,(6+i))]
    avgcor<-aggregate(subcor[,2], by=list(subcor[,1]), FUN=mean)
    rownames(avgcor)<-avgcor[,1]
    sort_avgcor <-avgcor[order(-avgcor[,2]), ]
    top_cor<-sort_avgcor[1:50, 1:2]
    top_cor$from<-symbol
    final_cor<-as.data.frame(top_cor[,c(3,1,2)])
    
    colnames(final_cor)<-c("From", "To", "strength")
    unique.genes<-data.frame(name=unique(final_cor[,2]))
    
    require(igraph)
    final_cor$strength=final_cor$strength * 10
    g <- graph.data.frame(final_cor, directed=TRUE, vertices=unique.genes)
    
    require(RColorBrewer)
    set.seed(1919)
    the.layout = layout.fruchterman.reingold(g, weights=E(g)$strength, coolexp=3, area=vcount(g)^2.3, repulserad=vcount(g)^3)[match(V(g)$name, V(g)$name),]
    #the.layout = layout.kamada.kawai(g)
    v.colors = brewer.pal(9,"YlOrRd") #YlGnBu or YlOrRd
    v.col = v.colors[cut(E(g)$strength,c(1,3,quantile(E(g)$strength,probs=seq(.3,1,length=7))),labels=F)][match(V(g)$name, V(g)$name)]

    l.size <-2 * E(g)$strength / max(E(g)$strength)+.2
    v.size <-25 * E(g)$strength / max(E(g)$strength)+10
    
    h<-simplify(g,remove.multiple=F,remove.loops=T)
    par(mar=c(0,0,0,0))
    pdf(file=paste(symbol,"-",subtype,"_NetworkPlot_",setlist[i],".pdf", sep=""),width=19, height=19)
    plot(h, layout=the.layout, vertex.size=v.size, vertex.color=v.col, vertex.frame.color=NA, edge.color="grey40",
         vertex.label=V(h)$name, vertex.label.font=2, vertex.label.color="black",
         edge.width=E(h)$strength, edge.arrow.size=.2, vertex.label.cex=l.size)
    graphics.off()
    
  }
  
}



