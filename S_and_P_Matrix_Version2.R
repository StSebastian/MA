SaPConst <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character"))

SaPConst$Datum <- as.Date(SaPConst$Datum)
SaPConst$Datum <- substr(SaPConst$Datum,1,7)

SaPcreate <- function(SaPConstitution = SaPConst){

	ColVec<-sort(unique(SaPConstitution$Datum),decreasing=F)
	DSCDPos<-!duplicated(SaPConstitution$DSCD)
	##RowVec<-SaPConstitution$DSCD[!duplicated(SaPConstitution$DSCD)]
	RowVec<-SaPConstitution$DSCD[DSCDPos]
	##SicVec<-SaPConstitution$WC07021[!duplicated(SaPConstitution$DSCD)]
	SicVec<-SaPConstitution$WC07021[DSCDPos]
	
	SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")

	RowLeng<-length(RowVec)
	ColLeng<-length(ColVec)+2+3
	SaPMat<-matrix(rep(NA,RowLeng*ColLeng),nrow=RowLeng,ncol=ColLeng)
	SaPMat<-as.data.frame(SaPMat)

	colnames(SaPMat)<-c("DSCD","SIC_four","SIC_three","SIC_two","SIC_one",ColVec)
	SaPMat[,1]<-RowVec
	SaPMat[,2]<-as.factor(SicVec)
	SaPMat[,3]<-as.factor(substr(SicVec,1,3))
	SaPMat[,4]<-as.factor(substr(SicVec,1,2))
	SaPMat[,5]<-as.factor(substr(SicVec,1,1))

	SaPMat
	}

SaPfill <- function(SaPMatrix = SaPMat,SaPConstitution = SaPConst){

	SaPvalues <- function(x){
	SaPMatrix[,x] <<- SaPMatrix[,"DSCD"]%in%SaPConstitution[SaPConstitution$Datum==colnames(SaPMatrix)[x],"DSCD"]
	invisible()
	}
	
	for(i in 6:ncol(SaPMatrix)){SaPvalues(i)}
	##temp <- sapply(6:ncol(SaPMatrix),SaPvalues)
	##rm(temp)

	SaPMatrix
	}


SaPMat <- SaPcreate(SaPConst)

SaPMat <- SaPfill(SaPMat, SaPConst)

##*****************************************************************

SicSep<-function(SicLevel,SicCol){SaPMat[SicCol]==SicLevel}

SicFour <- sapply(levels(SaPMat$SIC_four), SicSep, SicCol = "SIC_four")
SicThree <- sapply(levels(SaPMat$SIC_three), SicSep, SicCol = "SIC_three")
SicTwo <- sapply(levels(SaPMat$SIC_two), SicSep, SicCol = "SIC_two")
SicOne <- sapply(levels(SaPMat$SIC_one), SicSep, SicCol = "SIC_one")

SicPosList<-list(SicFour = SicFour, SicThree = SicThree, SicTwo = SicTwo, SicOne = SicOne ) 

##*****************************************************************

Sic.NaRm.Op <- function(Col,SicDig){
SicPosList[[SicDig]][is.na(SicPosList[[SicDig]][,Col]),Col] <<- FALSE
}

Sic.NaRm <- function(SicDig){
	
	for (i in 1:ncol(SicPosList[[SicDig]])){Sic.NaRm.Op(i,SicDig)}
	}

Sic.NaRm("SicOne"); Sic.NaRm("SicTwo"); Sic.NaRm("SicThree"); Sic.NaRm("SicFour")	

##********************************************************************

ColVec<-sort(unique(SaPConst$Datum),decreasing=F)

SicCreate <- function(SicDig){
	Col.SicDig <- SicPosList[[SicDig]]
	
	Matrix <- matrix(numeric(ncol(Col.SicDig)*(length(ColVec)+1)),nrow=ncol(Col.SicDig),ncol=(length(ColVec)+1))
	colnames(Matrix)<-c("Sic",ColVec)
	Matrix <- as.data.frame(Matrix)
	Matrix[,1]<-colnames(Col.SicDig)
	##Matrix <- cbind(colnames(Col.SicDig),Matrix)
	Matrix
	}

SicList <- sapply(c("SicFour","SicThree","SicTwo","SicOne"),SicCreate,simplify=F,USE.NAMES = TRUE)

##********************************************************************

Sic.Count.Op <- function(x,ListEle, BoMat){
SicList[[ListEle]][x,-1]<<-colSums(SaPMat[BoMat[,x],-c(1:5)])
}

Sic.Count <- function(ListEle, BoMat){
	temp<-sapply(1:ncol(BoMat),Sic.Count.Op,BoMat = BoMat, ListEle = ListEle)
	rm(temp)}

Sic.Count("SicOne",SicPosList$SicOne); Sic.Count("SicTwo",SicPosList$SicTwo)
Sic.Count("SicThree",SicPosList$SicThree); Sic.Count("SicFour",SicPosList$SicFour)

##************************************************************************

SapData <- read.csv("S&Pkons.csv",sep=";",dec=".")
##as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
Dates <- as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
ColNames <- c(colnames(SapData[,1:3]),substring(Dates,1,7))
colnames(SapData) <- ColNames

Sap_Set <- function(DscdCol = "DSCD", CharCol = "KPI", OneSicCol = NULL, TwoSicCol = NULL, ThrSicCol = NULL, FourSicCol = NULL){
	
	CharRows <- NULL
	list("DscdCol" = DscdCol, "CharCol" = CharCol, "OneSicCol" = OneSicCol,
		"TwoSicCol" = TwoSicCol, "ThrSicCol" = ThrSicCol, "FourSicCol" = FourSicCol,
		"CharRows" = CharRows)
	}

SapSet<-Sap_Set()

CharRowsSet <- function(SapDataTab = SapData, SapSetOb = SapSet){
	CharCol <- SapSetOb$CharCol
	CharColVals <- SapDataTab[[CharCol]] 
	CharVals <- as.character(unique(SapDataTab[, CharCol]))
	CharRows <- sapply(CharVals, function(x,y){x==CharColVals }, simplify=F, USE.NAMES = TRUE)
	CharRows
	}

SapSet$CharRows <- CharRowsSet()


TsSicCalc <- function(SapDataTab = SapData, SapSetOb = SapSet, CompChar = "MV", SapMat = SaPMat, ObsReq = 5){
	CharRows <- SapSetOb$CharRows
	##SicCol <- SapSetOb$
	SapDataTab <- SapDataTab[CharRows[[CompChar]], ] ##Sowohl die Entfernung der CharRows als auch der Sic Colums kannst du eine Ebene/funktion vorher machen
	DscdCol <- SapSetOb$DscdCol
	##SicRm <- SapDataTab[,SicCol]== Sic 
	##SapDataTab <- SapDataTab[SicRm, ]
	SapDataTab <- merge(SapMat[,c("DSCD","SIC_four")],
			SapDataTab,by.x="DSCD",by.y="DSCD",all=T,sort=F)
	SapDataTab <- SapDataTab[!is.na(SapDataTab[,"SIC_four"]),]

	SicList <- sapply(c("SicFour","SicThree","SicTwo","SicOne"),SicCreate,simplify=F,USE.NAMES = TRUE)
	##i <- 3
	for(i in seq(along = SicList)){
		SicListEl <- SicList[[i]]
		SicLength <- nchar(SicListEl[1,"Sic"])
		##if(SicLength==1){SicCol="SIC_one"}
		##if(SicLength==2){SicCol="SIC_two"}
		##if(SicLength==3){SicCol="SIC_three"}
		##if(SicLength==4){SicCol="SIC_four"}
		
		SapDataTab[,"SIC"] <- substring(SapDataTab[,"SIC_four"],1,SicLength)	##Sic Col noch definieren	
		
		SicListEl <- TsSicElCalc(SicListEl,SapDataTab,DscdCol,SapMat,ObsReq)

		##for(i in seq(along=colnames(SicListEl[,-1]))){
		##	Datum <- colnames(SicListEl)[i+1]
		##	DscdRm <- SapDataTab[,DscdCol] %in% SaPMat[SaPMat[,Datum],"DSCD"]
		##	SapDataTab2 <- SapDataTab[DscdRm, ]
		##	##CharVal <- CalcCharVal(SapDataTab2, Sic, SapMat, Datum, ObsReq)
		##	CharVal <- sapply(SicListEl[,"Sic"],CalcCharVal, SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq)
		##	SicListEl[,Datum] <- CharVal
		##	}
		SicList[[i]] <- SicListEl
		}
	SicList
	}

TsSicElCalc <- function(SicListEl,SapDataTab,DscdCol,SaPMat,ObsReq){
		for(i in seq(along=colnames(SicListEl[,-1]))){
			Datum <- colnames(SicListEl)[i+1]
			DscdRm <- SapDataTab[,DscdCol] %in% SaPMat[SaPMat[,Datum],"DSCD"]
			SapDataTab2 <- SapDataTab[DscdRm, ]
			##CharVal <- CalcCharVal(SapDataTab2, Sic, SapMat, Datum, ObsReq)
			CharVal <- sapply(SicListEl[,"Sic"],CalcCharVal, SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq)
			SicListEl[,Datum] <- CharVal
		}
		SicListEl
		}


CalcCharVal <- function(Sic, SapDataTab, SapMat, Datum, ObsReq){ ##SapMat ist SaPMat sollte aber später SaP Composition heißen also SapComp
	##CharRows <- SapSetOb$CharRows
	##DscdCol <- SapSetOb$DscdDCol
	##SicCol <- SapSetOb$  ##hier noch einfügen
	
	##SapDataTab <- SapDataTab[CharRows[[CompChar]], ] ##Sowohl die Entfernung der CharRows als auch der Sic Colums kannst du eine Ebene/funktion vorher machen
	SicRm <- SapDataTab[,"SIC"]== Sic 
	##SapDataTab <- SapDataTab[SicRm, ]
	##DscdRm <- SapDataTab[,DscdCol] %in% SapMat[SapMat[,Datum],]
	##SapDataTab <- SapDataTab[DscdRm, ]
	CharVal <- SapDataTab[SicRm,Datum]
	CharVal <- CharVal[CharVal != 0]
	if(length(CharVal) >= ObsReq){CharVal <- mean(CharVal, na.rm = T)
	}else {CharVal <- NA}

	CharVal
	}

SicListEl5<-SicListEl

rollapply(SicListEl5[,"Sic"],FUN=CalcCharVal,SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq,width=1)
	
	rollapply
	f<-as.zoo(data.frame(rep(1,9),rep(1,9),1:9))
	rollapply(f,FUN=mean,width=1,align="right",by.column=F)
	rollapply(f,FUN=function(x){if(x==1)TRUE},width=1,align="right",by.column=T)
	rollapply(f,FUN=mean,width=2,align="right",by.column=T)
	f<-as.zoo(data.frame(rep(1,9),rep(1,9),1:9,rep("a",9)))
	rollapply(f[,-4],FUN=function(x){if(x==1)TRUE},width=1,align="right",by.column=T)
	
	f<-as.zoo(data.frame(rep(1,9),rep(1,9),1:9))
	t(apply(f,1,function(x){rollapply(x,FUN=mean,width=2,align="left",by.column=T)}))
	t(rollapply(t(f),FUN=mean,width=2,align="left",by.column=T))	 
	
a<-merge(SaPMat[,c("DSCD","SIC_four","SIC_three","SIC_two","SIC_one")],SapData,by.x="DSCD",by.y="DSCD",all=T,sort=F)
a<-a[order(a[,"KPI"]),]

system.time(for (i in 1:nrow(SicListEl5)){SicListEl5[i,Datum]<-CalcCharVal(SicListEl5[i,1],SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq)})
system.time(a<-rollapply(SicListEl5[,"Sic"],FUN=CalcCharVal,SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq,width=1))
system.time(CharVal <- sapply(SicListEl5[,"Sic"],CalcCharVal, SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq))

SicListEl <- SicList[[4]]
system.time(g<-TsSicElCalc(SicListEl,SapDataTab1,DscdCol,SaPMat,ObsReq) )
system.time(f<-TsSicCalc(SapData,SapSet,"MV",SaPMat,5))