setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

SaPConst <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character"))

SaPConst$Datum <- as.Date(SaPConst$Datum)
SaPConst$Datum <- substr(SaPConst$Datum,1,7)

## Creates empty data frame which list the dates as columns and all the Companies that have been
## in the S&P 1500 as rows. The data frame will later be filled be TRUE and False values to indicated  
## the month of membership of the Companies in the S&P 1500


SaPcreate <- function(SaPConst = SaPConst){

	ColVec<-sort(unique(SaPConst$Datum),decreasing=F) ## filters the different dates
	DSCDPos<-!duplicated(SaPConst$DSCD) ## filters the different DSCD
	##RowVec<-SaPConst$DSCD[!duplicated(SaPConst$DSCD)]
	RowVec<-SaPConst$DSCD[DSCDPos] 
	##SicVec<-SaPConst$WC07021[!duplicated(SaPConst$DSCD)]
	SicVec<-SaPConst$WC07021[DSCDPos] ## filters different Sic positions
	
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

    
## fills SaPMat with TRUE and FALSE values according to their membership in the S&P 1500    
    
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

## next goal is to compute a list of four data frame each for a 1,2,3,and 4 Sic digit table
## the table will have the all the in SaPmat occuring Sics as colomnnames and list by use of 
## True and False values their belonging to the companies which are the rows 

SicSep<-function(SicLevel,SicCol){SaPMat[SicCol]==SicLevel}

SicFour <- sapply(levels(SaPMat$SIC_four), SicSep, SicCol = "SIC_four")
SicThree <- sapply(levels(SaPMat$SIC_three), SicSep, SicCol = "SIC_three")
SicTwo <- sapply(levels(SaPMat$SIC_two), SicSep, SicCol = "SIC_two")
SicOne <- sapply(levels(SaPMat$SIC_one), SicSep, SicCol = "SIC_one")

SicPosList<-list(SicFour = SicFour, SicThree = SicThree, SicTwo = SicTwo, SicOne = SicOne ) 

##*****************************************************************

## removing NAs from SicPosList

Sic.NaRm.Op <- function(Col,SicDig){
SicPosList[[SicDig]][is.na(SicPosList[[SicDig]][,Col]),Col] <<- FALSE
}

Sic.NaRm <- function(SicDig){
	
	for (i in 1:ncol(SicPosList[[SicDig]])){Sic.NaRm.Op(i,SicDig)}
	}

Sic.NaRm("SicOne"); Sic.NaRm("SicTwo"); Sic.NaRm("SicThree"); Sic.NaRm("SicFour")	

##********************************************************************

## creates an empty data frame which has dates as columns and Sics as rows depending on "SicFour","SicThree","SicTwo","SicOne"
## It should later be filled with the number of member companies to the S&P 1500 grouped by the same Sics

ColVec<-sort(unique(SaPConst$Datum),decreasing=F)

SicCreate <- function(SicDig){
	Col.SicDig <- SicPosList[[SicDig]]
	ColVec<-sort(unique(SaPConst$Datum),decreasing=F)
    
	Matrix <- matrix(numeric(ncol(Col.SicDig)*(length(ColVec)+1)),nrow=ncol(Col.SicDig),ncol=(length(ColVec)+1))
	colnames(Matrix)<-c("Sic",ColVec)
	Matrix <- as.data.frame(Matrix)
	Matrix[,1]<-colnames(Col.SicDig)
	##Matrix <- cbind(colnames(Col.SicDig),Matrix)
	Matrix
	}
    

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

##SapSet$CharRows <- CharRowsSet()



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
    CharVal <- CharVal[!is.na(CharVal)]
    ## CharVal <- CharVal[CharVal != 0 | is.na(CharVal)] ##besser musst noch hinzufügen
	if(length(CharVal) >= ObsReq){CharVal <- mean(CharVal, na.rm = T)
	}else {CharVal <- NA}

	CharVal
	}


TsSicCalc <- function(SapDataTab = SapData, SapSetOb = SapSet, CompChar = "MV", SapMat = SaPMat, ObsReq = 5){
	CharCol <- SapSetOb$CharCol
    CharLevel <- unique(as.character(SapData[,CharCol]))
    temp <- merge(SapMat[,c("DSCD","SIC_four")],
			SapDataTab,by.x="DSCD",by.y="DSCD",all=T,sort=F)  ##hier prüfen ob das mit dem unterschiedlichen typen factor und variablen kollidiert
    SapDataTab <- temp
    rm(temp)
	SapDataTab<-SapDataTab[order(SapDataTab$DSCD),]
    SapDataTab<-SapDataTab[order(match(SapDataTab$KPI,CharLevel)),]
    ##SapDataTab<-SapDataTab[order(match(SapDataTab$KPI,names(SapSetOb$CharRows))),]
        ##vll doch besser das ganze gleich in eine liste aufzuteilen(nach den KPI)
    
    SapSetOb$CharRows <- CharRowsSet()
    
    CharRows <- SapSetOb$CharRows
	##SicCol <- SapSetOb$
	SapDataTab <- SapDataTab[CharRows[[CompChar]], ] ##Sowohl die Entfernung der CharRows als auch der Sic Colums kannst du eine Ebene/funktion vorher machen
	DscdCol <- SapSetOb$DscdCol
	##SicRm <- SapDataTab[,SicCol]== Sic 
	##SapDataTab <- SapDataTab[SicRm, ]

    
    SapDataTab <- SapDataTab[!is.na(SapDataTab[,"SIC_four"]),]

	SicList <- sapply(c("SicFour","SicThree","SicTwo","SicOne"),SicCreate,simplify=F,USE.NAMES = TRUE)
	##i <- 3
	for(i in seq(along = SicList)){
	SicListEl <- SicList[[i]]
        #####SicListEl <- SicList[[1]] #####
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
        ######SicList[[4]] <- SicListEl #####
		}
	SicList
	}

   system.time(a<-TsSicCalc(SapDataTab = SapData, SapSetOb = SapSet, CompChar = "MV", SapMat = SaPMat, ObsReq = 5))
 
    
    