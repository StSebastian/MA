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

