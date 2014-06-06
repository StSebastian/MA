SaPConst <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character"))

SaPConst$Datum <- as.Date(SaPConst$Datum)
SaPConst$Datum <- substr(SaPConst$Datum,1,7)
SaPConst <- SaPConst

SaPMat <- SaPcreate(SaPConst)

SaPMat <- SaPfill(SaPMat, SaPConst)


SaPcreate <- function(SaPConstitution = SaPConst){

	ColVec<-sort(SaPConstitution$Datum[!duplicated(SaPConstitution$Datum)],decreasing=F)
	RowVec<-SaPConstitution$DSCD[!duplicated(SaPConstitution$DSCD)]
	SicVec<-SaPConstitution$WC07021[!duplicated(SaPConstitution$DSCD)]	
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

	temp <- sapply(6:ncol(SaPMatrix),SaPvalues)
	rm(temp)

	SaPMatrix
	}

SicFour<-function(x){
		SaPMat$SIC_four==x}

SicThree<-function(x){
		SaPMat$SIC_three==x}

SicTwo<-function(x){
		SaPMat$SIC_two==x}

SicOne<-function(x){
		SaPMat$SIC_one==x}

SicFour <- sapply(levels(SaPMat$SIC_four),SicFour)
SicThree <- sapply(levels(SaPMat$SIC_three),SicThree)
SicTwo <- sapply(levels(SaPMat$SIC_two),SicTwo)
SicOne <- sapply(levels(SaPMat$SIC_one),SicOne)

SicFour.NaRm <- function(x){
SicFour[is.na(SicFour[,x]),x] <<- FALSE
}

SicThree.NaRm <- function(x){
SicThree[is.na(SicThree[,x]),x] <<- FALSE
}

SicTwo.NaRm <- function(x){
SicTwo[is.na(SicTwo[,x]),x] <<- FALSE
}

SicOne.NaRm <- function(x){
SicOne[is.na(SicOne[,x]),x] <<- FALSE
}

temp<-sapply(1:ncol(SicFour), SicFour.NaRm)
temp<-sapply(1:ncol(SicThree), SicThree.NaRm)
temp<-sapply(1:ncol(SicTwo), SicTwo.NaRm)
temp<-sapply(1:ncol(SicOne), SicOne.NaRm)
rm(temp)


ColVec<-sort(SaPConst$Datum[!duplicated(SaPConst$Datum)],decreasing=F)

SicMatFour <- matrix(rep(NA,ncol(SicFour)*(length(ColVec)+1)),nrow=ncol(SicFour),ncol=(length(ColVec)+1))
colnames(SicMatFour)<-c("SIC_four",ColVec)
SicMatFour[,1]<-colnames(SicFour)

SicMatThree <- matrix(rep(NA,ncol(SicThree)*(length(ColVec)+1)),nrow=ncol(SicThree),ncol=(length(ColVec)+1))
colnames(SicMatThree)<-c("SIC_three",ColVec)
SicMatThree[,1]<-colnames(SicThree)

SicMatTwo <- matrix(rep(NA,ncol(SicTwo)*(length(ColVec)+1)),nrow=ncol(SicTwo),ncol=(length(ColVec)+1))
colnames(SicMatTwo)<-c("SIC_two",ColVec)
SicMatTwo[,1]<-colnames(SicTwo)

SicMatOne <- matrix(rep(NA,ncol(SicOne)*(length(ColVec)+1)),nrow=ncol(SicOne),ncol=(length(ColVec)+1))
colnames(SicMatOne)<-c("SIC_one",ColVec)
SicMatOne[,1]<-colnames(SicOne)

SicList <- list(SicMatFour, SicMatThree, SicMatTwo, SicMatOne)
SicList <- list(SicMatFour = SicMatFour, SicMatThree = SicMatThree, 
			SicMatTwo = SicMatTwo, SicMatOne = SicMatOne)

SicCountFour<-function(x){
SicList$SicMatFour[x,-1]<<-colSums(SaPMat[SicFour[,x],-c(1:5)])
}

SicCountThree<-function(x){
SicList$SicMatThree[x,-1]<<-colSums(SaPMat[SicThree[,x],-c(1:5)])
}

SicCountTwo<-function(x){
SicList$SicMatTwo[x,-1]<<-colSums(SaPMat[SicTwo[,x],-c(1:5)])
}

SicCountOne<-function(x){
SicList$SicMatOne[x,-1]<<-colSums(SaPMat[SicOne[,x],-c(1:5)])
}

temp <- sapply(1:ncol(SicFour),SicCountFour)
temp <- sapply(1:ncol(SicThree),SicCountThree)
temp <- sapply(1:ncol(SicTwo),SicCountTwo)
temp <- sapply(1:ncol(SicOne),SicCountOne)
rm(temp)

## kannst auch eine sapply in eine sapply packen

SicCount2<-function(x){
test5[x,-1]<<-colSums(SaPMat1[test4[,x],-c(1:5)])
}

head(SaPMat)
