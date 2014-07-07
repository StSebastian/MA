
##***************SDC_Table_einlesen*****************

SDCTab <- read.csv("SDCtest.csv",sep=";",dec=".",colClasses=c("character", "numeric", "character", "character", "character", 
							"character", "character", "character", "character", "character", "character", 
							"character", "character", "numeric", "numeric"))

names(SDCTab) <- c("SpDate","SpValueTrans","SpTaName","SpTaDscd","SpAcName",
		"SpAcDscd","SpAcSic","SpAcInd","SpTaSic","SpTaInd","SpDateAnn",
		"SpEqV","SpEpV","SpShAfterTra","SpShAcq")

SDCTab$SpValueTrans <- as.numeric(SDCTab3$SpValueTrans)
SDCTab$SpEqV <- as.numeric(SDCTab3$SpEqV)
SDCTab$SpEpV <- as.numeric(SDCTab3$SpEpV)


SDCTab$SpDate <- as.Date(strptime(SDCTab3$SpDate,"%m.%d.%Y"))
SDCTab$SpDateAnn <- as.Date(strptime(SDCTab3$SpDateAnn,"%m.%d.%Y"))
SDCTab$SpShAfterTra <- SDCTab3$SpShAfterTra/100
SDCTab$SpShAcq <- SDCTab3$SpShAcq/100

## optional

SDCTab3 <- SDCTab3[(substr(SDCTab3$SpAcSic,1,1)!=7&substr(SDCTab3$SpTaSic,1,1)!=7),]
SDCTab3 <- SDCTab3[SDCTab3$SpValueTrans&!is.na(SDCTab3$SpEpV)&!is.na(SDCTab3$SpEqV),]

substr(SDCTab3$SpTaSic,1,1)==7

substr(SDCTab3$SpAcSic,1,1)==7

SDCTab3<-SDCTab3[!is.na(SDCTab3$SpValueTrans)&&!is.na(SDCTab3$SpEqV)&&!is.na(SDCTab3$SpEpV),]
SDCTab3<-SDCTab3[!is.na(SDCTab3$SpValueTrans),]
SDCTab3<-SDCTab3[!is.na(SDCTab3$SpEpV),]
SDCTab3<-SDCTab3[!is.na(SDCTab3$SpEpV)&&!is.na(SDCTab3$SpEpV),]
SDCTab3<-SDCTab3[dem$SpValueTrans&!is.na(SDCTab3$SpEpV)&!is.na(SDCTab3$SpEqV),]



##***************ICC_Table_einlesen*****************

ICCTab<-read.csv("ICC_dataSet_2012-05-29_12_45_06.txt.txt",sep=";",stringsAsFactors=F,dec=",",colClasses=c("integer", "character", "numeric", "character", "numeric", "integer", "numeric", "numeric", "character", "integer", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
))



##***************ICC_Table_einlesen*****************

COMPTab<-read.csv("M&Akons.csv",header=T,stringsAsFactors=F,sep=";",dec=".")

NameCompCol <- function(COMPTable, NChar, StartDate)
	{
	StartDate <- as.POSIXlt( StartDate)
	number <- ncol(COMPTable) - NChar
	temp <- rep(StartDate,number)
	temp$mon <- temp$mon + 0:(number-1)
	temp <- substr(as.Date(temp),1,7)
	names(COMPTable) <- c(names(COMPTable[,1:NChar]),temp)
	COMPTable
	}

COMPTab<-NameCompCol(COMPTab,5,"1978-01-01")


ExtendFun <- function(COMPTab,years=6){
		monthAdd<-12*years
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(1,nrow(COMPTab))
		newFrame <- sapply(1:monthAdd,function(x){newRow})
		getDate <- function(x){
			time$mon <- time$mon+x
			substr(as.Date(time),1,7)
			}
		colDates <- getDate(1:monthAdd)	
		colnames(newFrame)<-colDates
		newFrame
		COMPTab <- cbind(COMPTab,newFrame)
		}



ExtendFun2 <- function(COMPTab){
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(NA,nrow(COMPTab))
		newFrame <- sapply(1:24,function(x){newRow})
		time$mon <- time$mon+1:24
		time<-substr(as.Date(time),1,7)
		colnames(newFrame)<-time
		newFrame
		COMPTab <- cbind(COMPTab,newFrame)
		}

ExtendFun <- function(COMPTab,years=6){
		monthAdd<-12*years
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(NA,nrow(COMPTab))
		time$mon <- time$mon+1:monthAdd
		time<-substr(as.Date(time),1,7)
		
		COMPTab[,time] <- newRow
		COMPTab
		}

COMPTab<-ExtendFun(COMPTab)


		}


NameCompCol <- function(COMPTable = COMPTab, NChar = 5 , StartDate = "1978-01-01"){
	
	StartDate <- as.POSIXlt( StartDate)
	number <- ncol(COMPTable) - NChar
	temp <- rep(StartDate,number)
	temp$mon <- temp$mon + 0:(number-1)
	temp <- substr(as.Date(temp),1,7)
	names(COMPTable) <- c(names(COMPTable[,1:NChar]),temp)
	COMPTable
	}


SDCadjust <- function(SDCTable = SDCTab){
		 
		names(SDCTable) <- c("SpDate","SpValueTrans","SpTaName","SpTaDscd","SpAcName",
		"SpAcDscd","SpAcSic","SpAcInd","SpTaSic","SpTaInd","SpDateAnn",
		"SpEqV","SpEpV","SpShAfterTra","SpShAcq")

		SDCTable$SpValueTrans <- as.numeric(SDCTable$SpValueTrans)
		SDCTable$SpEqV <- as.numeric(SDCTable$SpEqV)
		SDCTable$SpEpV <- as.numeric(SDCTable$SpEpV)


		SDCTable$SpDate <- as.Date(strptime(SDCTable$SpDate,"%m.%d.%Y"))
		SDCTable$SpDateAnn <- as.Date(strptime(SDCTable$SpDateAnn,"%m.%d.%Y"))
		SDCTable$SpShAfterTra <- SDCTable$SpShAfterTra/100
		SDCTable$SpShAcq <- SDCTable$SpShAcq/100
		SDCTable
		}
SDCTab<-SDCadjust()
TableRead()
	
##***************else************************

SDCCol3<-SDCset("SpDate","SpAcDscd","SpTaDscd","SpShAcq","SpAcSic" ,"SpTaSic")
ICCCol3<-ICCset("Datum","Company_Code","ICC_CT")


denk<-is.na(testob[,7])|is.na(testob[,8])|is.na(testob[,9])
demo5<-testob[!denk,]
sche<-((substr(demo5[,"SicTaCol"],1,1)==7)|(substr(demo5[,"SicAcCol"],1,1)==7))
separa<-SICSeparation(demo5[!sche,],"SicAcCol","SicTaCol")
testob3<-testG(SDCTab3,SDCCol3,ICCCol3,ICCTab3,EvenW3)
