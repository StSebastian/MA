
##***************SDC_Table_einlesen*****************

SDCTab3 <- read.csv("SDCtest.csv",sep=";",dec=".",colClasses=c("character", "numeric", "character", "character", "character", 
							"character", "character", "character", "character", "character", "character", 
							"character", "character", "numeric", "numeric"))

names(SDCTab3) <- c("SpDate","SpValueTrans","SpTaName","SpTaDscd","SpAcName",
		"SpAcDscd","SpAcSic","SpAcInd","SpTaSic","SpTaInd","SpDateAnn",
		"SpEqV","SpEpV","SpShAfterTra","SpShAcq")

SDCTab3$SpValueTrans <- as.numeric(SDCTab3$SpValueTrans)
SDCTab3$SpEqV <- as.numeric(SDCTab3$SpEqV)
SDCTab3$SpEpV <- as.numeric(SDCTab3$SpEpV)


SDCTab3$SpDate <- as.Date(strptime(SDCTab3$SpDate,"%m.%d.%Y"))
SDCTab3$SpDateAnn <- as.Date(strptime(SDCTab3$SpDateAnn,"%m.%d.%Y"))
SDCTab3$SpShAfterTra <- SDCTab3$SpShAfterTra/100
SDCTab3$SpShAcq <- SDCTab3$SpShAcq/100

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

ICCTab3<-read.csv("ICC_dataSet_2012-05-29_12_45_06.txt.txt",sep=";",stringsAsFactors=F,dec=",",colClasses=c("integer", "character", "numeric", "character", "numeric", "integer", "numeric", "numeric", "character", "integer", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
))



##***************ICC_Table_einlesen*****************

COMPTab2<-read.csv("M&Akons.csv",header=T,stringsAsFactors=F,sep=";",dec=".")

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

COMPTab2<-NameCompCol(COMPTab2,5,"1978-01-01")

ExtendFun <- function(COMPTab) 
		{tempi <- COMPTab
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		temp <- rep(NA,nrow(COMPTab))
			for(i in 1:24){
			count <- time
			count$mon <- time$mon+i
			namen<-substr(as.Date(count),1,7)
			zwi<-names(tempi)
			tempi <- cbind(tempi,temp)
			names(tempi)<-c(zwi,namen)
			}
		tempi
		}

COMPTab3<-ExtendFun(COMPTab2, ICCCol, COMPCol)





##***************else************************

SDCCol3<-SDCset("SpDate","SpAcDscd","SpTaDscd","SpShAcq","SpAcSic" ,"SpTaSic")
ICCCol3<-ICCset("Datum","Company_Code","ICC_CT")


denk<-is.na(testob[,7])|is.na(testob[,8])|is.na(testob[,9])
demo5<-testob[!denk,]
sche<-((substr(demo5[,"SicTaCol"],1,1)==7)|(substr(demo5[,"SicAcCol"],1,1)==7))
separa<-SICSeparation(demo5[!sche,],"SicAcCol","SicTaCol")
testob3<-testG(SDCTab3,SDCCol3,ICCCol3,ICCTab3,EvenW3)
