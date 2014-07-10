write.table(IccTab3,"test.txt",sep=";",row.names=F)
##write.table(IccTab,"IccTabelle.txt",sep=";",row.names=F)
setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

##source("C:/Users/Sebastian Stenzel/GitHub/test2/source_test.R")

##DSCDtest<-SDCTab[,c("SpAcDscd","SpTaDscd")]
##DSCDtest<-c(SDCTab[1:200,c("SpTaDscd")],SDCTab[1:200,"SpAcDscd"])

##SampleRows <- is.element(ICCTab[,"Company_Code"],DSCDtest)

##ICCSample <- ICCTab[SampleRows,]


library(zoo)

SDCTab <- read.csv("SDCtest.csv",sep=";",dec=".",colClasses=c("character", "numeric", "character", "character", "character", 
							"character", "character", "character", "character", "character", "character", 
							"character", "character", "numeric", "numeric"))


##ICCTab<-read.csv("ICC_dataSet_2012-05-29_12_45_06.txt.txt",sep=";",stringsAsFactors=F,dec=",",colClasses=c("integer", "character", "numeric", "character", "numeric", "integer", "numeric", "numeric", "character", "integer", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
##))

 ICCTab<-read.csv("ICCSample3.txt",sep=";",stringsAsFactors=F,dec=".",
                        colClasses=c("integer", "character", "numeric", "character", "numeric", "integer",
                        "numeric", "numeric", "character", "integer", "character", "Date", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", 
                        "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
                        "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric"))

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


COMPTab<-read.csv("M&Akons.csv",header=T,stringsAsFactors=F,sep=";",dec=".")


## *************setting table columns and event window**************

## functions: SDCset, ICCset, COMPset, EventWset 

## SDCset --> selecting M&A Date column, Acquiror (DSCD) column, Target (DSCD) Column
##         	 	      Percentage of Shares Acquired column, 
##			      SIC Code Acquiror column, SIC Code Target column
## names/headlines of the corrsponding colums should be given as arguments
## of type char to the function when called   			   

SDCset <- function(DatCol = "SpDate", AcCol = "SpAcDscd", TaCol = "SpTaDscd",
				    	ShareCol = "SpShAcq", SicAcCol = "SpAcSic", 
					SicTaCol = "SpTaSic"){    
	
	get <- function()data.frame(DatCol, AcCol, TaCol, ShareCol, 
				SicAcCol, SicTaCol, stringsAsFactors = F)

	setDat   <-  function(DAT) DatCol <<- DAT
	setAc    <-  function(AC)  AcCol <<- AC 	
	setTa    <-  function(TA)  TaCol <<- TA
	setShare <-  function(SHARE)  ShareCol <<- SHARE
	setSicAc <-  function(SAc) SicAcCol <<- SAc
	setSicTa <-  function(STa) SicTaCol <<- STa
	
	SDCCol <<- list(get = get, setDat = setDat,
			setAc = setAc, setTa = setTa, 
			setShare = setShare, setSicAc = setSicAc,
			setSicTa = setSicTa)

	SDCCol
	}



## ICCset --> selecting column of ICC Date, Company's DSCD
##         	 	      column of ICC
## names/headlines of the corrsponding colums should be given as arguments
## of type char to the function when called  

ICCset <- function(DatCol = "Datum", DscdCol = "Company_Code", 
					IccCol = "ICC_CT"){
	
	get <- function()	list("DatCol" = DatCol, "DscdCol" = DscdCol , 
                            "IccCol" = IccCol, "DscdLev" = DscdLev)
	
    DscdLev <- NULL
    
	setDat  <- function(DAT) DatCol <<- DAT
	setDscd <-  function(DSCD)  DscdCol <<- DSCD 	
	setIcc  <-  function(ICC)  IccCol <<- ICC	
	setDscdLev <- function(Lev) DscdLev <<- Lev
    
	ICCCol <<- list(get = get,setDat = setDat, setDscd = setDscd, 
                        setIcc = setIcc, setDscdLev = setDscdLev)
	
	ICCCol
	}

IccCovDat <- function(ICCset = ICCCol,ICCTable = ICCTab){
		DatCol <- ICCset$get()$DatCol
		ICCTable[ ,DatCol] <- substr(ICCTable[ ,DatCol],1,7)
		ICCTable
		}

## COMPset --> selecting Company's (DSCD) column, column of respective 
##         	 	       Company's Characteristics, Company's SIC Code column
## names/headlines of the corrsponding colums should be given as arguments
## of type char to the function when called 

COMPset <- function(DscdCol = "DSCD", CharacCol = "KPI",
					 SicCol = "WC07021"){	
	
	CharacList <- NULL
	get <- function()	list(DscdCol = DscdCol, CharacCol = CharacCol, 
					SicCol = SicCol, CharacList = CharacList)
	
	setDscd <- function(DSCD) DscdCol <<- DSCD
	setSic  <- function(SIC) SicCol <<- SIC	
	setCharac <- function(CharacC) CharacCol <<- CharacC
	setCharacList <- function(Charac) CharacList <<- Charac 
	
	COMPCol <<- list(get = get, setDscd = setDscd, 
			setCharac = setCharac, setSic = setSic,
			setCharacList = setCharacList)
	
	COMPCol
	}



## EventWset --> selecting period before and after M&A date which will be used 
## 		     for ICC prae and post M&A computation as well as Company's 
##               characteristics		 
## Five numeric arguments are given to the function. First two determine the computation 
## period prior to the M&A. Therefore number of month from M&A date to the first month 
## and the last month of the period are entered as negative values to the function.
## Same procedure applies for the following two arguments, they determine the computation
## period after the M&A. 
## The fifth argument is a the number of valid information,
## which need to exist in each of the period. If there exists to many missing values, 
## the corresponding M&A will be excluded from further computations.    

##EventWset <- function(PraeStart = -18, PraeLast = -3, PostStart = 3, PostLast = 18, 
##				MinObs = 3)
##	{	
##	EventWdata <<- list(PraeStart = PraeStart, PraeLast = PraeLast, 
##			   PostStart = PostStart, PostLast = PostLast,
##		 	   MinObs = MinObs)
##	}



EventWset <- function(Close = 3, Far = 16, Size = 6, 
				MinObs = 2)
	{	
	get <- function()	list(Close = Close, Far = Far, 
					Size =  Size, MinObs = MinObs)
	
	COMPTab<<-ExtendFun(COMPTab,ceiling(Far/12))

	setClose <- function(CloseVal) Close <<- CloseVal
	setFar  <- function(FarVal,COMPTable = COMPTab) {Far <<- FarVal
				COMPTab <<- ExtendFun(COMPTab,ceiling(FarVal/12))}	
	setSize <- function(SizeVal) Size <<- SizeVal
	setMinObs <- function(MinObsVal) MinObs <<- MinObsVal
	
	EventWdata <<- list(get = get, setClose = setClose, 
			setFar = setFar, setSize = setSize,
			setMinObs = setMinObs)

	##EventWdata <<- list(Start = Start, Last = Last, 
	##		   Size = Size, MinObs = MinObs)
	}

## ExtendFun --> Adds further empty columns to the company's characteristics table to account for
##               possible post M&A periods reaching beyond the last date of the time series.
## First argument is company's characteristics table, seccond the numer of colums to be added


ExtendFun <- function(COMPTab,years=6){
		monthAdd<-12*(years+1)
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(NA,nrow(COMPTab))
		time$mon <- time$mon+1:monthAdd
		time<-substr(as.Date(time),1,7)
		COMPTab[,time] <- newRow
		COMPTab
		}


SetCol <- function(){
		SDCset()
		ICCset()
		COMPset()
		EventWset()
		}
SetCol()
ICCTab<-IccCovDat()


##SDCset3 <- SDCset("SpDate", "SpAcDscd", "SpTaDscd", "SpShAcq", "SpAcSic", "SpTaSic") 
##ICCset3 <- ICCset("Datum", "Company_Code", "ICC_CT")
##COMPset3 <- COMPset("DSCD", "KPI", "WC07021")
##EventWset3 <- EventWset(-19, -2, 2, 19, 4)


## *************modifying COMPTable**************

## NameCompCol --> convertes the tables column names to a required standard. Which will be
## 			 "yyyy-mm" for all columns which refer to particular Date.	
## First argument is the stored table with the time series' of the company's characteristics, 
## the seccond argument the number of columns prior to the first time column and the third
## the starting date of the time series in the format "yyyy-mm-dd" (only year and month are
## truly relevant).

NameCompCol <- function(COMPTable = COMPTab, NChar = 5 , StartDate = "1978-01-01"){

	StartDate <- as.POSIXlt( StartDate)
	number <- ncol(COMPTable) - NChar
	temp <- rep(StartDate,number)
	temp$mon <- temp$mon + 0:(number-1)
	temp <- substr(as.Date(temp),1,7)
	names(COMPTable) <- c(names(COMPTable[,1:NChar]),temp)
	COMPTable
	}

COMPTab <- NameCompCol(COMPTab)


## CharacAdd --> creates an object given to another object created by the function COMPset().
##		     It will be stored in the variable "CharacList" of this object and gives 
##		     information in which rows of the company's characteristics table 
##		     each characteristic can be found. The object decrease computation time of
##		     other functions.
## Arguments are the company's characteristics table and the object created by the COMPset()
## function. 	


CharacAdd <- function(COMPTable, COMPCol)
	{ 
		CharacCol <- COMPCol$get()$CharacCol
		CharacCol <- COMPTable[,CharacCol]
		CharacCol <- as.factor(CharacCol)		

		Characs <- levels(CharacCol)
		CharacMat <- sapply(1:length(Characs),function(x){Characs[x] == CharacCol })
		colnames(CharacMat) <- Characs
		CharacMat <- as.data.frame(CharacMat)
		##COMPCol[[length(COMPCol)+1]] <- CharacMat
		COMPCol$setCharacList(CharacMat)
		##COMPCol$get()	
		COMPCol
	}

COMPCol <- CharacAdd(COMPTab, COMPCol)


DscdAdd <- function(ICCTab,ICCCol){
    
    DscdCol <- ICCCol$get()$DscdCol
    DscdCol <- substring(ICCTab[,DscdCol],1,2)
    Level <- levels(as.factor(DscdCol))
    DscdList <- sapply(1:length(Level),function(x){Level[x]==DscdCol})
    ##DscdList <- sapply(Level,function(x){x==DscdCol})
    colnames(DscdList) <- Level
    ICCCol$setDscdLev(DscdList)
    }
    
##DscdAdd(ICCTab,ICCCol)
## *************functions repeated for every M&A**************


## SDCget --> take the object storing the SDC columns, the SDC Table, 
##		  the row of the SDC Table (Selection of particular M&A) and 
##		  the object storing the Event Window details as argument. 
##		  In return it stores the SDC details SDC date, SDC prae and 
##		  post Event Window period, Acquiror and Target DSCD, the percentage 
##		  of shares acquired and the SIC Code of the Acquiror and the Target company   

##SDCget <- function(SdcCol = SDCCol, SDCTable, SDCRow, EVENTWset){
	
##	DatCol 	<- SdcCol$get()$DatCol	
##	AcquirorCol <- SdcCol$get()$AcCol
##	TargetCol   <- SdcCol$get()$TaCol
##	ShareAcCol  <- SdcCol$get()$ShareCol
##	SicAcqCol	<- SdcCol$get()$SicAcCol
##	SicTarCol   <- SdcCol$get()$SicTaCol
	
##	Datum     <- SDCTable[SDCRow,DatCol]
	
##	MuaDat      <- EventWget(Datum, EVENTWset) ## function will be explained next

##	AcquirorDscd <- SDCTable[SDCRow,AcquirorCol]
##	TargetDscd   <- SDCTable[SDCRow,TargetCol]
##	ShareAc      <- SDCTable[SDCRow,ShareAcCol]
##	SicAc		 <- SDCTable[SDCRow,SicAcqCol]	
##	SicTa		 <- SDCTable[SDCRow,SicTarCol]	
	
##	list( Datum = Datum, MuaDat = MuaDat, 
##		AcquirorDscd = AcquirorDscd, TargetDscd = TargetDscd, 
##		ShareAc = ShareAc, SicAc = SicAc, SicTa = SicTa)
##	}

SDCget <- function(SdcCol = SDCCol, SDCTable, SDCRow, EventWset){
	
	DatCol 	<- SdcCol$get()$DatCol	
	AcquirorCol <- SdcCol$get()$AcCol
	TargetCol   <- SdcCol$get()$TaCol
	ShareAcCol  <- SdcCol$get()$ShareCol
	SicAcqCol	<- SdcCol$get()$SicAcCol
	SicTarCol   <- SdcCol$get()$SicTaCol
	
	Datum     <- SDCTable[SDCRow,DatCol]
	
	MuaDat      <- EventWget(Datum, EventWset) ## function will be explained next

	AcquirorDscd <- SDCTable[SDCRow,AcquirorCol]
	TargetDscd   <- SDCTable[SDCRow,TargetCol]
	ShareAc      <- SDCTable[SDCRow,ShareAcCol]
	SicAc		 <- SDCTable[SDCRow,SicAcqCol]	
	SicTa		 <- SDCTable[SDCRow,SicTarCol]	
	
	list( Datum = Datum, MuaDat = MuaDat, 
		AcquirorDscd = AcquirorDscd, TargetDscd = TargetDscd, 
		ShareAc = ShareAc, SicAc = SicAc, SicTa = SicTa)
	}



## EventWget --> takes particular SDC Date as argument and the object storing
##               the details for prae and post M&A Event Window periods. 
##		     It returns a list with two vectors and one number. The vectors 
##  		     store all the dates in the prae and post M&A Event Window periods,
## 		     whereas the number just states the minimum non missig values for each
##		     earlier defined in the EventWset() functions.

##EventWget <- function(Datum, EventW)
##	{
##	Datum <- as.POSIXlt(Datum) 
##	DatPrae <- rep(Datum,EventW$PraeLast - EventW$PraeStart+1)
##	DatPost <- rep(Datum,EventW$PostLast - EventW$PostStart+1)

	
##	DatPrae$mon <- (EventW$PraeStart:EventW$PraeLast)+DatPrae$mon
##	DatPost$mon <- (EventW$PostStart:EventW$PostLast)+DatPost$mon
##	DatPrae <- substr(as.Date(DatPrae),1,7)
##	DatPost <- substr(as.Date(DatPost),1,7)
##	MinObs <- EventW$MinObs
	
##	list(DatPrae = DatPrae, DatPost = DatPost, MinObs = MinObs)
##	}

EventWget <- function(Datum, EventWset)
	{
	Datum <- as.POSIXlt(Datum) 
	Datum$mday <- 15
	DatPost <- DatPrae <- rep(Datum,EventWset$get()$Far - EventWset$get()$Close+1)
		
	DatPrae$mon <- DatPrae$mon-(EventWset$get()$Close:EventWset$get()$Far)
	DatPost$mon <- DatPost$mon+(EventWset$get()$Close:EventWset$get()$Far)
	DatPrae <- substr(as.Date(DatPrae),1,7)
	DatPost <- substr(as.Date(DatPost),1,7)
	MinObs <- EventWset$get()$MinObs
	Size <- EventWset$get()$Size	

	list(DatPrae = DatPrae, DatPost = DatPost, MinObs = MinObs, Size = Size)
	}



## ICCget --> takes the object storing the ICC Columns, the ICC table and
##		  the SDC object created by the SDCget() function and unique to
##		  each M&A.
##		  It gives the mean ICCs for the Acquiror and Target for the period
##		  prior to the M&A back and the mean ICC for the Acquiror after the
##            M&A. 

##ICCget <- function(IccCol = ICCCol, ICCTable, SDCget){
##	
##	DatCol  <- IccCol$get()$DatCol
##	DscdCol <- IccCol$get()$DscdCol
##	IccCol  <- IccCol$get()$IccCol
##	DatPrae <- SDCget$MuaDat$DatPrae
##	DatPost <- SDCget$MuaDat$DatPost
##	MinObs  <- SDCget$MuaDat$MinObs
##	ICCTab  <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	
##	Abbruch <- list(TaIcc = NA, AcIccPrae = NA, AcIccPost = NA)
##	if(is.na(SDCget$TargetDscd)|is.na(SDCget$AcquirorDscd))
##	{return (Abbruch)}
		
##		temp  <- ICCTab[, DscdCol] == SDCget$TargetDscd
##		tempTA  <- ICCTab[temp, c(DatCol,IccCol)]
##		tempDat <- is.element(substr(tempTA[,DatCol],1,7),DatPrae)
##		TaIcc <- tempTA[tempDat, IccCol]
##		TaIcc <- TaIcc[!is.na(TaIcc)]
##		if(length(TaIcc) < MinObs){return (Abbruch)}
##		TaIcc <- mean(TaIcc, na.rm = TRUE)
		
##		temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
##		tempAC  <- ICCTab[temp, c(DatCol,IccCol)]
##		tempDat <- is.element(substr(tempAC[,DatCol],1,7),DatPrae)
##		AcIccPrae <- tempAC[tempDat, IccCol]
##		AcIccPrae <- AcIccPrae[!is.na(AcIccPrae)]	## müsste AcIccPrae anstelle TaIcc sein
##		if(length(AcIccPrae) < MinObs){return (Abbruch)}
##		AcIccPrae <- mean(AcIccPrae, na.rm = TRUE)
	
##		tempDat <- is.element(substr(tempAC[,DatCol],1,7),DatPost)
##		AcIccPost <- tempAC[tempDat, IccCol]
##		AcIccPost <- AcIccPost[!is.na(AcIccPost)]
##		if(length(AcIccPost) < MinObs){return (Abbruch)}
##		AcIccPost <- mean(AcIccPost, na.rm = TRUE)
				
##		list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
##		}

ICCget <- function(IccCol = ICCCol, ICCTab, SDCget){
	
    ##mit 2D level und Zwischentabelle
    
	DatCol  <- IccCol$get()$DatCol
	DscdCol <- IccCol$get()$DscdCol
    DscdLev <- IccCol$get()$DscdLev    
	IccCol  <- IccCol$get()$IccCol
    
	DatPrae <- sort(SDCget$MuaDat$DatPrae)
	DatPost <- sort(SDCget$MuaDat$DatPost)
	MinObs  <- SDCget$MuaDat$MinObs
	Size  <- SDCget$MuaDat$Size
	##DscdRows <- is.element(ICCTable[,DscdCol],c(SDCget$TargetDscd,SDCget$AcquirorDscd))  ##prüfen ob schneller
	##ICCTab  <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	ICC <- rep(NA,length(DatPrae))
	ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

        Target2D <- substring(SDCget$TargetDscd,1,2)
		Acquiror2D <- substring(SDCget$AcquirorDscd,1,2)
        
        tempTA <- ICCTab[DscdLev[,Target2D],] 
        temp  <- tempTA[, DscdCol] == SDCget$TargetDscd
		tempTA  <- tempTA[temp, c(DatCol,IccCol)]
		tempDat <- is.element(tempTA[,DatCol],DatPrae)
		IccExist <- is.element(DatPrae,tempTA[tempDat,DatCol])
		ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol]
		
		tempAC <- ICCTab[DscdLev[,Acquiror2D],] 
		temp  <- tempAC[, DscdCol] == SDCget$AcquirorDscd
		tempAC  <- tempAC[temp, c(DatCol,IccCol)]
		tempDat <- is.element(tempAC[,DatCol],DatPrae)
		IccExist <- is.element(DatPrae,tempAC[tempDat,DatCol])
		ICC[IccExist,"AcIccPrae"] <- tempAC[tempDat, IccCol]
		
	
		tempDat <- is.element(tempAC[,DatCol],DatPost)
		IccExist <- is.element(DatPost,tempAC[tempDat,DatCol])
		ICC[IccExist,"AcIccPost"] <- tempAC[tempDat, IccCol]

		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
	
		ICC <- zoo(ICC)
		ICC <- rollapply(ICC,FUN = CalcMean,width = Size)
			
		##list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
		}
        
ICCget1 <- function(IccCol = ICCCol, ICCTab, SDCget){
	
    ##mit 2D level und weniger Zwischentabelle
    
	DatCol  <- IccCol$get()$DatCol
	DscdCol <- IccCol$get()$DscdCol
    DscdLev <- IccCol$get()$DscdLev    
	IccCol  <- IccCol$get()$IccCol
    
    ## DatCol  <- ICCCol$get()$DatCol
	##DscdCol <- ICCCol$get()$DscdCol
    ##DscdLev <- ICCCol$get()$DscdLev    
	##IccCol  <- ICCCol$get()$IccCol
    ##DatPrae <- sort((ab$MuaDat$DatPrae))
	##DatPost <- ab$MuaDat$DatPost
	##MinObs  <-ab$MuaDat$MinObs
	##Size  <- ab$MuaDat$Size
    
    
	DatPrae <- sort((SDCget$MuaDat$DatPrae))
	DatPost <- sort(SDCget$MuaDat$DatPost)
	MinObs  <- SDCget$MuaDat$MinObs
	Size  <- SDCget$MuaDat$Size
	##DscdRows <- is.element(ICCTable[,DscdCol],c(SDCget$TargetDscd,SDCget$AcquirorDscd))  ##prüfen ob schneller
	##ICCTab  <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	ICC <- rep(NA,length(DatPrae))
	ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

        Target2D <- substring(SDCget$TargetDscd,1,2)
		Acquiror2D <- substring(SDCget$AcquirorDscd,1,2)
        
        
        
        DscdRows <- DscdLev[,Target2D]
        temp  <- ICCTab[DscdRows, DscdCol] == SDCget$TargetDscd
        DscdRows[DscdRows] <- temp
        ##tempTA <- ICCTab[DscdLev[,Target2D],] 
        ##temp  <- ICCTab[, DscdCol] == SDCget$TargetDscd
		##tempTA  <- tempTA[temp, c(DatCol,IccCol)]
        
        ##tempDat <- is.element(ICCTab[DscdRows, DatCol],DatPrae)
        ##DscdRows[DscdRows] <- tempDat
		##IccExist <- is.element(DatPrae,ICCTab[DscdRows, DatCol])
		##ICC[IccExist,"TaIcc"] <- ICCTab[DscdRows, IccCol]
        
        tempTA <- ICCTab[DscdRows, ]
		tempDat <- is.element(tempTA[,DatCol],DatPrae)
		IccExist <- is.element(DatPrae,tempTA[tempDat,DatCol])
		ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol]
		
        DscdRows <- DscdLev[,Acquiror2D]
        temp  <- ICCTab[DscdRows, DscdCol] == SDCget$AcquirorDscd
        DscdRows[DscdRows] <- temp
        		##tempAC <- ICCTab[DscdLev[,Acquiror2D],] 
		##temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
		##tempAC  <- tempAC[temp, c(DatCol,IccCol)]
        
        tempAC <- ICCTab[DscdRows, ]
		tempDat <- is.element(tempAC[,DatCol],DatPrae)
		IccExist <- is.element(DatPrae,tempAC[tempDat,DatCol])
		ICC[IccExist,"AcIccPrae"] <- tempAC[tempDat, IccCol]
		
	
		tempDat <- is.element(tempAC[,DatCol],DatPost)
		IccExist <- is.element(DatPost,tempAC[tempDat,DatCol])
		ICC[IccExist,"AcIccPost"] <- tempAC[tempDat, IccCol]

		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
	
		ICC <- zoo(ICC)
		ICC <- rollapply(ICC,FUN = CalcMean,width = Size)
			
		##list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
		}

ICCget2 <- function(IccCol = ICCCol, ICCTab, SDCget){
	
    ##mit 2D level und keine Zwischentabelle
    
	DatCol  <- IccCol$get()$DatCol
	DscdCol <- IccCol$get()$DscdCol
    DscdLev <- IccCol$get()$DscdLev    
	IccCol  <- IccCol$get()$IccCol
    
    ## DatCol  <- ICCCol$get()$DatCol
	##DscdCol <- ICCCol$get()$DscdCol
    ##DscdLev <- ICCCol$get()$DscdLev    
	##IccCol  <- ICCCol$get()$IccCol
    ##DatPrae <- sort((ab$MuaDat$DatPrae))
	##DatPost <- ab$MuaDat$DatPost
	##MinObs  <-ab$MuaDat$MinObs
	##Size  <- ab$MuaDat$Size
    
    
	DatPrae <- sort((SDCget$MuaDat$DatPrae))
	DatPost <- sort(SDCget$MuaDat$DatPost)
	MinObs  <- SDCget$MuaDat$MinObs
	Size  <- SDCget$MuaDat$Size
	##DscdRows <- is.element(ICCTable[,DscdCol],c(SDCget$TargetDscd,SDCget$AcquirorDscd))  ##prüfen ob schneller
	##ICCTab  <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	ICC <- rep(NA,length(DatPrae))
	ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

        Target2D <- substring(SDCget$TargetDscd,1,2)
		Acquiror2D <- substring(SDCget$AcquirorDscd,1,2)
        
        
        
        DscdRows <- DscdLev[,Target2D]
        temp  <- ICCTab[DscdRows, DscdCol] == SDCget$TargetDscd
        DscdRows[DscdRows] <- temp
        ##tempTA <- ICCTab[DscdLev[,Target2D],] 
        ##temp  <- ICCTab[, DscdCol] == SDCget$TargetDscd
		##tempTA  <- tempTA[temp, c(DatCol,IccCol)]
        
        tempDat <- is.element(ICCTab[DscdRows, DatCol],DatPrae)
        a<<-tempDat
        f<<-DscdRows
        DscdRows[DscdRows] <- tempDat
		IccExist <- is.element(DatPrae,ICCTab[DscdRows, DatCol])
		ICC[IccExist,"TaIcc"] <- ICCTab[DscdRows, IccCol]
        
        ##tempTA <- ICCTab[DscdRows, ]
		##tempDat <- is.element(tempTA[,DatCol],DatPrae)
		##IccExist <- is.element(DatPrae,tempTA[tempDat,DatCol])
		##ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol]
		
        DscdRows <- DscdLev[,Acquiror2D]
        temp  <- ICCTab[DscdRows, DscdCol] == SDCget$AcquirorDscd
        DscdRows[DscdRows] <- temp
        DscdRowsPrae <- DscdRows
        		##tempAC <- ICCTab[DscdLev[,Acquiror2D],] 
		##temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
		##tempAC  <- tempAC[temp, c(DatCol,IccCol)]
        
        tempDat <- is.element(ICCTab[DscdRowsPrae, DatCol],DatPrae)
        DscdRowsPrae[DscdRowsPrae] <- tempDat
		IccExist <- is.element(DatPrae,ICCTab[DscdRowsPrae, DatCol])
		ICC[IccExist,"AcIccPrae"] <- ICCTab[ DscdRowsPrae, IccCol]
        
        ##tempAC <- ICCTab[DscdRows, ]
		##tempDat <- is.element(tempAC[,DatCol],DatPrae)
		##IccExist <- is.element(DatPrae,tempAC[tempDat,DatCol])
		##tempDat <- is.element(ICCTab[DscdRows, DatCol],DatPrae)
		
        
        tempDat <- is.element(ICCTab[DscdRows, DatCol],DatPost)
        DscdRows[DscdRows] <- tempDat
		IccExist <- is.element(DatPost,ICCTab[DscdRows, DatCol])
		ICC[IccExist,"AcIccPost"] <- ICCTab[DscdRows, IccCol]
	
		##tempDat <- is.element(tempAC[,DatCol],DatPost)
		##IccExist <- is.element(DatPost,tempAC[tempDat,DatCol])
		##ICC[IccExist,"AcIccPost"] <- tempAC[tempDat, IccCol]

		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
            
	
		ICC <- zoo(ICC)
		ICC <- rollapply(ICC,FUN = CalcMean,width = Size)
			
		##list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
		}
      
ICCget3 <- function(IccCol = ICCCol, ICCTab, SDCget){
	
    ##ohne 2D 
    
    
	DatCol  <- IccCol$get()$DatCol
	DscdCol <- IccCol$get()$DscdCol
    DscdLev <- IccCol$get()$DscdLev    
	IccCol  <- IccCol$get()$IccCol
    
    ## DatCol  <- ICCCol$get()$DatCol
	##DscdCol <- ICCCol$get()$DscdCol
    ##DscdLev <- ICCCol$get()$DscdLev    
	##IccCol  <- ICCCol$get()$IccCol
    ##DatPrae <- sort((ab$MuaDat$DatPrae))
	##DatPost <- ab$MuaDat$DatPost
	##MinObs  <-ab$MuaDat$MinObs
	##Size  <- ab$MuaDat$Size
    
    
	DatPrae <- sort((SDCget$MuaDat$DatPrae))
	DatPost <- sort(SDCget$MuaDat$DatPost)
	MinObs  <- SDCget$MuaDat$MinObs
	Size  <- SDCget$MuaDat$Size
	##DscdRows <- is.element(ICCTable[,DscdCol],c(SDCget$TargetDscd,SDCget$AcquirorDscd))  ##prüfen ob schneller
	##ICCTab  <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	ICC <- rep(NA,length(DatPrae))
	ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

        
        
    
        DscdRows  <- ICCTab[, DscdCol] == SDCget$TargetDscd
        ##tempTA <- ICCTab[DscdLev[,Target2D],] 
        ##temp  <- ICCTab[, DscdCol] == SDCget$TargetDscd
		##tempTA  <- tempTA[temp, c(DatCol,IccCol)]
        
        ##tempDat <- is.element(ICCTab[DscdRows, DatCol],DatPrae)
        ##DscdRows[DscdRows] <- tempDat
		##IccExist <- is.element(DatPrae,ICCTab[DscdRows, DatCol])
		##ICC[IccExist,"TaIcc"] <- ICCTab[DscdRows, IccCol]
        
        tempTA <- ICCTab[DscdRows, ]
		tempDat <- is.element(tempTA[,DatCol],DatPrae)
		IccExist <- is.element(DatPrae,tempTA[tempDat,DatCol])
		ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol]
		
        DscdRows  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
        		##tempAC <- ICCTab[DscdLev[,Acquiror2D],] 
		##temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
		##tempAC  <- tempAC[temp, c(DatCol,IccCol)]
        
        tempAC <- ICCTab[DscdRows, ]
		tempDat <- is.element(tempAC[,DatCol],DatPrae)
		IccExist <- is.element(DatPrae,tempAC[tempDat,DatCol])
		ICC[IccExist,"AcIccPrae"] <- tempAC[tempDat, IccCol]
		
	
		tempDat <- is.element(tempAC[,DatCol],DatPost)
		IccExist <- is.element(DatPost,tempAC[tempDat,DatCol])
		ICC[IccExist,"AcIccPost"] <- tempAC[tempDat, IccCol]

		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
	
		ICC <- zoo(ICC)
		ICC <- rollapply(ICC,FUN = CalcMean,width = Size)
			
		##list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
		}
##b<-data.frame(c(53,2,54,2),c("2014-10","2014-01","2014-12","2014-01"))
##a<-data.frame(rep(NA,5))
## rownames(a)<-c("2014-10","2014-12","2015-09","2013-02","2010-06")
## a[,1]<-c("2014-10","2014-02","2015-09","2014-12","2010-06")
  
## det<-function(x){f<-x==b[,2]
##if(sum(f)==0){NA}
## else{b[f,1]}}
 
      
ICCget4 <- function(IccCol = ICCCol, ICCTab, SDCget){
	
    
    
	DatCol  <- IccCol$get()$DatCol
	DscdCol <- IccCol$get()$DscdCol
    DscdLev <- IccCol$get()$DscdLev    
	IccCol  <- IccCol$get()$IccCol
    
    ## DatCol  <- ICCCol$get()$DatCol
	##DscdCol <- ICCCol$get()$DscdCol
    ##DscdLev <- ICCCol$get()$DscdLev    
	##IccCol  <- ICCCol$get()$IccCol
    ##DatPrae <- sort((ab$MuaDat$DatPrae))
	##DatPost <- ab$MuaDat$DatPost
	##MinObs  <-ab$MuaDat$MinObs
	##Size  <- ab$MuaDat$Size
    
    
	DatPrae <- sort((SDCget$MuaDat$DatPrae))
	DatPost <- sort(SDCget$MuaDat$DatPost)
	MinObs  <- SDCget$MuaDat$MinObs
	Size  <- SDCget$MuaDat$Size
	##DscdRows <- is.element(ICCTable[,DscdCol],c(SDCget$TargetDscd,SDCget$AcquirorDscd))  ##prüfen ob schneller
	##ICCTab  <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	ICC <- rep(NA,length(DatPrae))
    ICC <- list("PostICC"=data.frame(DatPost),"PraeICC"=data.frame("Target"=DatPrae,"Acquiror"=DatPrae))
    
    ##rownames(ICC$PostICC) <- 1:length(DatPost)
    ##rownames(ICC$PraeICC) <- -(length(DatPrae):1)
    rownames(ICC$PostICC) <- DatPost
    rownames(ICC$PraeICC) <- DatPrae

        DscdRows  <- ICCTab[, DscdCol] == SDCget$TargetDscd
        ##tempTA <- ICCTab[DscdLev[,Target2D],] 
        ##temp  <- ICCTab[, DscdCol] == SDCget$TargetDscd
		##tempTA  <- tempTA[temp, c(DatCol,IccCol)]
        
        ##tempDat <- is.element(ICCTab[DscdRows, DatCol],DatPrae)
        ##DscdRows[DscdRows] <- tempDat
		##IccExist <- is.element(DatPrae,ICCTab[DscdRows, DatCol])
		##ICC[IccExist,"TaIcc"] <- ICCTab[DscdRows, IccCol]
        
        DateFind <- function(date,IccTable,Period){
                    Match<-date==IccTable[,DatCol]
                    if(sum(Match)==0){NA}
                    else{IccTable[Match,IccCol]}
                    }
        
        tempTA <- ICCTab[DscdRows, ]
		ICC$PraeICC$Target <- rollapply(ICC$PraeICC$Target,DateFind,
                                IccTable=tempTA,width=1,by.column=F)
      
        ##tempDat <- is.element(tempTA[,DatCol],DatPrae)
		##IccExist <- is.element(DatPrae,tempTA[tempDat,DatCol])
		##ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol]
		
        DscdRows  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
        		##tempAC <- ICCTab[DscdLev[,Acquiror2D],] 
		##temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
		##tempAC  <- tempAC[temp, c(DatCol,IccCol)]
        
        tempAC <- ICCTab[DscdRows, ]
        ICC$PraeICC$Acquiror <- rollapply(ICC$PraeICC$Acquiror,DateFind,
                                IccTable=tempAC,width=1,by.column=F)
		
        ICC$PostICC[,1]          <- rollapply(ICC$Post,DateFind,
                                IccTable=tempAC,width=1,by.column=F)
        

		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
        
        ICC
		ICC$PostICC <- zoo(ICC$PostICC)
        ICC$PraeICC <- zoo(ICC$PraeICC)
		ICC$PostICC <- rollapply(ICC$PostICC,FUN = CalcMean,width = Size)
		ICC$PraeICC <- rollapply(ICC$PraeICC,FUN = CalcMean,width = Size)
        ICC
		##list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
		}



## COMPget --> does the same as function ICC get but with the table and returns the mean MV
##		   instead of the mean ICC.	

##COMPget <- function(CompCol = COMPCol, CompTable, SDCget)
##		{
##		CharacCol <- CompCol$get()$CharacList
##		CharacCol <- CharacCol$MV
##		DscdCol <- CompCol$get()$DscdCol
##		DatPrae <- SDCget$MuaDat$DatPrae
##		DatPost <- SDCget$MuaDat$DatPost
				
##		CompTablePrae <- CompTable[CharacCol ,c(DscdCol, DatPrae)]
##		CompTablePost <- CompTable[CharacCol ,c(DscdCol, DatPost)]
		
##		temp <- CompTablePrae[,DscdCol] == SDCget$TargetDscd
##		CompRowTa <- CompTablePrae[temp,DatPrae]	
##		TaMv <- rowMeans(CompRowTa,na.rm=T)
		
##		temp <- CompTablePrae[,DscdCol] == SDCget$AcquirorDscd
##		CompRowAc <- CompTablePrae[temp,DatPrae]	
##		AcMvPrae <- rowMeans(CompRowAc,na.rm=T)

##		CompRowAc <- CompTablePost[temp,DatPost]	
##		AcMvPost <- rowMeans(CompRowAc,na.rm=T)
		
##		if(any(sapply(list(TaMv,AcMvPrae,AcMvPost),length)==0))
##		{return (list(TaMv = NA, AcMvPrae = NA, AcMvPost = NA))}

##		list(TaMv = TaMv, AcMvPrae = AcMvPrae, AcMvPost = AcMvPost)
##		}	

COMPget <- function(CompCol = COMPCol, CompTable, SDCget)
		{
		CharacCol <- CompCol$get()$CharacList
		CharacCol <- CharacCol$MV
		DscdCol <- CompCol$get()$DscdCol
		DatPrae <- rev(SDCget$MuaDat$DatPrae)
		DatPost <- SDCget$MuaDat$DatPost
		MinObs  <- SDCget$MuaDat$MinObs
		Size 	  <- SDCget$MuaDat$Size

		CompTablePrae <- CompTable[CharacCol ,c(DscdCol, DatPrae)]
		CompTablePost <- CompTable[CharacCol ,c(DscdCol, DatPost)]
		
		Mv <- rep(NA,length(DatPrae))
		Mv <- data.frame("TaMv" = Mv, "AcMvPrae" = Mv,"AcMvPost" = Mv)

		temp <- CompTablePrae[,DscdCol] == SDCget$TargetDscd
		Mv$TaMv <- as.numeric(CompTablePrae[temp,DatPrae])	
		
		temp <- CompTablePrae[,DscdCol] == SDCget$AcquirorDscd
		Mv$AcMvPrae <- as.numeric(CompTablePrae[temp,DatPrae])	

		Mv$AcMvPost <- as.numeric(CompTablePost[temp,DatPost])
		
		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
	
		Mv <- zoo(Mv)
		Mv <- rollapply(Mv,FUN = CalcMean,width = Size)
		Mv

		##if(any(sapply(list(TaMv,AcMvPrae,AcMvPost),length)==0))
		##{return (list(TaMv = NA, AcMvPrae = NA, AcMvPost = NA))}

		##list(TaMv = TaMv, AcMvPrae = AcMvPrae, AcMvPost = AcMvPost)
		}


## ****************************************************************************


## SumTab <- function(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,eventW){
##	laenge <- 20
##	test2 <- as.data.frame(matrix(rep(NA,13*laenge),nrow=laenge,ncol=13))
##	CharacAdd(COMPTab,COMPCol)
##	for (i in 1:laenge){ 
##	temp  <- SDCget(SDCCol,SDCTab,i,eventW)
##	temp2 <- ICCget(ICCCol,ICCTab,temp)
##	temp3 <- COMPget(COMPCol,COMPTab,temp)
	
##	test2[i,] <- c(as.character(temp$Datum), temp$AcquirorDscd, temp$TargetDscd, 
##		temp$ShareAc, temp$SicAc , temp$SicTa , NA,
##		temp2$TaIcc, temp2$AcIccPrae, temp2$AcIccPost,
##		temp3$TaMv, temp3$AcMvPrae, temp3$AcMvPost)	
##	}
##	names(test2)<-c(names(SDCCol$get()),"SicSep",names(temp2),names(temp3))
##	test2$SicSep <- SICSeparation(test2)
##	test2
##	}


SumTab1 <- function(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWset){
	laenge <- 6
    ICCTab <- ICCTab[,as.character(ICCCol$get()[c("DatCol","DscdCol","IccCol")])]
    DSCDList <- c(SDCTab[1:laenge,c("SpTaDscd")],SDCTab[1:laenge,"SpAcDscd"])

    IccSampleRows <- is.element(ICCTab[,"Company_Code"],DSCDList)

    ICCTab <- ICCTab[IccSampleRows,]
    
    DscdAdd(ICCTab,ICCCol)
    
    nobs <- ((EventWset$get()$Far - EventWset$get()$Close+1)-EventWset$get()$Size+1)
	test2 <- as.data.frame(matrix(rep(NA,(7+6*nobs)*laenge),nrow=laenge,ncol=7+6*nobs))
	CharacAdd(COMPTab,COMPCol)
	for (i in 1:laenge){ 
	temp  <- SDCget(SDCCol,SDCTab,i,EventWset)
	temp2 <- ICCget3(ICCCol,ICCTab,temp)
	temp3 <- COMPget(COMPCol,COMPTab,temp)
	
	test2[i,] <- c(as.character(temp$Datum), temp$AcquirorDscd, temp$TargetDscd, 
		temp$ShareAc, temp$SicAc , temp$SicTa , NA,
		temp2$TaIcc, temp2$AcIccPrae, temp2$AcIccPost,
		temp3$TaMv, temp3$AcMvPrae, temp3$AcMvPost)	
	}
	
	##a <- EventWset$get()$Far : (EventWset$get()$Close - EventWset$get()$Size+1)
	IccColNames <- c(paste("TaIcc",-(nobs:1),sep="_"),paste("AcIccPrae",-(nobs:1),sep="_"),
			paste("AcIccPost",(1:nobs),sep="_"))

	MvColNames  <- c(paste("TaMv",-(nobs:1),sep="_"),paste("AcMvPrae",-(nobs:1),sep="_"),
			paste("AcMvPost",(1:nobs),sep="_"))

	names(test2)<-c(names(SDCCol$get()),"SicSep",IccColNames,MvColNames)
	test2[,c("ShareCol",IccColNames,MvColNames)]<-apply(test2[,c("ShareCol",IccColNames,MvColNames)],2,as.numeric)
	test2$SicSep <- SICSeparation(test2)
	test2
	}
    
system.time(testob<-SumTab1(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWdata))

    
SumTab <- function(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWset){
	##für ICCget4
    
    laenge <- 20
    ICCTab <- ICCTab[,as.character(ICCCol$get()[c("DatCol","DscdCol","IccCol")])]
    DSCDList <- c(SDCTab[1:laenge,c("SpTaDscd")],SDCTab[1:laenge,"SpAcDscd"])

    IccSampleRows <- is.element(ICCTab[,"Company_Code"],DSCDList)

    ICCTab <- ICCTab[IccSampleRows,]
    
    nobs <- ((EventWset$get()$Far - EventWset$get()$Close+1)-EventWset$get()$Size+1)
	test2 <- as.data.frame(matrix(rep(NA,(7+6*nobs)*laenge),nrow=laenge,ncol=7+6*nobs))
	CharacAdd(COMPTab,COMPCol)
	for (i in 1:laenge){ 
	temp  <- SDCget(SDCCol,SDCTab,i,EventWset)
	temp2 <- ICCget4(ICCCol,ICCTab,temp)
	temp3 <- COMPget(COMPCol,COMPTab,temp)
	
	test2[i,] <- c(as.character(temp$Datum), temp$AcquirorDscd, temp$TargetDscd, 
		temp$ShareAc, temp$SicAc , temp$SicTa , NA,
		temp2$PraeICC$Target, temp2$PraeICC$Acquiror, temp2$PostICC,
		temp3$TaMv, temp3$AcMvPrae, temp3$AcMvPost)	
	}
	
	##a <- EventWset$get()$Far : (EventWset$get()$Close - EventWset$get()$Size+1)
	IccColNames <- c(paste("TaIcc",-(nobs:1),sep="_"),paste("AcIccPrae",-(nobs:1),sep="_"),
			paste("AcIccPost",(1:nobs),sep="_"))

	MvColNames  <- c(paste("TaMv",-(nobs:1),sep="_"),paste("AcMvPrae",-(nobs:1),sep="_"),
			paste("AcMvPost",(1:nobs),sep="_"))

	names(test2)<-c(names(SDCCol$get()),"SicSep",IccColNames,MvColNames)
	test2[,c("ShareCol",IccColNames,MvColNames)]<-apply(test2[,c("ShareCol",IccColNames,MvColNames)],2,as.numeric)
	test2$SicSep <- SICSeparation(test2)
	test2
	}
    
system.time(testob<-SumTab(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWdata))

SICSeparation <- function(SummarySdc, AcSicCol = "SicAcCol", TaSicCol = "SicTaCol")
			{
			Sic0 <- SummarySdc[, AcSicCol] == SummarySdc[, TaSicCol]
			Sic1 <- substr(SummarySdc[, AcSicCol],1,1) != substr(SummarySdc[, TaSicCol],1,1)
				temp <- (Sic1 | Sic0)
			Sic2 <- substr(SummarySdc[, AcSicCol],2,2) != substr(SummarySdc[, TaSicCol],2,2)
			Sic2 <- Sic2==T & temp==F 
				temp <- temp | Sic2
			Sic3 <- substr(SummarySdc[, AcSicCol],3,3) != substr(SummarySdc[, TaSicCol],3,3)
			Sic3 <- Sic3==T & temp==F
				temp <- temp | Sic3
			Sic4 <- substr(SummarySdc[, AcSicCol],4,4) != substr(SummarySdc[, TaSicCol],4,4)
			Sic4 <- Sic4==T & temp==F
			
			SicCol <- rep(NA,nrow(SummarySdc))
			SicCol[Sic0] <- "sameSic" 
			SicCol[Sic1] <- "firstDigit"
			SicCol[Sic2] <- "secDigit"
			SicCol[Sic3] <- "thirdDigit"
			SicCol[Sic4] <- "fourthDigit"
			SicCol <- factor(SicCol,order=T,levels=c("sameSic","fourthDigit","thirdDigit","secDigit","firstDigit"))
			SicCol
			}

##system.time(testob<-SumTab(SDCTab3,SDCset3,ICCTab3,ICCset3,COMPTab3,COMPset3,EventWset3))
##system.time(testob<-SumTab2(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWdata))
##system.time(testob<-SumTab(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWdata))

system.time(testob<-SumTab(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWdata))




## adjIccCalc <- function(SumTab,NaRe = T, SicRe = T , SIC = 7, TargetSIC = "SicAcCol", AcquirorSIC = "SicTaCol",
##		TaIccCol = "TaIcc", AcIccPraeCol = "AcIccPrae", AcIccPostCol = "AcIccPost", 
##		TaMvCol = "TaMv", AcMvPraeCol =  "AcMvPrae", AcMvPostCol = "AcMvPost"){
		
##		if(NaRe){SumTab <- naClear(SumTab)}
		
##		if(SicRe){SumTab <- removeInd(SumTab,SIC = SIC, TargetSIC = TargetSIC, 
##						AcquirorSIC = AcquirorSIC)}

##		SumTab <- ICCDiff(SumTab, TaIccCol = TaIccCol, AcIccPraeCol = AcIccPraeCol,
##		AcIccPostCol = AcIccPostCol, TaMvCol = TaMvCol, AcMvPraeCol =  AcMvPraeCol,
##		AcMvPostCol = AcMvPostCol)

##		SumTab
##		}

adjIccCalc <- function(SumTab, EventWset, NaRe = F, SicRe = F , SIC = 7, 
			TargetSIC = "SicAcCol", AcquirorSIC = "SicTaCol"){
		
		if(NaRe){SumTab <- naClear(SumTab,EventWset)}
		
		if(SicRe){SumTab <- removeInd(SumTab,SIC = SIC, TargetSIC = TargetSIC, 
						AcquirorSIC = AcquirorSIC)}

##		SumTab <- ICCDiff(SumTab, TaIccCol = TaIccCol, AcIccPraeCol = AcIccPraeCol,
##		AcIccPostCol = AcIccPostCol, TaMvCol = TaMvCol, AcMvPraeCol =  AcMvPraeCol,
##		AcMvPostCol = AcMvPostCol)
		
		SumTab <- ICCDiff(SumTab, EventWset)

		SumTab
		}

		
naClear <- function(SumTab,EventWset){
		nobs <- ((EventWset$get()$Far - EventWset$get()$Close+1)-EventWset$get()$Size+1)
		zahl <- 1:nobs
		
		TaIccCol <- paste("TaIcc",-zahl,sep="_")
		AcIccPraeCol <- paste("AcIccPrae",-zahl,sep="_")
		AcIccPostCol <- paste("AcIccPost",zahl,sep="_")
		TaMvCol <- paste("TaMv",-zahl,sep="_")
		AcMvPraeCol <-  paste("AcMvPrae",-zahl,sep="_")
		AcMvPostCol <- paste("AcMvPost",zahl,sep="_")
		WeightedIccPraeCols <- paste("WeightedIccPrae",-zahl,sep="_")
		IccDiffCols <- paste("IccDiff",zahl,sep="_")

		temp <- (SumTab[,c(TaIccCol, AcIccPraeCol, AcIccPostCol, TaMvCol,
				AcMvPraeCol, AcMvPostCol, WeightedIccPraeCols, IccDiffCols)])
			temp <- apply(is.na(temp)|temp=="NaN",1,any)
		
		SumTab[!temp,]
		}

StatCalc <- function(SumTab,EventWset,Function,...){
		nobs <- ((EventWset$get()$Far - EventWset$get()$Close+1)-EventWset$get()$Size+1)
		zahl <- 1:nobs
		
		TaIccCol <- paste("TaIcc",-zahl,sep="_") ## noch entfernen
		AcIccPraeCol <- paste("AcIccPrae",-zahl,sep="_") ## noch entfernen
		AcIccPostCol <- paste("AcIccPost",zahl,sep="_") 
		TaMvCol <- paste("TaMv",-zahl,sep="_")  ## noch entfernen
		AcMvPraeCol <-  paste("AcMvPrae",-zahl,sep="_") ## noch entfernen
		AcMvPostCol <- paste("AcMvPost",zahl,sep="_") ## noch entfernen
		WeightedIccPraeCols <- paste("WeightedIccPrae",-zahl,sep="_")
		IccDiffCols <- paste("IccDiff",zahl,sep="_")
		##a<-list(...)
		##print(a)
		##grep("Icc|Mv",colnames(SumTab))
		temp <- (SumTab[,c(TaIccCol, AcIccPraeCol, AcIccPostCol, TaMvCol,
				AcMvPraeCol, AcMvPostCol, WeightedIccPraeCols, IccDiffCols)])
		##if(length(a)==0){return("ab")}
		##else {return("pap")}

		

		
		##if(length(list(...))==0){return(apply(temp,2,Function))}
		##else {return(apply(temp,2,Function,...))}

		
		if(length(list(...))==0){f<-(apply(temp,2,Function))}
		else {f<-(apply(temp,2,Function,...))}
		
		f
		
		##rr<-sapply(list("WeightedIccPrae","AcIccPost","IccDiff"),function(x){a<-grep(x,names(f));f[a]}) ## hier noch die List über die textfeldfilterung fertig stellen 
		##return(rr)

		
		##rr<-sapply(list(TaIccCol,IccDiffCols),function(x)f[x]) ## hier noch die List über die textfeldfilterung fertig stellen 
		##return(rr)
		}
StatCalc(testob2,EventWdata,mean,na.rm=T)

removeInd <- function(SumTab,SIC = 7,TargetSIC = "SicAcCol", AcquirorSIC = "SicTaCol"){
		temp <- SumTab[,c(TargetSIC,AcquirorSIC)]
			temp <- apply(temp,2,function(x)substr(x,1,1))
			temp <- apply(temp,1,function(x)any(x==SIC))
		
		SumTab[!temp,]
		}


## ICCDiff <- function(SummarySdc, TaIccCol = "TaIcc", AcIccPraeCol = "AcIccPrae",
##		AcIccPostCol = "AcIccPost", TaMvCol = "TaMv", AcMvPraeCol =  "AcMvPrae",
##		AcMvPostCol = "AcMvPost")
##			{
##			temp <- c(TaIccCol, AcIccPraeCol, AcIccPostCol,
##				     TaMvCol, AcMvPraeCol, AcMvPostCol)
##			SummarySdc[,temp] <- apply(SummarySdc[,temp],2,as.numeric)
##			temp <- SummarySdc[,TaIccCol] * SummarySdc[,TaMvCol]
##			temp <- temp + (SummarySdc[,AcIccPraeCol] * SummarySdc[,AcMvPraeCol])
##			temp <- temp/(SummarySdc[,TaMvCol] + SummarySdc[,AcMvPraeCol])

##			SummarySdcCalc <- cbind(SummarySdc, "weightedIcc" = temp)
			
##			temp <- SummarySdc[,AcIccPostCol] - temp
##			SummarySdcCalc <- cbind(SummarySdcCalc, "diffIcc" = temp) 
##			}

ICCDiff <- function(SummarySdc,EventWset){
			nobs <- ((EventWset$get()$Far - EventWset$get()$Close+1)-EventWset$get()$Size+1)
			zahl <- 1:nobs
			TaIccCol <- paste("TaIcc",rev(-zahl),sep="_")
			AcIccPraeCol <- paste("AcIccPrae"rev(-zahl),sep="_")
			AcIccPostCol <- paste("AcIccPost",zahl,sep="_")
			TaMvCol <- paste("TaMv", rev(-zahl),sep="_")
			AcMvPraeCol <-  paste("AcMvPrae",rev(-zahl),sep="_")
			AcMvPostCol <- paste("AcMvPost",zahl,sep="_")
			##grep("Icc|Mv",colnames(testob2))
			
			temp <- c(TaIccCol, AcIccPraeCol, AcIccPostCol,
				     TaMvCol, AcMvPraeCol, AcMvPostCol)


			SummarySdc[,temp] <- apply(SummarySdc[,temp],2,as.numeric)
			temp <- SummarySdc[,TaIccCol] * SummarySdc[,TaMvCol]
			temp <- temp + (SummarySdc[,AcIccPraeCol] * SummarySdc[,AcMvPraeCol])
			temp <- temp/(SummarySdc[,TaMvCol] + SummarySdc[,AcMvPraeCol])
			temp <- as.data.frame(temp)
			colnames(temp) <-  paste("WeightedIccPrae",rev(-zahl),sep="_")
			
			SummarySdcCalc <- cbind(SummarySdc, temp)
						
			
			temp <- SummarySdc[,AcIccPostCol] - rev(temp) ### frage nahc rev
			colnames(temp) <-  paste("IccDiff",zahl,sep="_")
			
			SummarySdcCalc <- cbind(SummarySdcCalc, temp) 
			SummarySdcCalc
			}

testob2<-adjIccCalc(testob,EventWdata)

grep("Icc|Mv",colnames(testob2))

b<-data.frame(c(53,2,54,2),c("2014-10","2014-01","2014-12","2014-01"))
a<-data.frame(rep(NA,5))
 rownames(a)<-c("2014-10","2014-12","2015-09","2013-02","2010-06")
 a[,1]<-c("2014-10","2014-02","2015-09","2014-12","2010-06")
  
 det<-function(x){f<-x==b[,2]
if(sum(f)==0){NA}
 else{b[f,1]}}
 
 rollapply(a,det,width=1,by.column=F)

