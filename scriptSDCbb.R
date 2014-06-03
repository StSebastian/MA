
## *************setting table columns and event window**************

## functions: SDCset, ICCset, COMPset, EventWset 

## SDCset --> selecting M&A Date column, Acquiror (DSCD) column, Target (DSCD) Column
##         	 	      Percentage of Shares Acquired column, 
##			      SIC Code Acquiror column, SIC Code Target column
## names/headlines of the corrsponding colums should be given as arguments
## of type char to the function when called   			   

SDCset <- function(DatCol, AcCol, TaCol, ShareCol, SicAcCol, SicTaCol){    
	
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

ICCset <- function(DatCol, DscdCol, IccCol){
	
	get <- function()	data.frame(DatCol, DscdCol , IccCol,
					stringsAsFactors = F)
	
	setDat  <- function(DAT) DatCol <<- DAT
	setDscd <-  function(DSCD)  DscdCol <<- DSCD 	
	setIcc  <-  function(ICC)  IccCol <<- ICC	
	
	ICCCol <<- list(get = get,setDat = setDat, 
			setDscd = setDscd, setIcc = setIcc)
	
	ICCCol
	}



## COMPset --> selecting Company's (DSCD) column, column of respective 
##         	 	       Company's Characteristics, Company's SIC Code column
## names/headlines of the corrsponding colums should be given as arguments
## of type char to the function when called 

COMPset <- function(DscdCol, CharacCol, SicCol)
	{	
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

EventWset <- function(PraeStart, PraeLast, PostStart, PostLast, MinObs)
	{	
	list(PraeStart = PraeStart, PraeLast = PraeLast, 
		PostStart = PostStart, PostLast = PostLast,
		MinObs = MinObs)
	}

SDCset3 <- SDCset("SpDate", "SpAcDscd", "SpTaDscd", "SpShAcq", "SpAcSic", "SpTaSic") 
ICCset3 <- ICCset("Datum", "Company_Code", "ICC_CT")
COMPset3 <- COMPset("DSCD", "KPI", "WC07021")
EventWset3 <- EventWset(-19, -2, 2, 19, 4)


## *************modifying COMPTable**************

## NameCompCol --> convertes the tables column names to a required standard. Which will be
## 			 "yyyy-mm" for all columns which refer to particular Date.	
## First argument is the stored table with the time series' of the company's characteristics, 
## the seccond argument the number of columns prior to the first time column and the third
## the starting date of the time series in the format "yyyy-mm-dd" (only year and month are
## truly relevant).

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

CompTab3 <- NameCompCol(CompTab3,5,"1978-01-01")

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



## ExtendFun --> Adds further empty columns to the company's characteristics table to account for
##               possible post M&A periods reaching beyond the last date of the time series.
## First argument is company's characteristics table, seccond the numer of colums to be added

ExtendFun <- function(COMPTab){
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time1)	
		newRow <- rep(NA,nrow(COMPTab))
		newFrame <- sapply(1:24,function(x){newRow})
		getDate <- function(x){
			time$mon <- time$mon+x
			substr(as.Date(time),1,7)
			}
		colDates <- getDate(1:24)	
		colnames(newFrame)<-colDates
		newFrame
		COMPTab <- cbind(COMPTab,newFrame)
		}


COMPset3 <- CharacAdd(COMPTab3, COMPset3)
COMPTab3 <- ExtendFun(COMPTab3,24)		


## *************functions repeated for every M&A**************


## SDCget --> take the object storing the SDC columns, the SDC Table, 
##		  the row of the SDC Table (Selection of particular M&A) and 
##		  the object storing the Event Window details as argument. 
##		  In return it stores the SDC details SDC date, SDC prae and 
##		  post Event Window period, Acquiror and Target DSCD, the percentage 
##		  of shares acquired and the SIC Code of the Acquiror and the Target company   

SDCget <- function(SdcCol = SDCCol, SDCTable, SDCRow, EVENTWset){
	
	DatCol 	<- SdcCol$get()$DatCol	
	AcquirorCol <- SdcCol$get()$AcCol
	TargetCol   <- SdcCol$get()$TaCol
	ShareAcCol  <- SdcCol$get()$ShareCol
	SicAcqCol	<- SdcCol$get()$SicAcCol
	SicTarCol   <- SdcCol$get()$SicTaCol
	
	Datum     <- SDCTable[SDCRow,DatCol]
	
	MuaDat      <- EventWget(Datum, EVENTWset) ## function will be explained next

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

EventWget <- function(Datum, EventW)
	{
	Datum <- as.POSIXlt(Datum) 
	DatPrae <- rep(Datum,EventW$PraeLast - EventW$PraeStart+1)
	DatPost <- rep(Datum,EventW$PostLast - EventW$PostStart+1)

	
	DatPrae$mon <- (EventW$PraeStart:EventW$PraeLast)+DatPrae$mon
	DatPost$mon <- (EventW$PostStart:EventW$PostLast)+DatPost$mon
	DatPrae <- substr(as.Date(DatPrae),1,7)
	DatPost <- substr(as.Date(DatPost),1,7)
	MinObs <- EventW$MinObs
	
	list(DatPrae = DatPrae, DatPost = DatPost, MinObs = MinObs)
	}


## ICCget --> takes the object storing the ICC Columns, the ICC table and
##		  the SDC object created by the SDCget() function and unique to
##		  each M&A.
##		  It gives the mean ICCs for the Acquiror and Target for the period
##		  prior to the M&A back and the mean ICC for the Acquiror after the
##            M&A. 

ICCget <- function(IccCol = ICCCol, ICCTable, SDCget){
	
	DatCol  <- IccCol$get()$DatCol
	DscdCol <- IccCol$get()$DscdCol
	IccCol  <- IccCol$get()$IccCol
	DatPrae <- SDCget$MuaDat$DatPrae
	DatPost <- SDCget$MuaDat$DatPost
	MinObs  <- SDCget$MuaDat$MinObs
	ICCTab  <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	
	Abbruch <- list(TaIcc = NA, AcIccPrae = NA, AcIccPost = NA)
	if(is.na(SDCget$TargetDscd)|is.na(SDCget$AcquirorDscd))
	{return (Abbruch)}
		
		temp  <- ICCTab[, DscdCol] == SDCget$TargetDscd
		tempTA  <- ICCTab[temp, c(DatCol,IccCol)]
		tempDat <- is.element(substr(tempTA[,DatCol],1,7),DatPrae)
		TaIcc <- tempTA[tempDat, IccCol]
		TaIcc <- TaIcc[!is.na(TaIcc)]
		if(length(TaIcc) < MinObs){return (Abbruch)}
		TaIcc <- mean(TaIcc, na.rm = TRUE)
		
		temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
		tempAC  <- ICCTab[temp, c(DatCol,IccCol)]
		tempDat <- is.element(substr(tempAC[,DatCol],1,7),DatPrae)
		AcIccPrae <- tempAC[tempDat, IccCol]
		AcIccPrae <- AcIccPrae[!is.na(TaIcc)]	## müsste AcIccPrae anstelle TaIcc sein
		if(length(AcIccPrae) < MinObs){return (Abbruch)}
		AcIccPrae <- mean(AcIccPrae, na.rm = TRUE)
	
		tempDat <- is.element(substr(tempAC[,DatCol],1,7),DatPost)
		AcIccPost <- tempAC[tempDat, IccCol]
		AcIccPost <- AcIccPost[!is.na(AcIccPost)]
		if(length(AcIccPost) < MinObs){return (Abbruch)}
		AcIccPost <- mean(AcIccPost, na.rm = TRUE)
				
		list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
		}



## COMPget --> does the same as function ICC get but with the table and returns the mean MV
##		   instead of the mean ICC.	

COMPget <- function(CompCol = COMPCol, CompTable, SDCget)
		{
		CharacCol <- CompCol$get()$CharacList
		CharacCol <- CharacCol$MV
		DscdCol <- CompCol$get()$DscdCol
		DatPrae <- SDCget$MuaDat$DatPrae
		DatPost <- SDCget$MuaDat$DatPost
		
		CompTablePrae <- CompTable[CharacCol ,c(DscdCol, DatPrae)]
		CompTablePost <- CompTable[CharacCol ,c(DscdCol, DatPost)]
		
		temp <- CompTablePrae[,DscdCol] == SDCget$TargetDscd
		CompRowTa <- CompTablePrae[temp,DatPrae]	
		TaMv <- rowMeans(CompRowTa,na.rm=T)
		
		temp <- CompTablePrae[,DscdCol] == SDCget$AcquirorDscd
		CompRowAc <- CompTablePrae[temp,DatPrae]	
		AcMvPrae <- rowMeans(CompRowAc,na.rm=T)

		CompRowAc <- CompTablePost[temp,DatPost]	
		AcMvPost <- rowMeans(CompRowAc,na.rm=T)
		
		if(any(sapply(list(TaMv,AcMvPrae,AcMvPost),length)==0))
		{return (list(TaMv = NA, AcMvPrae = NA, AcMvPost = NA))}

		list(TaMv = TaMv, AcMvPrae = AcMvPrae, AcMvPost = AcMvPost)
		}	



## ****************************************************************************


SumTab <- function(SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,eventW){
	laenge <- 300
	test2 <- as.data.frame(matrix(rep(NA,13*laenge),nrow=laenge,ncol=13))
	CharacAdd(COMPTab,COMPCol)
	for (i in 1:laenge){ 
	temp  <- SDCget(SDCCol,SDCTab,i,eventW)
	temp2 <- ICCget(ICCCol,ICCTab,temp)
	temp3 <- COMPget(COMPCol,COMPTab,temp)
	
	test2[i,] <- c(as.character(temp$Datum), temp$AcquirorDscd, temp$TargetDscd, 
		temp$ShareAc, temp$SicAc , temp$SicTa , NA,
		temp2$TaIcc, temp2$AcIccPrae, temp2$AcIccPost,
		temp3$TaMv, temp3$AcMvPrae, temp3$AcMvPost)	
	}
	names(test2)<-c(names(SDCCol$get()),"SicSep",names(temp2),names(temp3))
	test2$SicSep <- SICSeparation(test2)
	test2
	}

system.time(testob<-SumTab(SDCTab3,SDCset3,ICCTab3,ICCset3,COMPTab3,COMPset3,EventWset3))


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
			SicCol
			}


testob1<-naClear(testob)
 testob1<-removeInd(testob1)
testob1<-ICCDiff(testob1)

adjIccCalc <- function(SumTab,NaRe = T, SicRe = T , SIC = 7, TargetSIC = "SicAcCol", AcquirorSIC = "SicTaCol",
		TaIccCol = "TaIcc", AcIccPraeCol = "AcIccPrae", AcIccPostCol = "AcIccPost", 
		TaMvCol = "TaMv", AcMvPraeCol =  "AcMvPrae", AcMvPostCol = "AcMvPost"){
		
		if(NaRe){SumTab <- naClear(SumTab)}
		
		if(SicRe){SumTab <- removeInd(SumTab,SIC = SIC, TargetSIC = TargetSIC, 
						AcquirorSIC = AcquirorSIC)}

		SumTab <- ICCDiff(SumTab, TaIccCol = TaIccCol, AcIccPraeCol = AcIccPraeCol,
		AcIccPostCol = AcIccPostCol, TaMvCol = TaMvCol, AcMvPraeCol =  AcMvPraeCol,
		AcMvPostCol = AcMvPostCol)

		SumTab
		}

		
naClear <- function(SumTab){
		temp <- (SumTab[,c("TaIcc", "AcIccPrae", "AcIccPost", 
			            "TaMv", "AcMvPrae", "AcMvPost")])
			temp <- apply(is.na(temp)|temp=="NaN",1,any)
		
		SumTab[!temp,]
		}


removeInd <- function(SumTab,SIC = 7,TargetSIC = "SicAcCol", AcquirorSIC = "SicTaCol"){
		temp <- SumTab[,c(TargetSIC,AcquirorSIC)]
			temp <- apply(temp,2,function(x)substr(x,1,1))
			temp <- apply(temp,1,function(x)any(x==SIC))
		
		SumTab[!temp,]
		}


ICCDiff <- function(SummarySdc, TaIccCol = "TaIcc", AcIccPraeCol = "AcIccPrae",
		AcIccPostCol = "AcIccPost", TaMvCol = "TaMv", AcMvPraeCol =  "AcMvPrae",
		AcMvPostCol = "AcMvPost")
			{
			temp <- c(TaIccCol, AcIccPraeCol, AcIccPostCol,
				     TaMvCol, AcMvPraeCol, AcMvPostCol)
			SummarySdc[,temp] <- apply(SummarySdc[,temp],2,as.numeric)
			temp <- SummarySdc[,TaIccCol] * SummarySdc[,TaMvCol]
			temp <- temp + (SummarySdc[,AcIccPraeCol] * SummarySdc[,AcMvPraeCol])
			temp <- temp/(SummarySdc[,TaMvCol] + SummarySdc[,AcMvPraeCol])

			SummarySdcCalc <- cbind(SummarySdc, "weightedIcc" = temp)
			
			temp <- SummarySdc[,AcIccPostCol] - temp
			SummarySdcCalc <- cbind(SummarySdcCalc, "diffIcc" = temp) 
			}


Summarylist	<- function(SicSeperation)
			{
			temp <- SicSeperation$firstDigit[,c("TaIcc","AcIccPrae","AcIccPost",
									"weightedIcc","diffIcc")]
				print("firstDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$secDigit[,c("TaIcc","AcIccPrae","AcIccPost",
									"weightedIcc","diffIcc")]
				print("secDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$thirdDigit[,c("TaIcc","AcIccPrae","AcIccPost",
									"weightedIcc","diffIcc")]
				print("thirdDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$fourthDigit[,c("TaIcc","AcIccPrae","AcIccPost",
									"weightedIcc","diffIcc")]
				print("fourthDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$sameSic[,c("TaIcc","AcIccPrae","AcIccPost",
									"weightedIcc","diffIcc")]
				print("sameSic")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			}


## ****************************************************************************


MUAset <- function(SDCRow, SdcCol = SDCCol , IccCol = ICCCol, 
		CompCol = COMPCol, SDCTable, ICCTable, COMPTable, EventW)
			{
	 		MuaData  <- SDCget(SdcCol, SDCRow, SDCTable)
			IccData  <- ICCget(IccCol, ICCTable, MuaData, EventW)
			CompData <- COMPget (CompCol, COMPTable, MuaData, EventW)
		
			data.frame(	c(MuaDat = MuaData$MuaDat, AcquirorDscd = AcMuaData$AcquirorDscd, 
					TargetDscd = MuaData$TargetDscd, TaIcc = IccData$TaIcc, 
					AcIccPrae = IccData$AcIccPrae, AcIccPost = IccData$AcIccPost, 
					TaMv = CompData$TaMv, AcMvPrae = CompData$AcMvPrae, 
					AcMvPost = CompData$AcMvPost, TaSic = CompData$TaSic,
					AcSic = CompData$AcSic, ShareAc = MuaData$ShareAc
					))
			}

makeList <- function(SdcCol = SDCCol , IccCol = ICCCol, CompCol = COMPCol, 
		SDCTable, ICCTable, COMPTable, EventW)
			{
			intit<- MuAset(1,SdcCol, IccCol, CompCol,
				SDCTable, ICCTable, COMPTable, EventW)

			mat <- matrix(rep(NA,nrow(SDCTable)*ncol(intit),
				nrow = nrow(), ncol = ncol(intit)
			SummarySdc <- as.data.frame(mat)
			SummarySdc [1,] <- intit
			
				for (i in 2:nrow()){
				temp <- MuAset(i, SdcCol, IccCol, CompCol,
				SDCTable, ICCTable, COMPTable, EventW)
				SummarySdc[i,] <- temp
				}
			
			colnames(SummarySdc)<-names(temp)
			SummarySdc
			}	


makeList <- function(SdcCol = SDCCol , IccCol = ICCCol, CompCol = COMPCol, 
		SDCTable, ICCTable, COMPTable, EventW)
			{
			mat <- matrix(rep(NA,nrow()*SP),nrow=nrow(),ncol=SP)
			SummarySdc <- as.data.frame(mat)
				
				for (i in 1:nrow()){
				temp <- MuAset(i, SdcCol, IccCol, CompCol,
				SDCTable, ICCTable, COMPTable, EventW)
				SummarySdc[i,] <- temp
				}
			
			colnames(SummarySdc)<-names(temp)
			SummarySdc
			}		


lapply(x, runif, min = 0, max = 10)
lapply(SICSaperation, mean)
sapply(SICSaperation, function(x)mean(x$diffIcc))
	
testF <- function(SDCTab,SDCCol,COMPTab,COMPCol,eventW){
	laenge <- 20
	test2 <- as.data.frame(matrix(rep(NA,9*laenge),nrow=laenge,ncol=9))
	CharacAdd(COMPTab,COMPCol)
	##a<-str(COMPCol$get())
	##print(a)
	for (i in 1:laenge){ 
	temp <- SDCget(SDCCol,SDCTab,i,eventW)
	temp2 <- COMPget(COMPCol, COMPTab, temp)
	
	temp2$TaMv <- if(length(temp2$TaMv) == 0)NA else temp2$TaMv
	temp2$AcMvPrae <- if(length(temp2$AcMvPrae) == 0)NA else temp2$AcMvPrae
	temp2$AcMvPost <- if(length(temp2$AcMvPost) == 0)NA else temp2$AcMvPost

	test2[i,] <- c(as.character(temp$Datum), temp$AcquirorDscd, temp$TargetDscd, 
			temp$ShareAc, temp$SicAc, temp$SicTa,
			temp2$TaMv, temp2$AcMvPrae, temp2$AcMvPost)	
	}
	names(test2)<-c(names(SDCCol$get()),names(temp2))
	test2
	}
test1<-testF(SDCTab3,SDCset3,COMPTab3,COMPset3,EventWset3)



temp1 <- SDCget(SDCset3, SDCTab3, 1, EventWset3)
temp2 <- ICCget(ICCset3, ICCTab3, temp1)
temp3 <- COMPget(COMPset3, COMPTab3, temp1)

##sum(substr(testob[,"SicAcCol"],1,1)==7|substr(testob[,"SicTaCol"],1,1)==7)

if((length(a)==0|length(a)==0|length(c)==0)|(any(is.na(c(a,b,c))))
		{ (list(TaIcc = NA, AcIccPrae = NA, AcIccPost = NA))}
