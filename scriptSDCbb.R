SDCset <- function(DatCol, AcCol, TaCol, ShareCol, SicAcCol, SicTaCol)
{	get <- function()	data.frame(DatCol, AcCol, TaCol, ShareCol, 
				SicAcCol, SicTaCol, stringsAsFactors = F)

##	function() 	namecolAc
##	function()	namecolTa
	
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
	##list(get = get, setDat = setDat, 
	##	setAc = setAc, setTa = setTa)
	SDCCol
}

def <- SDCset("SP1","SP4","SP8")


ICCset <- function(DatCol, DscdCol, IccCol){
	
	get <- function()	data.frame(DatCol, DscdCol , IccCol,
					stringsAsFactors = F)
	
	setDat  <- function(DAT) DatCol <<- DAT
	setDscd <-  function(DSCD)  DscdCol <<- DSCD 	
	setIcc  <-  function(ICC)  IccCol <<- ICC	
	
	ICCCol <<- list(get = get,setDat = setDat, 
			setDscd = setDscd, setIcc = setIcc)
	##list(get = get,setDat = setDat, 
	##	setDscd = setDscd, setIcc = setIcc)
	
	ICCCol
	##noch vll ein dummy argument hinzufügen um von SDCget abzugrenzen
}


COMPset <- function(DscdCol, MvCol, SicCol)
{	get <- function()	data.frame(DscdCol, MvCol, SicCol, stringsAsFactors=F)
	
	setDscd <- function(DSCD) DscdCol <<- DSCD
	setMv   <- function(MV)  MvCol <<- MV 	
	setSic  <- function(SIC) SicCol <<- SIC	

	COMPCol <<- list(get = get, setDscd = setDscd, 
			setMv = setMv, setSic = setSic)
	
	##list(get = get, setDSCD = setDSCD, 
	##	setMV = setMV)
	COMPCol
}


EventWset <- function(PraeStart, PraeLast, PostStart, PostLast, MinObs)
	{	
	list(PraeStart = PraeStart, PraeLast = PraeLast, 
		PostStart = PostStart, PostLast = PostLast,
		MinObs = MinObs)
	}


EventWget <- function(Datum, EventW)
	{
	Datum <- as.POSIXlt(Datum) 
	DatPost <- rep(Datum,EventW$PraeLast - EventW$PraeStart)
	DatPrae <- rep(Datum,EventW$PostLast - EventW$PostStart)

	
	DatPrae$mon <- (EventW$PraeStart:EventW$PraeLast)+DatPrae$mon
	DatPost$mon <- (EventW$PostStart:EventW$PostLast)+DatPost$mon
	DatPrae <- substr(as.Date(DatPrae),1,7)
	DatPost <- substr(as.Date(DatPost),1,7)
	MinObs <- EventW$MinObs
	
	list(DatPrae = DatPrae, DatPost = DatPost, MinObs = MinObs)
	}


SDCget <- function(SdcCol = SDCCol, SDCTable, SDCRow, EVENTWset)
	{
	DatCol 	<- SdcCol$get()$DatCol ## "$DatCol" kommt von SDC get() data.frame	
	AcquirorCol <- SdcCol$get()$AcCol
	TargetCol   <- SdcCol$get()$TaCol
	ShareAcCol  <- SdcCol$get()$ShareCol
	SicAcqCol	<- SdcCol$get()$SicAcCol
	SicTarCol   <- SdcCol$get()$SicTaCol
	
	Datum     <- SDCTable[SDCRow,DatCol]
	
	MuaDat      <- EventWget(Datum, EVENTWset)

	## MuaDat	 <- strptime(MuaDat,"%m.%d.%Y")
	##MuaDatPrae <- as.POSIXlt(MuaDat)
	##MuaDatPrae$mon <- MuaDatPrae$mon - EventW
	##MuaDatPrae <- as.Date(MuaDatPrae)
	##MuaDatPost <- as.POSIXlt(MuaDat)
	##MuaDatPost$mon <- MuaDatPost$mon + EventW
 	##MuaDatPost <- as.Date(MuaDatPost)

	AcquirorDscd <- SDCTable[SDCRow,AcquirorCol]
	TargetDscd   <- SDCTable[SDCRow,TargetCol]
	ShareAc      <- SDCTable[SDCRow,ShareAcCol]
	SicAc		 <- SDCTable[SDCRow,SicAcqCol]	
	SicTa		 <- SDCTable[SDCRow,SicTarCol]	
	
	list( Datum = Datum, MuaDat = MuaDat, 
		AcquirorDscd = AcquirorDscd, TargetDscd = TargetDscd, 
		ShareAc = ShareAc, SicAc = SicAc, SicTa = SicTa)
}


ICCget <- function(IccCol = ICCCol, ICCTable, SDCget){
	DatCol  <- IccCol$get()$DatCol ## "$DatCol" kommt von ICC get() data.frame
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
		##if (sum(tempDat) < MinObs){return (Abbruch)}
		TaIcc <- tempTA[tempDat, IccCol]
		TaIcc <- TaIcc[!is.na(TaIcc)]
		if(length(TaIcc) < MinObs){return (Abbruch)}
		TaIcc <- mean(TaIcc, na.rm = TRUE)
		
		temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
		tempAC  <- ICCTab[temp, c(DatCol,IccCol)]
		tempDat <- is.element(substr(tempAC[,DatCol],1,7),DatPrae)
		##if (sum(tempDat) < MinObs){return (Abbruch)}
		AcIccPrae <- tempAC[tempDat, IccCol]
		AcIccPrae <- AcIccPrae[!is.na(TaIcc)]
		if(length(AcIccPrae) < MinObs){return (Abbruch)}
		AcIccPrae <- mean(AcIccPrae, na.rm = TRUE)
	
		tempDat <- is.element(substr(tempAC[,DatCol],1,7),DatPost)
		##if (sum(tempDat) < MinObs){return (Abbruch)}
		AcIccPost <- tempAC[tempDat, IccCol]
		AcIccPost <- AcIccPost[!is.na(AcIccPost)]
		if(length(AcIccPost) < MinObs){return (Abbruch)}
		AcIccPost <- mean(AcIccPost, na.rm = TRUE)
		
		##if(any(sapply(list(AcIccPost,AcIccPrae,TaIcc),length)==0)
		##	|any(is.na(c(AcIccPost,AcIccPrae,TaIcc))))
		##{return (Abbruch)}
		
		##if(length(AcIccPost)==0|length(AcIccPrae)==0|length(TaIcc)==0
		##|any(is.na(c(AcIccPost,AcIccPrae,TaIcc))))
		##{return (Abbruch)}
		
		list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
}


COMPget <- function(CompCol = COMPCol, COMPTable, SDCget)
		{
		DscdCol <- CompCol$get()$DscdCol
		MvCol
		SicCol  <- CompCol$get()$SicCol	
		DatPrae <- SDCget$MuaDatPrae
		DatPost <- SDCget$MuaDatPost	

		##DatPrae <- SDCget$MuaDat
		##DatPrae$mon <- DatPrae$mon - EventW
		##DatPost <- SDCget$MuaDat
		##DatPost$mon <- DatPost$mon + EventW
		
		DatColPrae <- substr(colnames(COMPTable),1,7) == substr(DatPrae,1,7)
		DatColPost <- substr(colnames(COMPTable),1,7) == substr(DatPost,1,7)

		temp <- COMPTable[, DscdCol] == SDCget$TargetDscd
		## temp <- COMPTable[temp, MvCol] == 
		TaMv <- COMPTable[temp, DatColPrae]		
		TaSic <- COMPTable[temp, SicCol]
	
		temp <- COMPTable[, DscdCol] == SDCget$AcquirorDscd
		## temp <- COMPTable[temp, MvCol] ==
		AcMvPrae <- COMPTable[temp, DatColPrae] 	
		AcMvPost <- COMPTable[temp, DatColPost] 	
		AcSic <- COMPTable[temp, SicCol]
	
		list(TaMv = TaMv, AcMvPrae = AcMvPrae, AcMvPost = AcMvPost,
		TaSic = TaSic, AcSic = AcSic)
	}	


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


ICCDiff <- function(SummarySdc, TaIccCol = TaIcc, AcIccPraeCol = AcIccPrae,
		AcIccPostCol = AcIccPost, TaMvCol = TaMv, AcMvPraeCol =  AcMvPrae
		AcMvPostCol = AcMvPost)
			{
			temp <- SummarySdc$TaIccCol * SummarySdc$TaMvCol
			temp <- temp + (SummarySdc$AcIccPraeCol * SummarySdc$AcMvPraeCol)
			temp <- temp/(SummarySdc$TaMvCol + SummarySdc$AcMvPraeCol)

			SummarySdcCalc <- cbind(SummarySdc, "weightedIcc" = temp)
			
			temp <- SummarySdc$AcIccPostCol - temp
			SummarySdcCalc <- cbind(SummarySdcCalc, "diffIcc" = temp) 
			}



SICSeparation <- function(SummarySdc, AcSicCol, TaSicCol)
			{	
			SummarySdc$TaIcc <- as.numeric(SummarySdc$TaIcc)
			SummarySdc$AcIccPrae <- as.numeric(SummarySdc$AcIccPrae)
			SummarySdc$AcIccPost <- as.numeric(SummarySdc$AcIccPost)

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

			list("firstDigit" = SummarySdc[Sic1,], "secDigit" = SummarySdc[Sic2,], 
			"thirdDigit" = SummarySdc[Sic3,], "fourthDigit" = SummarySdc[Sic4,], 
			"sameSic" = SummarySdc[Sic0,])
			} 

Summarylist	<- function(SicSeperation)
			{
			temp <- SicSeperation$firstDigit[,c("TaIcc","AcIccPrae","AcIccPost")]
				print("firstDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$secDigit[,c("TaIcc","AcIccPrae","AcIccPost")]
				print("secDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$thirdDigit[,c("TaIcc","AcIccPrae","AcIccPost")]
				print("thirdDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$fourthDigit[,c("TaIcc","AcIccPrae","AcIccPost")]
				print("fourthDigit")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			temp <- SicSeperation$sameSic[,c("TaIcc","AcIccPrae","AcIccPost")]
				print("sameSic")
				temp2<-nrow(temp)	
				print(temp2)
				print(summary(temp))
			}


lapply(x, runif, min = 0, max = 10)
lapply(SICSaperation, mean)
sapply(SICSaperation, function(x)mean(x$diffIcc))
	
testF <- function(SDCTab,SDCCol,eventW){
	laenge <- 1800
	test2 <- as.data.frame(matrix(rep(NA,6*laenge),nrow=laenge,ncol=6))
	for (i in 1:laenge){ 
	temp <- SDCget(SDCCol,SDCTab,i,eventW)
	test2[i,] <- c(as.character(temp$Datum), temp$AcquirorDscd, temp$TargetDscd, 
			temp$ShareAc, temp$SicAc, temp$SicTa)	
	}
	names(test2)<-names(SDCCol$get())
	test2
	}

testG <- function(SDCTab,SDCCol,ICCCol,ICCTab,eventW){
	laenge <- 1800
	test2 <- as.data.frame(matrix(rep(NA,9*laenge),nrow=laenge,ncol=9))
	for (i in 1:laenge){ 
	temp  <- SDCget(SDCCol,SDCTab,i,eventW)
	temp2 <- ICCget(ICCCol,ICCTab,temp)
	
	##temp2$TaIcc <- if(length(temp2$TaIcc) == 0)NA else temp2$TaIcc
	##temp2$AcIccPrae <- if(length(temp2$AcIccPrae) == 0)NA else temp2$AcIccPrae
	##temp2$AcIccPost <- if(length(temp2$AcIccPost) == 0)NA else temp2$AcIccPost

	test2[i,] <- c(as.character(temp$Datum), temp$AcquirorDscd, temp$TargetDscd, 
		temp$ShareAc, temp$SicAc , temp$SicTa ,
		temp2$TaIcc, temp2$AcIccPrae, temp2$AcIccPost)	
	}
	names(test2)<-c(names(SDCCol$get()),names(temp2))
	test2
	}
testob<-testG(SDCTab,SDCCol,ICCCol,ICCTab,12)


if((length(a)==0|length(a)==0|length(c)==0)|(any(is.na(c(a,b,c))))
		{ (list(TaIcc = NA, AcIccPrae = NA, AcIccPost = NA))}
