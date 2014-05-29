SDCset<-function(DatCol, AcCol, TaCol, ShareCol, SicAcCol, SicTaCol)
{	get<-function()	data.frame(DatCol, AcCol, TaCol, ShareCol, 
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

def<-SDCset("SP1","SP4","SP8")


ICCset <- function(DatCol, DscdCol, IccCol, SicCol){
	
	get <- function()	data.frame(DatCol, DscdCol , IccCol, SicCol,
					stringsAsFactors = F)
	
	setDat  <- function(DAT) DatCol <<- DAT
	setDscd <-  function(DSCD)  DscdCol <<- DSCD 	
	setIcc  <-  function(ICC)  IccCol <<- ICC
	setSic  <- function(SIC) SicCol <<- SIC	
	
	ICCCol <<- list(get = get,setDat = setDat, 
			setDscd = setDscd, setIcc = setIcc)
	##list(get = get,setDat = setDat, 
	##	setDscd = setDscd, setIcc = setIcc)
	
	ICCCol
	##noch vll ein dummy argument hinzufügen um von SDCget abzugrenzen
}


COMPset<-function(DscdCol, MvCol, SicCol)
{	get<-function()	data.frame(DscdCol, MvCol, SicCol, stringsAsFactors=F)
	
	setDscd <- function(DSCD) DscdCol <<- DSCD
	setMv   <- function(MV)  MvCol <<- MV 	
	setSic  <- function(SIC) SicCol <<- SIC	

	COMPCol <<- list(get = get, setDscd = setDscd, 
			setMv = setMv, setSic = setSic)
	
	##list(get = get, setDSCD = setDSCD, 
	##	setMV = setMV)
	COMPCol
}


SDCget<-function(SdcCol = SDCCol, SDCTable, SDCRow, EventW){
	
	DatCol 	<- SdcCol$get()$DatCol ## "$DatCol" kommt von SDC get() data.frame	
	AcquirorCol <- SdcCol$get()$AcCol
	TargetCol   <- SdcCol$get()$TaCol
	ShareAcCol  <- SdcCol$get()$ShareCol
	SicAcqCol	<- SdcCol$get()$SicAcCol
	SicTarCol    <- SdcCol$get()$SicTaCol

	MuaDat       <- SDCTable[SDCRow,DatCol]
	## MuaDat	 <- strptime(MuaDat,"%m.%d.%Y")
		
	MuaDatPrae <- as.POSIXlt(MuaDat)
	MuaDatPrae$mon <- MuaDatPrae$mon - EventW
	MuaDatPrae <- as.Date(MuaDatPrae)
	MuaDatPost <- as.POSIXlt(MuaDat)
	MuaDatPost$mon <- MuaDatPost$mon + EventW
 	MuaDatPost <- as.Date(MuaDatPost)

	AcquirorDscd <- SDCTable[SDCRow,AcquirorCol]
	TargetDscd   <- SDCTable[SDCRow,TargetCol]
	ShareAc      <- SDCTable[SDCRow,ShareAcCol]
	SicAc		 <- SDCTable[SDCRow,SicAcqCol]	
	SicTa		 <- SDCTable[SDCRow,SicTarCol]	
	
	list( MuaDat= MuaDat, MuaDatPrae = MuaDatPrae, MuaDatPost = MuaDatPost, 
		AcquirorDscd = AcquirorDscd, TargetDscd = TargetDscd, 
		ShareAc = ShareAc, SicAc = SicAc, SicTa = SicTa)
}


ICCget <- function(IccCol = ICCCol, ICCTable, SDCget){
	DatCol <- IccCol$get()$DatCol ## "$DatCol" kommt von ICC get() data.frame
	DscdCol <- IccCol$get()$DscdCol
	IccCol   <- IccCol$get()$IccCol
	DatPrae <- SDCget$MuaDatPrae
	DatPost <- SDCget$MuaDatPost
	ICCTab <- ICCTable[,c(DatCol,DscdCol,IccCol)]
	
	##DatPrae <- as.POSIXlt(SDCget$MuaDat)
	##DatPrae$mon <- DatPrae$mon - EventW
	##DatPrae <- as.Date(DatPrae)
	##DatPost <- as.POSIXlt(SDCget$MuaDat)
	##DatPost$mon <- DatPost$mon + EventW
 	##DatPost <- as.Date(DatPost)
	
	## für SCD Daten brauchst du auch noch prä und post Datum
	temp  <- ICCTab[, DscdCol] == SDCget$TargetDscd
	temp  <- ICCTab[temp, c(DatCol,IccCol)]
	tempDat <- substr(temp[,DatCol],1,7)== substr(DatPrae,1,7)
	TaIcc <- temp[tempDat, IccCol]
	##temp  <- ICCTable[temp, DatColPrae] == DatPrae 	##-x window
	##temp 	<- ICCTable[temp, DatCol]
	##temp  <- strptime(temp,"%d/%m/%y")				## hier noch formatierung prüfen
	##temp  <- (temp$mon == (DatPrae$mon)) && (temp$mon$year == DatPrae$year)
	##temp  <- substr(temp,1,7)== substr(DatPrae,1,7)
	##TaIcc <- ICCTable[temp, IccCol]
	
	temp  <- ICCTab[, DscdCol] == SDCget$AcquirorDscd
	tempAC  <- ICCTab[temp, c(DatCol,IccCol)]
	tempDat <- substr(tempAC[,DatCol],1,7)== substr(DatPrae,1,7)
	AcIccPrae <- tempAC[tempDat, IccCol]
	##temp  <- ICCTable[, DscdCol] == SDCget$AcquirorDscd
	##tempprae  <- ICCTable[temp, DatColPrae] == DatPrae 	## hier noch für die Daten aus ICC Tabelle konv
	##temp 	<- ICCTable[temp, DatCol]
	##temp  <- strptime(temp,"%d/%m/%y")				## hier noch formatierung prüfen
	##temp  <- (temp$mon == (DatPrae$mon)) && (temp$mon$year == DatPrae$year)
	##temp    <- substr(temp,1,7)== substr(DatPrae,1,7)
	##AcIccPrae <- ICCTable[temp, IccCol] 
	
	tempDat <- substr(tempAC[,DatCol],1,7)== substr(DatPost,1,7)
	AcIccPost <- tempAC[tempDat, IccCol]
	##temp  <- ICCTable[, DscdCol] == SDCget$AcquirorDscd
	##temppost  <- ICCTable[temp, DatColPost] == DatPost 	## hier noch für die Daten aus ICC Tabelle konv
	##temp 	<- ICCTable[temp, DatCol]
	##temp  <- substr(temp,1,7)== substr(DatPost,1,7) ## hier noch formatierung prüfen
	##temp  <- (temp$mon == (DatPost$mon)) && (temp$mon$year == DatPost$year)
	##AcIccPost <- ICCTable[temp, IccCol]

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
			temp <- temp + (SummarySdc$AcIccPrae * SummarySdc$AcMvPraeCol)
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
	test2[i,] <- c(as.character(temp$MuaDat), temp$AcquirorDscd, temp$TargetDscd, 
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
	
	temp2$TaIcc <- if(length(temp2$TaIcc) == 0)NA else temp2$TaIcc
	temp2$AcIccPrae <- if(length(temp2$AcIccPrae) == 0)NA else temp2$AcIccPrae
	temp2$AcIccPost <- if(length(temp2$AcIccPost) == 0)NA else temp2$AcIccPost

	test2[i,] <- c(as.character(temp$MuaDat), temp$AcquirorDscd, temp$TargetDscd, 
		temp$ShareAc, temp$SicAc , temp$SicTa ,
		temp2$TaIcc, temp2$AcIccPrae, temp2$AcIccPost)	
	}
	names(test2)<-c(names(SDCCol$get()),names(temp2))
	test2
	}
testob<-testG(SDCTab,SDCCol,ICCCol,ICCTab,12)
