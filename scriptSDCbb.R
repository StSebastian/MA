SDCset<-function(DatCol, AcCol, TaCol, ShareCol)
{	get<-function()	list(DatCol = DatCol, AcCol= AcCol , 
				TaCol = TaCol, ShareCol = ShareCol)

##	function() 	namecolAc
##	function()	namecolTa
	
	setDat   <-  function(DAT) DatCol <<- DAT
	setAc    <-  function(AC)  AcCol <<- AC 	
	setTa    <-  function(TA)  TaCol <<- TA
	setShare <-  function(SHARE)  ShareCol <<- SHARE
	
	SDCCol <<- list(get = get, setDat = setDat,
			setAc = setAc, setTa = setTa, 
			setShare = setShare)
	##list(get = get, setDat = setDat, 
	##	setAc = setAc, setTa = setTa)
	SDCCol
}

def<-SDCset("SP1","SP4","SP8")


ICCset <- function(DatCol, DscdCol, IccCol, SicCol){
	
	get <- function()	data.frame(DatCol, DscdCol , IccCol, SicCol)
	
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


SDCget<-function(SdcCol = SDCCol, SDCTable, SDCRow){
	
	DatCol 	<- SdcCol$get()$DatCol ## "$DatCol" kommt von SDC get() data.frame	
	AcquirorCol <- SdcCol$get()$AcCol
	TargetCol   <- SdcCol$get()$TaCol
	ShareAcCol  <- SdcCol$get()$ShareCol

	MuaDat       <- SDCTable[SDCRow,DatCol]
	MuaDat	 <- strptime(MuaDat,"%m.%d.%Y")
	AcquirorDscd <- SDCTable[SDCRow,AcquirorCol]
	TargetDscd   <- SDCTable[SDCRow,TargetCol]
	ShareAc      <- SDCTable[SDCRow,ShareAcCol]
	
	list(MuaDat = MuaDat, AcquirorDscd = AcquirorDscd,
		TargetDscd = TargetDscd, ShareAc = ShareAc)
}


ICCget <- function(IccCol = ICCCol, ICCTable, SDCget, EventW){
	DatCol <- IccCol$get()$DatCol ## "$DatCol" kommt von ICC get() data.frame
	DscdCol <- IccCol$get()$DscdCol
	IccCol   <- IccCol$get()$IccCol
	
	DatPrae <- SDCget$MuaDat
	DatPrae$mon <- DatPrae$mon - EventW
	DatPost <- SDCget$MuaDat
	DatPost$mon <- DatPost$mon + EventW
 
	## für SCD Daten brauchst du auch noch prä und post Datum
	temp  <- ICCTable[, DscdCol] == SDCget$TargetDscd
	##temp 	<- ICCTable[temp, DatColPrae] == DatPrae 	##-x window
	temp 	<- ICCTable[temp, DatCol]
	temp  <- strptime(temp,"%d/%m/%y")				## hier noch formatierung prüfen
	temp  <- (temp$mon == (DatPrae$mon)) && (temp$mon$year == DatPrae$year)
	TaIcc <- ICCTable[temp, IccCol]
	
	temp  <- ICCTable[, DscdCol] == SDCget$AcquirorDscd
	##tempprae  <- ICCTable[temp, DatColPrae] == DatPrae 	## hier noch für die Daten aus ICC Tabelle konv
	temp 	<- ICCTable[temp, DatCol]
	temp  <- strptime(temp,"%d/%m/%y")				## hier noch formatierung prüfen
	temp  <- (temp$mon == (DatPrae$mon)) && (temp$mon$year == DatPrae$year)
	AcIccPrae <- ICCTable[temp, IccCol] 
	
	temp  <- ICCTable[, DscdCol] == SDCget$AcquirorDscd
	##temppost  <- ICCTable[temp, DatColPost] == DatPost 	## hier noch für die Daten aus ICC Tabelle konv
	temp 	<- ICCTable[temp, DatCol]
	temp  <- substr(strptime(temp,"%d/%m/%y"),1,7)== substr(DatPost,1,7) ## hier noch formatierung prüfen
	##temp  <- (temp$mon == (DatPost$mon)) && (temp$mon$year == DatPost$year)
	AcIccPost <- ICCTable[temp, IccCol]

	list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
}


COMPget <- function(CompCol = COMPCol, COMPTable, SDCget, EventW)
		{
		DscdCol <- CompCol$get()$DscdCol
		MvCol
		SicCol  <- CompCol$get()$SicCol	
	
		DatPrae <- SDCget$MuaDat
		DatPrae$mon <- DatPrae$mon - EventW
		DatPost <- SDCget$MuaDat
		DatPost$mon <- DatPost$mon + EventW
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

lapply(x, runif, min = 0, max = 10)
lapply(SICSaperation, mean)
sapply(SICSaperation, function(x)mean(x$diffIcc))
	

