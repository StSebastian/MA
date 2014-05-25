SDCset<-function(DatCol, AcCol, TaCol, ShareCol)
{	get<-function()	data.frame(DatCol, AcCol , TaCol, ShareCol)

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
{	get<-function()	data.frame(DscdCol, MvCol, SicCol)
	
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
	DatColPrae
	DatColPost
	## für SCD Daten brauchst du auch noch prä und post Datum
	temp  <- ICCTable[, DscdCol] == SDCget$TargetDscd
	temp 	<- ICCTable[temp, DatColPrae] == SDCget$MuaDat	 	##-x window
	TaIcc <- ICCTable[temp, IccCol]
	
	temp  <- ICCtable[, DscdCol] == SDCget$AcquirorDscd
	tempprae  <- ICCTable[temp, DatColPrae] == SDCget$MuaDat 	##-x window
	AcIccPrae <- ICCTable[tempprae, IccCol]
	temppost  <- ICCTable[temp, DatColPost] == SDCget$MuaDat 	##+x window
	AcIccPost <- ICCTable[temppost, IccCol]

	list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
}


COMPget <- function(CompCol = COMPCol, COMPTable, SDCget, EventW){
	DscdCol <- CompCol$get()$DscdCol
	MvCol
	SicCol  <- CompCol$get()$SicCol	
	DatColPrae
	DatColPost

	temp <- COMPTable[, DscdCol] == SDCget$TargetDscd
	temp <- COMPTable[temp, MvCol] == 
	TaMv <- COMPTable[temp, DatColPrae]		##-x window
	TaSic <- COMPTable[temp, SicCol]
	
	temp <- COMPTable[, DscdCol] == SDCget$AcquirorDscd
	temp <- COMPTable[temp, MvCol] ==
	AcMvPrae <- COMPTable[temp, DatColPrae] 	##-x window
	AcSic <- COMPTable[temp, SicCol]
	
	AcMvPost <- COMPTable[temp, DatColPost] 	##+x window
	
	list(TaMv = TaMv, AcMvPrae = AcMvPrae, AcMvPost = AcMvPost,
		TaSic = TaSic, AcSic = AcSic)
}	


MuAset<-function(SDCRow, SdcCol = SDCCol , IccCol = ICCCol, 
		CompCol = COMPCol, SDCTable, ICCTable, COMPTable, EventW)
	{ 		MuaData  <- SDCget(SdcCol, SDCRow, SDCTable)
			IccData  <- ICCget(IccCol, ICCTable, MuaData, EventW)
			CompData <- COMPget (CompCol, COMPTable, MuaData, EventW)
		
			data.frame(	c(MuaData$MuaDat, MuaData$AcquirorDscd, 
					MuaData$TargetDscd,
					IccData$TaIcc, IccData$AcIccPrae,
					IccData$AcIccPost, 
					CompData$TaMv, CompData$AcMvPrae, 
					CompData$AcMvPost, CompData$TaSic,
					CompData$AcSic,
					MuaData$ShareAc
					))
}



