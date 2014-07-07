library(zoo)

Read_Table<-function(){
	setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")
    SdcTab <- read.csv("SDCtest.csv",sep=";",dec=".",colClasses=c("character", "numeric", "character", "character", "character", 
							"character", "character", "character", "character", "character", "character", 
							"character", "character", "numeric", "numeric"))
	
    ##write.table(test,"ICCSample.txt",sep=";",dec=",")
   
    IccTab<-read.csv("ICCSample3.txt",sep=";",stringsAsFactors=F,dec=".",
                        colClasses=c("integer", "character", "numeric", "character", "numeric", "integer",
                        "numeric", "numeric", "character", "integer", "character", "Date", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", 
                        "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
                        "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                        "numeric", "numeric", "numeric"))
    
    
    ##ICCTab<-read.csv("ICC_dataSet_2012-05-29_12_45_06.txt.txt",sep=";",stringsAsFactors=F,dec=",",
    ##                    colClasses=c("integer", "character", "numeric", "character", "numeric", "integer",
    ##                    "numeric", "numeric", "character", "integer", "character", "Date", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", 
    ##                   "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
    ##                    "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric"))
	
	CompanyTab <- read.csv("M&Akons.csv",header=T,stringsAsFactors=F,sep=";",dec=".")

	TableList <-list("SDC" = SdcTab, "ICC" = IccTab, "COMPANY" = CompanyTab)
	##TableList <- list("SDC" = SdcTab)	
	get <- function()TableList
	setTable <- function(NewTable)TableList <<- NewTable	
	TableStored <<- list(setTable = setTable, get = get)
	##TableObj$setTable(TableList)
	##TableObj <<- TableStored
    
	}

Read_Table()    
    
Sdc_Adjust <- function(TableStored){
		
		SdcTable <- TableStored$get()$SDC
		names(SdcTable) <- c("SpDate","SpValueTrans","SpTaName","SpTaDscd","SpAcName",
		"SpAcDscd","SpAcSic","SpAcInd","SpTaSic","SpTaInd","SpDateAnn",
		"SpEqV","SpEpV","SpShAfterTra","SpShAcq")

		SdcTable$SpValueTrans <- as.numeric(SdcTable$SpValueTrans)
		SdcTable$SpEqV <- as.numeric(SdcTable$SpEqV)
		SdcTable$SpEpV <- as.numeric(SdcTable$SpEpV)


		SdcTable$SpDate <- as.Date(strptime(SdcTable$SpDate,"%m.%d.%Y"))
		SdcTable$SpDateAnn <- as.Date(strptime(SdcTable$SpDateAnn,"%m.%d.%Y"))
		SdcTable$SpShAfterTra <- SdcTable$SpShAfterTra/100
		SdcTable$SpShAcq <- SdcTable$SpShAcq/100

		Tables <- TableStored$get()
		Tables$SDC <- SdcTable
		TableStored$setTable(Tables)
		}
        
Sdc_Adjust(TableStored)  


Name_Comp_Table <- function(TableStored = TableStored, NChar = 5 , StartDate = "1978-01-01"){
		
		CompanyTable <- TableStored$get()$COMPANY
		StartDate <- as.POSIXlt( StartDate)
		number <- ncol(CompanyTable) - NChar
		temp <- rep(StartDate,number)
		temp$mon <- temp$mon + 0:(number-1)
		temp <- substr(as.Date(temp),1,7)
		names(CompanyTable) <- c(names(CompanyTable[,1:NChar]),temp)

		Tables <- TableStored$get()
		Tables$COMPANY <- CompanyTable
		TableStored$setTable(Tables)
		}

Name_Comp_Table(TableStored) ## hier noch Problem mit doppelter Benennung
        
##Sdc_Set <- function(DatCol = "SpDate", AcCol = "SpAcDscd", TaCol = "SpTaDscd",
##				    	ShareCol = "SpShAcq", SicAcCol = "SpAcSic", 
##					SicTaCol = "SpTaSic"){    
##	
##	get <- function()data.frame(DatCol, AcCol, TaCol, ShareCol, 
##				SicAcCol, SicTaCol, stringsAsFactors = F)
##
##	setDat   <-  function(DAT) DatCol <<- DAT
##	setAc    <-  function(AC)  AcCol <<- AC 	
##	setTa    <-  function(TA)  TaCol <<- TA
##	setShare <-  function(SHARE)  ShareCol <<- SHARE
##	setSicAc <-  function(SAc) SicAcCol <<- SAc
##	setSicTa <-  function(STa) SicTaCol <<- STa
##	
##	SDCCol <<- list(get = get, setDat = setDat,
##			setAc = setAc, setTa = setTa, 
##			setShare = setShare, setSicAc = setSicAc,
##			setSicTa = setSicTa)
##
##	SDCCol
##	}
##
##Icc_Set <- function(DatCol = "Datum", DscdCol = "Company_Code", 
##					IccCol = "ICC_CT"){
##	
##	get <- function()	data.frame(DatCol, DscdCol , IccCol,
##					stringsAsFactors = F)
##	
##	setDat  <- function(DAT) DatCol <<- DAT
##	setDscd <-  function(DSCD)  DscdCol <<- DSCD 	
##	setIcc  <-  function(ICC)  IccCol <<- ICC	
##	
##	ICCCol <<- list(get = get,setDat = setDat, 
##			setDscd = setDscd, setIcc = setIcc)
##	
##	ICCCol
##	}
##Company_Set <- function(DscdCol = "DSCD", CharacCol = "KPI",
##					 SicCol = "WC07021"){	
##	
##	CharacList <- NULL
##	get <- function()	list(DscdCol = DscdCol, CharacCol = CharacCol, 
##					SicCol = SicCol, CharacList = CharacList)
##	
##	setDscd <- function(DSCD) DscdCol <<- DSCD
##	setSic  <- function(SIC) SicCol <<- SIC	
##	setCharac <- function(CharacC) CharacCol <<- CharacC
##	setCharacList <- function(Charac) CharacList <<- Charac 
##	
##	COMPCol <<- list(get = get, setDscd = setDscd, 
##			setCharac = setCharac, setSic = setSic,
##			setCharacList = setCharacList)
##	
##	COMPCol
##	}
Sdc_Set <- function(DatCol = "SpDate", AcCol = "SpAcDscd", TaCol = "SpTaDscd",
				    	ShareCol = "SpShAcq", SicAcCol = "SpAcSic", 
					SicTaCol = "SpTaSic"){    
	
	SDCCol <- list("DatCol" = DatCol, "AcCol" = AcCol, "TaCol" = TaCol, "ShareCol" = ShareCol,
                    "SicAcCol" = SicAcCol, "SicTaCol" = SicTaCol)
	}

Icc_Set <- function(DatCol = "Datum", DscdCol = "Company_Code", 
					IccCol = "ICC_CT"){
	
	ICCCol <- list("DatCol" = DatCol, "DscdCol" = DscdCol , "IccCol" = IccCol)
	}

Company_Set <- function(DscdCol = "DSCD", CharacCol = "KPI",
					 SicCol = "WC07021",CharacList = NULL){	
	if(!is.null(CharacList)){
                    COMPCol  <- ColPropList$get()$COMPANY
                    COMPCol$CharacList <- CharacList}
	else {
                    COMPCol <- list("DscdCol" = DscdCol, "CharacCol" = CharacCol, 
					"SicCol" = SicCol, "CharacList" = CharacList)}
                   COMPCol 
	}

Event_W_set <- function(Close = 3, Far = 16, Size = 6, 
				MinObs = 2)
	{	
	get <- function()	list(Close = Close, Far = Far, 
					Size =  Size, MinObs = MinObs)
	
	Extend_Fun(TableStored,ceiling(Far/12))
	
	CompanyTable <- TableStored$get()$COMPANY

	setClose <- function(CloseVal) Close <<- CloseVal
	setFar  <- function(FarVal,TableStore = TableStored) {Far <<- FarVal  ##TableStored fehler prüfen
				Extend_Fun(TableStore,ceiling(FarVal/12))}
	
	setSize <- function(SizeVal) Size <<- SizeVal
	setMinObs <- function(MinObsVal) MinObs <<- MinObsVal
	
	EventWdata <<- list(get = get, setClose = setClose, 
			setFar = setFar, setSize = setSize,
			setMinObs = setMinObs)

	##EventWdata <<- list(Start = Start, Last = Last, 
	##		   Size = Size, MinObs = MinObs)
	}
    

Extend_Fun <- function(TableStored = TableStored, years = 6){
		CompanyTable <- TableStored$get()$COMPANY
		monthAdd<-12*(years+1)
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(NA,nrow(CompanyTable))
		time$mon <- time$mon+1:monthAdd
		time<-substr(as.Date(time),1,7)
		CompanyTable[,time] <- newRow

		Tables <- TableStored$get()
		Tables$COMPANY <- CompanyTable
		TableStored$setTable(Tables)
		}


Col_Prop <- function(){

		PropList <- list(NULL)
		get <- function()PropList

		setSdc <- function(...) if (length(list(...))==0){PropList$SDC <<- Sdc_Set()}
						else {PropList$SDC <<- Sdc_Set(...)}
		setIcc <- function(...) if (length(list(...))==0){PropList$ICC <<- Icc_Set()}
						else {PropList$ICC <<- Icc_Set(...)}
		setCompany <- function(...) if (length(list(...))==0){PropList$COMPANY <<- Company_Set()}
						else {PropList$COMPANY <<- Company_Set(...)}
        setCharacList <- function(CompanyProp){PropList$COMPANY <<- CompanyProp}                
		setEventW <- function(...) if (length(list(...))==0){PropList$EVENTW <<- Event_W_set()}
						else {PropList$EVENTW <<- Event_W_set(...)}
        
        setStandard <- function(){
                        PropList$SDC <<- Sdc_Set()
                        PropList$ICC <<- Icc_Set()
                        PropList$COMPANY <<- Company_Set()
                        PropList$EVENTW <<- Event_W_set()
                        }
    
		ColPropList <<- list(get = get, setSdc = setSdc, setICC = setIcc, setCompany = setCompany,
					setEventW = setEventW, setStandard = setStandard)	
		}

Col_Prop()
ColPropList$setStandard()
        
Icc_Conv_Date <- function(ColPropList, TableStored){

		IccTable <- TableStored$get()$ICC
		IccProp <- ColPropList$get()$ICC
		DatCol <- IccProp$DatCol
		IccTable[ ,DatCol] <- substr(IccTable[ ,DatCol],1,7)
		
		Tables <- TableStored$get()
		Tables$ICC <- IccTable
		TableStored$setTable(Tables)
		}
        
Icc_Conv_Date(ColPropList, TableStored)


ICC_Prop_Charac_List_Set <- function(TableStored = TableStored, ColPropList){
		
		CompanyTable <- TableStored$get()$COMPANY 
		CompProp  <- ColPropList$get()$COMPANY
		CharacCol <- CompProp$CharacCol
		CharacCol <- CompanyTable[,CharacCol]
		CharacCol <- as.factor(CharacCol)		

		Characs <- levels(CharacCol)
		CharacMat <- sapply(1:length(Characs),function(x){Characs[x] == CharacCol })
		colnames(CharacMat) <- Characs
		CharacMat <- as.data.frame(CharacMat)	
        ColPropList$setCompany(CharacList = CharacMat)
		}
        
ICC_Prop_Charac_List_Set(TableStored, ColPropList)        

Sdc_Get <- function(ColPropList, TableStored, SDCRow,Carrier){
	
	SdcProp   <- ColPropList$get()$SDC
	SdcTable <- TableStored$get()$SDC
	
	DatCol 	<- SdcProp$DatCol	
	AcquirorCol <- SdcProp$AcCol
	TargetCol   <- SdcProp$TaCol
	ShareAcCol  <- SdcProp$ShareCol
	SicAcqCol	<- SdcProp$SicAcCol
	SicTarCol   <- SdcProp$SicTaCol
	
	Datum     <- SdcTable[SDCRow,DatCol]
	
	##MuaDat      <- Carrier$getEventW(Datum, TableStored) ## function will be explained next
    Carrier$getEventW(Datum, TableStored) ## function will be explained next

    
	AcquirorDscd <- SdcTable[SDCRow,AcquirorCol]
	TargetDscd   <- SdcTable[SDCRow,TargetCol]
	ShareAc      <- SdcTable[SDCRow,ShareAcCol]
	SicAc		 <- SdcTable[SDCRow,SicAcqCol]	
	SicTa		 <- SdcTable[SDCRow,SicTarCol]	
	
	list( Datum = Datum, ##MuaDat = MuaDat, 
		AcquirorDscd = AcquirorDscd, TargetDscd = TargetDscd, 
		ShareAc = ShareAc, SicAc = SicAc, SicTa = SicTa)
	}

Event_W_Get <- function(Datum, TableStored){
	
	EventWProp <- ColPropList$get()$EVENTW
	
	Datum <- as.POSIXlt(Datum) 
	Datum$mday <- 15
	DatPost <- DatPrae <- rep(Datum,EventWProp$get()$Far - EventWProp$get()$Close+1)
		
	DatPrae$mon <- DatPrae$mon-(EventWProp$get()$Close:EventWProp$get()$Far)
	DatPost$mon <- DatPost$mon+(EventWProp$get()$Close:EventWProp$get()$Far)
	DatPrae <- substr(as.Date(DatPrae),1,7)
	DatPost <- substr(as.Date(DatPost),1,7)
	MinObs <- EventWProp$get()$MinObs
	Size <- EventWProp$get()$Size	

	list(DatPrae = DatPrae, DatPost = DatPost, MinObs = MinObs, Size = Size)
	}


Icc_Get <- function(ColPropList, TableStored, Carrier){
	
	IccProp <- ColPropList$get()$ICC	
	IccTable <- TableStored$get()$ICCsample
	SdcData <- Carrier$get()$SDC
    PeriodData <- Carrier$get()$EVENTW
	
	DatCol  <- IccProp$DatCol
	DscdCol <- IccProp$DscdCol
	IccCol  <- IccProp$IccCol
	DatPrae <- sort(PeriodData$DatPrae)
	DatPost <- sort(PeriodData$DatPost)
	MinObs  <- PeriodData$MinObs
	Size    <- PeriodData$Size
	##DscdRows <- is.element(IccTable[,DscdCol],c(SdcData$TargetDscd,SdcData$AcquirorDscd))  ##prüfen ob schneller
	IccTable  <- IccTable[,c(DatCol,DscdCol,IccCol)]
	ICC <- rep(NA,length(DatPrae))
	ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

		temp  <- IccTable[, DscdCol] == SdcData$TargetDscd
		tempTA  <- IccTable[temp, c(DatCol,IccCol)]
		tempDat <- is.element(tempTA[,DatCol],DatPrae)
		IccExist <- is.element(DatPrae,tempTA[tempDat,DatCol])
		ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol]
		
		
		temp  <- IccTable[, DscdCol] == SdcData$AcquirorDscd
		tempAC  <- IccTable[temp, c(DatCol,IccCol)]
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


Comp_Get <- function(ColPropList, TableStored, DataRetrieve){
		
		CompProp <- ColPropList$get()$COMPANY
		CompanyTable <- TableStored$get()$COMPANY		
		SdcData <- DataRetrieve$get()$SDC	
        PeriodData <- Carrier$get()$EVENTW
    
		CharacCol <- CompProp$CharacList
		CharacCol <- CharacCol$MV
		DscdCol <- CompProp$DscdCol
		DatPrae <- sort(PeriodData$DatPrae)
		DatPost <- sort(PeriodData$DatPost)
		MinObs  <- PeriodData$MinObs
		Size 	<- PeriodData$Size

		CompanyTablePrae <- CompanyTable[CharacCol ,c(DscdCol, DatPrae)]
		CompanyTablePost <- CompanyTable[CharacCol ,c(DscdCol, DatPost)]
		
		Mv <- rep(NA,length(DatPrae))
		Mv <- data.frame("TaMv" = Mv, "AcMvPrae" = Mv,"AcMvPost" = Mv)

		temp <- CompanyTablePrae[,DscdCol] == SdcData$TargetDscd
		Mv$TaMv <- as.numeric(CompanyTablePrae[temp,DatPrae])	
		
		temp <- CompanyTablePrae[,DscdCol] == SdcData$AcquirorDscd
		Mv$AcMvPrae <- as.numeric(CompanyTablePrae[temp,DatPrae])	

		Mv$AcMvPost <- as.numeric(CompanyTablePost[temp,DatPost])
		
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



Data_Retrieve <- function(){

	MaaData <- list(NULL)
	getSdc <- function(...){MaaData$SDC <<- Sdc_Get(...)}
	getIcc <- function(...){MaaData$ICC <<- Icc_Get(...)}
	getCompany <- function(...){MaaData$COMPANY <<- Comp_Get(...)}
	getEventW <- function(...){MaaData$EVENTW <<- Event_W_Get(...)}
	get <- function()MaaData
	
	list(getSdc = getSdc, getIcc = getIcc, getCompany = getCompany, 
		getEventW = getEventW, get = get)
	}

Carrier <- Data_Retrieve()

Carrier$getSdc(ColPropList, TableStored, 20,Carrier)
Carrier$getIcc(ColPropList, TableStored, Carrier)
Carrier$getCompany(ColPropList, TableStored, Carrier)
Carrier$get()


##SumTab <- function(ColPropList,TableStored,SDCTab,SDCCol,ICCTab,ICCCol,COMPTab,COMPCol,EventWset){
SumTab <- function(ColPropList,TableStored){
	laenge <- 200
    ##ICCTab <- ICCTab[,as.character(ICCCol$get()[c("DatCol","DscdCol","IccCol")])]
    ##DSCDList <- c(SDCTab[1:laenge,c("SpTaDscd")],SDCTab[1:laenge,"SpAcDscd"])
    ##IccSampleRows <- is.element(ICCTab[,"Company_Code"],DSCDList)
    ##ICCTab <- ICCTab[IccSampleRows,]
    SdcTable <- TableStored$get()$SDC
    IccTable <- TableStored$get()$ICC
    IccTable <- IccTable[,as.character(ColPropList$get()$ICC)]
    
    AcquirorDscds <- ColPropList$get()$SDC$AcCol
    TargetDscds <- ColPropList$get()$SDC$TaCol
    IccDscdCol <- ColPropList$get()$ICC$DscdCol
    DscdsList <- c( SdcTable[1:laenge, AcquirorDscds], SdcTable[1:laenge,TargetDscds])
    IccSampleRows <- is.element(IccTable[,IccDscdCol],DscdsList)

    IccTable <- IccTable[IccSampleRows,]
    TableStoredTemp <- TableStored
    
    Tables <- TableStoredTemp$get()    
	Tables$ICCsample <- IccTable
	TableStoredTemp$setTable(Tables)
    
    EventWdata <- ColPropList$get()$EVENTW$get()
    nobs <- ((EventWdata$Far - EventWdata$Close+1)-EventWdata$Size+1)
	test2 <- as.data.frame(matrix(rep(NA,(7+6*nobs)*laenge),nrow=laenge,ncol=7+6*nobs))
	##CharacAdd(COMPTab,COMPCol)
	ICC_Prop_Charac_List_Set(TableStored, ColPropList) 
    
    Carrier <<- Data_Retrieve ()
    
    for (i in 1:laenge){ 
    ##Carrier <- DataRetrieve()
    Carrier$getSdc(ColPropList, TableStored, i,Carrier)
    Carrier$getIcc(ColPropList, TableStored, Carrier)
    Carrier$getCompany(ColPropList, TableStored, Carrier)
    ##print(Carrier$get()$SDC$TargetDscd)
    
    ##Carrier$getSdc(ColPropList, TableStored, 2,Carrier)
    ##Carrier$getIcc(ColPropList, TableStored, Carrier)
    ##Carrier$getCompany(ColPropList, TableStored, Carrier)
    
	##temp  <- SDCget(SDCCol,SDCTab,i,EventWset)
	##temp2 <- ICCget(ICCCol,ICCTab,temp)
	##temp3 <- COMPget(COMPCol,COMPTab,temp)
	
	test2[i,] <- c(as.character(Carrier$get()$SDC$Datum), Carrier$get()$SDC$AcquirorDscd, Carrier$get()$SDC$TargetDscd, 
		Carrier$get()$SDC$ShareAc, Carrier$get()$SDC$SicAc , Carrier$get()$SDC$SicTa , NA,
		Carrier$get()$ICC$TaIcc, Carrier$get()$ICC$AcIccPrae, Carrier$get()$ICC$AcIccPost,
		Carrier$get()$COMPANY$TaMv, Carrier$get()$COMPANY$AcMvPrae, Carrier$get()$COMPANY$AcMvPost)	
	}
    

	##a <- EventWset$get()$Far : (EventWset$get()$Close - EventWset$get()$Size+1)
	IccColNames <- c(paste("TaIcc",-(nobs:1),sep="_"),paste("AcIccPrae",-(nobs:1),sep="_"),
                    paste("AcIccPost",(1:nobs),sep="_"))

    MvColNames  <- c(paste("TaMv",-(nobs:1),sep="_"),paste("AcMvPrae",-(nobs:1),sep="_"),
                    paste("AcMvPost",(1:nobs),sep="_"))

	names(test2)<-c(names(ColPropList$get()$SDC),"SicSep",IccColNames,MvColNames)
	test2[,c("ShareCol",IccColNames,MvColNames)]<-apply(test2[,c("ShareCol",IccColNames,MvColNames)],2,as.numeric)
	test2$SicSep <- SICSeparation(test2)
  
	test2
	}
 
system.time(testob<-SumTab(ColPropList,TableStored)) 
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