library(zoo)
library(data.table)

Read_Table<-function(){
	setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")
    ##SdcTab <- read.csv("SDCtest.csv",sep=";",dec=".",colClasses=c("character", "numeric", "character", 
    ##                "character", "character", "character", "character", "character", "character", 
    ##                "character", "character", "character", "character", "numeric", "numeric"))

    SdcTab <- fread("SDCtest.csv",sep=";",colClasses=c("character", "numeric", "character", "character",
                            "character", "character", "character", "character", "character", "character", 
                            "character", "character", "character", "numeric", "numeric"))
    
    ##write.table(test,"ICCSample.txt",sep=";",dec=",")

    ##IccTab<-read.csv("ICC_dataSet_2012-05-29_12_45_06.txt.txt",sep=";",stringsAsFactors=F,dec=",",
    ##                 colClasses=c("integer", "character", "numeric", "character", "numeric", "integer",
    ##                    "numeric", "numeric", "character", "integer", "character", "Date", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", 
    ##                    "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
    ##                    "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric"))
    
    ##IccTab2<-fread("ICCSample3.txt")   
    
    IccTab<-fread("IccTabelle.txt")                    
                        
    ##IccTab2<-fread("ICC_dataSet_2012-05-29_12_45_06.txt.txt",sep=";",stringsAsFactors=F,##dec=",",
    ##                 colClasses=c("integer", "character", "numeric", "character", "numeric", "integer",
    ##                    "numeric", "numeric", "character", "integer", "character", "Date", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", 
    ##                    "numeric", "numeric", "Date", "numeric", "character", "Date", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", 
    ##                    "Date", "Date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
    ##                    "numeric", "numeric", "numeric"))
	
	##CompanyTab <- read.csv("M&Akons.csv",header=T,stringsAsFactors=F,sep=";",dec=".")

    CompanyTab <- fread("M&Akons.csv",header=T,stringsAsFactors=F,sep=";")

	TableList <-list("SDC" = SdcTab, "ICC" = IccTab, "COMPANY" = CompanyTab)
	
    Get <- function()TableList
	setTable <- function(NewTable)TableList <<- NewTable	
	
    TableStored <<- list(setTable = setTable, Get = Get)

    
	}

Read_Table()    



Name_Comp_Table <- function(TableStored = TableStored, NChar = 5 , StartDate = "1978-01-01"){
		
		CompanyTable <- TableStored$Get()$COMPANY
		StartDate <- as.POSIXlt( StartDate)
		number <- ncol(CompanyTable) - NChar
		temp <- rep(StartDate,number)
		temp$mon <- temp$mon + 0:(number-1)
		temp <- substr(as.Date(temp),1,7)
		##names(CompanyTable) <- c(names(CompanyTable)[1:NChar],temp)
        newNames <- c(names(CompanyTable)[1:NChar],temp)
        setnames(CompanyTable,names(CompanyTable),newNames)
        
		Tables <- TableStored$Get()
		Tables$COMPANY <- CompanyTable
		TableStored$setTable(Tables)
		}

Name_Comp_Table(TableStored) ## hier noch Problem mit doppelter Benennung


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

Company_Set <- function(DscdCol = "DSCD", CharacCol = "KPI",   ######hier noch prüfen
					 SicCol = "WC07021",CharacList = NULL){	
	if(!is.null(CharacList)){                                       ###hier noch umstellen
                    COMPCol  <- ColPropList$Get()$COMPANY
                    COMPCol$CharacList <- CharacList}
	else {
                   COMPCol <- list("DscdCol" = DscdCol, "CharacCol" = CharacCol, 
				   "SicCol" = SicCol, "CharacList" = CharacList)}
                   COMPCol 
	}

Event_W_set <- function(Close = 3, Far = 16, Size = 6, 
				MinObs = 2)
	{	
	
	Extend_Fun(TableStored,ceiling(Far/12))
    
    EventWdata <<- list(Close = Close, Far = Far, 
					Size =  Size, MinObs = MinObs)
                    
    ##	Get <- function()	list(Close = Close, Far = Far, 
	##				Size =  Size, MinObs = MinObs)                
                    
	##setClose <- function(CloseVal) Close <<- CloseVal
	##setFar  <- function(FarVal,TableStore = TableStored) {Far <<- FarVal  ##TableStored fehler prüfen
	##			Extend_Fun(TableStore,ceiling(FarVal/12))}
	
	##setSize <- function(SizeVal) Size <<- SizeVal
	##setMinObs <- function(MinObsVal) MinObs <<- MinObsVal
	
	##EventWdata <<- list(Get = Get, setClose = setClose, 
	##		setFar = setFar, setSize = setSize,
	##		setMinObs = setMinObs)

	##EventWdata <<- list(Start = Start, Last = Last, '########################################################
	##		   Size = Size, MinObs = MinObs)
	}
    

Extend_Fun <- function(TableStored = TableStored, years = 6){
		CompanyTable <- TableStored$Get()$COMPANY
		monthAdd<-12*(years+1)
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(NA,nrow(CompanyTable))
		time$mon <- time$mon+1:monthAdd
		time<-substr(as.Date(time),1,7)
		CompanyTable[,time] <- newRow
        ##CompanyTable[,{time}:=newRow]       ##########################################
        
		Tables <- TableStored$Get()
		Tables$COMPANY <- CompanyTable
		TableStored$setTable(Tables)
		}


Col_Prop <- function(){

		PropList <- list(NULL)
		Get <- function()PropList

		setSdc <- function(...) if (length(list(...))==0){PropList$SDC <<- Sdc_Set()}
						else {PropList$SDC <<- Sdc_Set(...)}
		setIcc <- function(...) if (length(list(...))==0){PropList$ICC <<- Icc_Set()}
						else {PropList$ICC <<- Icc_Set(...)}
		setCompany <- function(...) if (length(list(...))==0){PropList$COMPANY <<- Company_Set()}
						else {PropList$COMPANY <<- Company_Set(...)}
        setCharacList <- function(CompanyProp){PropList$COMPANY <<- CompanyProp}                
		setEventW <- function(...) if (length(list(...))==0){PropList$EVENTW <<- Event_W_set()}
						else {PropList$EVENTW <<- Event_W_set(...)}
        
        setDefault <- function(){
                        PropList$SDC <<- Sdc_Set()
                        PropList$ICC <<- Icc_Set()
                        PropList$COMPANY <<- Company_Set()
                        PropList$EVENTW <<- Event_W_set()
                        }
    
		ColPropList <<- list(Get = Get, setSdc = setSdc, setICC = setIcc, setCompany = setCompany,
					setEventW = setEventW, setDefault = setDefault)	
		}

Col_Prop()
ColPropList$setDefault()
        
    
Sdc_Adjust <- function(TableStored,ColPropList){
		
		SdcTable <- TableStored$Get()$SDC
        IccTable <- TableStored$Get()$ICC
		ColNamesSdc <- c("SpDate","SpValueTrans","SpTaName","SpTaDscd","SpAcName",
		"SpAcDscd","SpAcSic","SpAcInd","SpTaSic","SpTaInd","SpDateAnn",
		"SpEqV","SpEpV","SpShAfterTra","SpShAcq")
        setnames(SdcTable,names(SdcTable),ColNamesSdc )

        
        SdcTable[,SpValueTrans:=as.numeric(SpValueTrans)]
		SdcTable[,SpEqV:=as.numeric(SpEqV)]        
		SdcTable[,SpEpV:=as.numeric(SpEpV)]

        SdcTable[,SpDate:= as.Date(strptime(SpDate,"%m.%d.%Y"))]
		SdcTable[,SpDateAnn:= as.Date(strptime(SpDateAnn,"%m.%d.%Y"))]
		SdcTable[,SpShAfterTra:= SpShAfterTra/100]
		SdcTable[,SpShAcq:= SpShAcq/100]
        
        IccDatCol <- ColPropList$Get()$ICC$DatCol
        IccTable[,{IccDatCol}:=as.Date(strptime(IccTable[[IccDatCol]],"%Y-%m-%d"))]     
		Tables <- TableStored$Get()
		Tables$SDC <- SdcTable
		TableStored$setTable(Tables)
		}
        
Sdc_Adjust(TableStored,ColPropList)          
        
        
Icc_Conv_Date <- function(ColPropList, TableStored){

		IccTable <- TableStored$Get()$ICC
		IccProp <- ColPropList$Get()$ICC
		DatCol <- IccProp$DatCol
		IccDatCol <- ColPropList$Get()$ICC$DatCol  ##X
        
        IccTable[,{IccDatCol}:=substr(IccTable[[IccDatCol]],1,7)]
        IccDatCol <- ColPropList$Get()$ICC$DatCol                        

		Tables <- TableStored$Get()
		Tables$ICC <- IccTable
		TableStored$setTable(Tables)
		}
        
Icc_Conv_Date(ColPropList, TableStored)


Icc_Prop_Charac_List_Set <- function(TableStored = TableStored, ColPropList){
		
		CompanyTable <- TableStored$Get()$COMPANY 
		CompProp  <- ColPropList$Get()$COMPANY
		CharacCol <- CompProp$CharacCol
		CharacCol <- CompanyTable[[CharacCol]]          ##
		CharacCol <- as.factor(CharacCol)		

		Characs <- levels(CharacCol)
		CharacMat <- sapply(1:length(Characs),function(x){Characs[x] == CharacCol })
		colnames(CharacMat) <- Characs
		CharacMat <- as.data.table(CharacMat)	
        ColPropList$setCompany(CharacList = CharacMat)
		}
        
##Icc_Prop_Charac_List_Set(TableStored, ColPropList)        

Sdc_Get <- function(ColPropList, TableStored, SDCRow,Carrier){
	
	SdcProp   <- ColPropList$Get()$SDC
	SdcTable <- TableStored$Get()$SDC
	
	DatCol 	<- SdcProp$DatCol	
	AcquirorCol <- SdcProp$AcCol
	TargetCol   <- SdcProp$TaCol
	ShareAcCol  <- SdcProp$ShareCol
	SicAcqCol	<- SdcProp$SicAcCol
	SicTarCol   <- SdcProp$SicTaCol
	
	Datum     <- SdcTable[[DatCol]][SDCRow]


	##MuaDat      <- Carrier$retrieveEventW(Datum, TableStored) ## function will be explained next
    Carrier$retrieveEventW(Datum) ## function will be explained next

    
	AcquirorDscd <- SdcTable[[AcquirorCol]][SDCRow]   
	TargetDscd   <- SdcTable[[TargetCol]][SDCRow]     
	ShareAc      <- SdcTable[[ShareAcCol]][SDCRow]      
	SicAc		 <- SdcTable[[SicAcqCol]][SDCRow]     
	SicTa		 <- SdcTable[[SicTarCol]][SDCRow] 	    
	
	list( Datum = Datum, ##MuaDat = MuaDat, 
		AcquirorDscd = AcquirorDscd, TargetDscd = TargetDscd, 
		ShareAc = ShareAc, SicAc = SicAc, SicTa = SicTa)
	}

Event_W_Get <- function(Datum){
	
	EventWProp <- ColPropList$Get()$EVENTW
	
	Datum <- as.POSIXlt(Datum) 
	Datum$mday <- 15
	DatPost <- DatPrae <- rep(Datum,EventWProp$Far - EventWProp$Close+1)
		
    ##DatPrae$mon <- DatPrae$mon-(EventWProp$Close:EventWProp$Far)          
	DatPrae$mon <- DatPrae$mon-(EventWProp$Far:EventWProp$Close)    ##
	DatPost$mon <- DatPost$mon+(EventWProp$Close:EventWProp$Far)
	DatPrae <- substr(as.Date(DatPrae),1,7)
	DatPost <- substr(as.Date(DatPost),1,7)
	MinObs <- EventWProp$MinObs
	Size <- EventWProp$Size	

	list(DatPrae = DatPrae, DatPost = DatPost, MinObs = MinObs, Size = Size)
	}


Icc_Get <- function(ColPropList, TableStored, Carrier, Sample=FALSE){
	
	IccProp <- ColPropList$Get()$ICC	
	
    if(Sample){IccTable <- TableStored$Get()$ICCsample}
    else{IccTable <- TableStored$Get()$ICC}
	
    SdcData <- Carrier$Get()$SDC
    PeriodData <- Carrier$Get()$EVENTW
	
	DatCol  <- IccProp$DatCol
	DscdCol <- IccProp$DscdCol
	IccCol  <- IccProp$IccCol
	DatPrae <- sort(PeriodData$DatPrae)
	DatPost <- sort(PeriodData$DatPost)
	MinObs  <- PeriodData$MinObs
	Size    <- PeriodData$Size
	##DscdRows <- is.element(IccTable[,DscdCol],c(SdcData$TargetDscd,SdcData$AcquirorDscd))  ##prüfen ob schneller
	IccTable  <- IccTable[,c(DatCol,DscdCol,IccCol),with=F]             ##
	ICC <- rep(NA,length(DatPrae))
	ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

		temp  <- IccTable[[DscdCol]] == as.character(SdcData$TargetDscd)              ##
		tempTA  <- IccTable[temp, c(DatCol,IccCol),with=F]              ##

        
        tempDat <- is.element(tempTA[[DatCol]],DatPrae)   
		IccExist <- is.element(DatPrae,tempTA[[DatCol]][tempDat])       ##
		ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol,with=F]         ##

		
		temp  <- IccTable[[DscdCol]] == SdcData$AcquirorDscd            ##
		tempAC  <- IccTable[temp, c(DatCol,IccCol),with=F]              ##
		tempDat <- is.element(tempAC[[DatCol]],DatPrae)                 ##
		IccExist <- is.element(DatPrae,tempAC[[DatCol]][tempDat])       ##
		ICC[IccExist,"AcIccPrae"] <- tempAC[tempDat, IccCol,with=F]     ##   

		tempDat <- is.element(tempAC[[DatCol]],DatPost)     
		IccExist <- is.element(DatPost,tempAC[[DatCol]][tempDat])  
		ICC[IccExist,"AcIccPost"] <- tempAC[tempDat, IccCol,with=F]     ##
      
		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
	
		ICC <- zoo(ICC)
		ICC <- rollapply(ICC,FUN = CalcMean,width = Size)
			
		##list(TaIcc = TaIcc, AcIccPrae = AcIccPrae, AcIccPost = AcIccPost)
		}


Comp_Get <- function(ColPropList, TableStored, Carrier){
		
		CompProp <- ColPropList$Get()$COMPANY
		CompanyTable <- TableStored$Get()$COMPANY		
		SdcData <-  Carrier$Get()$SDC	
        PeriodData <- Carrier$Get()$EVENTW
    
		CharacCol <- CompProp$CharacList
		CharacCol <- CharacCol$MV
		DscdCol <- CompProp$DscdCol
		DatPrae <- sort(PeriodData$DatPrae)
		DatPost <- sort(PeriodData$DatPost)
		MinObs  <- PeriodData$MinObs
		Size 	<- PeriodData$Size

		CompanyTablePrae <- CompanyTable[CharacCol ,c(DscdCol, DatPrae),with=F]
		CompanyTablePost <- CompanyTable[CharacCol ,c(DscdCol, DatPost),with=F]
		
		Mv <- rep(NA,length(DatPrae))
		Mv <- data.frame("TaMv" = Mv, "AcMvPrae" = Mv,"AcMvPost" = Mv)

		temp <- CompanyTablePrae[[DscdCol]] == SdcData$TargetDscd               ##
		Mv$TaMv <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])	            ##
		
		temp <- CompanyTablePrae[[DscdCol]] == SdcData$AcquirorDscd             ##
		Mv$AcMvPrae <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])	        ##

        Mv$AcMvPost <- as.numeric(CompanyTablePost[,DatPost,with=F][temp])           ##
		##Mv$AcMvPost <- as.numeric(CompanyTablePost[temp,DatPost])
		
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
	retrieveSdc <- function(...){MaaData$SDC <<- Sdc_Get(...)}
	retrieveIcc <- function(...){MaaData$ICC <<- Icc_Get(...)}
	retrieveCompany <- function(...){MaaData$COMPANY <<- Comp_Get(...)}
	retrieveEventW <- function(...){MaaData$EVENTW <<- Event_W_Get(...)}
	Get <- function()MaaData
	
	list(retrieveSdc = retrieveSdc, retrieveIcc = retrieveIcc, retrieveCompany = retrieveCompany, 
		retrieveEventW = retrieveEventW, Get = Get)
	}

##Carrier <- Data_Retrieve()

##Carrier$retrieveSdc(ColPropList, TableStored,6,Carrier)
##Carrier$retrieveIcc(ColPropList, TableStored, Carrier)
##Carrier$retrieveCompany(ColPropList, TableStored, Carrier)
##Carrier$Get()



SumTab <- function(ColPropList,TableStored){
	laenge <- 1800

    SdcTable <- TableStored$Get()$SDC
    IccTable <- TableStored$Get()$ICC
    IccTable <- IccTable[,as.character(ColPropList$Get()$ICC),with=F]
    
    AcquirorDscds <- ColPropList$Get()$SDC$AcCol
    TargetDscds <- ColPropList$Get()$SDC$TaCol
    IccDscdCol <- ColPropList$Get()$ICC$DscdCol
    DscdsList <- c( SdcTable[[AcquirorDscds]][1:laenge], SdcTable[[TargetDscds]][1:laenge])
    IccSampleRows <- is.element(IccTable[[IccDscdCol]],DscdsList)

    IccTable <- IccTable[IccSampleRows,]
    Tables <- TableStored$Get()    
	Tables$ICCsample <- IccTable
	TableStored$setTable(Tables)
    
    EventWdata <- ColPropList$Get()$EVENTW
    nobs <- ((EventWdata$Far - EventWdata$Close+1)-EventWdata$Size+1)
	test2 <- as.data.frame(matrix(rep(NA,(7+6*nobs)*laenge),nrow=laenge,ncol=7+6*nobs))
	Icc_Prop_Charac_List_Set(TableStored, ColPropList) 
    
    Carrier <<- Data_Retrieve ()
    
    for (i in 1:laenge){ 

    Carrier$retrieveSdc(ColPropList, TableStored, i,Carrier)
    Carrier$retrieveIcc(ColPropList, TableStored, Carrier, Sample=TRUE)
    Carrier$retrieveCompany(ColPropList, TableStored, Carrier)

	test2[i,] <- c(as.character(Carrier$Get()$SDC$Datum), Carrier$Get()$SDC$AcquirorDscd, Carrier$Get()$SDC$TargetDscd, 
		Carrier$Get()$SDC$ShareAc, Carrier$Get()$SDC$SicAc , Carrier$Get()$SDC$SicTa , NA,
		Carrier$Get()$ICC$TaIcc, Carrier$Get()$ICC$AcIccPrae, Carrier$Get()$ICC$AcIccPost,
		Carrier$Get()$COMPANY$TaMv, Carrier$Get()$COMPANY$AcMvPrae, Carrier$Get()$COMPANY$AcMvPost)	
	}
    
	IccColNames <- c(paste("TaIcc",-(nobs:1),sep="_"),paste("AcIccPrae",-(nobs:1),sep="_"),
                    paste("AcIccPost",(1:nobs),sep="_"))

    MvColNames  <- c(paste("TaMv",-(nobs:1),sep="_"),paste("AcMvPrae",-(nobs:1),sep="_"),
                    paste("AcMvPost",(1:nobs),sep="_"))


    names(test2)<-c("Date","Acquiror_Dscd","Target_Dscd","Perc_Shares_Acquired","Acquiror_Sic","Target_Sic",
                "SicSep",IccColNames,MvColNames)
    
    test2[,c("Perc_Shares_Acquired",IccColNames,MvColNames)]<-apply(test2[,c("Perc_Shares_Acquired",
                IccColNames,MvColNames)],2,as.numeric)
	test2$SicSep <- Sic_Separation(test2)
  
	test2
	}
 

Sic_Separation <- function(SummarySdc, AcSicCol = "Acquiror_Dscd", TaSicCol = "Target_Dscd")
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
            
system.time(testob<-SumTab(ColPropList,TableStored)) 