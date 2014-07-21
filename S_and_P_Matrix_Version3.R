setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

SaPConst <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character"))

SaPConst$Datum <- as.Date(SaPConst$Datum)
SaPConst$Datum <- substr(SaPConst$Datum,1,7)

## Creates empty data frame which list the dates as columns and all the Companies that have been
## in the S&P 1500 as rows. The data frame will later be filled be TRUE and False values to indicated  
## the month of membership of the Companies in the S&P 1500


SaPcreate <- function(SaPConst = SaPConst){

	ColVec<-sort(unique(SaPConst$Datum),decreasing=F) ## filters the different dates
	DSCDPos<-!duplicated(SaPConst$DSCD) ## filters the different DSCD
	##RowVec<-SaPConst$DSCD[!duplicated(SaPConst$DSCD)]
	RowVec<-SaPConst$DSCD[DSCDPos] 
	##SicVec<-SaPConst$WC07021[!duplicated(SaPConst$DSCD)]
	SicVec<-SaPConst$WC07021[DSCDPos] ## filters different Sic positions
	
	SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")

	RowLeng<-length(RowVec)
	ColLeng<-length(ColVec)+2+3
	SaPMat<-matrix(rep(NA,RowLeng*ColLeng),nrow=RowLeng,ncol=ColLeng)
	SaPMat<-as.data.frame(SaPMat)

	colnames(SaPMat)<-c("DSCD","SIC_four","SIC_three","SIC_two","SIC_one",ColVec)
	SaPMat[,1]<-RowVec
	SaPMat[,2]<-as.factor(SicVec)
	SaPMat[,3]<-as.factor(substr(SicVec,1,3))
	SaPMat[,4]<-as.factor(substr(SicVec,1,2))
	SaPMat[,5]<-as.factor(substr(SicVec,1,1))

	SaPMat
	}


SapMatConv<-function(SaPMat){
        for (i_col in 6:ncol(SaPMat)){
            temp <- SaPMat[,i_col]
            temp[temp] <- SaPMat[temp,"DSCD"]
            SaPMat[,i_col] <- temp
            }
        SaPMatDscd <<- SaPMat  
        }        
            
    
## fills SaPMat with TRUE and FALSE values according to their membership in the S&P 1500    
    
SaPfill <- function(SaPMatrix = SaPMat,SaPConstitution = SaPConst){

	SaPvalues <- function(x){
	SaPMatrix[,x] <<- SaPMatrix[,"DSCD"]%in%SaPConstitution[SaPConstitution$Datum==colnames(SaPMatrix)[x],"DSCD"]
	invisible()
	}
	
	for(i in 6:ncol(SaPMatrix)){SaPvalues(i)}
	##temp <- sapply(6:ncol(SaPMatrix),SaPvalues)
	##rm(temp)

	SaPMatrix
	}


SaPMat <- SaPcreate(SaPConst)

SaPMat <- SaPfill(SaPMat, SaPConst)


##*****************************************************************

## next goal is to compute a list of four data frame for 1-,2-,3-,and 4-Sic-digit long tables.
## the table will have the all the in SaPmat occuring Sics as colomnnames and list by use of 
## True and False values their belonging to the companies which are the rows 

##SicSep<-function(SicLevel,SicCol){SaPMat[SicCol]==SicLevel}

##SicFour <- sapply(levels(SaPMat$SIC_four), SicSep, SicCol = "SIC_four")
##SicThree <- sapply(levels(SaPMat$SIC_three), SicSep, SicCol = "SIC_three")
##SicTwo <- sapply(levels(SaPMat$SIC_two), SicSep, SicCol = "SIC_two")
##SicOne <- sapply(levels(SaPMat$SIC_one), SicSep, SicCol = "SIC_one")

##SicPosList<-list(SicFour = SicFour, SicThree = SicThree, SicTwo = SicTwo, SicOne = SicOne ) 

##*****************************************************************

## removing NAs from SicPosList

##Sic.NaRm.Op <- function(Col,SicDig){
##SicPosList[[SicDig]][is.na(SicPosList[[SicDig]][,Col]),Col] <<- FALSE
##}

##Sic.NaRm <- function(SicDig){
##	
##	for (i in 1:ncol(SicPosList[[SicDig]])){Sic.NaRm.Op(i,SicDig)}
##	}

##Sic.NaRm("SicOne"); Sic.NaRm("SicTwo"); Sic.NaRm("SicThree"); Sic.NaRm("SicFour")	

##********************************************************************

## creates an empty data frame which has dates as columns and Sics as rows depending on "SicFour","SicThree","SicTwo","SicOne"
## It should later be filled with the number of member companies to the S&P 1500 grouped by the same Sics

##ColVec<-sort(unique(SaPConst$Datum),decreasing=F)

##SicCreate <- function(SicDig){
##	Col.SicDig <- SicPosList[[SicDig]]
##	ColVec<-sort(unique(SaPConst$Datum),decreasing=F)
##    
##	Matrix <- matrix(numeric(ncol(Col.SicDig)*(length(ColVec)+1)),nrow=ncol(Col.SicDig),ncol=(length(ColVec)+1))
##	colnames(Matrix)<-c("Sic",ColVec)
##	Matrix <- as.data.frame(Matrix)
##	Matrix[,1]<-colnames(Col.SicDig)
##	##Matrix <- cbind(colnames(Col.SicDig),Matrix)
##	Matrix
##	}
 
##SicList3 <- sapply(c("SicFour","SicThree","SicTwo","SicOne"),SicCreate,simplify=F,USE.NAMES = TRUE)
 

    
 SicCreate <- function(SicDig){
    if(SicDig=="SicFour"){SicDig <- "SIC_four"}
    else if(SicDig=="SicThree"){SicDig <- "SIC_three"}
    else if(SicDig=="SicTwo"){SicDig <- "SIC_two"}
    else if(SicDig=="SicOne"){SicDig <- "SIC_one"}
	##Col.SicDig <- SicPosList[[SicDig]]
	Col.SicDig <- levels(SaPMat[,SicDig])
    ColVec<-sort(unique(SaPConst$Datum),decreasing=F)
    
	Matrix <- matrix(numeric(length(Col.SicDig)*(length(ColVec)+1)),nrow=length(Col.SicDig),ncol=(length(ColVec)+1))
	colnames(Matrix)<-c("Sic",ColVec)
	Matrix <- as.data.frame(Matrix)
	Matrix[,1]<-Col.SicDig
	##Matrix <- cbind(colnames(Col.SicDig),Matrix)
	Matrix
	}   
    

##************************************************************************

SapData <- read.csv("S&Pkons.csv",sep=";",dec=".")
##as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
Dates <- as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
ColNames <- c(colnames(SapData[,1:3]),substring(Dates,1,7))
colnames(SapData) <- ColNames

Sap_Set <- function(DscdCol = "DSCD", CharCol = "KPI", OneSicCol = NULL, TwoSicCol = NULL, ThrSicCol = NULL, FourSicCol = NULL){
	
	CharRows <- NULL
	list("DscdCol" = DscdCol, "CharCol" = CharCol, "OneSicCol" = OneSicCol,
		"TwoSicCol" = TwoSicCol, "ThrSicCol" = ThrSicCol, "FourSicCol" = FourSicCol,
		"CharRows" = CharRows)
	}

SapSet<-Sap_Set()

## adds T/F list to SapSet, the list gives information in which columns of SapData which KPI can be found

CharRowsSet <- function(SapDataTab = SapData, SapSetOb = SapSet){
	CharCol <- SapSetOb$CharCol
	CharColVals <- SapDataTab[[CharCol]] 
	CharVals <- as.character(unique(SapDataTab[, CharCol]))
	CharRows <- sapply(CharVals, function(x,y){x==CharColVals }, simplify=F, USE.NAMES = TRUE)
	CharRows
	}

##SapSet$CharRows <- CharRowsSet()



TsSicElCalc <- function(SicListEl,SapDataTab,DscdCol,SaPMat,ObsReq){
		for(i in seq(along=colnames(SicListEl[,-1]))){
			Datum <- colnames(SicListEl)[i+1]
			DscdRm <- SapDataTab[,DscdCol] %in% SaPMat[SaPMat[,Datum],"DSCD"]
			SapDataTab2 <- SapDataTab[DscdRm, ]
			##CharVal <- CalcCharVal(SapDataTab2, Sic, SapMat, Datum, ObsReq)
			CharVal <- sapply(SicListEl[,"Sic"],CalcCharVal, SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq)
			SicListEl[,Datum] <- CharVal
		}
		SicListEl
		}


CalcCharVal <- function(Sic, SapDataTab, SapMat, Datum, ObsReq){ ##SapMat ist SaPMat sollte aber später SaP Composition heißen also SapComp
	##CharRows <- SapSetOb$CharRows
	##DscdCol <- SapSetOb$DscdDCol
	##SicCol <- SapSetOb$  ##hier noch einfügen
	
	##SapDataTab <- SapDataTab[CharRows[[CompChar]], ] ##Sowohl die Entfernung der CharRows als auch der Sic Colums kannst du eine Ebene/funktion vorher machen
	SicRm <- SapDataTab[,"SIC"]== Sic 
	##SapDataTab <- SapDataTab[SicRm, ]
	##DscdRm <- SapDataTab[,DscdCol] %in% SapMat[SapMat[,Datum],]
	##SapDataTab <- SapDataTab[DscdRm, ]
	CharVal <- SapDataTab[SicRm,Datum]
	CharVal <- CharVal[CharVal != 0]
    CharVal <- CharVal[!is.na(CharVal)]
    ## CharVal <- CharVal[CharVal != 0 | is.na(CharVal)] ##besser musst noch hinzufügen
	if(length(CharVal) >= ObsReq){CharVal <- mean(CharVal, na.rm = T)
	}else {CharVal <- NA}

	CharVal
	}


TsSicCalc <- function(SapDataTab = SapData, SapSetOb = SapSet, CompChar = "MV", SapMat = SaPMat, ObsReq = 5){
   
   
    SapDataTab<- merge(SapMat[,c("DSCD","SIC_four")],SapDataTab,by.x="DSCD",by.y="DSCD",all=T,sort=F)
   
	CharCol <- SapSetOb$CharCol
    CharLevel <- unique(as.character(SapData[,CharCol]))
    
    SapDataTab <- sapply(CharLevel,FUN=function(x){y<-SapDataTab[CharCol]==x;y=SapDataTab[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
    
    
    ##AddSicNumber <- function (SapDataElement){
    ##  SapDataElement<- merge(SaPMat[,c("DSCD","SIC_four")],SapDataElement,by.x="DSCD",by.y="DSCD",all=T,sort=F) 
    ##        SapDataElement <- SapDataElement [order(SapDataElement$DSCD),]
    ##    }
    

    
        
    ##SapDataTab <- lapply(SapDataTab,AddSicNumber)
   
   ##lapply(CharLevel,FUN=function(x){y<-SapDataTab[CharCol]==x;SapDataTab[y,]})

    ### temp <- merge(SapMat[,c("DSCD","SIC_four")],
	##		SapDataTab,by.x="DSCD",by.y="DSCD",all=T,sort=F)  ##hier prüfen ob das mit dem unterschiedlichen typen factor und variablen kollidiert
    ##SapDataTab <- temp
    ## rm(temp)
	##SapDataTab<-SapDataTab[order(SapDataTab$DSCD),]
    ##SapDataTab<-SapDataTab[order(match(SapDataTab$KPI,CharLevel)),]
    ##SapDataTab<-SapDataTab[order(match(SapDataTab$KPI,names(SapSetOb$CharRows))),]
        ##vll doch besser das ganze gleich in eine liste aufzuteilen(nach den KPI)
    
    ##SapSetOb$CharRows <- CharRowsSet()
    
    ##CharRows <- SapSetOb$CharRows
	##SicCol <- SapSetOb$
	SapDataTab <- SapDataTab[[CompChar]] ##Sowohl die Entfernung der CharRows als auch der Sic Colums kannst du eine Ebene/funktion vorher machen
	DscdCol <- SapSetOb$DscdCol
	##SicRm <- SapDataTab[,SicCol]== Sic 
	##SapDataTab <- SapDataTab[SicRm, ]

    
    SapDataTab <- SapDataTab[!is.na(SapDataTab[,"SIC_four"]),]
    
	SicList <- sapply(c("SicFour","SicThree","SicTwo","SicOne"),SicCreate,simplify=F,USE.NAMES = TRUE)
	##i <- 3
	for(i in seq(along = SicList)){
	SicListEl <- SicList[[i]]
        #####SicListEl <- SicList[[1]] #####
		SicLength <- nchar(SicListEl[1,"Sic"])
		##if(SicLength==1){SicCol="SIC_one"}
		##if(SicLength==2){SicCol="SIC_two"}
		##if(SicLength==3){SicCol="SIC_three"}
		##if(SicLength==4){SicCol="SIC_four"}
		
		SapDataTab[,"SIC"] <- substring(SapDataTab[,"SIC_four"],1,SicLength)	##Sic Col noch definieren	
		
		SicListEl <- TsSicElCalc(SicListEl,SapDataTab,DscdCol,SapMat,ObsReq)

		##for(i in seq(along=colnames(SicListEl[,-1]))){
		##	Datum <- colnames(SicListEl)[i+1]
		##	DscdRm <- SapDataTab[,DscdCol] %in% SaPMat[SaPMat[,Datum],"DSCD"]
		##	SapDataTab2 <- SapDataTab[DscdRm, ]
		##	##CharVal <- CalcCharVal(SapDataTab2, Sic, SapMat, Datum, ObsReq)
		##	CharVal <- sapply(SicListEl[,"Sic"],CalcCharVal, SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq)
		##	SicListEl[,Datum] <- CharVal
		##	}
		SicList[[i]] <- SicListEl
        ######SicList[[4]] <- SicListEl #####
		}
	SicList
	}

   system.time(abb<-TsSicCalc(SapDataTab = SapData, SapSetOb = SapSet, CompChar = "MV", SapMat = SaPMat, ObsReq = 5))
 
### nächsten drei formeln gehen darauf ein wie für die SumTab vom anderen R Code die Verbindung zu den Tabellen hier hergestellt werden kann
### um die korrelationen zu erhalten 
 
 
AddCorCol <- function(SumTable,ColObject,CompProp){
        CorVector <- rep(NA,nrow(SumTable))
        for(i_row in 1:nrow(SumTable)){
            CorVector[i_row] <- CalcIndustryCor(SumTable[i_row,"SicAcquiror"],SumTable[i_row,"SicTarget"],SumTable[i_row,"Date"],CompProp)
            }
        CorVector            
        }
        
  
CalcIndustryCor <- function(SicAcquiror,SicTarget,Date,CompProp){
        TimePeriod <- 10     ##     das irgendwie vorher definieren
        Date <- as.POISXlt(Date)
        Date$years <- Date$years - (TimePeriod:1)  ## prüfen ob das mit der rheihenfolge passt
        AcIndustryTs <- CalcIndustryTs(Date,SicAcquiror,SicDigits=1)  ## das mit 1 ist noch nicht gut --> verwirrden
        TaIndustryTs <- CalcIndustryTs(Date,SicTarget,SicDigits=1)    ## das mit 1 ist noch nicht gut --> verwirrden
        if(any(is.na(AcIndustryTs,TaIndustryTs){return(NA)}
        #### hier noch die regression mit dem Marktindex einfügen
        cor(rowMeans(AcIndustryTs),RowMeans(TaIndustryTs))
        }
        
CalcIndustryTs <- function(Date,Sic,SicDigits){
        NoObsReq = 10        ## das irgendwie vorher definieren   
        NoCompReq = 10       ## das irgendwie vorher definieren
        if(SicDigits>4){return(NA)}  ## das mit 1 ist noch nicht gut --> verwirrden
        ### es ist noch nicht auf die Sic länge eingegangen
        ### es ist noch nicht auf die auswahl der industry eingegangen
        Table <- Table[[x]][,Sic]   ######### hier noch richtige liste auswähleen
        ##if(sum(Table)<=10){return((Date,Sic,x+1))}
        IndustryTs <- Table[Bo,Date]
        ##SuffObs <- rowSums(!is.na(IndustryTs))>=10
        ##IndustryTs <- IndustryTs(SuffObs,)
        if(sum(rowSums(!is.na(IndustryTs))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
            return(IndustryTs[rowSums(!is.na(IndustryTs))>=NoObsReq,])}     ## returns only those companies with enough observations
        else {CalcIndustryTs(Date,Sic,SicDigits+1)}                         ## if "if"-check fails same function is called searching Sic-table with one Sic digit more
        }        
        

##*****************************************************************

## next goal is to compute a list of four data frame for 1-,2-,3-,and 4-Sic-digit long tables.
## the table will have the all the in SaPmat occuring Sics as colomnnames and list by use of 
## True and False values their belonging to the companies which are the rows 

SicSep<-function(SicLevel,SicCol){SaPMat[SicCol]==SicLevel}

SicFour <- sapply(levels(SaPMat$SIC_four), SicSep, SicCol = "SIC_four")
SicThree <- sapply(levels(SaPMat$SIC_three), SicSep, SicCol = "SIC_three")
SicTwo <- sapply(levels(SaPMat$SIC_two), SicSep, SicCol = "SIC_two")
SicOne <- sapply(levels(SaPMat$SIC_one), SicSep, SicCol = "SIC_one")

SicPosList<-list(SicFour = SicFour, SicThree = SicThree, SicTwo = SicTwo, SicOne = SicOne ) 

##*****************************************************************   

            
Read_SapData <- function(SapMat=SaPMat,CompPropCol="KPI"){        
        SapData <- read.csv("S&Pkons.csv",sep=";",dec=".")
        Dates <- as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
        ColNames <- c(colnames(SapData[,1:3]),substring(Dates,1,7))
        colnames(SapData) <- ColNames        

        CharLevel <- unique(as.character(SapData[,CompPropCol]))
        
        SapData<- merge(SapMat[,c("DSCD","SIC_four")],SapData,by.x="DSCD",by.y="DSCD",all=T,sort=F)
         
        SapData <- sapply(CharLevel,FUN=function(x){y<-SapData[CompPropCol]==x;y=SapData[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE) 
        SapData <<- SapData
        }

Read_SapData()
        

##erg<-function(SapData = SapData2,SaPMat){
##        
##        SapDataBoolean <- sapply(names(SapData),function(x){},simplify=F,USE.NAMES = TRUE)
##        for(i_table in 1:length(SapData)){
##       rownames(SaPMat)<-SaPMat[,"DSCD"]
##        SapDataBoolean[[i_table]]<-SaPMat[SapData[[i_table]][,"DSCD"],]
##        }cd Github
##        SapDataBoolean
##        }

Compute_SapDataBoolean<-function(SapData = SapData,SaPMat){         ## hier nochdeoppelte Benennung
        
        SapDataBoolean <- sapply(names(SapData),function(x){x},simplify=F,USE.NAMES = TRUE)
        for(i_table in SapDataBoolean){
        rownames(SaPMat)<-SaPMat[,"DSCD"]
        SapDataBoolean[[i_table]]<-SaPMat[SapData[[i_table]][,"DSCD"],]
        }
        SapDataBoolean
        }        
        
        
a<-"TA";summary(SapData2[[a]][,"DSCD"]==fef[[a]][,"DSCD"])
 

function(SapDataBoolean,SicAcquiror,SicTarget,Date,CompProp){
        TimePeriod <- 10     ##     das irgendwie vorher definieren
        Date <- as.POSIXlt(Date)
        Date$year <- Date$year - (TimePeriod:1)  ## prüfen ob das mit der rheihenfolge passt
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
        AcIndustryTs <- CalcIndustryTs(Date,SicAcquiror,SicDigits=4)  ## das mit 1 ist noch nicht gut --> verwirrden
        TaIndustryTs <- CalcIndustryTs(Date,SicTarget,SicDigits=4)    ## das mit 1 ist noch nicht gut --> verwirrden
        if(any(is.na(AcIndustryTs,TaIndustryTs){return(NA)}
        #### hier noch die regression mit dem Marktindex einfügen
        cor(rowMeans(AcIndustryTs),RowMeans(TaIndustryTs))
        
        } 
        
CalcIndustryTs1<-function(BooleanSapData,SapData,Date,Sic,SicDigits=4,KPI,Method){
        NoObsReq<-8
        NoCompReq<-5
        ##if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
            if(SicDigits==0){return(NA)}
            Boolean <- substring(BooleanSapData[[KPI]]$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
            Boolean[is.na(Boolean)]<-FALSE

            ##BooleanSapData[[KPI]][,]==Sic
            if(Method == "one"){   
            Spalten<-Boolean*BooleanSapData[[KPI]][,max(Date)]
            IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
            } 
            else if(Method == "two"){   
            Spalten<-rowSums(Boolean*BooleanSapData[[KPI]][,Date])>0
            IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
            }  
            else if (Method == "three"){ 
            Spalten<-Boolean*BooleanSapData[[KPI]][,Date]
            ##apply(Spalten,1,function(x){SapData[[KPI]][,Date][x]})
            IndustryTs <- sapply(1:length(Date),function(x){SapData[[KPI]][,Date][,x][as.logical(Spalten[,x])]})
            }
            ##apply(SapData[[KPI]][,Date],1,function(x){[x]})
        if(sum(rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
            return(IndustryTs[rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq,])}     ## returns only those companies with enough observations
        else {CalcIndustryTs1(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
        
        }

 CalcIndustryTs2<-function(BooleanSapData,SapData,Date,Sic,SicDigits=4,KPI,Method){
        NoObsReq<-8
        NoCompReq<-5
        ##if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
     
        if(SicDigits==0){return(NA)}
        Boolean <- substring(BooleanSapData[[KPI]]$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
        Boolean[is.na(Boolean)]<-FALSE

        if(Method == "one"|Method == "two"){    
            ##BooleanSapData[[KPI]][,]==Sic
            if(Method == "one"){   
                Spalten<-Boolean*BooleanSapData[[KPI]][,max(Date)]
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
                } 
                else if(Method == "two"){   
                Spalten<-rowSums(Boolean*BooleanSapData[[KPI]][,Date])>0
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
                }  
                
            if(sum(rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs[rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq,])}     ## returns only those companies with enough observations
                else {CalcIndustryTs2(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
        else if(Method == "three"){ 
            Spalten<-Boolean*BooleanSapData[[KPI]][,Date]
            ##apply(Spalten,1,function(x){SapData[[KPI]][,Date][x]})
            IndustryTs <- sapply(1:length(Date),function(x){SapData[[KPI]][,Date][,x][as.logical(Spalten[,x])]})
            
            if(sum(sapply(IndustryTs,length)>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs[sapply(IndustryTs,length)>=NoObsReq])}     ## returns only those companies with enough observations
                else {CalcIndustryTs2(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
            
            
            ##apply(SapData[[KPI]][,Date],1,function(x){[x]})

        
        }     


CalcCorr<-function(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method){
        TimePeriod <- 10     ##     das irgendwie vorher definieren
        Date <- as.POSIXlt(Date)
        Date$year <- Date$year - (TimePeriod:1)  ## prüfen ob das mit der rheihenfolge passt
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
        AcIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicAcquiror,SicDigits=4,KPI,Method)  ## das mit 1 ist noch nicht gut --> verwirrden
        TaIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicTarget,SicDigits=4,KPI,Method)    ## das mit 1 ist noch nicht gut --> verwirrden
        ##print(TaIndustryTs)
        ##print(AcIndustryTs)
        ##if(any(is.na(AcIndustryTs,TaIndustryTs))){return(NA)}
            if(length(AcIndustryTs)==1|length(TaIndustryTs)==1){return(NA)}
        #### hier noch die regression mit dem Marktindex einfügen
        ##return(list(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        return(cor(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        ##return(list(AcIndustryTs,TaIndustryTs))
        } 
      
        
CalcIndustryTs<-function(BooleanSapData,SapData,Date,Sic,SicDigits=4,KPI,Method){
        NoObsReq<-8
        NoCompReq<-5
        ##if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
            SicL<-nchar(Sic)
        if (SicL<4){Sic<-paste(paste(rep(0,4-SicL),collapse=""),Sic,sep="")}
        
        if(SicDigits==0){return(NA)}
        Boolean <- substring(BooleanSapData[[KPI]]$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
        Boolean[is.na(Boolean)]<-FALSE

        if(Method == "one"|Method == "two"){    
            ##BooleanSapData[[KPI]][,]==Sic
            if(Method == "one"){   
                Spalten<-Boolean*BooleanSapData[[KPI]][,max(Date)]
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
                } 
                else if(Method == "two"){   
                Spalten<-rowSums(Boolean*BooleanSapData[[KPI]][,Date])>0
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
                }  
                
            if(sum(rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs[rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq,])}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
        else if(Method == "three"){ 
            Spalten<-Boolean*BooleanSapData[[KPI]][,Date]

            ##apply(Spalten,1,function(x){SapData[[KPI]][,Date][x]})
            IndustryTs <- sapply(Date,function(i_Date){SapData[[KPI]][,Date][,i_Date][as.logical(Spalten[,i_Date])]},USE.NAMES = TRUE)
            names(IndustryTs) <- Date
            
            if(sum(sapply(IndustryTs,length)>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs[sapply(IndustryTs,length)>=NoObsReq])}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
            
            
            ##apply(SapData[[KPI]][,Date],1,function(x){[x]})

        
        }   
        
  
CalcCorr(Boolen,SapData,6022,6034,"2011-07-21","MV","two")
        
system.time(CalcIndustryTs(Boolen,SapData,Date,6022,SicDigits=4,"MV","two") )
        
CalcIndustryTs2(Boolen,SapData,Date,6034,SicDigits=4,"MV","two")        

CalcIndustryTs(Boolen,SapData,Date,6034,SicDigits=4,"MV","three")   
 
ders<-function(x){print(x);if(x>0){ders(x-1)}} 

CorRow<-function(SumTab,BooleanSapData,SapData,KPI,Method){
        N_Values <- nrow(SumTab)
        Correlations <- rep(NA,N_Values)
        ##SicAcquiror,SicTarget,Date,
        for(n_row in 1:N_Values){
            SicAcquiror <- SumTab[n_row,"SicAc"]
            SicTarget <- SumTab[n_row,"SicTa"]
            Date <- SumTab[n_row,"Date"]
            Correlations[n_row] <- CalcCorr(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method)
            }
        Correlations <- as.numeric(Correlations)
        }
            
MethodOneTwo<-function(Date,Boolean,SicDigits=1,KPI,Method){

        }        

CalcIndustryTs(Date,Sic,SicDigits=1){
        BooleanSapData$Sic==Sic
        Method1    
        Spalten<-Boolean*SapDataBoolean[[KPI]][,max(Date)]
        ##SapData[SapDataBoolean[[KPI]][,max(Date)],]    
        Method2    
        Spalten<-colSums(Boolean*SapDataBoolean[[KPI]][,Date])>0
        ##Spalten<-apply(Boolean*SapDataBoolean[[KPI]][,Date],1,any)
        ##SapData[apply(SapDataBoolean[[KPI]][,Date],1,any),]    
        Method3 
        Spalten<-Boolean*SapDataBoolean[[KPI]][,Date]
        apply(Spalten,1,function(x){SapData[[KPI]][,Date][x]})
        sapply(,1,function(x)SapData[[KPI]][,Date][,x][Spalten[,1],]
        ##apply(SapData[[KPI]][,Date],1,function(x){[x]})
                if(sum(rowSums(!is.na(IndustryTs))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
            return(IndustryTs[rowSums(!is.na(IndustryTs))>=NoObsReq,])}     ## returns only those companies with enough observations
        else {CalcIndustryTs(Date,Sic,SicDigits+1)}
        
        }          
        
TsSicCalc <- function(SapDataTab = SapData2, SapSetOb = SapSet, CompChar = "MV", SapMat = SaPMat, ObsReq = 5){
   
   	CharCol <- SapSetOb$CharCol
   
    ##AddSicNumber <- function (SapDataElement){
    ##  SapDataElement<- merge(SaPMat[,c("DSCD","SIC_four")],SapDataElement,by.x="DSCD",by.y="DSCD",all=T,sort=F) 
    ##        SapDataElement <- SapDataElement [order(SapDataElement$DSCD),]
    ##    }
    

    
        
    ##SapDataTab <- lapply(SapDataTab,AddSicNumber)
   
   ##lapply(CharLevel,FUN=function(x){y<-SapDataTab[CharCol]==x;SapDataTab[y,]})

    ### temp <- merge(SapMat[,c("DSCD","SIC_four")],
	##		SapDataTab,by.x="DSCD",by.y="DSCD",all=T,sort=F)  ##hier prüfen ob das mit dem unterschiedlichen typen factor und variablen kollidiert
    ##SapDataTab <- temp
    ## rm(temp)
	##SapDataTab<-SapDataTab[order(SapDataTab$DSCD),]
    ##SapDataTab<-SapDataTab[order(match(SapDataTab$KPI,CharLevel)),]
    ##SapDataTab<-SapDataTab[order(match(SapDataTab$KPI,names(SapSetOb$CharRows))),]
        ##vll doch besser das ganze gleich in eine liste aufzuteilen(nach den KPI)
    
    ##SapSetOb$CharRows <- CharRowsSet()
    
    ##CharRows <- SapSetOb$CharRows
	##SicCol <- SapSetOb$
	SapDataTab <- SapDataTab[[CompChar]] ##Sowohl die Entfernung der CharRows als auch der Sic Colums kannst du eine Ebene/funktion vorher machen
	DscdCol <- SapSetOb$DscdCol
	##SicRm <- SapDataTab[,SicCol]== Sic 
	##SapDataTab <- SapDataTab[SicRm, ]

    
    SapDataTab <- SapDataTab[!is.na(SapDataTab[,"SIC_four"]),]
    
	SicList <- sapply(c("SicFour","SicThree","SicTwo","SicOne"),SicCreate,simplify=F,USE.NAMES = TRUE)
	##i <- 3
	for(i in seq(along = SicList)){
	SicListEl <- SicList[[i]]
        #####SicListEl <- SicList[[1]] #####
		SicLength <- nchar(SicListEl[1,"Sic"])
		##if(SicLength==1){SicCol="SIC_one"}
		##if(SicLength==2){SicCol="SIC_two"}
		##if(SicLength==3){SicCol="SIC_three"}
		##if(SicLength==4){SicCol="SIC_four"}
		
		SapDataTab[,"SIC"] <- substring(SapDataTab[,"SIC_four"],1,SicLength)	##Sic Col noch definieren	
		
		SicListEl <- TsSicElCalc(SicListEl,SapDataTab,DscdCol,SapMat,ObsReq)

		##for(i in seq(along=colnames(SicListEl[,-1]))){
		##	Datum <- colnames(SicListEl)[i+1]
		##	DscdRm <- SapDataTab[,DscdCol] %in% SaPMat[SaPMat[,Datum],"DSCD"]
		##	SapDataTab2 <- SapDataTab[DscdRm, ]
		##	##CharVal <- CalcCharVal(SapDataTab2, Sic, SapMat, Datum, ObsReq)
		##	CharVal <- sapply(SicListEl[,"Sic"],CalcCharVal, SapDataTab = SapDataTab2, SapMat = SaPMat, Datum = Datum, ObsReq = ObsReq)
		##	SicListEl[,Datum] <- CharVal
		##	}
		SicList[[i]] <- SicListEl
        ######SicList[[4]] <- SicListEl #####
		}
	SicList
	}
        
  system.time(abb<-TsSicCalc(SapDataTab = SapData2, SapSetOb = SapSet, CompChar = "MV", SapMat = SaPMat, ObsReq = 5))
         

     