######################
######################
##### for data Table##
#####Year/month#######
######################

CalcCorr<-function(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method){
       
        Date <- as.POSIXlt(Date)
        Date$mday <- 15        
            TimePeriod <- 10     ##     das irgendwie vorher definieren
            Date$mon <- Date$mon - (TimePeriod:1)  ## prüfen ob das mit der reihenfolge passt
            
            ##TimePeriod <- 7 
            ##Date$mon<- 00
            ##Date$year <- Date$year - (TimePeriod:1)  ## prüfen ob das mit der reihenfolge passt
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
        
        ##DateSap <- Date[Date>=1995]
        ##if(length(DateSap)<5){return(NA)} 
        ##if(length(DateSap)==0){return(NA)}
        DateSap <- Date
        print(Date)
        ##print(Date)
        AcIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date, DateSap,SicAcquiror,SicDigits=4,KPI,Method)  ## das mit 1 ist noch nicht gut --> verwirrden
        TaIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date, DateSap,SicTarget,SicDigits=4,KPI,Method)    ## das mit 1 ist noch nicht gut --> verwirrden

        ##print(TaIndustryTs)
        ##print(AcIndustryTs)
        ##if(any(is.na(AcIndustryTs,TaIndustryTs))){return(NA)}
            if(length(AcIndustryTs)==1|length(TaIndustryTs)==1){return(NA)}
            
            if(Method=="three"){
                return(cor(sapply(AcIndustryTs,sum,na.rm=T),sapply(TaIndustryTs,sum,na.rm=T)))
                }
            
        #### hier noch die regression mit dem Marktindex einfügen
        ##return(list(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        return(cor(colMeans(AcIndustryTs[,-(1:2),with=F],na.rm=T),colMeans(TaIndustryTs[,-(1:2),with=F],na.rm=T)))
        ##return(list(AcIndustryTs,TaIndustryTs))
        } 
 

CalcIndustryTs<-function(BooleanSapData,SapData,Date, DateSap,Sic,SicDigits=4,KPI,Method){ ## für eintabelliges BooleanSapData mit data table
            NoObsReq<-8
            NoCompReq<-5
            ### NoObsReq<-5
            ###NoCompReq<-4
        ##if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
            SicL<-nchar(Sic)
        if (SicL<4){Sic<-paste(paste(rep(0,4-SicL),collapse=""),Sic,sep="")}
        if(SicDigits==0){return(NA)}
        ##if(SicDigits==0){return(NA)}
        ##if else (SicDigits==4){SIC_Col<-SIC_four}
        ##if else (SicDigits==3){SIC_Col<-SIC_three}
        ##if else (SicDigits==2){SIC_Col<-SIC_two}
        ##if else (SicDigits==1){SIC_Col<-SIC_one}
        
        ##setkeyv(BooleanSapData,SIC_Col)
        Boolean <- substring(BooleanSapData$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
        Boolean[is.na(Boolean)]<-FALSE

        if(Method == "one"|Method == "two"){    
            ##BooleanSapData[[KPI]][,]==Sic
            if(Method == "one"){   
                Spalten<-Boolean*BooleanSapData[[max(Date)]]
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                } 
                else if(Method == "two"){   
                Spalten<-rowSums(Boolean*BooleanSapData[, DateSap,with=F])>0
                ##Spalten <- apply(Boolean*BooleanSapData[,Date,with=F],1,any)
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                }  
                
            if(sum(rowSums(!is.na(IndustryTs[,Date,with=F]))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs[rowSums(!is.na(IndustryTs[,Date,with=F]))>=NoObsReq,])}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date, DateSap,Sic,SicDigits-1,KPI,Method)}
            }
        else if(Method == "three"){ 
            Spalten<-Boolean*BooleanSapData[,Date,with=F]

            ##apply(Spalten,1,function(x){SapData[[KPI]][,Date][x]})
            IndustryTs <- sapply(Date,function(i_Date){SapData[[KPI]][[i_Date]][as.logical(Spalten[[i_Date]])]},simplify=F,USE.NAMES = TRUE) ##
            ##names(IndustryTs) <- Date
            
            if(all(sapply(IndustryTs,length)>=NoCompReq)){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs)}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date, DateSap,Sic,SicDigits-1,KPI,Method)}
            }
            
            
            ##apply(SapData[[KPI]][,Date],1,function(x){[x]})

        
        }   
 
        
  
##CalcCorr(Boolen,SapData,7371,7379,"2001-09-18","MV","two")
        
##system.time(CalcIndustryTs(Boolen,SapData,Date,6022,SicDigits=4,"MV","two") )
        

#######*******************************************************************
## Function um CalcCorr Funktion auf die ganze Tabelle SumTab anzuwenden

CorRow<-function(SumTab,BooleanSapData,SapData,KPI,Method){##data.table
        N_Values <- nrow(SumTab)
        ##N_Values <- 300
        Correlations <- rep(NA,N_Values)
        ##SicAcquiror,SicTarget,Date,
        for(n_row in 1:N_Values){
            Date <- SumTab$Date[n_row]
            SicAcquiror <- SumTab$Acquiror_Sic[n_row]
            SicTarget <- SumTab$Target_Sic[n_row]
  
            ##Date <- SumTab[n_row,"Date",with=F]
            ##SicAcquiror <- SumTab[n_row,"Acquiror_Sic",with=F]
            ##SicTarget <- SumTab[n_row,"Target_Sic",with=F]
            
            if(SicAcquiror==SicTarget){Correlations[n_row]<-1;next}
            if(as.Date(Date)<as.Date("1995-11-30")){Correlations[n_row]<-NA;next}
            print(n_row)
            Correlations[n_row] <- CalcCorr(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method)
            }
        Correlations <- as.numeric(Correlations)
        }
 
 
##system.time( b<-CorRow(testob,Boolen,SapData,"MV","one",Calc=F))

system.time( b<-CorRow(testob,Boolen,SapData,"MV","two"))


######################
######################
##### for data Table##
######################
######################

setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

library(data.table)
testob<-fread("testob.txt",sep=";",stringsAsFactors=F)
##testob[,Date:=as.Date(Date)]
##system.time(SaPConst1 <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character")))
system.time(SaPConst <- fread("S&Pverlauf_für_R.csv",sep=";",colClasses=c("character", "character","character","numeric","character")))

##SaPConst1$Datum <- as.Date(SaPConst1$Datum)
##SaPConst1$Datum <- substr(SaPConst1$Datum,1,7)
SaPConst[,Datum:=substr(as.Date(Datum),1,7)]


## Creates empty data frame which list the dates as columns and all the Companies that have been
## in the S&P 1500 as rows. The data frame will later be filled be TRUE and False values to indicated  
## the month of membership of the Companies in the S&P 1500


SaPcreate <- function(SaPConst = SaPConst,as_char=FALSE){

	ColVec<-sort(unique(SaPConst$Datum),decreasing=F) ## filters the different dates
	DSCDPos<-!duplicated(SaPConst$DSCD) ## filters the different DSCD
	##RowVec<-SaPConst$DSCD[!duplicated(SaPConst$DSCD)]
	RowVec<-SaPConst$DSCD[DSCDPos] 
	##SicVec<-SaPConst$WC07021[!duplicated(SaPConst$DSCD)]
	SicVec<-SaPConst$WC07021[DSCDPos] ## filters different Sic positions
    NamesVec<-SaPConst$Name[DSCDPos]
	
	SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")

	RowLeng<-length(RowVec)
	ColLeng<-length(ColVec)+3+3
	SaPMat<-matrix(rep(NA,RowLeng*ColLeng),nrow=RowLeng,ncol=ColLeng)
	SaPMat<-as.data.table(as.data.frame(SaPMat))

	setnames(SaPMat,colnames(SaPMat),c("Name","DSCD","SIC_four","SIC_three","SIC_two","SIC_one",ColVec))
	##SaPMat[,1]<-NamesVec
    ##SaPMat[,2]<-RowVec
	##SaPMat[,3]<-as.factor(SicVec)
	##SaPMat[,4]<-as.factor(substr(SicVec,1,3))
	##SaPMat[,5]<-as.factor(substr(SicVec,1,2))
	##SaPMat[,6]<-as.factor(substr(SicVec,1,1))
    SaPMat[,Name:=NamesVec]
    SaPMat[,DSCD:=RowVec]
	SaPMat[,SIC_four:=as.factor(SicVec)]
	SaPMat[,SIC_three:=as.factor(substr(SicVec,1,3))]
	SaPMat[,SIC_two:=as.factor(substr(SicVec,1,2))]
	SaPMat[,SIC_one:=as.factor(substr(SicVec,1,1))]
    
    if(as_char){SaPMat[,{ColVec}:=as.character(rep(NA,RowLeng))]}
    setkey(SaPMat,"DSCD")
	SaPMat
	}
  
SaPcreate <- function(SaPConst = SaPConst,as_char=FALSE){

	ColVec<-sort(unique(SaPConst$Datum),decreasing=F) ## filters the different dates
	DSCDPos<-!duplicated(SaPConst$DSCD) ## filters the different DSCD
	##RowVec<-SaPConst$DSCD[!duplicated(SaPConst$DSCD)]
	RowVec<-SaPConst$DSCD[DSCDPos] 
	##SicVec<-SaPConst$WC07021[!duplicated(SaPConst$DSCD)]
	SicVec<-SaPConst$WC07021[DSCDPos] ## filters different Sic positions
    NamesVec<-SaPConst$Name[DSCDPos]
	
	SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")

	##RowLeng<-length(RowVec)
	##ColLeng<-length(ColVec)+3+3
	##SaPMat<-matrix(rep(NA,RowLeng*ColLeng),nrow=RowLeng,ncol=ColLeng)
	##SaPMat<-as.data.table(as.data.frame(SaPMat))

	##setnames(SaPMat,colnames(SaPMat),c("Name","DSCD","SIC_four","SIC_three","SIC_two","SIC_one",ColVec))
	##SaPMat[,1]<-NamesVec
    ##SaPMat[,2]<-RowVec
	##SaPMat[,3]<-as.factor(SicVec)
	##SaPMat[,4]<-as.factor(substr(SicVec,1,3))
	##SaPMat[,5]<-as.factor(substr(SicVec,1,2))
	##SaPMat[,6]<-as.factor(substr(SicVec,1,1))

    SaPMat<-data.table(Name=NamesVec)
    SaPMat[,c("DSCD","SIC_four","SIC_three","SIC_two","SIC_one"):=data.frame((RowVec),SicVec,
                    substr(SicVec,1,3),substr(SicVec,1,2),substr(SicVec,1,1),stringsAsFactors=F)]

    RowLeng<-length(RowVec)

    if(as_char){SaPMat[,{ColVec}:=as.character(rep(NA,RowLeng))]}
    else {SaPMat[,{ColVec}:=rep(NA,RowLeng)]}
    SaPMat[["2012-10"]]<-as.character(NA)

    ##if(as_char){SaPMat[,{ColVec}:=SaPMat[[ColVec]]]}
    setkey(SaPMat,"DSCD")
	SaPMat
	}
  
SaPfill <- function(SaPMatrix = SaPMat,SaPConstitution = SaPConst){

	SaPvalues <- function(x){
	SaPMatrix[[x]] <<- SaPMatrix[,DSCD]%in%SaPConstitution[SaPConstitution$Datum==colnames(SaPMatrix)[x],DSCD]
	invisible()
	}
	
	for(i in 7:ncol(SaPMatrix)){SaPvalues(i)}
	##temp <- sapply(6:ncol(SaPMatrix),SaPvalues)
	##rm(temp)

	SaPMatrix
	}
  
system.time(SaPMat <- SaPcreate(SaPConst))
##system.time(SaPMat2 <- SaPcreate(SaPConst,as_char=TRUE))
system.time(SaPMat <- SaPfill(SaPMat, SaPConst))
##set(SaPMat2,which(SaPMat[["2012-11"]]),j="adsf",value=SaPMat[["DSCD"]][SaPMat[["2012-11"]]])
##set(SaPMat2,which(SaPMat[["2012-11"]]),j="2012-11",value=SaPMat[["DSCD"]][SaPMat[["2012-11"]]])
##n_col<-"2012-12"
##set(SaPMat2,i=which(SaPMat[[n_col]]),j=n_col,value=SaPMat[["DSCD"]][SaPMat[[n_col]]])

##SaPMat2[SaPMat[[9]],(9):=SaPMat[["DSCD"]][SaPMat[[9]]]]
###***********************************************************************************************************
## ließt tabelle mit den company properties ein
SapData_Read <- function(SapMat=SaPMat,CompPropCol="KPI",NaRm = FALSE){
            
        SapData <- fread("S&Pkons.csv",sep=";",stringsAsFactors=F)
        ## SapData2 <- read.csv("S&Pkons.csv",sep=";",dec=".",stringsAsFactors=F)
        
        ColNames <- as.character(colnames(SapData)[-c(1:3)])
        NewColNames <- c(colnames(SapData[,1:3]),substring(ColNames,1,7))
 
        setnames(SapData,ColNames,NewColNames)

        CharLevel <- unique(as.character(SapData[[CompPropCol]]))
        ##CharLevel <- unique(as.character(SapData2[,"KPI"]))
        SapData<- merge(SapMat[,c("DSCD","SIC_four"),with=F],SapData,by="DSCD",all=T,sort=F)
        ##if(NaRm){SapData <- SapData[!is.na(SapData[,"SIC_four"]),]}
        ##SapData3<- merge(SaPMat[,c("DSCD","SIC_four")],SapData2,by.x="DSCD",by.y="DSCD",all=T,sort=F) 
       
        SapData <- sapply(CharLevel,FUN=function(x){y<-SapData[[CompPropCol]]==x;y=SapData[y,]},simplify=F,USE.NAMES = TRUE) 
        ##SapData <<- SapData
    
       ##SapData4  <- sapply(CharLevel,FUN=function(x){y<-SapData2["KPI"]==x;y=SapData2[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        ##SapData  <- sapply(CharLevel,function(x){y<-SapData[CompPropCol]==x;y=SapData[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        ##SapData5 <- sapply(SapData4,FUN=function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){x[order(x$DSCD),]},simplify=F,USE.NAMES = TRUE)

       SapData <- sapply(names(SapData),SapData_Complete,SapData=SapData,simplify=F,USE.NAMES = TRUE)
       
       if(NaRm){SapData <- sapply(names(SapData),function(x){SapData[[x]][!is.na(SapData[[x]][,"SIC_four"]),]},simplify=F,USE.NAMES = TRUE)}
    if(NaRm){SapData <- sapply(names(SapData),function(x){SapData[[x]][!is.na(SapData[[x]][["SIC_four"]]),]},simplify=F,USE.NAMES = TRUE)}

       SapData<<-SapData
       }


       SapData_Complete  <- function(CompPropLevel,SapData){
            PropTable <- SapData[[CompPropLevel]]
            MissingDscd <- !is.element(SaPMat$DSCD,PropTable$DSCD)

            if(sum(MissingDscd)==0){
                   PropTable<-PropTable[order(PropTable$DSCD),]
                   rownames(PropTable)<-PropTable$DSCD
                   return(PropTable)
                   }
            
            MissingDscd <- SaPMat[MissingDscd,c("DSCD","SIC_four","Name"),with=F]
            NaVal <- matrix(rep(NA,nrow(MissingDscd)*(ncol(PropTable)-4)),ncol=ncol(PropTable)-4,nrow=nrow(MissingDscd))

            NewPropTable<-cbind(MissingDscd,"KPI"=CompPropLevel,NaVal)

            setnames(NewPropTable,colnames(NewPropTable),colnames(PropTable))
            ##colnames(NewPropTable) <- colnames(PropTable)
            NewPropTable <- rbind(PropTable, NewPropTable)
            NewPropTable<-NewPropTable[order(NewPropTable$DSCD),]
            rownames(NewPropTable)<-NewPropTable$DSCD
            NewPropTable
            }
        
system.time(SapData_Read())
##system.time(SapData_Read(SapMat=SaPMat,CompPropCol="KPI",NaRm = TRUE))



##*******************************************************************************************************************
## neue tabelle die identisch zu  SapData aufgebaut ist aber anstelle der Unternehmenskennzahlen die zugehörigkeit zum SaP 1500 angibt
######### hier könntest du noch dran arbeiten und erreichen dass SapData in jeder Tabelle der Liste immer alle Unternehmen nennt bzw. für die
######### für die keine Kennzahlenwerte vorhanden sind NA einträgt dann bräuchstest du nur eine zugehärigkeitstablle T/F die für jede SapData dann 
######### dieselbe ist

##Boolen<-Compute_SapDataBoolean(SapData,SaPMat)  

Compute_SapDataBoolean<-function(SapData = SapData,SaPMat){         ## hier nochdeoppelte Benennung
        setkey(SaPMat,DSCD)
        SapDataBoolean<-SaPMat[SapData$MV$DSCD,]

        SapDataBoolean
        }           

Boolen<-Compute_SapDataBoolean(SapData,SaPMat)               

### ********************************************************************************************************************
## zwei funktion die auf einander zugreifen um die Korrelationen aus SumTab zu berechen
## es wird die SapMat, 


CalcCorr<-function(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method){
        TimePeriod <- 10     ##     das irgendwie vorher definieren
       
        Date <- as.POSIXlt(Date)
        Date$mday <- 15        
        Date$mon <- Date$mon - (TimePeriod:1)  ## prüfen ob das mit der reihenfolge passt
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
        print(Date)
        ##print(Date)
        AcIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicAcquiror,SicDigits=4,KPI,Method)  ## das mit 1 ist noch nicht gut --> verwirrden
        TaIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicTarget,SicDigits=4,KPI,Method)    ## das mit 1 ist noch nicht gut --> verwirrden
        ##print(TaIndustryTs)
        ##print(AcIndustryTs)
        ##if(any(is.na(AcIndustryTs,TaIndustryTs))){return(NA)}
            if(length(AcIndustryTs)==1|length(TaIndustryTs)==1){return(NA)}
            
            if(Method=="three"){
                return(cor(sapply(AcIndustryTs,sum,na.rm=T),sapply(TaIndustryTs,sum,na.rm=T)))
                }
            
        #### hier noch die regression mit dem Marktindex einfügen
        ##return(list(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        return(cor(colMeans(AcIndustryTs[,-(1:2),with=F],na.rm=T),colMeans(TaIndustryTs[,-(1:2),with=F],na.rm=T)))
        ##return(list(AcIndustryTs,TaIndustryTs))
        } 
 

CalcIndustryTs<-function(BooleanSapData,SapData,Date,Sic,SicDigits=4,KPI,Method){ ## für eintabelliges BooleanSapData mit data table
        NoObsReq<-8
        NoCompReq<-5
        ##if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
            SicL<-nchar(Sic)
        if (SicL<4){Sic<-paste(paste(rep(0,4-SicL),collapse=""),Sic,sep="")}
        if(SicDigits==0){return(NA)}
        ##if(SicDigits==0){return(NA)}
        ##if else (SicDigits==4){SIC_Col<-SIC_four}
        ##if else (SicDigits==3){SIC_Col<-SIC_three}
        ##if else (SicDigits==2){SIC_Col<-SIC_two}
        ##if else (SicDigits==1){SIC_Col<-SIC_one}
        
        ##setkeyv(BooleanSapData,SIC_Col)
        Boolean <- substring(BooleanSapData$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
        Boolean[is.na(Boolean)]<-FALSE

        if(Method == "one"|Method == "two"){    
            ##BooleanSapData[[KPI]][,]==Sic
            if(Method == "one"){   
                Spalten<-Boolean*BooleanSapData[[max(Date)]]
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                } 
                else if(Method == "two"){   
                Spalten<-rowSums(Boolean*BooleanSapData[,Date,with=F])>0
                ##Spalten <- apply(Boolean*BooleanSapData[,Date,with=F],1,any)
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                }  
                
            if(sum(rowSums(!is.na(IndustryTs[,Date,with=F]))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs[rowSums(!is.na(IndustryTs[,Date,with=F]))>=NoObsReq,])}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
        else if(Method == "three"){ 
            Spalten<-Boolean*BooleanSapData[,Date,with=F]

            ##apply(Spalten,1,function(x){SapData[[KPI]][,Date][x]})
            IndustryTs <- sapply(Date,function(i_Date){SapData[[KPI]][[i_Date]][as.logical(Spalten[[i_Date]])]},simplify=F,USE.NAMES = TRUE) ##
            ##names(IndustryTs) <- Date
            
            if(all(sapply(IndustryTs,length)>=NoCompReq)){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs)}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
            
            
            ##apply(SapData[[KPI]][,Date],1,function(x){[x]})

        
        }   
 
        
  
##CalcCorr(Boolen,SapData,7371,7379,"2001-09-18","MV","two")
        
##system.time(CalcIndustryTs(Boolen,SapData,Date,6022,SicDigits=4,"MV","two") )
        

#######*******************************************************************
## Function um CalcCorr Funktion auf die ganze Tabelle SumTab anzuwenden

CorRow<-function(SumTab,BooleanSapData,SapData,KPI,Method){##data.table
        N_Values <- nrow(SumTab)
        ##N_Values <- 50
        Correlations <- rep(NA,N_Values)
        ##SicAcquiror,SicTarget,Date,
        for(n_row in 1:N_Values){
            Date <- SumTab$Date[n_row]
            SicAcquiror <- SumTab$Acquiror_Sic[n_row]
            SicTarget <- SumTab$Target_Sic[n_row]
  
            ##Date <- SumTab[n_row,"Date",with=F]
            ##SicAcquiror <- SumTab[n_row,"Acquiror_Sic",with=F]
            ##SicTarget <- SumTab[n_row,"Target_Sic",with=F]
            
            if(SicAcquiror==SicTarget){Correlations[n_row]<-1;next}
            if(as.Date(Date)<as.Date("1995-11-30")){Correlations[n_row]<-NA;next}
  
            Correlations[n_row] <- CalcCorr(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method)
            }
        Correlations <- as.numeric(Correlations)
        }
 
 
##system.time( b<-CorRow(testob,Boolen,SapData,"MV","one",Calc=F))

system.time( b<-CorRow(testob,Boolen,SapData,"MV","two"))

######################
######################
##### for data frame##
######################
######################

setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

library(data.table)
testob<-read.table("testob.txt",sep=";",stringsAsFactors=F,header = T)
##testob[,Date:=as.Date(Date)]
system.time(SaPConst <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character")))
##system.time(SaPConst <- fread("S&Pverlauf_für_R.csv",sep=";",colClasses=c("character", "character","character","numeric","character")))

SaPConst$Datum <- as.Date(SaPConst$Datum)
SaPConst$Datum <- substr(SaPConst$Datum,1,7)
##SaPConst[,Datum:=substr(as.Date(Datum),1,7)]


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
    NamesVec<-SaPConst$Name[DSCDPos]
	
	SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")

	RowLeng<-length(RowVec)
	ColLeng<-length(ColVec)+3+3
	SaPMat<-matrix(rep(NA,RowLeng*ColLeng),nrow=RowLeng,ncol=ColLeng)
	SaPMat<-as.data.frame(SaPMat)

	colnames(SaPMat)<-c("Name","DSCD","SIC_four","SIC_three","SIC_two","SIC_one",ColVec)
	SaPMat[,1]<-NamesVec
    SaPMat[,2]<-RowVec
	SaPMat[,3]<-as.factor(SicVec)
	SaPMat[,4]<-as.factor(substr(SicVec,1,3))
	SaPMat[,5]<-as.factor(substr(SicVec,1,2))
	SaPMat[,6]<-as.factor(substr(SicVec,1,1))

	SaPMat
	}    
    
  
SaPfill <- function(SaPMatrix = SaPMat,SaPConstitution = SaPConst){

	SaPvalues <- function(x){
	SaPMatrix[,x] <<- SaPMatrix[,"DSCD"]%in%SaPConstitution[SaPConstitution$Datum==colnames(SaPMatrix)[x],"DSCD"]
	invisible()
	}
	
	for(i in 7:ncol(SaPMatrix)){SaPvalues(i)}
	##temp <- sapply(6:ncol(SaPMatrix),SaPvalues)
	##rm(temp)

	SaPMatrix
	}    



system.time(SaPMat <- SaPcreate(SaPConst))

system.time(SaPMat <- SaPfill(SaPMat, SaPConst))



SapData_Read <- function(SapMat=SaPMat,CompPropCol="KPI",NaRm = FALSE){
            
        SapData <- read.csv("S&Pkons.csv",sep=";",dec=".",stringsAsFactors=F)
        ## SapData2 <- read.csv("S&Pkons.csv",sep=";",dec=".",stringsAsFactors=F)
        Dates <- as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
        ColNames <- c(colnames(SapData[,1:3]),substring(Dates,1,7))
        colnames(SapData) <- ColNames        

        CharLevel <- unique(as.character(SapData[,CompPropCol]))
        ##CharLevel <- unique(as.character(SapData2[,"KPI"]))
        SapData<- merge(SapMat[,c("DSCD","SIC_four")],SapData,by.x="DSCD",by.y="DSCD",all=T,sort=F)
        ##if(NaRm){SapData <- SapData[!is.na(SapData[,"SIC_four"]),]}
        ##SapData3<- merge(SaPMat[,c("DSCD","SIC_four")],SapData2,by.x="DSCD",by.y="DSCD",all=T,sort=F) 
       
        SapData <- sapply(CharLevel,FUN=function(x){y<-SapData[CompPropCol]==x;y=SapData[y,]},simplify=F,USE.NAMES = TRUE) 
        ##SapData <<- SapData
    
       ##SapData4  <- sapply(CharLevel,FUN=function(x){y<-SapData2["KPI"]==x;y=SapData2[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        ##SapData  <- sapply(CharLevel,function(x){y<-SapData[CompPropCol]==x;y=SapData[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        ##SapData5 <- sapply(SapData4,FUN=function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){x[order(x$DSCD),]},simplify=F,USE.NAMES = TRUE)

       SapData <- sapply(names(SapData),SapData_Complete,SapData=SapData,SaPMat=SapMat,simplify=F,USE.NAMES = TRUE)
       
       if(NaRm){SapData <- sapply(names(SapData),function(x){SapData[[x]][!is.na(SapData[[x]][,"SIC_four"]),]},simplify=F,USE.NAMES = TRUE)}
   
       SapData<<-SapData
       }


SapData_Complete  <- function(CompPropLevel,SapData,SaPMat){
            PropTable <- SapData[[CompPropLevel]]
            MissingDscd <- !is.element(SaPMat$DSCD,PropTable$DSCD)

            if(sum(MissingDscd)==0){
                   PropTable<-PropTable[order(PropTable$DSCD),]
                   rownames(PropTable)<-PropTable$DSCD
                   return(PropTable)
                   }
            
            MissingDscd <- SaPMat[MissingDscd,c("DSCD","SIC_four","Name")]
            NaVal <- matrix(rep(NA,nrow(MissingDscd)*(ncol(PropTable)-4)),ncol=ncol(PropTable)-4,nrow=nrow(MissingDscd))

            NewPropTable<-cbind(MissingDscd,"KPI"=CompPropLevel,NaVal)

            setnames(NewPropTable,colnames(NewPropTable),colnames(PropTable))
            ##colnames(NewPropTable) <- colnames(PropTable)
            NewPropTable <- rbind(PropTable, NewPropTable)
            NewPropTable<-NewPropTable[order(NewPropTable$DSCD),]
            rownames(NewPropTable)<-NewPropTable$DSCD
            NewPropTable
        }

##system.time(SapData_Read())
system.time(SapData_Read(SapMat=SaPMat,CompPropCol="KPI",NaRm = FALSE))

Compute_SapDataBoolean<-function(SapData = SapData,SaPMat){         ## hier nochdeoppelte Benennung
        rownames(SaPMat)<-SaPMat[,"DSCD"]
        SapDataBoolean<-SaPMat[SapData$MV[,"DSCD"],]

        SapDataBoolean
        }   
      
Boolen<-Compute_SapDataBoolean(SapData,SaPMat)      
      
CalcCorr<-function(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method){
        TimePeriod <- 10     ##     das irgendwie vorher definieren
       
        Date <- as.POSIXlt(Date)
        Date$mday <- 15        
        Date$mon <- Date$mon - (TimePeriod:1)  ## prüfen ob das mit der reihenfolge passt
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
        ##print(Date)
        AcIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicAcquiror,SicDigits=4,KPI,Method)  ## das mit 1 ist noch nicht gut --> verwirrden
        TaIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicTarget,SicDigits=4,KPI,Method)    ## das mit 1 ist noch nicht gut --> verwirrden
        ##print(TaIndustryTs)
        ##print(AcIndustryTs)
        ##if(any(is.na(AcIndustryTs,TaIndustryTs))){return(NA)}
            if(length(AcIndustryTs)==1|length(TaIndustryTs)==1){return(NA)}
            
            if(Method=="three"){
                return(cor(sapply(AcIndustryTs,sum,na.rm=T),sapply(TaIndustryTs,sum,na.rm=T)))
                }
            
        #### hier noch die regression mit dem Marktindex einfügen
        ##return(list(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        return(cor(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        ##return(list(AcIndustryTs,TaIndustryTs))
        } 
 


CalcIndustryTs<-function(BooleanSapData,SapData,Date,Sic,SicDigits=4,KPI,Method){ ## für eintabelliges BooleanSapData
        NoObsReq<-8
        NoCompReq<-5
        ##if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
        ##else if(SicDigits=1){}
            SicL<-nchar(Sic)
        if (SicL<4){Sic<-paste(paste(rep(0,4-SicL),collapse=""),Sic,sep="")}
        
        if(SicDigits==0){return(NA)}
        Boolean <- substring(BooleanSapData$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
        Boolean[is.na(Boolean)]<-FALSE

        if(Method == "one"|Method == "two"){    
            ##BooleanSapData[[KPI]][,]==Sic
            if(Method == "one"){   
                Spalten<-Boolean*BooleanSapData[,max(Date)]
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
                } 
                else if(Method == "two"){   
                Spalten<-rowSums(Boolean*BooleanSapData[,Date])>0
                IndustryTs <- SapData[[KPI]][as.logical(Spalten),c("DSCD","SIC_four",Date)]
                }  
                
            if(sum(rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq)>=NoCompReq){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs[rowSums(!is.na(IndustryTs[,Date]))>=NoObsReq,])}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
        else if(Method == "three"){ 
            Spalten<-Boolean*BooleanSapData[,Date]

            ##apply(Spalten,1,function(x){SapData[[KPI]][,Date][x]})
            IndustryTs <- sapply(Date,function(i_Date){SapData[[KPI]][,Date][,i_Date][as.logical(Spalten[,i_Date])]},USE.NAMES = TRUE)
            names(IndustryTs) <- Date
            
            if(all(sapply(IndustryTs,length)>=NoCompReq)){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs)}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
            
            
            ##apply(SapData[[KPI]][,Date],1,function(x){[x]})

        
        }   
              
       

  
CalcCorr(Boolen,SapData,8744,9223,"2005-11-04","MV","two")
        
system.time(CalcIndustryTs(Boolen,SapData,Date,6022,SicDigits=4,"MV","two") )

system.time(CalcIndustryTs(Boolen,SapData,Date,9223,SicDigits=4,"MV","two") )


 CorRow<-function(SumTab,BooleanSapData,SapData,KPI,Method){##data.frame
        N_Values <- nrow(SumTab)
        ##N_Values <- 50
        Correlations <- rep(NA,N_Values)
        ##SicAcquiror,SicTarget,Date,
        
        for(n_row in 1:N_Values){
            Date <- SumTab[n_row,"Date"]
            SicAcquiror <- SumTab[n_row,"Acquiror_Sic"]
            SicTarget <- SumTab[n_row,"Target_Sic"]
            
            if(SicAcquiror==SicTarget){Correlations[n_row]<-1;next}
            if(as.Date(Date)<as.Date("1995-11-30")){Correlations[n_row]<-NA;next}
  
            Correlations[n_row] <- CalcCorr(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method)
            }
        Correlations <- as.numeric(Correlations)
        }
 
system.time( b<-CorRow(testob,Boolen,SapData,"MV","one")) 


######################
######################
##### for data frame##
######with list#######
######################

setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

library(data.table)
testob<-read.table("testob.txt",sep=";",stringsAsFactors=F,header = T)
##testob[,Date:=as.Date(Date)]
system.time(SaPConst <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character")))
##system.time(SaPConst <- fread("S&Pverlauf_für_R.csv",sep=";",colClasses=c("character", "character","character","numeric","character")))

SaPConst$Datum <- as.Date(SaPConst$Datum)
SaPConst$Datum <- substr(SaPConst$Datum,1,7)
##SaPConst[,Datum:=substr(as.Date(Datum),1,7)]


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
    NamesVec<-SaPConst$Name[DSCDPos]
	
	SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")

	RowLeng<-length(RowVec)
	ColLeng<-length(ColVec)+3+3
	SaPMat<-matrix(rep(NA,RowLeng*ColLeng),nrow=RowLeng,ncol=ColLeng)
	SaPMat<-as.data.frame(SaPMat)

	colnames(SaPMat)<-c("Name","DSCD","SIC_four","SIC_three","SIC_two","SIC_one",ColVec)
	SaPMat[,1]<-NamesVec
    SaPMat[,2]<-RowVec
	SaPMat[,3]<-as.factor(SicVec)
	SaPMat[,4]<-as.factor(substr(SicVec,1,3))
	SaPMat[,5]<-as.factor(substr(SicVec,1,2))
	SaPMat[,6]<-as.factor(substr(SicVec,1,1))

	SaPMat
	}    
    
  
SaPfill <- function(SaPMatrix = SaPMat,SaPConstitution = SaPConst){

	SaPvalues <- function(x){
	SaPMatrix[,x] <<- SaPMatrix[,"DSCD"]%in%SaPConstitution[SaPConstitution$Datum==colnames(SaPMatrix)[x],"DSCD"]
	invisible()
	}
	
	for(i in 7:ncol(SaPMatrix)){SaPvalues(i)}
	##temp <- sapply(6:ncol(SaPMatrix),SaPvalues)
	##rm(temp)

	SaPMatrix
	}    



system.time(SaPMat <- SaPcreate(SaPConst))

system.time(SaPMat <- SaPfill(SaPMat, SaPConst))



SapData_Read <- function(SapMat=SaPMat,CompPropCol="KPI"){        
        SapData <- read.csv("S&Pkons.csv",sep=";",dec=".",stringsAsFactors=F)
        ## SapData2 <- read.csv("S&Pkons.csv",sep=";",dec=".",stringsAsFactors=F)
        Dates <- as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
        ColNames <- c(colnames(SapData[,1:3]),substring(Dates,1,7))
        colnames(SapData) <- ColNames        

        CharLevel <- unique(as.character(SapData[,CompPropCol]))
        ##CharLevel <- unique(as.character(SapData2[,"KPI"]))
        SapData<- merge(SapMat[,c("DSCD","SIC_four")],SapData,by.x="DSCD",by.y="DSCD",all=T,sort=F)
        ##SapData3<- merge(SaPMat[,c("DSCD","SIC_four")],SapData2,by.x="DSCD",by.y="DSCD",all=T,sort=F) 
        ##SapData <- sapply(CharLevel,FUN=function(x){y<-SapData[CompPropCol]==x;y=SapData[y,]},simplify=F,USE.NAMES = TRUE) 

      
        SapData  <- sapply(CharLevel,FUN=function(x){y<-SapData[CompPropCol]==x;y=SapData[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        ##SapData  <- sapply(CharLevel,function(x){y<-SapData[CompPropCol]==x;y=SapData[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        ##SapData <- sapply(SapData,FUN=function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
        SapData <- sapply(SapData,FUN=function(x){x[order(x$DSCD),]},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){x[order(x$DSCD),]},simplify=F,USE.NAMES = TRUE)
       SapData<<-SapData
        }
##*************


##system.time(SapData_Read())
system.time(SapData_Read(SapMat=SaPMat,CompPropCol="KPI"))

 
##**********************************************************        
Compute_SapDataBoolean<-function(SapData = SapData,SaPMat){         ## hier nochdeoppelte Benennung
        
        SapDataBoolean <- sapply(names(SapData),function(x){x},simplify=F,USE.NAMES = TRUE)
        for(i_table in SapDataBoolean){
        rownames(SaPMat)<-SaPMat[,"DSCD"]
        SapDataBoolean[[i_table]]<-SaPMat[SapData[[i_table]][,"DSCD"],]
        }
        SapDataBoolean
        }        
##*******
      
Boolen<-Compute_SapDataBoolean(SapData,SaPMat)      
      
CalcCorr<-function(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method){
        TimePeriod <- 10     ##     das irgendwie vorher definieren
       
        Date <- as.POSIXlt(Date)
        Date$mday <- 15        
        Date$mon <- Date$mon - (TimePeriod:1)  ## prüfen ob das mit der reihenfolge passt
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
        ##print(Date)
        AcIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicAcquiror,SicDigits=4,KPI,Method)  ## das mit 1 ist noch nicht gut --> verwirrden
        TaIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,SicTarget,SicDigits=4,KPI,Method)    ## das mit 1 ist noch nicht gut --> verwirrden
        ##print(TaIndustryTs)
        ##print(AcIndustryTs)
        ##if(any(is.na(AcIndustryTs,TaIndustryTs))){return(NA)}
            if(length(AcIndustryTs)==1|length(TaIndustryTs)==1){return(NA)}
            
            if(Method=="three"){
                return(cor(sapply(AcIndustryTs,sum,na.rm=T),sapply(TaIndustryTs,sum,na.rm=T)))
                }
            
        #### hier noch die regression mit dem Marktindex einfügen
        ##return(list(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        return(cor(colMeans(AcIndustryTs[,-(1:2)],na.rm=T),colMeans(TaIndustryTs[,-(1:2)],na.rm=T)))
        ##return(list(AcIndustryTs,TaIndustryTs))
        } 
 
         
CalcIndustryTs<-function(BooleanSapData,SapData,Date,Sic,SicDigits=4,KPI,Method){ ##für BooleanSapData als liste
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
            
            if(all(sapply(IndustryTs,length)>=NoCompReq)){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs)}     ## returns only those companies with enough observations
                else {CalcIndustryTs(BooleanSapData,SapData,Date,Sic,SicDigits-1,KPI,Method)}
            }
            
            
            ##apply(SapData[[KPI]][,Date],1,function(x){[x]})

        
        }   

  
CalcCorr(Boolen,SapData,8744,9223,"2005-11-04","MV","two")
        
system.time(CalcIndustryTs(Boolen,SapData,Date,6022,SicDigits=4,"MV","two") )

system.time(CalcIndustryTs(Boolen,SapData,Date,9223,SicDigits=4,"MV","two") )


 CorRow<-function(SumTab,BooleanSapData,SapData,KPI,Method){##data.frame
        N_Values <- nrow(SumTab)
        ##N_Values <- 50
        Correlations <- rep(NA,N_Values)
        ##SicAcquiror,SicTarget,Date,
        
        for(n_row in 1:N_Values){
            Date <- SumTab[n_row,"Date"]
            SicAcquiror <- SumTab[n_row,"Acquiror_Sic"]
            SicTarget <- SumTab[n_row,"Target_Sic"]
            
            if(SicAcquiror==SicTarget){Correlations[n_row]<-1;next}
            if(as.Date(Date)<as.Date("1995-11-30")){Correlations[n_row]<-NA;next}
  
            Correlations[n_row] <- CalcCorr(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,KPI,Method)
            }
        Correlations <- as.numeric(Correlations)
        }
 
system.time( b<-CorRow(testob,Boolen,SapData,"MV","one")) 


##*****************************************************************


######################
###for creating ######
###SaPMat with #######
###DSCDs instead #####
##### of T/F  ########
######################

setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

library(data.table)
testob<-read.table("testob.txt",sep=";",stringsAsFactors=F,header = T)
##testob[,Date:=as.Date(Date)]
system.time(SaPConst <- read.csv("S&Pverlauf_für_R.csv",sep=";",dec=".",colClasses=c("character", "character","character","numeric","character")))
##system.time(SaPConst <- fread("S&Pverlauf_für_R.csv",sep=";",colClasses=c("character", "character","character","numeric","character")))

SaPConst$Datum <- as.Date(SaPConst$Datum)
SaPConst$Datum <- substr(SaPConst$Datum,1,7)
##SaPConst[,Datum:=substr(as.Date(Datum),1,7)]


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
    NamesVec<-SaPConst$Name[DSCDPos]
	
	SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")

	RowLeng<-length(RowVec)
	ColLeng<-length(ColVec)+3+3
	SaPMat<-matrix(rep(NA,RowLeng*ColLeng),nrow=RowLeng,ncol=ColLeng)
	SaPMat<-as.data.frame(SaPMat)

	colnames(SaPMat)<-c("Name","DSCD","SIC_four","SIC_three","SIC_two","SIC_one",ColVec)
	SaPMat[,1]<-NamesVec
    SaPMat[,2]<-RowVec
	SaPMat[,3]<-as.factor(SicVec)
	SaPMat[,4]<-as.factor(substr(SicVec,1,3))
	SaPMat[,5]<-as.factor(substr(SicVec,1,2))
	SaPMat[,6]<-as.factor(substr(SicVec,1,1))

	SaPMat
	}    
    
  
SaPfill <- function(SaPMatrix = SaPMat,SaPConstitution = SaPConst){

	SaPvalues <- function(x){
	SaPMatrix[,x] <<- SaPMatrix[,"DSCD"]%in%SaPConstitution[SaPConstitution$Datum==colnames(SaPMatrix)[x],"DSCD"]
	invisible()
	}
	
	for(i in 7:ncol(SaPMatrix)){SaPvalues(i)}
	##temp <- sapply(6:ncol(SaPMatrix),SaPvalues)
	##rm(temp)

	SaPMatrix
	}    



system.time(SaPMat <- SaPcreate(SaPConst))

system.time(SaPMat <- SaPfill(SaPMat, SaPConst))

##**********************************************************
SapMatConv<-function(SaPMat){
        for (i_col in 7:ncol(SaPMat)){
            temp <- SaPMat[,i_col]
            temp[temp] <- SaPMat[temp,"DSCD"]
            SaPMat[,i_col] <- temp
            }
        SaPMatDscd <<- SaPMat  
        }        
            
    
## fills SaPMat with TRUE and FALSE values according to their membership in the S&P 1500  

SapMatConv( SaPMat) 
 ##**********************************************************
##***************************************************   
##**********************************************************

##SapData_Read function die aufgrunfd von merge wesentlich länger brauch als die anderen

SapData_Read <- function(SapMat=SaPMat,CompPropCol="KPI"){        
        SapData <- read.csv("S&Pkons.csv",sep=";",dec=".",stringsAsFactors=F)
        ## SapData2 <- read.csv("S&Pkons.csv",sep=";",dec=".",stringsAsFactors=F)
        Dates <- as.Date(substring(colnames(SapData[,-c(1:3)]),2,11),"%Y.%m.%d")
        ColNames <- c(colnames(SapData[,1:3]),substring(Dates,1,7))
        colnames(SapData) <- ColNames        

        CharLevel <- unique(as.character(SapData[,CompPropCol]))
        ##CharLevel <- unique(as.character(SapData2[,"KPI"]))
        SapData<- merge(SapMat[,c("DSCD","SIC_four")],SapData,by.x="DSCD",by.y="DSCD",all=T,sort=F)
        ##SapData3<- merge(SaPMat[,c("DSCD","SIC_four")],SapData2,by.x="DSCD",by.y="DSCD",all=T,sort=F) 
        ##SapData <- sapply(CharLevel,FUN=function(x){y<-SapData[CompPropCol]==x;y=SapData[y,]},simplify=F,USE.NAMES = TRUE) 

      
        SapData  <- sapply(CharLevel,FUN=function(x){y<-SapData[CompPropCol]==x;y=SapData[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        ##SapData  <- sapply(CharLevel,function(x){y<-SapData[CompPropCol]==x;y=SapData[y,];y[order(y$DSCD),]},simplify=F,USE.NAMES = TRUE)
        SapData <- sapply(SapData,FUN=function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
        SapData <- sapply(SapData,FUN=function(x){x[order(x$DSCD),]},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){merge(SaPMat[,c("DSCD","SIC_four")],x,by.x="DSCD",by.y="DSCD",all=T,sort=F)},simplify=F,USE.NAMES = TRUE)
       ### SapData <- sapply(SapData,function(x){x[order(x$DSCD),]},simplify=F,USE.NAMES = TRUE)
       SapData<<-SapData
        }
##*************


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
 
### wird für TaSicCalc benötigt
    
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
## nur Verzeichnis für spaltenbenennung in SAP Data

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

            


##erg<-function(SapData = SapData2,SaPMat){
##        
##        SapDataBoolean <- sapply(names(SapData),function(x){},simplify=F,USE.NAMES = TRUE)
##        for(i_table in 1:length(SapData)){
##       rownames(SaPMat)<-SaPMat[,"DSCD"]
##        SapDataBoolean[[i_table]]<-SaPMat[SapData[[i_table]][,"DSCD"],]
##        }cd Github
##        SapDataBoolean
##        }




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
        
CalcIndustryTs0 <- function(Date,Sic,SicDigits){
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
        else {CalcIndustryTs0(Date,Sic,SicDigits+1)}                         ## if "if"-check fails same function is called searching Sic-table with one Sic digit more
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


            
MethodOneTwo<-function(Date,Boolean,SicDigits=1,KPI,Method){

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
         

     