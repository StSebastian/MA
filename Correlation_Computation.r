## Der nachfolgende R-Code dient zur Berechnung der Korrelationen zwischen 
## den M&A Partnern. Die Korrelationen werden auf Basis von Sales- und Marketvalue-
## Daten zwischen den jeweiligen Industriesegmenten der Partner berechnet. Die 
## Werte der Industriesegmente werden durch S&P 1500 Unternehmen bestimmt, die den 
## korrespondierenden Segmenten angehören.

## Der Table „….“ muss bereits in R erstellt worden sein. Er wird an die letzte 
## Funktion „….“ übergeben, die sich jeden M&A-Eintrag zieht und anhand der SICs 
## und des Datums die Korrelationen bestimmt.
## Zusätzlich müssen die Dokumente „….“ Und „…“ im lokalen R-Verzeichnis abgelegt sein.



## Folgende Schritte werden durchlaufen

## 1.   Erstellung eines Datensatzes, der die Zugehörigkeit von Unternehmen zum S&P 
##      1500 angibt.

## 2.   Erstellung eines Datensatzes, der für die S&P 1500 Unternehmen die Entwicklung
##      der zur Korrelationsberechnung notwendigen Unternehmensdaten (Sales, MV, etc.)
##      speichert.

## 3.   Erstellung eines Datensatzes, der angibt, in welchen Monaten die Unternehmen 
##      in dem Datensatz von Schritt 2 S&P 1500 Unternehmen waren.

## 4.   Berechnung neuer Unternehmensdaten (aus Datensatz Schritt 2),die Grundlage 
##      für Korrelation sein sollen - relative Entwicklung der Sales und Sales to MV

## 5.   Festlegung von Optionen für Korrelationsbestimmung.

## 6.   Berechnung der Entwicklung der S&P 1500 Indexwerte als Benchmark für die Entwicklung 
##      von Sales to MV Werten und relativen Sales. Falls später nicht Korrelationen direkt 
##      zwischen den Segmenten, sondern nur zwischen den Residuen einer Regression 
##      von Segmentwerten auf Indexwerte bestimmt werden sollen.  

## 7.   Berechnung der Korrelation      



## Folgende Funktionen werden angewendet:

## 1.   Zum Erstellen eines Datensatzes, der anhand von logischen Werten die 
##      Zugehörigkeiten von Unternehmen zum S&P 1500 für jeden Monat ausweist: 
## 1.1	SaPcreate():  Erstellt leeren data.table der Unternehmen der Zeilen 
##      nach und Monate den Spalten nach ordnet.
## 1.2	SaPfill(): Füllt den von SaPcreate() erstellten data.table mit 
##      logischen Werten je nach Zugehörigkeit des Unternehmens zum S&P 1500.

## 2.	Einlesen und Überarbeiten eines Datensatzes mit Unternehmensdaten, zur 
##      Erstellung einer Liste von verschiedenen Datensätzen, aufgeteilt nach 
##      Unternehmenskennzahlen (Sales, MV etc.):
## 2.1	SapData_Read(): Liest Datei „…“ ein, teilt die Tabelle in verschiedene 
##      Tabellen auf, jede für eine Unternehmenskennzahl und übergibt die Tabellen 
##      als Liste. Außerdem wird auf SapData_Complete zugegriffen.
## 2.2	SapData_Complete(): Stellt Identität zwischen Datensätzen her, die in einer 
##      Liste (in SapData_Read() übergeben) enthalten sind. Das heißt jeder Datastream 
##      Code bzw. jedes Unternehmen wird in jedem Datensatz in der List an derselben 
##      Position auftauchen.

## 3.	Zum Erstellen eines data.table, der, ähnlich dem von SaPcreate() SaPfill() unter 
##      Schritt 1 erstellten, die Zugehörigkeit von Unternehmen zum S&P 1500 aufzeigt, 
##      allerdings mit Unternehmen(Datastream Codes) geordnet nach der Ordnung in den 
##      Datensätzen in der Liste von Schritt 2. 
##      Compute_SapDataBoolean()

## 4.	Grundlage der Korrelationsbestimmung sind die Sales-Entwicklungen der 
##      Industriesegmente. Zur Berechnung  werden die Marktwertgewichteten Sales 
##      und alternativ die relative Sales-Entwicklung verwendet, die von den folgenden
##      Funktionen berechnet werden.    . 
## 4.1	CalcSalesDevYearly: Berechnung der relativen Entwicklung der Sales pro Unternehmen. 
## 4.2	CalcSalesByMV: Berechnung des Wertgewichteten Sales jedes Unternehmens.

## 5.	Zur Bestimmung von Parametern auf deren Grundlage die Korrelationen 
##      berechnet werden sollen. Set_Cor_Options() ist die wichtigste Funktion. 
##      Die von ihr zurückgegebene Liste, legte fest nach welchen Kriterien die 
##      Korrelationen bestimmt werden sollen z.B. Zeitraum, Toleranz für fehlende 
##      Werte Berechnungsmethode etc.. Genauere Informationen sind im Dokument „…“
##      Set_Cor_Options()

## 6	Zur Berechnung der Entwicklung des S&P 1500 als Benchmark Index. Wenn die 
##      Korrelationen nicht direkt zwischen den Segmenten, sondern nur zwischen 
##      den Residuen einer Regression von Segmentwerten auf Indexwerte bestimmt 
##      werden sollen:  
## 6.1	IndexCompositionDscd(): Ähnlich dem in Schritt 3 von Compute_SapDataBoolean() 
##      erstellten Datensatz zur S&P 1500 Zugehörigkeit, aber anstelle von 
##      logischen Werten werden die Datastream Codes in den Spalten wiederholt 
##      um die Zugehörigkeit der Unternehmen zum S&P 1500 zu kennzeichnen.
## 6.2	Calc_Index_Values: Erstellt Datensatz, der für jeden Monat oder jedes Jahr 
##      eine Zeitreihe der Entwicklung der zurückliegenden Indexwerte angibt.

## 7	Zur Berechnung der Korrelationen auf Basis der durch Set_Cor_Options 
##      spezifizierten Argumente und der in dem data.table „…“ enthaltenen M&As.
## 7.1	CalcCorr(): Berechnet die Korrelation zwischen zwei Industriesegmenten, die 
##      durch einen M&A Eintrag in „…“ definiert sind. Die Zeitreihen für jeden SIC-Code 
##      werden durch jeweiligen Aufruf der Funktion CalcIndustryTs() in CalcCorr() bestimmt.
## 7.2	CalcIndustryTs: Gibt in Abhängigkeit vom M&A Datum und einem SIC-Codes eine 
##      Zeitreihe von Werten einer Unternehmenskennzahl(z.B. Sales to MV) für das durch 
##      den SIC-Code spezifizierte Industriesegment wieder (berechnet aus S&P 1500 Segmenten.
## 7.3	CorRow: Wendet Funktion CalcCorr() auf jedes Element der Tabelle „…“ an,
##      die die Übersicht von M&As enthält, die mit dem R-Code „….“ erstellt wurde



setwd("C:/Users/Sebastian Stenzel/Desktop/Neuer Ordner (2)/R input test")

library(data.table)
testob<-fread("testob.txt",sep=";",stringsAsFactors=F)

system.time(SaPConst <- fread("S&Pverlauf_für_R.csv",sep=";",colClasses=c("character", "character","character","numeric","character")))

SaPConst[,Datum:=substr(as.Date(Datum),1,7)]




##1.    Erstellung eines Datensatzes, der Zugehörigkeit von Unternehmen zum S&P 
##      1500 angibt.

##1.1   SaPcreate():  Erstellt leeren data.table, der sämtliche Unternehmen, die im 
#####   Untersuchungszeitraum im S&P 1500 gelistet waren in Zeilen und Spalten
#####   nach Monaten ordnet. Die darauffolgende Funktion wird den Datensatz 
#####   durch logische Werte ergänzen, die die Zugehörigkeit der Unternehmen zum S&P 1500 
#####   für jedes Datum anzeigen.
SaPcreate <- function(SaPConst = SaPConst,as_char=FALSE){

        ColVec <- sort(unique(SaPConst$Datum),decreasing=F)
        ## Sammlung sämtlicher Daten des Untersuchungszeitraumen (Jahr und Monat)
        
        DSCDPos <- !duplicated(SaPConst$DSCD)
        ## Logischer Vektor, der für jedes Unternehmen im Datensatz an dessen erster
        ## Position im Datensatz TRUE enthält.
        
        RowVec<-SaPConst$DSCD[DSCDPos] 
        ## Sammlung sämtlicher Datastream Codes im Datensatz
        
        SicVec<-SaPConst$WC07021[DSCDPos] 
        ## Sammlung sämtlicher SIC Codes im Datensatz
        
        NamesVec<-SaPConst$Name[DSCDPos]
        ## Sammlung der Unternehmensnamen
        
        RowLeng<-length(RowVec)
        
        SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")
        ## Alle SIC Codes, die weniger als 4 Ziffern haben, werden durch Setzen von
        ## "0" auf vier verlängert.
        
        ## Erstellung des leeren data.tables, der lediglich Informationen zum Unternehmen 
        ## (Datastream Code, Sic Code) enthält. Im nächsten Schritt wird er durch logische
        ## Werte ergänzt, die die Zugehörigkeit der Unternehmen zum S&P 1500 nach Datum
        ## anzeigen.
        SaPMat<-data.table(Name=NamesVec)
        SaPMat[,c("DSCD","SIC_four","SIC_three","SIC_two","SIC_one"):=data.frame((RowVec),SicVec,
                        substr(SicVec,1,3),substr(SicVec,1,2),substr(SicVec,1,1),stringsAsFactors=F)]

        if(as_char){SaPMat[,{ColVec}:=as.character(rep(NA,RowLeng))]}
        else {SaPMat[,{ColVec}:=rep(NA,RowLeng)]}
        SaPMat[["2012-10"]]<-as.character(NA)

        setkey(SaPMat,"DSCD")
        SaPMat
        }
  
##1.2	SaPfill (): füllt den von SaPcreate() erstellten data.table mit 
#####   logischen Werten je nach Zugehörigkeit des Unternehmens zum S&P 1500
SaPfill <- function(SaPMatrix = SaPMat,SaPConstitution = SaPConst){

        SaPvalues <- function(x){
            SaPMatrix[[x]] <<- SaPMatrix[,DSCD]%in%SaPConstitution[SaPConstitution$Datum==colnames(SaPMatrix)[x],DSCD]
            ## SaPConstitution ist ein Datensatz (aus hier zwei relevanten Spalten), der für jeden
            ## Monat, in dem ein Unternehmen im S&P 1500 war, dieses Unternehmen mit seinem Datastream 
            ## Code und dem Monat (plus Jahr) für diesen Monat einmal enthält.
            ## Rechts des %in% wird von dem Datensatz die Spalte der Datastream Codes ausgewählt,
            ## aber nur die Zeilen, für die das Datum gerade dem Datum der ausgewählten Spalte der 
            ## SaPMatrix entspricht (colnames(SaPMatrix)[x]) - folglich nur den S&P 1500 für diesen
            ## Monat.
            ## Durch den Vergleich mit SaPMatrix[,DSCD] links von %in% entsteht ein logischer Vektor
            ## der die S&P 1500 Zugehörigkeit der Unternehmen in der S&PMatrix angibt. DerVektor wird der
            ## Spalte des untersuchten Datums in der SaPMatrix[[x]] zugeordnet.
            invisible()
            }
        
        ## Anwendung der Funktion SaPvalues auf alle Datumsspalten der SaPMatrix
        for(i in 7:ncol(SaPMatrix)){SaPvalues(i)}

        SaPMatrix
        }

##  Aufruf von SaPcreate() und Zuweisung der Rückgabetabelle zu SaPMat    
    system.time(SaPMat <- SaPcreate(SaPConst))
##  Aufruf von SaPfill() und Überschreibung von SaPMat mit der gefüllten Tabelle   
    system.time(SaPMat <- SaPfill(SaPMat, SaPConst))

    
    
    
## 2.   Erstellung eines Datensatzes, der für die S&P 1500 Unternehmen die Entwicklung
##      der zur Korrelationsberechnung notwendigen Unternehmenswerte (Sales, MV, etc.)
##      speichert.

##2.1	SapData_Read(): Liest Datei „…“ ein, teilt die Tabelle in verschiedene 
#####   Tabellen auf, jede für eine Unternehmenskennzahl  und übergibt die 
#####   Tabellen als Liste. Außerdem wird auf SapData_Complete() zugegriffen.
SapData_Read <- function(SapMat=SaPMat,CompPropCol="KPI",NaRm = FALSE){
            
        SapData <- fread("S&Pkons.csv",sep=";",stringsAsFactors=F)
        
        ## Entfernung der Wochentage aus den Spaltennamen (es verbleibt nur
        ## noch "Jahr-Monat")
        ColNames <- as.character(colnames(SapData)[-c(1:3)])
        NewColNames <- c(colnames(SapData[,1:3]),substring(ColNames,1,7))
        setnames(SapData,ColNames,NewColNames)
        
        ## Filterung der verschiedenen Unternehmenskennzahlen (MV, Sales etc.)
        CharLevel <- unique(as.character(SapData[[CompPropCol]]))
        
        ## Erweiterung der Tabelle um Spalten mit Informationen zum Industriesegment (Sic)
        SapData<- merge(SapMat[,c("DSCD","SIC_four"),with=F],SapData,by="DSCD",all=T,sort=F)
        
        ## Datensatz wird in Liste von mehreren Datensätzen gesplittet, ein Datensatz für 
        ## jede Unternehmenskennzahl
        SapData <- sapply(CharLevel,FUN=function(x){y<-SapData[[CompPropCol]]==x;y=SapData[y,]},simplify=F,USE.NAMES = TRUE) 
       
        ## Anwendung der Funktion SapData_Complete() auf jeden Datensatz in Liste.
        ## Hängt Unternehmen mit Datastream Codes und NA-Werten an Datensätze an
        ## wenn sie in den ursprünglichen nicht erfasst waren und ordnet diese, sodass
        ## jeder Datensatz die gleiche Länge mit gleicher Zeilenreihenfolge (nach Unternehmen) hat.
        SapData <- sapply(names(SapData),SapData_Complete,SapData=SapData,simplify=F,USE.NAMES = TRUE)
        
        ## Eventuell Bereinigung des Datensatzes um Unternehmen für die keine Informationen
        ## zur Industrieklassifizierung vorliegen
        if(NaRm){SapData <- sapply(names(SapData),function(x){SapData[[x]][!is.na(SapData[[x]][["SIC_four"]]),]},simplify=F,USE.NAMES = TRUE)}

        SapData<<-SapData
        }

##2.2	SapData_Complete(): Stellt Identität zwischen den data.table her, die in einer Liste 
#####   enthalten sind, die in SapData_Read() übergegeben wird. Das heißt jeder Datastream 
#####   Code bzw. jedes Unternehmen muss in jedem Datensatz auftauche und auch an derselben 
#####   Position sein. Wichtig für die spätere Auswertung.
SapData_Complete  <- function(CompPropLevel,SapData){
        PropTable <- SapData[[CompPropLevel]]
            
        MissingDscd <- !is.element(SaPMat$DSCD,PropTable$DSCD)
        ## Überprüfung welche Unternehmen noch nicht im Datensatz sind
            
        ## Abbruch falls keine Unternehmen fehlen, zuvor noch Ordnung der Zeilen-
        ## reihenfolge nach Datastream Codes
        if(sum(MissingDscd)==0){ 
               PropTable<-PropTable[order(PropTable$DSCD),]
               rownames(PropTable)<-PropTable$DSCD
               return(PropTable)
               }
            
        ## Zwischenspeicherung der Unternehmen, die noch nicht erfasst sind und 
        ## Ergänzung des Datensatzes durch NA-Werte sowie zugehörige Spaltennamen
        MissingDscd <- SaPMat[MissingDscd,c("DSCD","SIC_four","Name"),with=F]
        NaVal <- matrix(rep(NA,nrow(MissingDscd)*(ncol(PropTable)-4)),ncol=ncol(PropTable)-4,nrow=nrow(MissingDscd))
        NewPropTable<-cbind(MissingDscd,"KPI"=CompPropLevel,NaVal)
        setnames(NewPropTable,colnames(NewPropTable),colnames(PropTable))

        ## Verlängerung des ursprünglichen Datensatzes um fehlende Unternehmen
        ## und Ordnung der Zeilenreihenfolge nach Datastream Codes
        NewPropTable <- rbind(PropTable, NewPropTable)
        NewPropTable<-NewPropTable[order(NewPropTable$DSCD),]
        rownames(NewPropTable)<-NewPropTable$DSCD
        NewPropTable
        }

##  Aufruf von SapData_Read() erstellt Liste SapData            
    system.time(SapData_Read())

    
    
    
## 3.   Erstellung eines Datensatzes, der angibt in welchen Monaten die Unternehmen 
##      in dem Dantensatz von Schritt 2 S&P 1500 Unternehmen waren.
    
##3.    SaPfill(): Erstellt ähnlich zu Funktion SaPfill () eine Tabelle, die die 
#####   Zugehörigkeit von Unternehmen zum S&P 1500 aufzeigt, allerdings wird die Ordnung 
#####   der Unternehmen genau der in den Datensätzen aus Schritt 2 angepasst.
Compute_SapDataBoolean<-function(SapData = SapData,SaPMat){        
        setkey(SaPMat,DSCD)
        SapDataBoolean<-SaPMat[SapData$MV$DSCD,]

        SapDataBoolean
        }           

##  Aufruf von SapDataBoolean() weist die Tabelle der Variable Boolen zu          
    Boolen<-Compute_SapDataBoolean(SapData,SaPMat)               

    
    
    
## 4.   Berechnung neuer Unternehmensdaten (aus Datensatz Schritt 2),die Grundlage 
##      für Korrelation sein sollen - relative Entwicklung der Sales und Sales to MV.
    
##4.1	CalcSalesDevYearly(): Berechnet die relative Entwicklung der Sales pro Unternehmen,
#####   hängt die Tabelle SalesDev an die Liste SapData aus Schritt 2 an
CalcSalesDevYearly <- function(sap_data = SapData, startDate = "1978-01-01", endDate = "2014-01-01"){

        KPI = "NetSalesRev"
        
        ## Erstellung von zwei Zeitreihen die von der eine um ein Jahr abgeschnitten ist
        startDate <- as.POSIXlt(startDate)
        endDate <- as.POSIXlt(endDate)

        ## Erstellung der längeren Reihe
        YearsCoveredOne <- as.POSIXlt(endDate)
        YearsCoveredOne$year <- startDate$year:endDate$year
        YearsCoveredOne <- substr(YearsCoveredOne,1,7)
        
        ## Erstellung der kürzeren Reihe
        YearsCoveredTwo<-YearsCoveredOne[-length(YearsCoveredOne)]

        ## Berechnung der relativen Entwicklung der Sales und Erweiterung der Tabelle um
        ## Informationen zu Unternehmen
        SalesDev <- data.table(t(diff(t(sap_data[[KPI]][,YearsCoveredOne,with=F]),lag=1)))/sap_data[[KPI]][,YearsCoveredTwo,with=F]
        SalesDev <- cbind(sap_data[[KPI]][,c("DSCD","SIC_four","Name"),with=F],KPI="SalesDev",SalesDev)
        
        
        ## Entfernung von Inf-Werten
            ## Bestimmung von Spalten, in denen sich Inf-Werte befinden
            InfVal<-data.table(InfCol=which(SalesDev==Inf)%/%nrow(SalesDev)+1)
            
            ## Bestimmung von Zeilen, in denen sich Inf-Werte befinden
            InfVal[,InfRow:=which(SalesDev==Inf)%%nrow(SalesDev)]
                if(nrow(InfVal)>0){
                    for(i_row in 1:nrow(InfVal)){
                    SalesDev[InfVal$InfRow[i_row],InfVal$InfCol[i_row]:=NA]
                    ## Ersetzung Inf-Werte durch NA
                    }    
                }
        setkey(SalesDev,"DSCD")
        SapData[["SalesDev"]] <<- SalesDev
        }

##4.2	CalcSalesByMV: Berechnung des wertgewichteten Sales jedes Unternehmens,
#####   hängt die Tabelle SalesByMV an die Liste SapData aus Schritt 2 an.
CalcSalesByMV <- function(sap_data = SapData, startDate = "1978-01-01", endDate = "2014-01-01"){

        kpi_one <- "NetSalesRev"
        kpi_two <- "MV"

        ## Erstellung von zwei Zeitreihen, von der eine um ein Jahr abgeschnitten ist
        startDate <- as.POSIXlt(startDate)
        endDate <- as.POSIXlt(endDate)
        
         ## Erstellung der längeren Reihe
        YearsCoveredOne <- as.POSIXlt(endDate)
        YearsCoveredOne$year <- startDate$year:endDate$year
        YearsCoveredOne <- substr(YearsCoveredOne,1,7)
        
        ## Erstellung der kürzeren Reihe
        YearsCoveredTwo<-YearsCoveredOne[-length(YearsCoveredOne)]

        ## Division der Sales durch MV und Erweiterung der Tabelle um
        ## Informationen zu Unternehmen
        SalesByMV <- sap_data[[kpi_one]][,YearsCoveredOne,with=F]/sap_data[[kpi_two]][,YearsCoveredOne,with=F]
        SalesByMV <- cbind(sap_data[[kpi_one]][,c("DSCD","SIC_four","Name"),with=F],KPI="SalesByMV",SalesByMV)
        
        
        ## Entfernung von Inf-Werten analog zu CalcSalesDevYearly()
            InfVal<-data.table(InfCol=which(SalesByMV==Inf)%/%nrow(SalesByMV)+1)
            InfVal[,InfRow:=which(SalesByMV==Inf)%%nrow(SalesByMV)]
            if(nrow(InfVal)>0){
                for(i_row in 1:nrow(InfVal)){
                    SalesByMV[InfVal$InfRow[i_row],InfVal$InfCol[i_row]:=NA]
                }    
            }
        setkey(SalesByMV,"DSCD")
        SapData[["SalesByMV"]] <<- SalesByMV
        }

##  Aufruf von CalcSalesByMV() hängt Tabelle SalesDev an die Liste SapData an       
    CalcSalesByMV()    

##  Aufruf von CalcSalesDevYearly() hängt Tabelle SalesByMV an die Liste SapData an           
    CalcSalesDevYearly()  

    
    
    
## 5.   Festlegung von Optionen für Korrelationsbestimmung.    
    
##5.    Set_Cor_Options(): Bestimmt Parameter, auf dessen Grundlage die Korrelationen berechnet 
####    werden sollen. Set_Cor_Options() ist die wichtigste Funktion. Die von ihr zurückgegebene
####    Liste legte fest, nach welchen Kriterien die Korrelationen bestimmt werden sollen z.B. 
####    Zeitraum, Toleranz für fehlende Werte Berechnungsmethode etc.. Genauere Informationen 
####    sind im Dokument „…“. Die Werte sind bei Aufruf vordefiniert, können aber 
####    angepasst werden, wobei nicht jede Kombination möglich ist.
Set_Cor_Options<-function(NoObsReqMonth = 8,NoCompReqMonth = 5,NoObsReqYear= 5, NoCompReqYear= 4,
                    Period="year",PeriodLengthYear=7,PeriodLengthMonth=10,Method="one",EntityProperty="MV"
                    ,BreakYear="cut",SpExessCor=F){
        CorCalcOptions<<-list(Month=data.frame("NoObsReqMonth"=NoObsReqMonth,"NoCompReqMonth"=NoCompReqMonth),
                Year=data.frame("NoObsReqYear"=NoObsReqYear,"NoCompReqYear"=NoCompReqYear),"Period"=Period,
                "PeriodLengthYear"=PeriodLengthYear,"PeriodLengthMonth"=PeriodLengthMonth,"Method"=Method,
                "EntityProperty"=EntityProperty, "BreakYear"=BreakYear,"SpExessCor"=SpExessCor)
        }
##  Aufruf von Set_Cor_Options() erstellt Liste CorCalcOptions  
    Set_Cor_Options()

    
    
    
## 6.   Berechnung der Entwicklung der S&P 1500 als Benchmark für die Entwicklung der
##      Unternehmensdaten. Falls später nicht Korrelationen direkt zwischen den Segmenten,
##      sondern nur zwischen den Residuen einer Regression von Segmentwerten auf 
##      Indexwerte bestimmt werden sollen.  
    
##6.1	IndexCompositionDscd():  Erstellt Tabelle ähnlich der Rückgabe von 
#####   Compute_SapDataBoolean() in Schritt 3. Statt logischer Werten 
#####   werden aber Datastream Codes in den Spalten anstelle von TRUE-Werten 
#####   wiederholt, um die Zugehörigkeit der Unternehmen zum S&P 1500 zu kennzeichnen.
IndexCompositionDscd <- function(sap_mat=SaPMat){
        ## Zunächst Erstellung einer leeren Tabelle mit Daten und Datastream Codes
        ## Wie zuvor über SaPcreate() aus Schritt 1
        sap_mat_char <- SaPcreate(SaPConst,as_char=TRUE)

        for (i_col in names(sap_mat)[-(1:6)]){
            ## Zuweisung der Datastream Codes    
            set(sap_mat_char,i=which(sap_mat[[i_col]]),j=i_col,value=sap_mat[["DSCD"]][sap_mat[[i_col]]])
            ## i gibt die zu selektierenden Reihen an
            ## j selektiert die Spalte
            ## value die zuzuweisenden Datastream Codes
            }
        sap_mat_char
        }
##  Aufruf von IndexCompositionDscd() und Zuweisung zu SpCompositionDscd          
    SpCompositionDscd <- IndexCompositionDscd ()    

##6.2	Calc_Index_Values() erstellt Datensatz, der die Entwicklung des S&P 1500 
#####   für eine bestimmte Kennzahl über einen vordefinierten Zeitraum wiedergibt.
#####   Dabei ist der Spaltenname das Jahr "0" (bei jährlicher Berechnung) bzw. Jahr plus
#####   Monat. In der Spalte sind vom Jahr "0" ausgehend die zurückliegenden monatlichen
#####   bzw. jährlichen Indexwerte abgetragen.
Calc_Index_Values <- function(sp_composition_dscd = SpCompositionDscd,options = CorCalcOptions){
        ## Es wird für jede Methode "one"/"two" ein Datensatz erstellt und beide
        ## zum Ende in einer Liste gespeichert.
        ## Für beide Fälle muss zunächst festgelegt werden, ob jährliche oder monatliche
        ## Zeitreihen bestimmt werden sollen. Danach werden ausgehend von Datum "0" (in for-Schleife,
        ## da jedes Datum im Datensatz einmal Datum "0" ist) die zurückliegenden Daten für
        ## die Zeitreihe berechnet. Mit diesen können dann die Unternehmenswerte der S&P 1500
        ## Unternehmen bestimmt und für das jeweilige Datum gemittelt werden.
        ## Methode "one" unterscheidet sich von Methode "two" hinsichtlich der Auswahl der 
        ## S&P 1500 Unternehmen. Methode "one" selektiert nur solche, die zum Zeitpunkt "0"
        ## im S&P 1500 waren, Methode "two" sämtliche über das ganze Intervall/Periode.   
                
        
        ## Entsprechend der in CorCalcOptions vordefinierten Optionen werden jährliche
        ## oder monatliche Intervalle mit unterschiedlicher Länge berechnet.
        if(options$Period=="year"){period_length <- options$PeriodLengthYear-1}
        else {period_length <- options$PeriodLengthMonth-1}
        
        ## Erstellung der leeren Datensätze für die Berechnungen mit Methode "one" und
        ## "two"
        sp_index_value_one <- data.table("EntityProperty"=-(period_length:0))
        sp_index_value_two <- data.table("EntityProperty"=-(period_length:0))
        sp_index_value_one[,names(SapData[[options$EntityProperty]]):=as.numeric(NA)]
        sp_index_value_two[,names(SapData[[options$EntityProperty]]):=as.numeric(NA)]
        
        
            YearsMean <- function(Date){
                Years <- grep(substr(Date,1,4),names(SapData[[options$EntityProperty]]))
                YearsMean <- mean(rowMeans(SapData[[options$EntityProperty]][sap_composition_period,names(SapData[[options$EntityProperty]])[Years],with=F],na.rm=T),na.rm=T) 
                YearsMean}
            
            IntervalMean <- function(index){ 
               year<<-c(year,names(SapData[[options$EntityProperty]])[index[2]])    
               interval_index <- names(SapData[[options$EntityProperty]])[(index[1]+1):index[2]]
               interval_mean <- mean(rowMeans(SapData[[options$EntityProperty]][sap_composition_period,interval_index,with=F],na.rm=T),na.rm=T)  
               interval_mean}
        
       
        ## Berechnung für Methode "one"
        for(col_name in names(SapData[[options$EntityProperty]])[-(1:4)]){
            ## Weil im S&P 1500 Datensatz nur die Zusammensetzung von Ende 1994 bis Ende 2013
            ## enthalten ist, wird die Berechnung auf den Bereich begrenzt.
            if(col_name>"2014" |col_name<"1994-12" ){next}
                if(TRUE){
                    ## col_name<-"2013-08"
                    ## Umwandlung des Spaltennamens zum Typ POSIXlt
                    col_Date<-paste(col_name,"-15",sep="")
                    col_Date<-as.POSIXlt(col_Date)
                    
                    ## je nach Auswahl von Jahres- bzw. Monatsintervallen - Bildung von Daten für Zeitreihe
                    ## durch Subtraktion der Jahres/Monats-Intervalle von col_Date
                    if(options$Period=="year"){col_Date$year<-(col_Date$year-period_length):col_Date$year}
                    else {col_Date$mon<-(col_Date$mon-period_length):col_Date$mon}
                    
                    ## Bestimmung Spaltenindizes für spätere Auswahl
                    col_index <- which(is.element(names(SapData[[options$EntityProperty]]),substr(col_Date,1,7)))
                    }
                else{
                    col_index <- which(names(SapData[[options$EntityProperty]])== col_name)
                    col_index <- (col_index-period_length):col_index
                    }
            ## Berechnung der Zeitreihe        
            sp_index_value_one[[col_name]]<-(colMeans(SapData[[options$EntityProperty]][sp_composition_dscd[[col_name]],col_index,with=F],na.rm=T))
            ## Vom Datensatz SapData werden nur die Unternehmen ausgewählt, die zum Datum "col_name" im
            ## S&P 1500 waren und für diese werden die Werte zu den durch col_index definierten Zeitpunkten
            ## entnommen. Durch ColMeans werden die Mittelwerte zu den Daten berechnet        
            }
       
        ## Berechnung für Methode "two"
        for(col_name in names(SapData[[options$EntityProperty]])[-(1:4)]){
            ## Weil im S&P 1500 Datensatz nur die Zusammensetzung von Ende 1994 bis Ende 2013
            ## enthalten ist, wird die Berechnung auf den Bereich begrenzt.
            if(col_name>"2014" |col_name<"1994-12" ){next}
                
                if(TRUE){
                    ## Umwandlung des Spaltennamens zum Typ POSIXlt
                    col_Date<-paste(col_name,"-15",sep="")
                    col_Date<-as.POSIXlt(col_Date)
                    
                    ## je nach Auswahl von Jahres- Monatsintervallen - Bildung von Daten für Zeitreihe
                    ## durch Subtraktion der Jahres/Monats-Intervalle von col_Date
                        if(options$Period=="year"){col_Date$year<-(col_Date$year-period_length):col_Date$year}
                        else {col_Date$mon<-(col_Date$mon-period_length):col_Date$mon}
                    
                    ## Bestimmung Spaltenindizes für spätere Auswahl
                    col_index <- which(is.element(names(SapData[[options$EntityProperty]]),substr(col_Date,1,7)))
                    
                    ## Für Methode "two" werden nicht nur S&P 1500 Unternehmen zum Zeitpunkt "0",
                    ## sondern sämtliche die in dem Betrachtungszeitraum im S&P 1500 waren
                    ## berücksichtig. Daher hier auch Auswahl der SP Spalten für jedes Datum.
                    sp_col_index <- which(is.element(names(sp_composition_dscd),substr(col_Date,1,7)))
                    
                    ## Abbruch wenn keine Werte vorhanden
                    if(length(sp_col_index)==0){next}
                    
                    ## Wenn der Zeitraum vor das Jahr 1995 zurückgeht, immer automatische
                    ## Verlängerung bis zum ersten Datum der Erfassung der S&P 1500 Zusammensetzung.
                    if( length(sp_col_index)<(period_length+1)){sp_col_index<-c(7,sp_col_index)}
                    }
                else{
                    col_index <- which(names(SapData[[options$EntityProperty]])== col_name)
                    col_index <- (col_index-period_length):col_index
                    sp_col_index <- which(is.element(names(sp_composition_dscd),names(SapData[[options$EntityProperty]])[col_index]))}
            
            if((length(sp_col_index)<1)){next}
            
            ## Zur Auswahl aller S&P-Composition Spalten zwischen dem minimalen und maximalen Spaltenindex.
            ## Vorher waren nur einzelne Punkte im Intervall erfasst, wenn die Intervalllänge jährlich
            ## gewesen wäre.
            sp_col_index <- min( sp_col_index): max ( sp_col_index)

            ## Entnahme aller Datastream Codes für die Unternehmen, die im Betrachtungszeitraum im S&P 1500
            ## Index gewesen sind (Betrachtungszeitraum wird durch Spaltenindizes sp_col_index) definiert. 
            sap_composition_period <- unique(as.vector(as.matrix(sp_composition_dscd[,sp_col_index,with=F])))
                
                if(FALSE){ ## computes mean values for each year in the periods
                    sp_index_value_two[[col_name]]<-sapply(substr(col_Date,1,4),FUN=YearsMean)
                    }
                if(FALSE){ ## computes mean values for each period 
                    col_index<-c((col_index[1]-12),col_index)
                    sp_index_value_two[[col_name]]<-rollapply(col_index,width=2,FUN=IntervalMean)
                    }
                if(TRUE){
                    ##Berechnung der Zeitreihe analog zu Methode "one"    
                    sp_index_value_two[[col_name]]<-colMeans(SapData[[options$EntityProperty]][sap_composition_period,col_index,with=F],na.rm=T)
                    }
            }
        list("one"=sp_index_value_one,"two"=sp_index_value_two)
        }    

##  Aufruf von Calc_Index_Values() und Zuweisung zu IndexValues  
    system.time(IndexValues<-Calc_Index_Values()  )        




## 7.   Berechnung der Korrelation
##      Funktion CalcIndustryTs() entnimmt die Zeitreihe, Funktion CalcCorr() bestimmt 
##      Korrelation, Funktion CorRow() wendet CalcCorr() auf jeden M&A der Tabelle ".."
##      an.    

##7.1	CalcCorr(): Berechnet die Korrelation zwischen zwei Industriesegmenten, die 
#####   durch einen M&A Eintrag in „…“ definiert sind. Die Zeitreihen für jeden SIC-Code 
#####   werden durch jeweiligen Aufruf der Funktion CalcIndustryTs() in CalcCorr() bestimmt.
CalcCorr<-function(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,options=CorCalcOptions,index_values = IndexValues){
     
    ## Bestimmung Daten (Intervall) für die Zeitreihe, falls jährliche Intervalle verwendet werden sollen    
    if(options$Period=="year"){        
        TimePeriod <- options$PeriodLengthYear     
        Date <- as.POSIXlt(Date)
        Date$mday <- 15    
        Date$year <- Date$year - (TimePeriod:1) 
        Date$mon <- 0
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
         
        NoObsReq<-options$Year$NoObsReqYear
        ## Zwischenspeicherung der Mindestbeobachtungen bzw. für wieviel Zeitpunkte im Intevall
        ## Beobachtungen vorhanden sein müssen   
        }
    
    ## Bestimmung der Daten für die Zeitreihe, falls monatliche Intervalle verwendet werden sollen    
    else {
        TimePeriod <- options$PeriodLengthMonth
        Date <- as.POSIXlt(Date)
        Date$mday <- 15        
        Date$mon <- Date$mon - (TimePeriod:1)
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)

        NoObsReq<-options$Month$NoObsReqMonth
        ## Zwischenspeicherung der Mindestbeobachtungen bzw. für wieviel Zeitpunkte im Intevall
        ## Beobachtungen vorhanden sein müssen  
        }    
        
        ## Falls Methode "two" angewendet werden soll muss auch ein Zeitraum bestimmt werden
        ## innerhalbe dessen alle S&P 1500 Unternehmen bestimmt werden     
        if(options$Method == "two"){
            if(options$BreakYear=="cut" | options$BreakYear=="extend"){DateSap<-Date[Date>=1995]}##;Date<-DateSap}
            ## Industriekorrelationen werden mit S&P 1500 Unternehmen berechnet
            ## Das Intervall DateSap wird bei unter 1995 abgeschnitten, weil zuvor keine
            ## Angaben zur S&P 1500 Zusammensetzung vorhanden sind
            
            if(options$BreakYear=="cut" & length(DateSap) < NoObsReq){return(NA)}
            ## Auswahl cut bricht ab, sobald die Anzahl an 
            }
            
        ## Für Methode "one" spielt nur das letzte (jüngste) Datum ein Rolle, DateSap muss aber trotzdem
        ## bestimmt werden, ist aber identisch zu Date    
        else {DateSap <- Date} 

        ## Wenn DateSap leer ist (keine Einträge zur S&P 1500 Zusammensetzung für den
        ## Zeitpunkt) erfolgt Abbruch    
        if(length(DateSap)==0){return(NA)}
        
        ## Berechnung der Zeitreihen durch Aufruf von CalcIndustryTs()
        AcIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,DateSap,SicAcquiror,SicDigits=4,options)  
        TaIndustryTs <- CalcIndustryTs(BooleanSapData,SapData,Date,DateSap,SicTarget,SicDigits=4,options) 
            
            ## Abbruch falls eine Zeitreihe NA ist
            if(length(AcIndustryTs)==1|length(TaIndustryTs)==1){return(NA)}
            
            if(options$Method=="three"){
                return(cor(sapply(AcIndustryTs,sum,na.rm=T),sapply(TaIndustryTs,sum,na.rm=T)))
                }
        
        ## Bestimmung Mittelwert für Zeitpunkte im Intervall  
        AcIndustryTs <- colMeans(AcIndustryTs[,-(1:2),with=F],na.rm=T)
        TaIndustryTs <- colMeans(TaIndustryTs[,-(1:2),with=F],na.rm=T)
        
        ## Falls Korrelationen nicht direkt bestimmt werden sollen, sondern 
        ## über die Korrelation der Residuen einer Regression 
        ## der Zeitreihe auf die Entwicklung des S&P als Index:    
        if(options$SpExessCor){    
            x <- index_values[[options$Method]][[max(DateSap)]]
            coefficient <- coef(lm(AcIndustryTs~x))
            AcIndustryTs <- (AcIndustryTs-(coefficient["(Intercept)"]-coefficient["x"]*x))
            coefficient <- coef(lm(TaIndustryTs~x))
            TaIndustryTs <- (TaIndustryTs-(coefficient["(Intercept)"]-coefficient["x"]*x))
            }
            
        return(cor(AcIndustryTs,TaIndustryTs))
        } 
 
##7.2	CalcIndustryTs: Gibt in Abhängigkeit von M&A Datum und eines SIC-Codes eine 
#####   Zeitreihe von Werten für ein durch den Sic Code spezifiziertes Industriesegment
#####   an.
CalcIndustryTs<-function(BooleanSapData,SapData,Date,DateSap,Sic,SicDigits=4,options){
        ## CalcIndustryTs() bestimmt nur eine Zeitreihe, wenn genug Beobachtungen
        ## im Industriesegment vorliegen. Ist das nicht der Fall, ruft sich die Funktion
        ## wieder selber auf, wechselt aber im nächsten Schritt auf ein übergeordnetes 
        ## Industriesegment. Das wird erreicht, indem zu Beginn alle S&P Unternehmen
        ## selektiert werden, deren SIC Code identisch mit dem untersuchten SIC Code sind.
        ## Falls das zu wenige sind, werden bei der nächsten Prüfung nur die ersten drei 
        ## SIC Stellen verglichen, dann zwei etc., bis genug Beobachtungen vorliegen oder
        ## abgebrochen wird.    
        
        ## Zunächst Bestimmung der minimal erforderlichen Beobachtungen
        ## NoObsReq Mindestbeobachtungen pro Intervall pro Unternehmen 
        ## NoCompReq Mindestzahl an Unternehmen nach Auswertung von NoObsReq
        ## Je nach Auswahl von Jahr/Monat können sich NoObsReq und NoCompReq
        ## unterscheiden.
        if(options$Period=="year"){NoObsReq<-options$Year$NoObsReqYear; NoCompReq<-options$Year$NoCompReqYear}
        else {NoObsReq<-options$Month$NoObsReqMonth; NoCompReq<-options$Month$NoCompReqMonth}

        ## Verlängerung aller SICs auf 4 Stellen
        SicL<-nchar(Sic)
        if (SicL<4){Sic <- paste(paste(rep(0,4-SicL),collapse=""),Sic,sep="")}
        
        ## bei jedem Rekursionsaufruf wird SicDigits um eins kleiner. Nimmt SicDigits
        ## "0" an, gibt es ngibt es auf keiner Ebene genug Unternehmen wird die
        ## Funktion abgebrochen.
        if (SicDigits==0){return(NA)}

        ## Auswahl der Zeilen (aus S&P Zugehörigkeits Tabelle), die zum Industriesegment
        ## des untersuchten Unternehmens passen.
        ## Der Ausschnitt unterscheidet sich zwischen den rekursiven Aufrufen dieser Funktion.
        ## Je mehr Rekursionen pro Unternehmen gestartet werden, desto mehr Stellen werden
        ## von dem SIC Code (SicDigits wird kleiner 4,3,2,1) abgeschnitten. So wird von spezifischen
        ## zu übergeordneten Industrie Segmenten geprüft. 
        Boolean <- substring(BooleanSapData$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
        Boolean[is.na(Boolean)] <- FALSE

        if(options$Method == "one"|options$Method == "two"){    
            if(options$Method == "one"){   
                
                ## Für Methode "one" werden nur die S&P Unternehmen zum letzten Datum vor
                ## M&A berücksichtigt
                Spalten <- Boolean*BooleanSapData[[max(DateSap)]]
                
                ## Berechnung der Industrie Zeitreihe (für Segment)
                IndustryTs <- SapData[[options$EntityProperty]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                } 
            else if(options$Method == "two"){

                ## Für Methode "two" werden sämtliche S&P 1500 Unternehmen im Berechnungszeitraum
                ## berücksichtigt
                Spalten<-rowSums(Boolean*BooleanSapData[,DateSap,with=F]) > 0
                IndustryTs <- SapData[[options$EntityProperty]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                }  
             
            ## Prüfung, ob Bedingung für Mindestbeobachtungen erfüllt ist,
            ## falls nicht wird rekursiver Aufruf dieser Funktion mit SIC Vergleich um eine
            ## Stelle verschoben (es wird praktisch auf ein übergeordnetes SIC-Level
            ## beim nächsten Aufruf geprüft)    
            if(sum(rowSums(!is.na(IndustryTs[,Date,with=F])) >= NoObsReq) >= NoCompReq){
                ## Test, ob zu wenig Beobachtungen pro Unternehmen oder zu wenig Unternehmen pro Industrie
                ## im Zeitraum
                
                ## Zurückgabe nur der Unternehmen, die die NoObsReq-Bedingung erfüllen.
                return(IndustryTs[rowSums(!is.na(IndustryTs[,Date,with=F])) >= NoObsReq,])}
            
            ## Rekursiver Aufruf
            else {CalcIndustryTs(BooleanSapData,SapData,Date,DateSap,Sic,SicDigits-1,options)}
            }
            
        else if(options$Method == "three"){
            Spalten <- Boolean*BooleanSapData[,Date,with=F]
            IndustryTs <- sapply(Date,function(i_Date){SapData[[options$EntityProperty]][[i_Date]][as.logical(Spalten[[i_Date]])]},simplify=F,USE.NAMES = TRUE) ##

            if(all(sapply(IndustryTs,length)>=NoCompReq)){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs)}     ## returns only those companies with enough observations
            else {CalcIndustryTs(BooleanSapData,SapData,Date,DateSap,Sic,SicDigits-1,options)}
            }

        }   
 
      


##7.3	CorRow: Wendet Funktion CalcCorr() auf jedes Element der Tabelle „…“ an,
#####   die die Übersicht von M&A enthält, die mit dem R-Code „….“ erstellt wurde
CorRow<-function(SumTab,BooleanSapData,SapData,options){##data.table
        N_Values <- nrow(SumTab)
        ##N_Values <- 50
        
        ## Erstellung eines leerer Datensatz, in dem Korrelationen gespeichert werden sollen
        Correlations <- rep(NA,N_Values)
        
        ## for-Schleife startet Prüfung für jeden M&A (jede Zeile) im Datensatz 
        for(n_row in 1:N_Values){
            ## Entnahme relevanter M&A Daten    
            Date <- SumTab$Date[n_row]
            SicAcquiror <- SumTab$Acquiror_Sic[n_row]
            SicTarget <- SumTab$Target_Sic[n_row]
            
            ## Falls SICs der M&A Unternehmen identisch sind, ist Korrelation = 1
            if(SicAcquiror==SicTarget){Correlations[n_row] <- 1;next}
            
            ## Entfernung von M&As vor Untersuchungszeitraum
            if(as.Date(Date) < as.Date("1995-11-30")){Correlations[n_row] <- NA;next}
  
            ## Aufruf der Funktion für Korrelationsberechnung und Zuweisung des Ergebnisses
            Correlations[n_row] <- CalcCorr(BooleanSapData,SapData,SicAcquiror,SicTarget,Date,options,IndexValues)
            }
        Correlations <- as.numeric(Correlations)
        }
 
  system.time( b<-CorRow(testob,Boolen,SapData,CorCalcOptions)) 