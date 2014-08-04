## Der nachfolgende R-Code dient zur Berechnung der Korrelationen zwischen 
## den M&A Partnern. Die Korrelationen werden auf Basis von Sales- und Marketvalue-
## Daten zwischen den jeweiligen Industriesegmenten der Partner berechnet. Die 
## Werte der Industriesegmente werden durch S&P 1500 Unternehmen bestimmt, die den 
## korrespondierenden Segmenten angehören.

## Der SDC Datensatz "SDC_Dataset.csv" muss im R-Verzeichnis sein. Er wird
## von der letzten Funktion Calc_MandA_Cor() aufgerufen, die sich jeden M&A-Eintrag 
## zieht und anhand der SICs und des Datums die Korrelationen bestimmt.
## Zusätzlich müssen die Datensätze "S&P_Composition.csv" und "S&P_Company_Data.csv" 
## im lokalen R-Verzeichnis abgelegt sein.



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
## 1.1  Sp_Composition_Table(): Liest Datensatz ein, der für jeden Monat, in dem ein 
##      Unternehmen im S&P 1500 war, dieses Unternehmen mit seinem Datastream Code und 
##      dem Monat (plus Jahr) für diesen Monat einmal enthält.  
## 1.2	Sp_Composition_Table():  Erstellt leeren data.table der Unternehmen der Zeilen 
##      nach und Monate den Spalten nach ordnet.
## 1.3	Composition_Logic(): Füllt den von Sp_Composition_Table() erstellten data.table mit 
##      logischen Werten je nach Zugehörigkeit des Unternehmens zum S&P 1500. Stellt den, 
##      in Schritt 1.1 eingelesenen Datensatz, besser dar.

## 2.	Einlesen und Überarbeiten eines Datensatzes mit Unternehmensdaten, zur 
##      Erstellung einer Liste von verschiedenen Datensätzen, aufgeteilt nach 
##      Unternehmenskennzahlen (Sales, MV etc.):
## 2.1	SapData_Read(): Liest Datei "S&P_Company_Data.csv" ein, teilt die Tabelle in 
##      verschiedene Tabellen auf, jede für eine Unternehmenskennzahl, und übergibt 
##      die Tabellen als Liste. Außerdem wird auf SapData_Complete zugegriffen.
## 2.2	SapData_Complete(): Stellt Identität zwischen Datensätzen her, die in einer 
##      Liste (in SapData_Read() übergeben) enthalten sind. Das heißt, jeder Datastream 
##      Code bzw. jedes Unternehmen wird in jedem Datensatz in der List an derselben 
##      Position auftauchen.

## 3.	Zum Erstellen eines data.table, der, ähnlich dem von Sp_Composition_Table() Composition_Logic() unter 
##      Schritt 1 erstellten, die Zugehörigkeit von Unternehmen zum S&P 1500 aufzeigt, 
##      allerdings mit Unternehmen(Datastream Codes) geordnet nach der Ordnung in den 
##      Datensätzen in der Liste von Schritt 2. 
##      Composition_Logic_Ordered()

## 4.	Grundlage der Korrelationsbestimmung sind die Sales-Entwicklungen der 
##      Industriesegmente. Zur Berechnung  werden die marktwertgewichteten Sales 
##      und alternativ die relative Sales-Entwicklung verwendet, die von den folgenden
##      Funktionen berechnet werden.    . 
## 4.1	Calc_Sales_Dev_Yearly(): Berechnung der relativen Entwicklung der Sales pro Unternehmen. 
## 4.2	Calc_Sales_by_MV(): Berechnung der marktwertgewichteten Sales jedes Unternehmens.

## 5.	Zur Bestimmung von Parametern, auf deren Grundlage die Korrelationen 
##      berechnet werden sollen. Set_Cor_Options() ist die wichtigste Funktion. 
##      Die von ihr zurückgegebene Liste, legte fest, nach welchen Kriterien die 
##      Korrelationen bestimmt werden sollen z.B. Zeitraum, Toleranz für fehlende 
##      Werte Berechnungsmethode etc.. Genauere Informationen sind im Dokument 
##        "Erklärung_Set_Cor_Options"
##      Set_Cor_Options()

## 6	Zur Berechnung der Entwicklung des S&P 1500 als Benchmark Index. Wenn die 
##      Korrelationen nicht direkt zwischen den Segmenten, sondern nur zwischen 
##      den Residuen einer Regression von Segmentwerten auf Indexwerte bestimmt 
##      werden sollen:  
## 6.1	IndexCompositionDscd(): Ähnlich dem in Schritt 3 von Composition_Logic_Ordered() 
##      erstellten Datensatz zur S&P 1500 Zugehörigkeit, aber anstelle von 
##      logischen Werten werden die Datastream Codes in den Spalten wiederholt 
##      um die Zugehörigkeit der Unternehmen zum S&P 1500 zu kennzeichnen.
## 6.2	Calc_Index_Values: Erstellt Datensatz, der für jeden Monat oder jedes Jahr 
##      eine Zeitreihe der Entwicklung der zurückliegenden Indexwerte angibt. Die
##      Funktion wird als Teil von Calc_MandA_Cor() (Schritt 7.3) aufgerufen.

## 7	Zur Berechnung der Korrelationen auf Basis der durch Set_Cor_Options 
##      spezifizierten Argumente und der in dem SDC Datensatz "SDC_Dataset.csv"
##      (der eingelesen wird) gelisteten M&As.
## 7.1	Calc_Corr(): Berechnet die Korrelation zwischen zwei Industriesegmenten, die 
##      durch einen M&A Eintrag in SDC Datensatz definiert sind. Die Zeitreihen für jeden SIC-Code 
##      werden durch jeweiligen Aufruf der Funktion Calc_Industry_Ts() in Calc_Corr() bestimmt.
## 7.2	Calc_Industry_Ts(): Gibt in Abhängigkeit vom M&A Datum und einem SIC-Code eine 
##      Zeitreihe von Werten einer Unternehmenskennzahl(z.B. Sales to MV) für das durch 
##      den SIC-Code spezifizierte Industriesegment wieder (berechnet aus S&P 1500 Segmenten).
## 7.3	Calc_MandA_Cor(): Liest Tabelle "SDC_Dataset.csv" ein und wendet Funktion Calc_Corr() 
##      auf jedes Element bzw. jeden M&A an.



setwd("C:/Users/Sebastian Stenzel/Desktop/R_Data/Cor_Data/")

library(data.table)


##1.    Erstellung eines Datensatzes, der Zugehörigkeit von Unternehmen zum S&P 
##      1500 angibt.

##1.1   Sp_Composition_Table(): Liest Datensatz "S&P_Composition.csv" ein, der für jeden Monat, 
#####   in dem ein Unternehmen im S&P 1500 war, dieses Unternehmen mit seinem Datastream Code und 
#####   dem Monat (plus Jahr) für diesen Monat einmal enthält. 
Read_SP_Composition <- function(){
    
        sp_composition <- fread("S&P_Composition.csv",sep=";",colClasses=c("character", 
                            "character","character","numeric","character"))

        sp_composition[,Datum:=substr(as.Date(Datum),1,7)]
        SPComposition <<- sp_composition
        }
        
##  Aufruf von Read_SP_Composition() bei Erstellung von SPComposition Datensatz in Funktion    
    Read_SP_Composition()

##1.2   Sp_Composition_Table():  Erstellt leeren data.table, der sämtliche Unternehmen, die im 
#####   Untersuchungszeitraum im S&P 1500 gelistet waren, in Zeilen und Spalten nach Monaten ordnet. 
#####   Die Funktion wird von anderen Funktionen aufgerufen, um den Datensatz mit logischen Werten 
#####   oder Datastream Codes zu füllen, die die Zugehörigkeit der Unternehmen zum S&P 1500 für 
#####   jedes Datum anzeigen.
Sp_Composition_Table <- function(sp_composition = SPComposition,as_char=FALSE){

        ColVec <- sort(unique(sp_composition$Datum),decreasing=F)
        ## Sammlung sämtlicher Daten des Untersuchungszeitraums (Jahr und Monat)
        
        DSCDPos <- !duplicated(sp_composition$DSCD)
        ## Logischer Vektor, der für jedes Unternehmen im Datensatz an dessen erster
        ## Position im Datensatz TRUE enthält.
        
        RowVec<-sp_composition$DSCD[DSCDPos] 
        ## Sammlung sämtlicher Datastream Codes im Datensatz
        
        SicVec<-sp_composition$WC07021[DSCDPos] 
        ## Sammlung sämtlicher SIC Codes im Datensatz
        
        NamesVec<-sp_composition$Name[DSCDPos]
        ## Sammlung der Unternehmensnamen
        
        RowLeng<-length(RowVec)
        
        SicVec[nchar((SicVec))==3]<-paste("0",SicVec[nchar((SicVec))==3],sep="")
        ## Alle SIC Codes, die weniger als 4 Ziffern haben, werden durch Setzen von
        ## "0" auf vier verlängert.
        
        ## Erstellung des leeren data.tables, der lediglich Informationen zum Unternehmen 
        ## (Datastream Code, Sic Code) enthält. Im nächsten Schritt wird er durch logische
        ## Werte ergänzt, die die Zugehörigkeit der Unternehmen zum S&P 1500 nach Datum
        ## anzeigen.
        SPCompositionTable<-data.table(Name=NamesVec)
        SPCompositionTable[,c("DSCD","SIC_four","SIC_three","SIC_two","SIC_one"):=data.frame((RowVec),SicVec,
                        substr(SicVec,1,3),substr(SicVec,1,2),substr(SicVec,1,1),stringsAsFactors=F)]

        if(as_char){SPCompositionTable[,{ColVec}:=as.character(rep(NA,RowLeng))]}
        else {SPCompositionTable[,{ColVec}:=rep(NA,RowLeng)]}
        SPCompositionTable[["2012-10"]]<-as.character(NA)

        setkey(SPCompositionTable,"DSCD")
        SPCompositionTable
        }
  
##1.3	Composition_Logic (): füllt den von Sp_Composition_Table() erstellten data.table mit 
#####   logischen Werten je nach Zugehörigkeit des Unternehmens zum S&P 1500. Stellt den, 
#####      in Schritt 1.1 eingelesenen Datensatz, besser dar.
Composition_Logic <- function(sp_composition = SPComposition){
        
        sp_composition_table <- Sp_Composition_Table(SPComposition)
        
        SaPvalues <- function(x){
            sp_composition_table[[x]] <<- sp_composition_table[,DSCD]%in%sp_composition[sp_composition$Datum==colnames(sp_composition_table)[x],DSCD]
            ## sp_composition ist ein Datensatz (aus hier zwei relevanten Spalten), der für jeden
            ## Monat, in dem ein Unternehmen im S&P 1500 war, dieses Unternehmen mit seinem Datastream 
            ## Code und dem Monat (plus Jahr) für diesen Monat einmal enthält.
            ## Rechts des %in% wird von dem Datensatz die Spalte der Datastream Codes ausgewählt,
            ## aber nur die Zeilen, für die das Datum gerade dem Datum der ausgewählten Spalte der 
            ## sp_composition_table entspricht (colnames(sp_composition_table)[x]) - folglich nur den S&P 1500 für diesen
            ## Monat.
            ## Durch den Vergleich mit sp_composition_table[,DSCD] links von %in% entsteht ein logischer Vektor,
            ## der die S&P 1500 Zugehörigkeit der Unternehmen in der S&PMatrix angibt. Der Vektor wird der
            ## Spalte des untersuchten Datums in der sp_composition_table[[x]] zugeordnet.
            invisible()
            }
        
        ## Anwendung der Funktion SaPvalues auf alle Datumsspalten der sp_composition_table
        for(i in 7:ncol(sp_composition_table)){SaPvalues(i)}

        SPCompositionLogic <<- sp_composition_table
        }

##  Aufruf von Composition_Logic() bei Erstellung von SPCompositionLogic Datensatz in Funktion   
    Composition_Logic(SPComposition)

    
    
    
## 2.   Erstellung eines Datensatzes, der für die S&P 1500 Unternehmen die Entwicklung
##      der zur Korrelationsberechnung notwendigen Unternehmenswerte (Sales, MV, etc.)
##      speichert.

##2.1	SapData_Read(): Liest Datei "S&P_Company_Data.csv" ein, teilt die Tabelle in 
#####   verschiedene Tabellen auf, jede für eine Unternehmenskennzahl und übergibt die 
#####   Tabellen als Liste. Außerdem wird auf SapData_Complete() zugegriffen.
SapData_Read <- function(sp_composition_logic=SPCompositionLogic,CompPropCol="KPI",NaRm = FALSE){
            
        SapData <- fread("S&P_Company_Data.csv",sep=";",stringsAsFactors=F)
        
        ## Entfernung der Wochentage aus den Spaltennamen (es verbleibt nur
        ## noch "Jahr-Monat")
        ColNames <- as.character(colnames(SapData)[-c(1:3)])
        NewColNames <- c(colnames(SapData[,1:3]),substring(ColNames,1,7))
        setnames(SapData,ColNames,NewColNames)
        
        ## Filterung der verschiedenen Unternehmenskennzahlen (MV, Sales etc.)
        CharLevel <- unique(as.character(SapData[[CompPropCol]]))
        
        ## Erweiterung der Tabelle um Spalten mit Informationen zum Industriesegment (Sic)
        SapData<- merge(sp_composition_logic[,c("DSCD","SIC_four"),with=F],SapData,by="DSCD",all=T,sort=F)
        
        ## Datensatz wird in Liste von mehreren Datensätzen gesplittet, ein Datensatz für 
        ## jede Unternehmenskennzahl
        SapData <- sapply(CharLevel,FUN=function(x){y<-SapData[[CompPropCol]]==x;y=SapData[y,]},simplify=F,USE.NAMES = TRUE) 
       
        ## Anwendung der Funktion SapData_Complete() auf jeden Datensatz in der Liste.
        ## Hängt Unternehmen mit Datastream Codes und NA-Werten an Datensätze an,
        ## wenn sie in den ursprünglichen nicht erfasst waren und ordnet diese, sodass
        ## jeder Datensatz die gleiche Länge mit gleicher Zeilenreihenfolge (nach Unternehmen) hat.
        SapData <- sapply(names(SapData),SapData_Complete,SapData=SapData,sp_composition_logic=sp_composition_logic,
                                simplify=F,USE.NAMES = TRUE)
        
        ## Eventuell Bereinigung des Datensatzes um Unternehmen, für die keine Informationen
        ## zur Industrieklassifizierung vorliegen
        if(NaRm){SapData <- sapply(names(SapData),function(x){SapData[[x]][!is.na(SapData[[x]][["SIC_four"]]),]},
                                simplify=F,USE.NAMES = TRUE)}

        SapData<<-SapData
        }

##2.2	SapData_Complete(): Stellt Identität zwischen den data.table her, die in einer Liste 
#####   enthalten sind, die in SapData_Read() übergegeben wird. Das heißt, jeder Datastream 
#####   Code bzw. jedes Unternehmen muss in jedem Datensatz auftauchen und auch an derselben 
#####   Position sein. Wichtig für die spätere Auswertung.
SapData_Complete  <- function(CompPropLevel,SapData,sp_composition_logic){
        PropTable <- SapData[[CompPropLevel]]
            
        MissingDscd <- !is.element(sp_composition_logic$DSCD,PropTable$DSCD)
        ## Überprüfung, welche Unternehmen noch nicht im Datensatz sind
            
        ## Abbruch, falls keine Unternehmen fehlen, zuvor noch Ordnung der Zeilen-
        ## reihenfolge nach Datastream Codes
        if(sum(MissingDscd)==0){ 
               PropTable<-PropTable[order(PropTable$DSCD),]
               rownames(PropTable)<-PropTable$DSCD
               return(PropTable)
               }
            
        ## Zwischenspeicherung der Unternehmen, die noch nicht erfasst sind und 
        ## Ergänzung des Datensatzes durch NA-Werte sowie zugehörige Spaltennamen
        MissingDscd <- sp_composition_logic[MissingDscd,c("DSCD","SIC_four","Name"),with=F]
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

    
    
    
## 3.   Erstellung eines Datensatzes, der angibt, in welchen Monaten die Unternehmen 
##      in dem Dantensatz von Schritt 2 S&P 1500 Unternehmen waren.
    
##3.    Composition_Logic_Ordered(): Erstellt ähnlich zu Funktion Composition_Logic () eine Tabelle, die die 
#####   Zugehörigkeit von Unternehmen zum S&P 1500 aufzeigt, allerdings wird die Ordnung 
#####   der Unternehmen genau der in den Datensätzen aus Schritt 2 angepasst.
Composition_Logic_Ordered<-function(SapData = SapData,sp_composition_logic = SPCompositionLogic){        
        setkey(sp_composition_logic,DSCD)
        sp_composition_logic<-sp_composition_logic[SapData$MV$DSCD,]

        SPCompositionLogic <<- sp_composition_logic
        }           

##  Aufruf von SapDataBoolean() weist die Tabelle der Variable SPCompositionLogic zu          
    Composition_Logic_Ordered(SapData,SPCompositionLogic)               

    
    
    
## 4.   Berechnung neuer Unternehmensdaten (aus Datensatz Schritt 2),die Grundlage 
##      für Korrelation sein sollen - relative Entwicklung der Sales und Sales to MV.
    
##4.1	Calc_Sales_Dev_Yearly(): Berechnet die relative Entwicklung der Sales pro Unternehmen,
#####   hängt die Tabelle SalesDev an die Liste SapData aus Schritt 2 an
Calc_Sales_Dev_Yearly <- function(sap_data = SapData, start_date = "1978-01-01", end_date = "2014-01-01"){

        KPI = "NetSalesRev"
        
        ## Erstellung von zwei Zeitreihen, von denen eine um ein Jahr abgeschnitten ist
        start_date <- as.POSIXlt(start_date)
        end_date <- as.POSIXlt(end_date)

        ## Erstellung der längeren Reihe
        YearsCoveredOne <- as.POSIXlt(end_date)
        YearsCoveredOne$year <- start_date$year:end_date$year
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

##4.2	Calc_Sales_by_MV(): Berechnung des wertgewichteten Sales jedes Unternehmens,
#####   hängt die Tabelle SalesByMV an die Liste SapData aus Schritt 2 an.
Calc_Sales_by_MV <- function(sap_data = SapData, start_date = "1978-01-01", end_date = "2014-01-01"){

        kpi_one <- "NetSalesRev"
        kpi_two <- "MV"

        ## Erstellung von zwei Zeitreihen, von denen eine um ein Jahr abgeschnitten ist
        start_date <- as.POSIXlt(start_date)
        end_date <- as.POSIXlt(end_date)
        
         ## Erstellung der längeren Reihe
        YearsCoveredOne <- as.POSIXlt(end_date)
        YearsCoveredOne$year <- start_date$year:end_date$year
        YearsCoveredOne <- substr(YearsCoveredOne,1,7)
        
        ## Erstellung der kürzeren Reihe
        YearsCoveredTwo<-YearsCoveredOne[-length(YearsCoveredOne)]

        ## Division der Sales durch MV und Erweiterung der Tabelle um
        ## Informationen zu Unternehmen
        SalesByMV <- sap_data[[kpi_one]][,YearsCoveredOne,with=F]/sap_data[[kpi_two]][,YearsCoveredOne,with=F]
        SalesByMV <- cbind(sap_data[[kpi_one]][,c("DSCD","SIC_four","Name"),with=F],KPI="SalesByMV",SalesByMV)
        
        
        ## Entfernung von Inf-Werten analog zu Calc_Sales_Dev_Yearly()
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

##  Aufruf von Calc_Sales_by_MV() hängt Tabelle SalesDev an die Liste SapData an       
    Calc_Sales_by_MV()    

##  Aufruf von Calc_Sales_Dev_Yearly() hängt Tabelle SalesByMV an die Liste SapData an           
    Calc_Sales_Dev_Yearly()  

    
    
    
## 5.   Festlegung von Optionen für Korrelationsbestimmung.    
    
##5.    Set_Cor_Options(): Bestimmt Parameter, auf deren Grundlage die Korrelationen berechnet 
####    werden sollen. Set_Cor_Options() ist die wichtigste Funktion. Die von ihr zurückgegebene
####    Liste legte fest, nach welchen Kriterien die Korrelationen bestimmt werden sollen z.B. 
####    Zeitraum, Toleranz für fehlende Werte, Berechnungsmethode etc.. Genauere Informationen 
####    sind im Dokument "Erklärung_Set_Cor_Options". Die Werte sind bei Aufruf vordefiniert, können aber 
####    angepasst werden, wobei nicht jede Kombination möglich ist.
Set_Cor_Options<-function(NoObsReqMonth = 8,NoCompReqMonth = 5,NoObsReqYear= 5, NoCompReqYear= 4,
                    Period="year",PeriodLengthYear=7,PeriodLengthMonth=10,Method="one",EntityProperty="SalesByMV"
                    ,SpExessCor=F){
        CorCalcOptions<<-list(Month=data.frame("NoObsReqMonth"=NoObsReqMonth,"NoCompReqMonth"=NoCompReqMonth),
                Year=data.frame("NoObsReqYear"=NoObsReqYear,"NoCompReqYear"=NoCompReqYear),"Period"=Period,
                "PeriodLengthYear"=PeriodLengthYear,"PeriodLengthMonth"=PeriodLengthMonth,"Method"=Method,
                "EntityProperty"=EntityProperty,"SpExessCor"=SpExessCor)
        }
##  Aufruf von Set_Cor_Options() erstellt Liste CorCalcOptions  
    Set_Cor_Options()


    
## 6.   Berechnung der Entwicklung der S&P 1500 als Benchmark für die Entwicklung der
##      Unternehmensdaten. Falls später nicht Korrelationen direkt zwischen den Segmenten,
##      sondern nur zwischen den Residuen einer Regression von Segmentwerten auf 
##      Indexwerte bestimmt werden sollen.  
    
##6.1	IndexCompositionDscd():  Erstellt Tabelle ähnlich der Rückgabe von 
#####   Composition_Logic_Ordered() in Schritt 3. Statt logischer Werte 
#####   werden aber Datastream Codes in den Spalten anstelle von TRUE-Werten 
#####   wiederholt, um die Zugehörigkeit der Unternehmen zum S&P 1500 zu kennzeichnen.
IndexCompositionDscd <- function(sp_composition_logic=SPCompositionLogic,sp_composition = SPComposition){
        ## Zunächst Erstellung einer leeren Tabelle mit Daten und Datastream Codes
        ## Wie zuvor über Sp_Composition_Table() aus Schritt 1
        sp_composition_dscd <- Sp_Composition_Table(sp_composition,as_char=TRUE)

        for (i_col in names(sp_composition_logic)[-(1:6)]){
            ## Zuweisung der Datastream Codes    
            set(sp_composition_dscd,i=which(sp_composition_logic[[i_col]]),j=i_col,value=sp_composition_logic[["DSCD"]][sp_composition_logic[[i_col]]])
            ## i gibt die zu selektierenden Reihen an
            ## j selektiert die Spalte
            ## value die zuzuweisenden Datastream Codes
            }
        sp_composition_dscd
        }
##  Aufruf von IndexCompositionDscd() und Zuweisung zu SpCompositionDscd          
    SpCompositionDscd <- IndexCompositionDscd ()    

##6.2	Calc_Index_Values() erstellt Datensatz, der die Entwicklung des S&P 1500 
#####   für eine bestimmte Kennzahl über einen vordefinierten Zeitraum wiedergibt.
#####   Dabei ist der Spaltenname das Jahr "0" (bei jährlicher Berechnung) bzw. Jahr plus
#####   Monat. In der Spalte sind vom Jahr "0" ausgehend die zurückliegenden monatlichen
#####   bzw. jährlichen Indexwerte abgetragen. Die Funktion wird als Teil von Calc_MandA_Cor() 
#####   (Schritt 7.3) aufgerufen.
Calc_Index_Values <- function(sp_composition_dscd = SpCompositionDscd,cor_options = CorCalcOptions){
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
        if(cor_options$Period=="year"){period_length <- cor_options$PeriodLengthYear-1}
        else {period_length <- cor_options$PeriodLengthMonth-1}
        
        ## Erstellung der leeren Datensätze für die Berechnungen mit Methode "one" und
        ## "two"
        sp_index_value_one <- data.table("EntityProperty"=-(period_length:0))
        sp_index_value_two <- data.table("EntityProperty"=-(period_length:0))
        sp_index_value_one[,names(SapData[[cor_options$EntityProperty]]):=as.numeric(NA)]
        sp_index_value_two[,names(SapData[[cor_options$EntityProperty]]):=as.numeric(NA)]
        
        
            YearsMean <- function(Date){
                Years <- grep(substr(Date,1,4),names(SapData[[cor_options$EntityProperty]]))
                YearsMean <- mean(rowMeans(SapData[[cor_options$EntityProperty]][sap_composition_period,names(SapData[[cor_options$EntityProperty]])[Years],with=F],na.rm=T),na.rm=T) 
                YearsMean}
            
            IntervalMean <- function(index){ 
               year<<-c(year,names(SapData[[cor_options$EntityProperty]])[index[2]])    
               interval_index <- names(SapData[[cor_options$EntityProperty]])[(index[1]+1):index[2]]
               interval_mean <- mean(rowMeans(SapData[[cor_options$EntityProperty]][sap_composition_period,interval_index,with=F],na.rm=T),na.rm=T)  
               interval_mean}
        
       
        ## Berechnung für Methode "one"
        for(col_name in names(SapData[[cor_options$EntityProperty]])[-(1:4)]){
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
                    if(cor_options$Period=="year"){col_Date$year<-(col_Date$year-period_length):col_Date$year}
                    else {col_Date$mon<-(col_Date$mon-period_length):col_Date$mon}
                    
                    ## Bestimmung Spaltenindizes für spätere Auswahl
                    col_index <- which(is.element(names(SapData[[cor_options$EntityProperty]]),substr(col_Date,1,7)))
                    }
                else{
                    col_index <- which(names(SapData[[cor_options$EntityProperty]])== col_name)
                    col_index <- (col_index-period_length):col_index
                    }
            ## Berechnung der Zeitreihe        
            sp_index_value_one[[col_name]]<-(colMeans(SapData[[cor_options$EntityProperty]][sp_composition_dscd[[col_name]],col_index,with=F],na.rm=T))
            ## Vom Datensatz SapData werden nur die Unternehmen ausgewählt, die zum Datum "col_name" im
            ## S&P 1500 waren und für diese werden die Werte zu den durch col_index definierten Zeitpunkten
            ## entnommen. Durch ColMeans werden die Mittelwerte zu den Daten berechnet        
            }
       
        ## Berechnung für Methode "two"
        for(col_name in names(SapData[[cor_options$EntityProperty]])[-(1:4)]){
            ## Weil im S&P 1500 Datensatz nur die Zusammensetzung von Ende 1994 bis Ende 2013
            ## enthalten ist, wird die Berechnung auf den Bereich begrenzt.
            if(col_name>"2014" |col_name<"1994-12" ){next}
                
                if(TRUE){
                    ## Umwandlung des Spaltennamens zum Typ POSIXlt
                    col_Date<-paste(col_name,"-15",sep="")
                    col_Date<-as.POSIXlt(col_Date)
                    
                    ## je nach Auswahl von Jahres- Monatsintervallen - Bildung von Daten für Zeitreihe
                    ## durch Subtraktion der Jahres/Monats-Intervalle von col_Date
                        if(cor_options$Period=="year"){col_Date$year<-(col_Date$year-period_length):col_Date$year}
                        else {col_Date$mon<-(col_Date$mon-period_length):col_Date$mon}
                    
                    ## Bestimmung Spaltenindizes für spätere Auswahl
                    col_index <- which(is.element(names(SapData[[cor_options$EntityProperty]]),substr(col_Date,1,7)))
                    
                    ## Für Methode "two" werden nicht nur S&P 1500 Unternehmen zum Zeitpunkt "0",
                    ## sondern sämtliche, die in dem Betrachtungszeitraum im S&P 1500 waren,
                    ## berücksichtigt. Daher hier auch Auswahl der SP Spalten für jedes Datum.
                    sp_col_index <- which(is.element(names(sp_composition_dscd),substr(col_Date,1,7)))
                    
                    ## Abbruch, wenn keine Werte vorhanden
                    if(length(sp_col_index)==0){next}
                    
                    ## Wenn der Zeitraum vor das Jahr 1995 zurückgeht, immer automatische
                    ## Verlängerung bis zum ersten Datum der Erfassung der S&P 1500 Zusammensetzung.
                    if( length(sp_col_index)<(period_length+1)){sp_col_index<-c(7,sp_col_index)}
                    }
                else{
                    col_index <- which(names(SapData[[cor_options$EntityProperty]])== col_name)
                    col_index <- (col_index-period_length):col_index
                    sp_col_index <- which(is.element(names(sp_composition_dscd),names(SapData[[cor_options$EntityProperty]])[col_index]))}
            
            if((length(sp_col_index)<1)){next}
            
            ## Zur Auswahl aller S&P-Composition Spalten zwischen dem minimalen und maximalen Spaltenindex.
            ## Vorher waren nur einzelne Punkte im Intervall erfasst, wenn die Intervalllänge jährlich
            ## gewesen wäre.
            sp_col_index <- min( sp_col_index): max ( sp_col_index)

            ## Entnahme aller Datastream Codes für die Unternehmen, die im Betrachtungszeitraum im S&P 1500
            ## Index gewesen sind (Betrachtungszeitraum wird durch Spaltenindizes sp_col_index), definiert. 
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
                    sp_index_value_two[[col_name]]<-colMeans(SapData[[cor_options$EntityProperty]][sap_composition_period,col_index,with=F],na.rm=T)
                    }
            }
        list("one"=sp_index_value_one,"two"=sp_index_value_two)
        }    

        


## 7.   Berechnung der Korrelation
##      Funktion Calc_Industry_Ts() entnimmt die Zeitreihe, Funktion Calc_Corr() bestimmt 
##      Korrelation, Funktion Calc_MandA_Cor() wendet Calc_Corr() auf jeden M&A im SDC Datensat
##      ("SDC_Dataset.csv") an.    

##7.1	Calc_Corr(): Berechnet die Korrelation zwischen zwei Industriesegmenten, die 
#####   durch einen M&A Eintrag in SDC Datensatz definiert sind. Die Zeitreihen für jeden SIC-Code 
#####   werden durch jeweiligen Aufruf der Funktion Calc_Industry_Ts() in Calc_Corr() bestimmt.
Calc_Corr<-function(sp_composition_logic,sap_data,SicAcquiror,SicTarget,Date,cor_options=CorCalcOptions,index_values){
     
    ## Bestimmung der Daten (Intervall) für die Zeitreihe, falls jährliche Intervalle verwendet werden sollen    
    if(cor_options$Period=="year"){        
        TimePeriod <- cor_options$PeriodLengthYear     
        Date <- as.POSIXlt(Date)
        Date$mday <- 15    
        Date$year <- Date$year - (TimePeriod:1) 
        Date$mon <- 0
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)
         
        NoObsReq<-cor_options$Year$NoObsReqYear
        ## Zwischenspeicherung der Mindestbeobachtungen bzw. für wieviele Zeitpunkte im Intervall
        ## Beobachtungen vorhanden sein müssen   
        }
    
    ## Bestimmung der Daten für die Zeitreihe, falls monatliche Intervalle verwendet werden sollen    
    else {
        TimePeriod <- cor_options$PeriodLengthMonth
        Date <- as.POSIXlt(Date)
        Date$mday <- 15        
        Date$mon <- Date$mon - (TimePeriod:1)
        Date<-as.Date(Date)
        Date<-substring(as.Date(Date),1,7)
        Date<-sort(Date)

        NoObsReq<-cor_options$Month$NoObsReqMonth
        ## Zwischenspeicherung der Mindestbeobachtungen bzw. für wieviele Zeitpunkte im Intervall
        ## Beobachtungen vorhanden sein müssen  
        }    
        
        ## Falls Methode "two" angewendet werden soll, muss auch ein Zeitraum bestimmt werden,
        ## innerhalbe dessen alle S&P 1500 Unternehmen bestimmt werden     
        if(cor_options$Method == "two"){
            ## if(cor_options$BreakYear=="cut" | cor_options$BreakYear=="extend"){DateSap<-Date[Date>=1995]}##;Date<-DateSap}
            ## Industriekorrelationen werden mit S&P 1500 Unternehmen berechnet
            ## Das Intervall DateSap wird bei unter 1995 abgeschnitten, weil zuvor keine
            ## Angaben zur S&P 1500 Zusammensetzung vorhanden sind
            DateSap <- Date[Date>=1995]
            if(length(DateSap)==0){return(NA)}
           
            DateSap <- which(is.element(colnames(sp_composition_logic),DateSap))

            ## Wenn der Zeitraum vor das Jahr 1995 zurückgeht, immer automatische
            ## Verlängerung bis zum ersten Datum der Erfassung der S&P 1500 Zusammensetzung.
            if(length(DateSap)<length(Date)){DateSap<-c(7,DateSap)}
            DateSap<-min(DateSap):max(DateSap)

            }
            
        ## Für Methode "one" spielt nur das letzte (jüngste) Datum ein Rolle, DateSap muss aber trotzdem
        ## bestimmt werden, ist aber identisch zu Date    
        else {DateSap <- Date} 

        ## Wenn DateSap leer ist (keine Einträge zur S&P 1500 Zusammensetzung für den
        ## Zeitpunkt), erfolgt Abbruch    
        if(length(DateSap)==0){return(NA)}
        
        ## Berechnung der Zeitreihen durch Aufruf von Calc_Industry_Ts()
        AcIndustryTs <- Calc_Industry_Ts(sp_composition_logic,sap_data,Date,DateSap,SicAcquiror,SicDigits=4,cor_options)  
        TaIndustryTs <- Calc_Industry_Ts(sp_composition_logic,sap_data,Date,DateSap,SicTarget,SicDigits=4,cor_options) 
            
            ## Abbruch, falls eine Zeitreihe NA ist
            if(length(AcIndustryTs)==1|length(TaIndustryTs)==1){return(NA)}
            
            if(cor_options$Method=="three"){
                return(cor(sapply(AcIndustryTs,sum,na.rm=T),sapply(TaIndustryTs,sum,na.rm=T)))
                }
        
        ## Bestimmung Mittelwert für Zeitpunkte im Intervall  
        AcIndustryTs <- colMeans(AcIndustryTs[,-(1:2),with=F],na.rm=T)
        TaIndustryTs <- colMeans(TaIndustryTs[,-(1:2),with=F],na.rm=T)
        
        ## Falls Korrelationen nicht direkt bestimmt werden sollen, sondern 
        ## über die Korrelation der Residuen einer Regression 
        ## der Zeitreihe auf die Entwicklung des S&P als Index:    

      if(cor_options$SpExessCor){    
            x <- index_values[[cor_options$Method]][[max(Date)]]
      
            coefficient <- coef(lm(AcIndustryTs~x))
            AcIndustryTs <- (AcIndustryTs-(coefficient["(Intercept)"]-coefficient["x"]*x))
            coefficient <- coef(lm(TaIndustryTs~x))
            TaIndustryTs <- (TaIndustryTs-(coefficient["(Intercept)"]-coefficient["x"]*x))
            }
            
        return(cor(AcIndustryTs,TaIndustryTs))
        } 
 
##7.2	Calc_Industry_Ts(): Gibt in Abhängigkeit von M&A Datum und einem SIC-Code eine 
#####   Zeitreihe von Werten für ein durch den Sic Code spezifiziertes Industriesegment
#####   an.
Calc_Industry_Ts<-function(sp_composition_logic,sap_data,Date,DateSap,Sic,SicDigits=4,cor_options){
        ## Calc_Industry_Ts() bestimmt nur eine Zeitreihe, wenn genug Beobachtungen
        ## im Industriesegment vorliegen. Ist das nicht der Fall, ruft sich die Funktion
        ## wieder selbst auf, wechselt aber im nächsten Schritt auf ein übergeordnetes 
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
        if(cor_options$Period=="year"){NoObsReq<-cor_options$Year$NoObsReqYear; NoCompReq<-cor_options$Year$NoCompReqYear}
        else {NoObsReq<-cor_options$Month$NoObsReqMonth; NoCompReq<-cor_options$Month$NoCompReqMonth}

        ## Verlängerung aller SICs auf 4 Stellen
        SicL<-nchar(Sic)
        if (SicL<4){Sic <- paste(paste(rep(0,4-SicL),collapse=""),Sic,sep="")}
        
        ## Bei jedem Rekursionsaufruf wird SicDigits um eins kleiner. Nimmt SicDigits
        ## "0" an, gibt es auf keiner Ebene genug Unternehmen und die Funktion
        ## Funktion wird abgebrochen.
        if (SicDigits==0){return(NA)}

        ## Auswahl der Zeilen (aus S&P Zugehörigkeits Tabelle), die zum Industriesegment
        ## des untersuchten Unternehmens passen.
        ## Der Ausschnitt unterscheidet sich, zwischen den rekursiven Aufrufen dieser Funktion.
        ## Je mehr Rekursionen pro Unternehmen gestartet werden, desto mehr Stellen werden
        ## von dem SIC Code (SicDigits wird kleiner 4,3,2,1) abgeschnitten. So wird von spezifischen
        ## zu übergeordneten Industrie Segmenten geprüft. 
        Boolean <- substring(sp_composition_logic$SIC_four,1,SicDigits)==substring(Sic,1,SicDigits)
        Boolean[is.na(Boolean)] <- FALSE

        if(cor_options$Method == "one"|cor_options$Method == "two"){    
            if(cor_options$Method == "one"){   
                
                ## Für Methode "one" werden nur die S&P Unternehmen zum letzten Datum vor
                ## M&A berücksichtigt
                Spalten <- Boolean*sp_composition_logic[[max(DateSap)]]
                
                ## Berechnung der Industrie Zeitreihe (für Segment)
                IndustryTs <- sap_data[[cor_options$EntityProperty]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                } 
            else if(cor_options$Method == "two"){

                ## Für Methode "two" werden sämtliche S&P 1500 Unternehmen im Berechnungszeitraum
                ## berücksichtigt

                Spalten<-rowSums(Boolean*sp_composition_logic[,DateSap,with=F]) > 0
                IndustryTs <- sap_data[[cor_options$EntityProperty]][as.logical(Spalten),c("DSCD","SIC_four",Date),with=F]
                }  
             
            ## Prüfung, ob Bedingung für Mindestbeobachtungen erfüllt ist,
            ## falls nicht, wird rekursiver Aufruf dieser Funktion mit SIC Vergleich um eine
            ## Stelle verschoben (es wird praktisch auf ein übergeordnetes SIC-Level
            ## beim nächsten Aufruf geprüft)    
            if(sum(rowSums(!is.na(IndustryTs[,Date,with=F])) >= NoObsReq) >= NoCompReq){
                ## Test, ob zu wenig Beobachtungen pro Unternehmen oder zu wenig Unternehmen pro Industrie
                ## im Zeitraum
                
                ## Zurückgabe nur der Unternehmen, die die NoObsReq-Bedingung erfüllen.
                return(IndustryTs[rowSums(!is.na(IndustryTs[,Date,with=F])) >= NoObsReq,])}
            
            ## Rekursiver Aufruf
            else {Calc_Industry_Ts(sp_composition_logic,sap_data,Date,DateSap,Sic,SicDigits-1,cor_options)}
            }
            
        else if(cor_options$Method == "three"){
            Spalten <- Boolean*sp_composition_logic[,Date,with=F]
            IndustryTs <- sapply(Date,function(i_Date){sap_data[[cor_options$EntityProperty]][[i_Date]][as.logical(Spalten[[i_Date]])]},simplify=F,USE.NAMES = TRUE) ##

            if(all(sapply(IndustryTs,length)>=NoCompReq)){          ## test whether there are too less observations per company or to less companies for the industry
                return(IndustryTs)}     ## returns only those companies with enough observations
            else {Calc_Industry_Ts(sp_composition_logic,sap_data,Date,DateSap,Sic,SicDigits-1,cor_options)}
            }

        }   
 
      


##7.3	Calc_MandA_Cor(): Liest Tabelle "SDC_Dataset.csv" ein und wendet Funktion Calc_Corr() 
#####   auf jedes Element bzw. jeden M&A an.
Calc_MandA_Cor<-function(sp_composition_logic = SPCompositionLogic,sap_data = SapData,cor_options = CorCalcOptions, sp_composition_dscd = SpCompositionDscd){
        ##  Aufruf von Calc_Index_Values() zur Berechnung der S&P 1500 Index Werte
        index_values<-Calc_Index_Values(sp_composition_dscd, cor_options)
        
        sdc_data <- fread("SDC_Dataset.csv",sep=";",colClasses=c("character", "numeric", "character", "character",
                                "character", "character", "character", "character", "character", "character", 
                                "character", "character", "character", "numeric", "numeric"))
                                
        sdc_data[,Date_Effective:=as.character(as.Date(strptime(Date_Effective,"%m.%d.%Y")))]

        SdcData<<-sdc_data
        
        N_Values <- nrow(sdc_data)
        
        ## Erstellung eines leeren Datensatz, in dem Korrelationen gespeichert werden sollen
        Correlations <- rep(NA,N_Values)
        
        ## for-Schleife startet Prüfung für jeden M&A (jede Zeile) im Datensatz 
        for(n_row in 1:N_Values){
     
            ## Entnahme relevanter M&A Daten    
            Date <- sdc_data$Date_Effective[n_row]
            SicAcquiror <- sdc_data$Ac_SIC[n_row]
            SicTarget <- sdc_data$Ta_SIC[n_row]
            
            ## Entfernung von M&As vor Untersuchungszeitraum
            if(as.Date(Date) < as.Date("1995-11-30")){Correlations[n_row] <- NA;next}
            
            ## Falls SICs der M&A Unternehmen identisch sind, ist Korrelation = 1
            if(SicAcquiror==SicTarget){Correlations[n_row] <- 1;next}
  
            ## Aufruf der Funktion für Korrelationsberechnung und Zuweisung des Ergebnisses
            Correlations[n_row] <- Calc_Corr(sp_composition_logic,sap_data,SicAcquiror,SicTarget,Date,cor_options,index_values)
            }
        Correlations <- as.numeric(Correlations)
        }
 
 

## Berechnungen für Auswertung

## Zunächst Berechnung mit Methode "one" und direkten Korrelation zwischen Industriesegmenten 
    MethOne_TotalCor<-Calc_MandA_Cor(SPCompositionLogic,SapData,CorCalcOptions)

## Als Zweites Berechnung mit Korrelation von Residuen einer Regression der Segmente auf den S&P 1500 
    CorCalcOptions$SpExessCor<-T
    MethOne_ResCor<-Calc_MandA_Cor(SPCompositionLogic,SapData,CorCalcOptions)    
    
## Berechnung mit Methode "two" und Verwendung von Korrelationen mit Residuen    
    CorCalcOptions$Method<-"two"
    MethTwo_ResCor<-Calc_MandA_Cor(SPCompositionLogic,SapData,CorCalcOptions)    
 
## Berechnung mit Methode "two" und direkten Korrelation zwischen Segmenten   
    CorCalcOptions$SpExessCor<-F
    MethTwo_TotalCor<-Calc_MandA_Cor(SPCompositionLogic,SapData,CorCalcOptions)


## Zusammenfassung der Date und Speicherung des Datensatzes    
    KorrTable <- cbind(SdcData[,c("Date_Effective","Ac_SIC","Ta_SIC"),with=F], MethOne_TotalCor , MethOne_ResCor, MethTwo_ResCor, MethTwo_TotalCor)

    write.table(KorrTable,"KorrTable.txt",sep=";",col.names=T,row.names=F))
  
