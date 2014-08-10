## In diesem R-Code wird aus den Dateien ICC_Dataset.txt, SDC_Dataset.csv 
## und COMPANY_Dataset.csv eine Tabelle MaATable erstellt, die für jeden M&A 
## die Impliziten Kapitalkosten vor und nach dem M&A berechnet. Mit dieser 
## Information kann später untersucht werden, ob sich die ICCs vor (prea ICC) 
## M&A von denen danach(post ICC) unterscheiden. Die Tabelle wird später 
## noch durch Berechnungen in dem R-Code Correlation_Computation.r ergänzt.

## Die ICCs werden nach Marktwerten und entsprechend den erworbenen
## Unternehmensanteilen gewichtet. Weil es keinen bestimmten Zeitpunkt vor 
## und nach M&A gibt, der sich zur Bestimmung einer ICC-Differenz  anbietet, 
## werden vor und nach M&A Zeiträume definiert. Innerhalb dieser Zeiträume 
## werden für eine bestimmte Spanne, rollierend durchschnittliche ICC post/prae 
## bestimmt. Aus den Differenzen der korrespondierenden post und prae ICCs werden 
## dann die ICC Differenzen bestimmt. (Korrespondierende ICCs wären post und prae ICCs,
## deren Berechnungsfenster sich in gleicher zeitlicher Distanz vor und nach M&A befindet.)



## Folgende Schritte werden durchlaufen:

## 1.   Einlesen der Datensätze und deren Modifizierung.

## 2.   Definition und Speicherung der für die Auswertung relevanten 
##      Spaltennamen und der Parameter für die ICC Berechnungsintervalle.

## 3.	Weitere Modifizierung der Datensätze durch Angaben aus Schritt 2.

## 4.   Erstellung der Funktionen, die auf die Datensätze zugreifen und die
##      die Daten verarbeiten, um für jeden M&A die ICCs zu bestimmen.

## 5.   Anwendung der Funktionen aus Schritt 4 zur Erstellung eines finalen
##      Datensatzes, der für jeden M&A die ICCs angibt.



## Folgenden Funktionen werden angewendet:

## 1.	Zum Einlesen aller drei Datensätze (ICC, SDC und COMPANY) 
##      und deren Bearbeitung. 
## 1.1  Read_Table(): Liest Datensätze ein und gibt Liste von Funktionen 
##      zurück. Über die Funktionen kann später auf die Tabellen zugegriffen 
##      werden.
## 1.2	Name_Comp_Table(): Passt die Benennung vom COMPANY-Datensatz an.

## 2.	Zur Erstellung eines Verzeichnisses, in dem für alle drei 
##      Tabellen die relevanten Spaltennamen festgehalten werden, aber
##      auch die Details für die Zeitfenster zur ICC Bestimmung definiert 
##      werden. Diese Funktionen werden ihrerseits durch die Funktion Data_Prop() 
##      in einer Liste gebündelt.
## 2.1 	Sdc_Set(): Speichert Spaltennamen zur SDC-Tabelle
## 2.2	Icc_Set(): Speichert Spaltennamen zur ICC-Tabelle
## 2.3	Company_Set(): Speichert Spaltennamen der Tabelle mit 
##      Unternehmensdaten (COMPANY-Datensatz)
## 2.4	Icc_Period_Set(): Bestimmt Lage des prae und post Zeitinervalle
##      zur ICC Bestimmung sowie die Größe des innerhalb dieses Zeitraumes 
##      rollierenden Berechnungsfensters.
## 2.5	Extend_Fun(): Wird in Icc_Period_Set() aufgerufen und passt die Länge 
##      des Datensatzes mit Untenehmensdaten an, falls durch die Größe des 
##      post Zeitfensters die Länge nicht ausreicht.
## 2.6 	Data_Prop(): Die Funktionen 2.1 bis 2.4 werden als Listenelemente in 
##      einer von der Funktion Data_Prop() erstellten Liste gebündelt.

## 3.	Weitere Funktionen zur Modifizierung der Datensätze
## 3.1	Dataset_Adjust(): Modifiziert die Datentypen in den Spalten des SDC 
##      und ICC Datensatzes.
## 3.2 	Company_Prop_List_Set(): Untersucht, an welchen Positionen im 
##      Datensatz für Unternehmensdaten (COMPANY-Datensatz) welche 
##      Unternehmensdaten MV, Sales etc. auftauchen und gibt eine 
##      Liste mit logischen Datensätzen für jeden Unternehmensdatentyp zurück.

## 4.	Funktionen zum Zugriff auf Datensätze sowie zur Bearbeitung der Daten
##      bis zur Berechnung der ICC. Ähnlich Data_Prop() werden die 
##      Funktionen in einer Liste (durch die Funktion Data_Retrieve()) 
##      gebündelt.
## 4.1	Sdc_Get(): Entnimmt Daten zu M&A aus SDC Datensatz und berechnet 
##      gleichzeitig durch Zugriff auf Icc_Period_Get() die Zeitfenster zur ICC 
##      Berechnung.
## 4.2	Icc_Period_Get(): Berechnet für ein übergebenes Datum die post 
##      und prae M&A Zeitintervalle. 
## 4.3	Icc_Get(): Entnimmt für die Zeitfenster und Datastream Codes der 
##      M&A Partner die ICC-Werte aus dem ICC Datensatz und berechnet die
##      rollierenden post und prae ICCs.
## 4.4	Company_Get(): Entnimmt für die Zeitfenster und Datastream Codes der 
##      M&A Partner die Marktwerte aus dem COMPANY-Datensatz. 
##      Mit diesen können später die gewichteten ICCs bestimmt werden. Auch
##      hier werden die MVs rollierend berechnet.
## 4.5	Data_Retrieve(): Erstellt Liste, über die auf die Funktionen 4.1, 
##      4.3, 4.4 zugegriffen werden kann.

## 5. 	Die Funktion Compute_MandA_Table() erstellt die finale Tabelle, die für jeden 
##      M&A die ICCs von Acquiror und Target vor bzw. für Target auch nach 
##      M&A angibt. Durch Aufruf von Funktionen 5.3 bis 5.4 (in Compute_MandA_Table())
##      werden die gewichteten ICCs aus den ICCs von Target und Aquiror bestimmt und die
##      Differenz zu den korrepondierenden ICCs nach M&A.
## 5.1	Compute_MandA_Table(): Ruft für jeden M&A Sdc_Get(),Icc_Get() und Company_Get() auf, 
##      berechnet aus den erhaltenen Informationen alle ICCs zum M&A und gibt 
##      die Daten in einer neuen Tabelle zurück.
## 5.2	Sic_Separation(): Wird als Teil von Compute_MandA_Table aufgerufen und fügt eine 
##      zusätzliche Spalte zur Differenzierung von M&A nach SIC-Unterschieden an.
## 5.3	Calc_Weighted_Icc_prae(): Berechnet aus MV und ICC des Targets und Acquirors sowie dem
##      am Target erworbenen Unternehmensanteil die gewichtete ICC vor M&A.
## 5.4	Calc_Icc_Diff(): Berechnet aus gewichteten ICCs und korrespondierenden post M&A ICCs des 
##      Acquirors die ICC Differenz, die durchc den M&A entstanden ist.     


library(zoo)
library(data.table)

setwd("C:/Users/Sebastian Stenzel/Desktop/R_Data/ICC_Data")
        
## 1.   Einlesen der Datensätze und deren Modifizierung.

##1.1   Read_Table(): Liest SDC, ICC und CompanyData Datensätze ein. 
#####   Erstellt Funktionsliste TableStored über deren Funktionen 
#####   Set und Get auf die Liste der Datensätze zugegriffen 
#####   werden kann.
Read_Table<-function(){


        SdcData <- fread("SDC_Dataset.csv",sep=";",colClasses=c("character", "numeric", "character", "character",
                                "character", "character", "character", "character", "character", "character", 
                                "character", "character", "character", "numeric", "numeric"))

        IccData<-fread("ICC_Dataset.txt")                    
               
        CompanyData <- fread("COMPANY_Dataset.csv",header=T,stringsAsFactors=F,sep=";")

        TableList <-list("SDC" = SdcData, "ICC" = IccData, "COMPANY" = CompanyData)
        
        ## Zugriffsfunktionen
        Get <- function()TableList
        Set <- function(NewTable)TableList <<- NewTable	
        
        ## Erstellt Liste
        TableStored <<- list(Set = Set, Get = Get)
        }

##	Aufruf von Read_Table() ->  Liste TableStored wird ohne Zuweisung erstellt
    Read_Table()    

##1.2   Name_Comp_Table(): Benennt Spalten des CompanyData Datensatzes nach Jahr und Monat 
Name_Comp_Table <- function(TableStored = TableStored, NChar = 5 , StartDate = "1978-01-01"){
		## 	Nchar = 5 bezieht sich auf Spalte fünf in der Tabelle CompanyData. 
        ##            Spaltennamen 1 bis 5 sollen nicht geändert werden 
        ##            – Unternehmenswerte beginnen erst ab Spalte 6
        
		CompanyTable <- TableStored$Get()$COMPANY
		StartDate <- as.POSIXlt( StartDate)
		number <- ncol(CompanyTable) - NChar
		temp <- rep(StartDate,number)
        
        ## Spaltennamen als Jahr plus Monat
		temp$mon <- temp$mon + 0:(number-1)
		
        ## Löschen der Wochentage
        temp <- substr(as.Date(temp),1,7)
        newNames <- c(names(CompanyTable)[1:NChar],temp)
        
        ## Benennung
        setnames(CompanyTable,names(CompanyTable),newNames)
        
        ## Überschreibung alter Datensätze
		Tables <- TableStored$Get()
		Tables$COMPANY <- CompanyTable
		TableStored$Set(Tables)
		}

##  Aufruf von Name_Comp_Table(), Zuweisung des modifizierten Datensatzes innerhalb der Funktion
    Name_Comp_Table(TableStored)


    
    
## 2.   Definition und Speicherung der für die Auswertung relevanten 
##      Spaltennamen und der Parameter für die ICC Berechnungsintervalle.
    
##2.1 	Sdc_Set(): Funktion, die Spaltennamen für SDC-Tabelle zentral 
#####   speichert. Die Funktion wird später nicht direkt aufgerufen, 
#####   sondern mit anderen "Set()"-Funktionen durch die Funktion 
#####   Data_Prop() in der Liste DataPropList gespeichert    
        
Sdc_Set <- function(DatEffCol = "SpDateEff",DatAnnCol ="SpDateAnn", AcCol = "SpAcDscd", TaCol = "SpTaDscd",
                            ShareCol = "SpShAcq", SicAcCol = "SpAcSic", 
                        SicTaCol = "SpTaSic"){    
        
        SDCCol <- list("DatEffCol" = DatEffCol, "DatAnnCol" = DatAnnCol, "AcCol" = AcCol, "TaCol" = TaCol, 
                       "ShareCol" = ShareCol, "SicAcCol" = SicAcCol, "SicTaCol" = SicTaCol)
        }    
        

##2.2	Icc_Set(): Funktion, die Spaltennamen für ICC-Tabelle zentral 
#####   speichert. Die Funktion wird später nicht direkt aufgerufen, 
#####   sondern mit anderen "Set()"-Funktionen durch die Funktion 
#####   Data_Prop() in der Liste DataPropList gespeichert
Icc_Set <- function(DatCol = "Datum", DscdCol = "Company_Code", 
                        IccCol = "ICC_CT"){
        
        ICCCol <- list("DatCol" = DatCol, "DscdCol" = DscdCol , "IccCol" = IccCol)
        }

##2.3	Company_Set(): Funktion, die Spaltennamen für CompanyData-Tabelle 
#####   zentral speichert. Die Funktion wird später nicht direkt aufgerufen, 
#####   sondern mit anderen "Set()"-Funktionen durch die Funktion Data_Prop() 
#####   in der Liste DataPropList gespeichert
Company_Set <- function(DscdCol = "DSCD", CharacCol = "Properties",   
                         SicCol = "WC07021",CharacList = NULL){	
        if(!is.null(CharacList)){                                      
            COMPCol  <- DataPropList$Get()$COMPANY
            COMPCol$CharacList <- CharacList}
        else {
            COMPCol <- list("DscdCol" = DscdCol, "CharacCol" = CharacCol, 
            "SicCol" = SicCol, "CharacList" = CharacList)}
            COMPCol 
        }


##2.4	Icc_Period_Set(): Funktion, durch die prae und post Zeitintervall um den 
#####   M&A definiert werden und für deren Zeiträume die ICCs entnommen werden. 
#####   Die Funktion wird später nicht direkt aufgerufen, sondern mit anderen 
#####   "Set()"-Funktionen durch die Funktion Data_Prop() in der Liste DataPropList 
#####   gespeichert. Außerdem wird innerhalb der Funktion die Funktion Extend_Fun() 
#####   aufgerufen, um die Länge des CompanyData Datensatzes anzupassen.
Icc_Period_Set <- function(Close = 1, Far = 63, Size = 12, 
				MinObs =6){	
        ##	Close: Wieviele Monate vor/nach M&A soll das prae/post ICC-Zeitintervall beginnen         
        ##  Far: Entfernung des vom M&A am weitesten entfernten Monats im Zeitintervall
        ##  Size: ICCs werden nicht als ein Mittelwert für den ganzen durch Close und 
        ##        Far definierten Zeitraum berechnet, sondern durch mehrere Mittelwerte, 
        ##        die rollierend innerhalb des Fensters bestimmt werden. Die Länge der 
        ##        rollierenden Fenster wird durch Size (in Monaten) festgelegt.  
        ##  MinObs: Minimal notwendige Anzahl an Beobachtungen für jedes rollierende 
        ##          Berechnungsfenster. Sind es weniger, wird NA zurückgegeben.
        
        ##  Aufruf von Funktion Extend_Fun(), um gegebenenfalls CompanyData Datensatz um neue
        ##  Spalten für Monate zu erweitern, falls durch Definition des Zeitfensters Daten
        ##  für Zeitpunkte abgefragt werden sollten, für die keine Spalten vorhanden sind.
        Extend_Fun(TableStored,ceiling(Far/12))
        
        IccPeriodData <- list(Close = Close, Far = Far, 
                        Size =  Size, MinObs = MinObs)
        }
    
##2.5	Extend_Fun(): wird in Icc_Period_Set() aufgerufen.  Wenn durch die Größe des 
#####   post ICC-Zeitintervalls nach Unternehmensdaten eines Datums gefragt wird, das nicht 
#####   im CompanyData Datensatz enthalten ist, schlägt die Abfrage fehl. Extend_Fun 
#####   fügt daher weitere Spalten mit NA-Werten für in der Zukunft liegende Zeitpunkte 
#####   an den CompanyData Datensatz an.  
Extend_Fun <- function(TableStored , years = 6){
		CompanyTable <- TableStored$Get()$COMPANY
		monthAdd<-12*(years+1)
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(NA,nrow(CompanyTable))
		time$mon <- time$mon+1:monthAdd
		time<-substr(as.Date(time),1,7)
		CompanyTable[,time] <- newRow
        
		Tables <- TableStored$Get()
		Tables$COMPANY <- CompanyTable
		TableStored$Set(Tables)
		}

##2.6 	Data_Prop(): Die Funktion 2.1 bis 2.4 werden als Listenelemente in 
#####   einer von der Funktion Data_Prop() erstellten Liste DataPropList gebündelt.

Data_Prop <- function(){

		PropList <- list(NULL)
		Get <- function()PropList

        ## Funktionen rufen vorherige Funktionen unter 2. auf
		setSdc <- function(...) if (length(list(...))==0){PropList$SDC <<- Sdc_Set()}
						else {PropList$SDC <<- Sdc_Set(...)}
		setIcc <- function(...) if (length(list(...))==0){PropList$ICC <<- Icc_Set()}
						else {PropList$ICC <<- Icc_Set(...)}
		setCompany <- function(...) if (length(list(...))==0){PropList$COMPANY <<- Company_Set()}
						else {PropList$COMPANY <<- Company_Set(...)}
        setCharacList <- function(CompanyProp){PropList$COMPANY <<- CompanyProp}                
		setIccPeriod <- function(...) if (length(list(...))==0){PropList$ICCPeriod <<- Icc_Period_Set()}
						else {PropList$ICCPeriod <<- Icc_Period_Set(...)}
        
        ## Funktion zum Setzen von Standardwerten
        setDefault <- function(){
                        PropList$SDC <<- Sdc_Set()
                        PropList$ICC <<- Icc_Set()
                        PropList$COMPANY <<- Company_Set()
                        PropList$ICCPeriod <<- Icc_Period_Set()
                        }
    
        ## Erstellt Liste
		DataPropList <<- list(Get = Get, setSdc = setSdc, setICC = setIcc, setCompany = setCompany,
					setIccPeriod = setIccPeriod, setDefault = setDefault)	
		}        

##  Aufruf von Data_Prop() ->  Liste DataPropList wird erstellt ohne Zuweisung        
##    Data_Prop()
Data_Prop()
##  Setzen von Standardwerten für DataPropList     
    DataPropList$setDefault()

    
    
    
## 3.	Weitere Modifizierung der Datensätze durch Information aus Schritt 2.
    
##3.1	Dataset_Adjust(): Modifiziert die Datentypen für SDC und ICC Datensatz 
#####   sowie die Spaltennamen des SDC Datensatz    

Dataset_Adjust <- function(TableStored,DataPropList){
		
		SdcTable <- TableStored$Get()$SDC
        IccTable <- TableStored$Get()$ICC
		ColNamesSdc <- c("SpDateEff","SpValueTrans","SpTaName","SpTaDscd","SpAcName",
		"SpAcDscd","SpAcSic","SpAcInd","SpTaSic","SpTaInd","SpDateAnn",
		"SpEqV","SpEpV","SpShAfterTra","SpShAcq")
        setnames(SdcTable,names(SdcTable),ColNamesSdc )

        
        SdcTable[,SpValueTrans:=as.numeric(SpValueTrans)]
		SdcTable[,SpEqV:=as.numeric(SpEqV)]        
		SdcTable[,SpEpV:=as.numeric(SpEpV)]

        SdcTable[,SpDateEff:= as.Date(strptime(SpDateEff,"%m.%d.%Y"))]
		SdcTable[,SpDateAnn:= as.Date(strptime(SpDateAnn,"%m.%d.%Y"))]
		SdcTable[,SpShAfterTra:= SpShAfterTra/100]
		SdcTable[,SpShAcq:= SpShAcq/100]
        
        IccDatCol <- DataPropList$Get()$ICC$DatCol
        IccTable[,{IccDatCol}:=substr(as.Date(strptime(IccTable[[IccDatCol]],"%Y-%m-%d")),1,7)]     
		
        ## Überschreibung alter Datensätze
        Tables <- TableStored$Get()
		Tables$SDC <- SdcTable
		TableStored$Set(Tables)
		}        
        
##  Aufruf von Dataset_Adjust()        
##    Dataset_Adjust(TableStored,DataPropList)     
Dataset_Adjust(TableStored,DataPropList)          
        
##3.2   Company_Prop_List_Set(): Untersucht, an welchen Positionen im 
#####   Datensatz für Unternehmensdaten welche Unternehmensdaten MV, Sales etc.
#####   auftauchen und gibt eine Liste mit logischen Datensätzen für jeden
#####   Unternehmensdatentyp zurück. Dieser wird an DataPropList angehängt.
#####   Die Aktion soll später bei den Berechnungen Zeit einsparen.
Company_Prop_List_Set <- function(TableStored = TableStored, DataPropList){
		
		CompanyTable <- TableStored$Get()$COMPANY 
		CompProp  <- DataPropList$Get()$COMPANY
		CharacCol <- CompProp$CharacCol
		CharacCol <- CompanyTable[[CharacCol]]          
		CharacCol <- as.factor(CharacCol)		

		Characs <- levels(CharacCol)
		CharacMat <- sapply(1:length(Characs),function(x){Characs[x] == CharacCol })
		colnames(CharacMat) <- Characs
		CharacMat <- as.data.table(CharacMat)	
        
        ## Anhängen an DataPropList
        DataPropList$setCompany(CharacList = CharacMat)
		}
              
##4.1	Sdc_Get(): Entnimmt Daten zu M&A aus SDC Datensatz und berechnet 
#####   gleichzeitig durch Zugriff auf AccessData$retrieveIccPeriod(Datum) 
#####   das Zeitintervall zur ICC Berechnung
        
Sdc_Get <- function(DataPropList, TableStored, SDCRow,access_data = AccessData){
        ##  Zunächst werden die Spaltennamen des SDC Datensatzes aus DataPropList
        ##  zwischengespeichert, ebenso wie der SDC Datensatz.
        ##  Dann wird durch den Funktionsaufruf access_data$retrieveIccPeriod
        ##  die Berechnung der prae und post Zeitintervalle ausgelöst,
        ##  zum Funktionsende die M&A Daten eingeholt und in einer Liste
        ##  zurückgegeben.
        
        ##  SDCRow: Welche Zeile im SDC Datensatz ausgewertet wird
        ##          im Prinzip der M&A, der ausgewertet wird.
        ##  AccessData: Liste, in der Funktionen zur Datenauswertung zusammen-
        ##              gefasst sind - wird später genauer erläutert.   
        
        SdcProp  <- DataPropList$Get()$SDC   ## Aufruf der Spaltenbenennung des SDC Datensatzes
        SdcTable <- TableStored$Get()$SDC   ## Aufruf des SDC Datensatze
        
        ## Zwischenspeicherung der SDC Spaltennamen
        ##DatCol 	<- SdcProp$DatCol
        DatEffectiveCol <- SdcProp$DatEffCol    
        DatAnnouncedCol <- SdcProp$DatAnnCol
        AcquirorCol <- SdcProp$AcCol
        TargetCol   <- SdcProp$TaCol
        ShareAcCol  <- SdcProp$ShareCol
        SicAcqCol	<- SdcProp$SicAcCol
        SicTarCol   <- SdcProp$SicTaCol
        
        ##Datum     <- SdcTable[[DatCol]][SDCRow]
        DatEffective  <- SdcTable[[DatEffectiveCol]][SDCRow]
        DatAnnounced  <- SdcTable[[DatAnnouncedCol]][SDCRow]
        ##access_data$retrieveIccPeriod(Datum) 
        access_data$retrieveIccPeriod(DateAnnounced = DatAnnounced, DateEffective = DatEffective) 
        ## access_data: wird später erklärt. Hier wird eine Funktion aufgerufen,
        ##              die für das M&A Datum die post/prae Zeitintervalle berechnet 
        ##              und speichert.    
        
        
        ## Entnahme der M&A Informationen
        AcquirorDscd <- SdcTable[[AcquirorCol]][SDCRow]   
        TargetDscd   <- SdcTable[[TargetCol]][SDCRow]     
        ShareAc      <- SdcTable[[ShareAcCol]][SDCRow]      
        SicAc		 <- SdcTable[[SicAcqCol]][SDCRow]     
        SicTa		 <- SdcTable[[SicTarCol]][SDCRow] 	    
        
        ## Rückgabe der M&A Informationen
        list( DatEffective = DatEffective, DatAnnounced = DatAnnounced , AcquirorDscd = AcquirorDscd, 
            TargetDscd = TargetDscd, ShareAc = ShareAc, SicAc = SicAc, SicTa = SicTa)
        }        

##4.2	Icc_Period_Get(): Berechnet für ein übergebenes Datum die post und prae
#####   M&A Zeitintervalle

##Icc_Period_Get <- function(Datum){
Icc_Period_Get <- function(DateAnnounced,DateEffective){
	
        icc_period_data <- DataPropList$Get()$ICCPeriod ## Aufruf der zuvor definierten Parameter für Zeifenster
        
        IccPraeDat<-as.POSIXlt(DateAnnounced)
        IccPostDat<-as.POSIXlt(DateEffective)
        ##Datum <- as.POSIXlt(Datum) 
        ##Datum$mday <- 15
        ##DatPost <- DatPrae <- rep(Datum,icc_period_data$Far - icc_period_data$Close+1)
        IccPraeDat$mday <- IccPraeDat$mday - 7    
        IccPraeDat$mon <- IccPraeDat$mon - 2   
        IccPostDat$year <- IccPostDat$year + 1 
        IccPostDat$mon <- 5   
        
        IccPraeDat <- substring(IccPraeDat,1,7)
        IccPostDat <- substring(IccPostDat,1,7)    
        MinObs <-"hier"
        Size  <- "hier"
        ##DatPrae$mon <- DatPrae$mon-(icc_period_data$Far:icc_period_data$Close)    ##  Zuweisung des prae Zeitintervalls
        ##DatPost$mon <- DatPost$mon+(icc_period_data$Close:icc_period_data$Far)    ##  Zuweisung des post Zeitintervalls
        ##DatPrae <- substr(as.Date(DatPrae),1,7) ##  Entfernung der Wochentage (für Auswertung sind nur Monate von Bedeutung)
        ##DatPost <- substr(as.Date(DatPost),1,7) ##  Entfernung der Wochentage (für Auswertung sind nur Monate von Bedeutung)
        ##MinObs <- icc_period_data$MinObs
        ##Size <- icc_period_data$Size	## Größe der rollierenden Berechnungsfenster

        list(DatPrae = IccPraeDat, DatPost = IccPostDat, MinObs = MinObs, Size = Size)
        }        


##4.3	Icc_Get(): entnimmt für die Zeitfenster und Datastream Codes der M&A Partner 
#####   die ICC-Werte aus dem ICC Datensatz und berechnet innerhalb der beiden Zeitintervalle
#####   (post/prae M&A) rollierend Mittelwerte unter der Bedingung, dass die geforderte Anzahl
#####   an Mindestbeobachtungen erfüllt ist (-->DataPropList$Get()$ICCPeriod$MinObs<--)


Icc_Get<- function(DataPropList, TableStored, access_data = AccessData, Sample=FALSE){
        ## Nach Aufruf von Spaltenbenennung und ICC Datensatz
        ## werden zunächst prae ICCs für Target- und Acquiror-Unternehmen
        ## sowie post ICCs für Acquiror-Unternehmen aus ICC Datensatz
        ## entnommen und diese in dem data.frame ICC zwischengespeichert.
        ## Danach werden durch rollapply die rollierenden Mittelwerte für alle
        ## Berechnungsfenster im Zeitintervall berechnet.
        
        IccProp <- DataPropList$Get()$ICC	## Aufruf der Spaltenbenennung des ICC Datensatzes
        
        if(Sample){IccTable <- TableStored$Get()$ICCsample} #
        else{IccTable <- TableStored$Get()$ICC}   
        ##  Legt fest, ob gesamter ICC Datensatz durchsucht werden soll oder ein vorher erstellter
        ##  reduzierter Datensatz, der alle notwendigen Daten enthält. Zur Einsparung von Rechenzeit. 
        
        SdcData <- access_data$Get()$SDC        ##  Aufruf der von SDC_get() entnommenen M&A Informationen
        PeriodData <- access_data$Get()$ICCPeriod  ##  Aufruf der von Icc_Period_Get() erstellten Zeitfenster
        DatPrae <- PeriodData$DatPrae
        DatPost <- PeriodData$DatPost
        
        ## Zwischenspeicherung der ICC Spaltennamen
        DatCol  <- IccProp$DatCol
        DscdCol <- IccProp$DscdCol
        IccCol  <- IccProp$IccCol
        
        ## Zwischenspeicherung der Zeitfenster Daten
        ##DatPrae <- sort(PeriodData$DatPrae)
        ##DatPost <- sort(PeriodData$DatPost)
        ##MinObs  <- PeriodData$MinObs
        ##Size    <- PeriodData$Size
        
        ## Aufruf des ICC Datensatzes
        IccTable  <- IccTable[,c(DatCol,DscdCol,IccCol),with=F]             ##
        
        ## data.frame zur Speicherung der ICCs
        ##ICC <- rep(NA,length(DatPrae))
        ICC <- rep(NA,2)
        
        ## Am Ende soll ein Dataframe mit ICC Zeitreihen für das Target vor, den
        ## Acquiror vor und nach M&A im ICC data.frame gespeichert werden.
        ## Der data.frame "ICC" ist die leere Vorlage.
        ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

            temp  <- IccTable[[DscdCol]] == as.character(SdcData$TargetDscd) ############
            ## Filterung der Target-Datastream Codeeinträge im ICC Datensatz
            
            tempTA  <- IccTable[temp, c(DatCol,IccCol),with=F]          
            ## Neuzuweisung des ICC Datensatzes mit ausschließlich Informationen zum Target 
            
            ##tempDat <- is.element(tempTA[[DatCol]],DatPrae)
            tempDat <- tempTA[[DatCol]]==DatPrae
            ## Suche nach Einträgen zum prae Zeitfenster im Target ICC Datensatz
           
            ##IccExist <- is.element(DatPrae,tempTA[[DatCol]][tempDat])       
            ## Um zu verhindern, dass fehlende Werte (nicht NA, sondern gar 
            ## kein Eintrag zu dem Datum) im Untersuchungszeitraum nicht
            ## verschwinden, müssen auch deren Positionen ermittelt werden und 
            ## beim Eintrag in die ICC Tabelle berücksichtigt werden
            if(length(tempDat)==0|sum(tempDat)==0){ICC[1:2,"TaIcc"] <-NA}
            else{        
                ICC[1,"TaIcc"] <- tempTA[tempDat, DatCol,with=F]
                ICC[2,"TaIcc"] <- tempTA[tempDat, IccCol,with=F]
                }
            ##ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol,with=F]         
            ## Zuweisung der ICCs zum leeren data.frame "ICC"
            
            ## Analoge Berechnung für ICC des Acquirors
            temp  <- IccTable[[DscdCol]] == SdcData$AcquirorDscd            
            tempAC  <- IccTable[temp, c(DatCol,IccCol),with=F]              
            ##tempDat <- is.element(tempAC[[DatCol]],DatPrae)                 
            ##IccExist <- is.element(DatPrae,tempAC[[DatCol]][tempDat])
            tempDat <- tempAC[[DatCol]]==DatPrae
            ##ICC[IccExist,"AcIccPrae"] <- tempAC[tempDat, IccCol,with=F]        
            
            if(length(tempDat)==0|sum(tempDat)==0){ICC[1:2,"AcIccPrae"] <-NA}
            else{    
                ICC[1,"AcIccPrae"] <- tempAC[tempDat, DatCol,with=F]
                ICC[2,"AcIccPrae"] <- tempAC[tempDat, IccCol,with=F]
                }
            ##tempDat <- is.element(tempAC[[DatCol]],DatPost)     
            ##IccExist <- is.element(DatPost,tempAC[[DatCol]][tempDat])  
            tempDat <- is.element(tempAC[[DatCol]],DatPost)  
            ##ICC[IccExist,"AcIccPost"] <- tempAC[tempDat, IccCol,with=F]
            if(length(tempDat)==0|sum(tempDat)==0){ICC[1:2,"AcIccPost"] <-NA}
            else{ 
                ICC[1,"AcIccPost"] <- tempAC[tempDat, DatCol,with=F]
                ICC[2,"AcIccPost"] <- tempAC[tempDat, IccCol,with=F]            
                }
            ## Im folgenden wird der data.frame "ICC" in ein Objekt vom Typ zoo umgewandelt.
            ## Darin werden für jede Spalte (TaIcc, AcIccPrae, AcIccPost) rollierend Mittelwerte
            ## bestimmt. Sollten pro Berechnungszeitraum weniger Beobachtungen vorhanden sein
            ## als durch "MinObs" definiert, findet keine Berechnung statt, sondern es wird ein
            ## NA zurückgegeben.
		##CalcMean<-function(x){
		##	if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
		##	else NA}
	
		##ICC <- zoo(ICC)
		##ICC <- rollapply(ICC,FUN = CalcMean,width = Size)
		ICC	
		}        
        
        
##4.4	Company_Get: Entnimmt für die Zeitfenster und Datastream Codes der M&A 
#####   Partner die Marktwerte aus dem Datensatz mit Unternehmensdaten und 
#####   berechnet innerhalb der beiden Zeitfenster (post/prae M&A) rollierend 
#####   Mittelwerte unter der Bedingung, dass die geforderte Anzahl Mindest-
#####   beobachtungen erfüllt ist. Mit MV-Mittelwerten können später die gewichteten  
#####   ICCs bestimmt werden.
Company_Get <- function(DataPropList, TableStored, access_data = AccessData){
	## Im Prinzip zu ICC_Get() analoge Bestimmung von Marktwerten
		
        CompProp <- DataPropList$Get()$COMPANY
		CompanyTable <- TableStored$Get()$COMPANY		
		SdcData <-  access_data$Get()$SDC	
        PeriodData <- access_data$Get()$ICCPeriod
    
		CharacCol <- CompProp$CharacList
		CharacCol <- CharacCol$MV
		DscdCol <- CompProp$DscdCol
        DatPrae <- PeriodData$DatPrae
		DatPost <- PeriodData$DatPost
		##DatPrae <- sort(PeriodData$DatPrae)
		##DatPost <- sort(PeriodData$DatPost)
		##MinObs  <- PeriodData$MinObs
		##Size 	<- PeriodData$Size
        
        ## Die Entnahme und Berechnung der MV-Mittelwerte sind ähnlich der der ICC.
        ## Es entfällt aber die Prüfung auf nicht eingetragene Daten, weil im
        ## CompanyData Datensatz jedes Datum mit mindestens einem NA-Wert enthalten
        ## ist. Außerdem sind die Daten selbst Spaltennamen und können ohne Überprüfung
        ## mit "is.element" direkt ausgewählt werden.

		CompanyTablePrae <- CompanyTable[CharacCol ,c(DscdCol, DatPrae),with=F]
		CompanyTablePost <- CompanyTable[CharacCol ,c(DscdCol, DatPost),with=F]
		
		##Mv <- rep(NA,length(DatPrae))
        Mv <- rep(NA,2)
		Mv <- data.frame("TaMv" = Mv, "AcMvPrae" = Mv,"AcMvPost" = Mv)
  
		temp <- CompanyTablePrae[[DscdCol]] == SdcData$TargetDscd               
        ##Mv$TaMv <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])
        Mv[1,"TaMv"] <- DatPrae	
    
        Mv[2,"TaMv"] <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])	    
		## hier direkte Auswahl der Daten über Spaltennamen ohne is.element oder
        ## Überprüfung auf nicht verzeichnete Daten

		temp <- CompanyTablePrae[[DscdCol]] == SdcData$AcquirorDscd             
		##Mv$AcMvPrae <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])	  
        Mv[1,"AcMvPrae"] <- DatPrae
        Mv[2,"AcMvPrae"] <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])    
    
        ##Mv$AcMvPost <- as.numeric(CompanyTablePost[,DatPost,with=F][temp])           
		Mv[1,"AcMvPost"] <- DatPost
        Mv[2,"AcMvPost"] <- as.numeric(CompanyTablePost[,DatPost,with=F][temp]) 
        
        ## Berechnung rollierender Mittelwerte analog zu ICC_Get()
		##CalcMean<-function(x){
		##	if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
		##	else NA}
	
		##Mv <- zoo(Mv)
		##Mv <- rollapply(Mv,FUN = CalcMean,width = Size)
		Mv
		}
     


##4.5	Data_Retrieve(): Auf Sdc_Get(), Icc_Get() und Company_Get() wird
#####   nicht direkt zugegriffen, sondern durch Data_Retrieve() wird eine
#####   Liste der Funktionen erstellt, über die auf die Funktionen 
#####   4.1, 4.3,  4.4 zugegriffen werden kann. Die Liste speichert außerdem
#####   die jeweils zum M&A entnommenen und berechneten Daten in MaaData.
        
Data_Retrieve <- function(){

        MaaData <- list(NULL)
        retrieveSdc <- function(...){MaaData$SDC <<- Sdc_Get(...)}
        retrieveIcc <- function(...){MaaData$ICC <<- Icc_Get(...)}
        retrieveCompany <- function(...){MaaData$COMPANY <<- Company_Get(...)}
        retrieveIccPeriod <- function(...){MaaData$ICCPeriod <<- Icc_Period_Get(...)}
        Get <- function()MaaData
        
        list(retrieveSdc = retrieveSdc, retrieveIcc = retrieveIcc, retrieveCompany = retrieveCompany, 
            retrieveIccPeriod = retrieveIccPeriod, Get = Get)
        }        

        


## 5.   Anwendung der Funktionen aus Schritt 4 zur Erstellung eines finalen
##      Datensatzes, der für jeden M&A die ICCs angibt.        

##5.1	Compute_MandA_Table(): Ruft für jeden M&A Sdc_Get(),Icc_Get() und Company_Get() auf 
#####   (bzw. indirekt über eine von Data_Retrieve() erstellte Liste). 
#####   Berechnet aus den erhaltenen Informationen alle ICCs zum M&A und 
#####   gibt die Daten in einer neuen Tabelle zurück.
            
            
Compute_MandA_Table <- function(DataPropList,TableStored){
    ## Zuerst wird ein kleinerer ICC Datensatz erstellt, um die Berechnungen zu
    ## beschleunigen. Dann wird eine leere Tabelle aufgebaut, in der später die
    ## M&A Daten gespeichert werden.
    ## In der for-Schleife wird jeder M&A im SDC Datensatz durchlaufen, durch
    ## die retrieve SDC/ICC/Company Aufrufe die M&A Daten und ICCs entnommen und danach 
    ## im Datensatz gespeichert. Außerdem wird eine Spalte zur Klassifizierung des M&As
    ## nach SIC Unterschieden angehängt (bis zu welcher SIC-Ziffer sich die M&A
    ## Partner ähneln bzw. unterscheiden).
        ## Auswahl der Datensätze
        SdcTable <- TableStored$Get()$SDC
        IccTable <- TableStored$Get()$ICC
        
        laenge <- nrow(SdcTable)
        
        ## Reduzierung des ICC Datensatzes auf relevante Spalten 
        IccTable <- IccTable[,as.character(DataPropList$Get()$ICC),with=F]
        
            ## In den folgenden zehn Zeilen wird der ICC Datensatz auf einen
            ## neuen verkleinert, in dem nur diejenigen Zeilen noch vorhanden sind,
            ## in denen Daten für Unternehmen gespeichert sind, die auch im SDC Datensatz
            ## aufgeführt sind.
            AcquirorDscds <- DataPropList$Get()$SDC$AcCol
            TargetDscds <- DataPropList$Get()$SDC$TaCol
            IccDscdCol <- DataPropList$Get()$ICC$DscdCol
            
            ## Sammeln aller Datastream Codes aus SDC Datensatz
            DscdsList <- c( SdcTable[[AcquirorDscds]][1:laenge], SdcTable[[TargetDscds]][1:laenge])
            IccSampleRows <- is.element(IccTable[[IccDscdCol]],DscdsList)

            IccTable <- IccTable[IccSampleRows,]
            Tables <- TableStored$Get()    
            Tables$ICCsample <- IccTable
            TableStored$Set(Tables)
        
        ## Erstellung eines leeren Datensatze, in dem später die M&A Daten gespeichert werden
        ##icc_period_data <- DataPropList$Get()$ICCPeriod
        ##nobs <- ((icc_period_data$Far - icc_period_data$Close+1)-icc_period_data$Size+1)
        nobs <- 2
        maa_table <- as.data.frame(matrix(rep(NA,(8+6*nobs)*laenge),nrow=laenge,ncol=8+6*nobs))
        
        ## Aufruf von Funktion 3.2
        Company_Prop_List_Set(TableStored, DataPropList) 
        
        ## das Rückgabeobjekt von Data_Retrieve() muss vor Verwendung bereits existieren
        AccessData <<- Data_Retrieve ()
        
        
            ## Start der tatsächlichen Datenentnahme und der Berechnung     
            for (i_row in 1:laenge){ ## i_row für jede Zeile des SDC Datensatzes
                AccessData$retrieveSdc(DataPropList, TableStored, i_row,AccessData) ## Anwendung von Sdc_Get()
                AccessData$retrieveIcc(DataPropList, TableStored, AccessData, Sample=TRUE) ## Anwendung von Icc_Get()
                AccessData$retrieveCompany(DataPropList, TableStored, AccessData) ## Anwendung von Company_Get()

                ## Zuweisung der Werte zu der M&A Tabelle    
                maa_table[i_row,] <- c(as.character(AccessData$Get()$SDC$DatEffective),as.character(AccessData$Get()$SDC$DatAnnounced), 
                    AccessData$Get()$SDC$AcquirorDscd, AccessData$Get()$SDC$TargetDscd, 
                    AccessData$Get()$SDC$ShareAc, AccessData$Get()$SDC$SicAc , AccessData$Get()$SDC$SicTa , NA,
                    AccessData$Get()$ICC$TaIcc, AccessData$Get()$ICC$AcIccPrae, AccessData$Get()$ICC$AcIccPost,
                    AccessData$Get()$COMPANY$TaMv, AccessData$Get()$COMPANY$AcMvPrae, AccessData$Get()$COMPANY$AcMvPost)	
            }            
     ##DatEffective = DatEffective, DatAnnounced = DatAnnounced 
    
        ## Benennung der Spalten der finalen Tabelle
        ##IccColNames <- c(paste("TaIcc",-(nobs:1),sep="_"),paste("AcIccPrae",-(nobs:1),sep="_"),
        ##                paste("AcIccPost",(1:nobs),sep="_+"))
          IccColNames <- c("IccTaDate","TaIcc","IccAcPraeDate","AcIccPrae","IccAcPostDate","AcIccPost")

        ##MvColNames  <- c(paste("TaMv",-(nobs:1),sep="_"),paste("AcMvPrae",-(nobs:1),sep="_"),
        ##                paste("AcMvPost",(1:nobs),sep="_"))
        MvColNames  <- c("MvTaDate","TaMv","MvAcPraeDate","AcMvPrae","MvAcPostDate","AcMvPost")

        names(maa_table)<-c("DateEffective","DateAnnounced","Acquiror_Dscd","Target_Dscd","Perc_Shares_Acquired","Acquiror_Sic","Target_Sic",
                   "SicSep",IccColNames,MvColNames)
        
        ## Änderung der Datentypen der Tabelle auf numeric
        num_col<-c("Perc_Shares_Acquired","TaIcc","AcIccPrae","AcIccPost","TaMv","AcMvPrae","AcMvPost") 
        maa_table[,num_col]<-apply(maa_table[,num_col],2,as.numeric)
        
        ## Zuweisung neuer Spalte, die angibt, an welcher Sic Ziffer sich die Targer und Acquiror Sic unterscheiden
        maa_table$SicSep <- Sic_Separation(maa_table)
        maa_table <- Calc_Weighted_Icc_prae(maa_table=maa_table)
        maa_table <- Calc_Icc_Diff (maa_table=maa_table)
        maa_table <- Calc_Adj_Icc_Diff(maa_table=maa_table)
        maa_table
        }
 
##5.2	Sic_Separation(): Wertet aus, bis zu welcher Position die Sic des Targets dem 
#####   Acquiror gleich ist oder ob beide Sics identisch sind
Sic_Separation <- function(SummarySdc, AcSicCol = "Acquiror_Sic", TaSicCol = "Target_Sic")
			{
            ## Überprüfung, bis zu welcher Stelle Acquiror bzw. Target Sic gleich bzw. identisch sind
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
			
            ## Zuweisung entsprechender Bezeichnung
			SicCol <- rep(NA,nrow(SummarySdc))
			SicCol[Sic0] <- "sameSic" 
			SicCol[Sic1] <- "firstDigit"
			SicCol[Sic2] <- "secDigit"
			SicCol[Sic3] <- "thirdDigit"
			SicCol[Sic4] <- "fourthDigit"
			SicCol <- factor(SicCol,order=T,levels=c("sameSic","fourthDigit","thirdDigit","secDigit","firstDigit"))
			SicCol
			}
            

##5.3	Calc_Weighted_Icc_prae(): Berechnet aus MV und ICC des Targets und Acquirors sowie dem
#####   am Target erworbenen Unternehmensanteil die gewichtete ICC vor M&A.
Calc_Weighted_Icc_prae <- function(maa_table = MaATable,acquiror_icc_prae = "AcIccPrae",target_icc_prae = "TaIcc",
                            acquiror_mv_prae="AcMvPrae",target_mv_prae = "TaMv",share_acquired = "Perc_Shares_Acquired"){
            ## Bestimmung, wie viele Berechnungen im Interval vor M&A durchgeführt wurden 
            ##n_icc_calc <- length(grep( acquiror_icc_prae,colnames(maa_table)))
           ## n_row <- nrow(maa_table)
            
            ## Erstellung vom leeren Datensatzes als Vorlage
            ##weighted_icc <- data.frame("WeightedIccPrae_-9"= rep(NA,n_row))
            
            ## Erstellung von Namen der neuen Spalten
            ##weighted_icc_col_names <- paste("WeightedIccPrae",n_icc_calc:1,sep="_-")
            ##weighted_icc[,weighted_icc_col_names] <- rep(NA,n_row)
            ##weighted_icc<-weighted_icc[,-1]
            
            ## Zwischenspeicherung von Spalte, die Information zu erworbenen
            ## Unternehmensanteil beinhaltet
            share <- maa_table[,share_acquired] 
            
            ## Index i ist Zahl, die an die Namen "AcIccPrae" und "TaIcc" angehängt wird, um
            ## so die richtigen Target- und Acquiror- ICC bzw. MV Spalten vom MaATable zu selektieren. 
            ##for(i in 1:n_icc_calc){
            
                   ## Auswahl der Spalten   
                   share <- maa_table[,share_acquired] 
                   acquiror_icc_col <- maa_table[acquiror_icc_prae]
                   acquiror_mv_col  <- maa_table[acquiror_mv_prae]
                   target_icc_col   <- maa_table[target_icc_prae]
                   target_mv_col    <- maa_table[target_mv_prae]
                           
                   ## Berechnung der gewichteten ICC 
                   weighted_icc_col <- ((acquiror_icc_col*acquiror_mv_col  +  target_icc_col*target_mv_col*share)
                                                 /       (acquiror_mv_col  +  target_mv_col*share))
                   
                   names(weighted_icc_col)<-"WeightedIccPrae"
            ## Anhängen der neuen Spalten an MaATable        
            ##maa_table <- cbind(maa_table,as.data.table(weighted_icc))
            maa_table <- cbind(maa_table,weighted_icc_col)
            maa_table
            }

##5.4	Calc_Icc_Diff(): Berechnet aus gewichteten ICCs und korrespondierenden post M&A ICC des 
#####   Acquirors die ICC Differenz, die durchc den M&A entstanden ist.            
Calc_Icc_Diff <- function(maa_table = MaATable, weighted_icc_prae ="WeightedIccPrae", acquiror_icc_prae ="AcIccPost"){
            ## Bestimmung, wieviele Berechnungen im Intervall vor M&A durchgeführt wurden
            ##n_icc_calc <- length(grep(weighted_icc_prae,colnames(maa_table)))
            ##n_row <- nrow(maa_table)
              
            ## Erstellung eines leeren Datensatzes als Vorlage        
            ##diff_icc <- data.frame("IccDifference_1"= rep(NA,n_row))
            ##diff_icc_col_names <- paste("IccDifference",1:n_icc_calc,sep="_")
            ##diff_icc[,diff_icc_col_names] <- rep(NA,n_row)
            
            ## Analog zu Calc_Weighted_Icc_prae() werden durch Index i die korrespondierenden 
            ## WeightedICC und AcIccPost ausgewählt und aus ihnen die ICC Differenz der M&As
            ## bestimmt    
            ## Index i ist eine Zahl, die an die Namen "AcIccPrae" und "TaIcc" angehängt wird, um
            ## so die richtigen Target- und Acquiror- ICC bzw. MV Spalten vom MaATable zu selektieren.
  
                   
                   ## Auswahl der Spalten    
                   weighted_icc_col <- maa_table[weighted_icc_prae]
                   acquiror_icc_col <- maa_table[acquiror_icc_prae]
                   
                   ## Berechnung der Differenz
                   diff_icc <- acquiror_icc_col - weighted_icc_col
                   names( diff_icc)<-"IccDifference"
            ## Anhängen der neuen Spalten an MaATable          
            maa_table <- cbind(maa_table,diff_icc)
            maa_table
            }


Calc_Adj_Icc_Diff <- function(maa_table = MaATable, icc_diff ="IccDifference", acquiror_mv_prae="AcMvPrae",
                                target_mv_prae = "TaMv",share_acquired = "Perc_Shares_Acquired"){

  
                   
                   ## Auswahl der Spalten    
                   icc_diff_col <- maa_table[icc_diff]
                   target_mv_prae_col <- maa_table[target_mv_prae]
                   acquiror_mv_prae_col <- maa_table[acquiror_mv_prae]
                   share_acquired_col <- maa_table[share_acquired]
                   
                   ## Berechnung adjustierten Differenz
                   adj_diff_icc <- (icc_diff_col * acquiror_mv_prae_col) / (target_mv_prae_col * share_acquired_col)
                   names( adj_diff_icc)<-"IccAdjDifference"
            ## Anhängen der neuen Spalten an MaATable          
            maa_table <- cbind(maa_table,adj_diff_icc)
            maa_table
            }            
            
            
## Start der Berechnung und Speicherung des Datensatzes           
   
## ICC_CT
    MaATable_CT<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_CT,"MaATable_CT.txt",sep=";",col.names=T,row.names=F)
    
## ICC_OJ
    DataPropList$setICC(IccCol="ICC_OJ")
    MaATable_OJ<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_OJ,"MaATable_OJ.txt",sep=";",col.names=T,row.names=F)

## ICC_MPEG
    DataPropList$setICC(IccCol="ICC_MPEG")
    MaATable_MPEG<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_MPEG,"MaATable_MPEG.txt",sep=";",col.names=T,row.names=F)

## ICC_GLS  
    DataPropList$setICC(IccCol="ICC_GLS")
    MaATable_GLS<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_GLS,"MaATable_GLS.txt",sep=";",col.names=T,row.names=F)
    
    
## RP_CT
    DataPropList$setICC(IccCol="RP_CT")
    MaATable_RP_CT<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_RP_CT,"MaATable_RP_CT.txt",sep=";",col.names=T,row.names=F)
    
## RP_OJ
    DataPropList$setICC(IccCol="RP_OJ")
    MaATable_RP_OJ<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_RP_OJ,"MaATable_RP_OJ.txt",sep=";",col.names=T,row.names=F)

## RP_MPEG
    DataPropList$setICC(IccCol="RP_MPEG")
    MaATable_RP_MPEG<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_RP_MPEG,"MaATable_RP_MPEG.txt",sep=";",col.names=T,row.names=F)

## RP_GLS  
    DataPropList$setICC(IccCol="RP_GLS")
    MaATable_RP_GLS<-Compute_MandA_Table(DataPropList,TableStored)    

    write.table(MaATable_RP_GLS,"MaATable_RP_GLS.txt",sep=";",col.names=T,row.names=F)
    
###########
###########
###########

library(zoo)
library(data.table)

setwd("C:/Users/Sebastian Stenzel/Desktop/R_Data/ICC_Data")
        
## 1.   Einlesen der Datensätze und deren Modifizierung.

##1.1   Read_Table(): Liest SDC, ICC und CompanyData Datensätze ein. 
#####   Erstellt Funktionsliste TableStored über deren Funktionen 
#####   Set und Get auf die Liste der Datensätze zugegriffen 
#####   werden kann.
Read_Table<-function(){


        SdcData <- fread("SDC_Dataset.csv",sep=";",colClasses=c("character", "numeric", "character", "character",
                                "character", "character", "character", "character", "character", "character", 
                                "character", "character", "character", "numeric", "numeric"))

        IccData<-fread("ICC_Dataset.txt")                    
               
        CompanyData <- fread("COMPANY_Dataset.csv",header=T,stringsAsFactors=F,sep=";")

        TableList <-list("SDC" = SdcData, "ICC" = IccData, "COMPANY" = CompanyData)
        
        ## Zugriffsfunktionen
        Get <- function()TableList
        Set <- function(NewTable)TableList <<- NewTable	
        
        ## Erstellt Liste
        TableStored <<- list(Set = Set, Get = Get)
        }

##	Aufruf von Read_Table() ->  Liste TableStored wird ohne Zuweisung erstellt
    Read_Table()    

##1.2   Name_Comp_Table(): Benennt Spalten des CompanyData Datensatzes nach Jahr und Monat 
Name_Comp_Table <- function(TableStored = TableStored, NChar = 5 , StartDate = "1978-01-01"){
		## 	Nchar = 5 bezieht sich auf Spalte fünf in der Tabelle CompanyData. 
        ##            Spaltennamen 1 bis 5 sollen nicht geändert werden 
        ##            – Unternehmenswerte beginnen erst ab Spalte 6
        
		CompanyTable <- TableStored$Get()$COMPANY
		StartDate <- as.POSIXlt( StartDate)
		number <- ncol(CompanyTable) - NChar
		temp <- rep(StartDate,number)
        
        ## Spaltennamen als Jahr plus Monat
		temp$mon <- temp$mon + 0:(number-1)
		
        ## Löschen der Wochentage
        temp <- substr(as.Date(temp),1,7)
        newNames <- c(names(CompanyTable)[1:NChar],temp)
        
        ## Benennung
        setnames(CompanyTable,names(CompanyTable),newNames)
        
        ## Überschreibung alter Datensätze
		Tables <- TableStored$Get()
		Tables$COMPANY <- CompanyTable
		TableStored$Set(Tables)
		}

##  Aufruf von Name_Comp_Table(), Zuweisung des modifizierten Datensatzes innerhalb der Funktion
    Name_Comp_Table(TableStored)


    
    
## 2.   Definition und Speicherung der für die Auswertung relevanten 
##      Spaltennamen und der Parameter für die ICC Berechnungsintervalle.
    
##2.1 	Sdc_Set(): Funktion, die Spaltennamen für SDC-Tabelle zentral 
#####   speichert. Die Funktion wird später nicht direkt aufgerufen, 
#####   sondern mit anderen "Set()"-Funktionen durch die Funktion 
#####   Data_Prop() in der Liste DataPropList gespeichert    
Sdc_Set <- function(DatCol = "SpDate", AcCol = "SpAcDscd", TaCol = "SpTaDscd",
                            ShareCol = "SpShAcq", SicAcCol = "SpAcSic", 
                        SicTaCol = "SpTaSic"){    
        
        SDCCol <- list("DatCol" = DatCol, "AcCol" = AcCol, "TaCol" = TaCol, "ShareCol" = ShareCol,
                        "SicAcCol" = SicAcCol, "SicTaCol" = SicTaCol)
        } 
        

##2.2	Icc_Set(): Funktion, die Spaltennamen für ICC-Tabelle zentral 
#####   speichert. Die Funktion wird später nicht direkt aufgerufen, 
#####   sondern mit anderen "Set()"-Funktionen durch die Funktion 
#####   Data_Prop() in der Liste DataPropList gespeichert
Icc_Set <- function(DatCol = "Datum", DscdCol = "Company_Code", 
                        IccCol = "ICC_CT"){
        
        ICCCol <- list("DatCol" = DatCol, "DscdCol" = DscdCol , "IccCol" = IccCol)
        }

##2.3	Company_Set(): Funktion, die Spaltennamen für CompanyData-Tabelle 
#####   zentral speichert. Die Funktion wird später nicht direkt aufgerufen, 
#####   sondern mit anderen "Set()"-Funktionen durch die Funktion Data_Prop() 
#####   in der Liste DataPropList gespeichert
Company_Set <- function(DscdCol = "DSCD", CharacCol = "Properties",   
                         SicCol = "WC07021",CharacList = NULL){	
        if(!is.null(CharacList)){                                      
            COMPCol  <- DataPropList$Get()$COMPANY
            COMPCol$CharacList <- CharacList}
        else {
            COMPCol <- list("DscdCol" = DscdCol, "CharacCol" = CharacCol, 
            "SicCol" = SicCol, "CharacList" = CharacList)}
            COMPCol 
        }


##2.4	Icc_Period_Set(): Funktion, durch die prae und post Zeitintervall um den 
#####   M&A definiert werden und für deren Zeiträume die ICCs entnommen werden. 
#####   Die Funktion wird später nicht direkt aufgerufen, sondern mit anderen 
#####   "Set()"-Funktionen durch die Funktion Data_Prop() in der Liste DataPropList 
#####   gespeichert. Außerdem wird innerhalb der Funktion die Funktion Extend_Fun() 
#####   aufgerufen, um die Länge des CompanyData Datensatzes anzupassen.
Icc_Period_Set <- function(Close = 1, Far = 63, Size = 12, 
				MinObs =6){	
        ##	Close: Wieviele Monate vor/nach M&A soll das prae/post ICC-Zeitintervall beginnen         
        ##  Far: Entfernung des vom M&A am weitesten entfernten Monats im Zeitintervall
        ##  Size: ICCs werden nicht als ein Mittelwert für den ganzen durch Close und 
        ##        Far definierten Zeitraum berechnet, sondern durch mehrere Mittelwerte, 
        ##        die rollierend innerhalb des Fensters bestimmt werden. Die Länge der 
        ##        rollierenden Fenster wird durch Size (in Monaten) festgelegt.  
        ##  MinObs: Minimal notwendige Anzahl an Beobachtungen für jedes rollierende 
        ##          Berechnungsfenster. Sind es weniger, wird NA zurückgegeben.
        
        ##  Aufruf von Funktion Extend_Fun(), um gegebenenfalls CompanyData Datensatz um neue
        ##  Spalten für Monate zu erweitern, falls durch Definition des Zeitfensters Daten
        ##  für Zeitpunkte abgefragt werden sollten, für die keine Spalten vorhanden sind.
        Extend_Fun(TableStored,ceiling(Far/12))
        
        IccPeriodData <- list(Close = Close, Far = Far, 
                        Size =  Size, MinObs = MinObs)
        }
    
##2.5	Extend_Fun(): wird in Icc_Period_Set() aufgerufen.  Wenn durch die Größe des 
#####   post ICC-Zeitintervalls nach Unternehmensdaten eines Datums gefragt wird, das nicht 
#####   im CompanyData Datensatz enthalten ist, schlägt die Abfrage fehl. Extend_Fun 
#####   fügt daher weitere Spalten mit NA-Werten für in der Zukunft liegende Zeitpunkte 
#####   an den CompanyData Datensatz an.  
Extend_Fun <- function(TableStored , years = 6){
		CompanyTable <- TableStored$Get()$COMPANY
		monthAdd<-12*(years+1)
		time <- as.Date("2014-01-01")
		time <- as.POSIXlt(time)	
		newRow <- rep(NA,nrow(CompanyTable))
		time$mon <- time$mon+1:monthAdd
		time<-substr(as.Date(time),1,7)
		CompanyTable[,time] <- newRow
        
		Tables <- TableStored$Get()
		Tables$COMPANY <- CompanyTable
		TableStored$Set(Tables)
		}

##2.6 	Data_Prop(): Die Funktion 2.1 bis 2.4 werden als Listenelemente in 
#####   einer von der Funktion Data_Prop() erstellten Liste DataPropList gebündelt.
Data_Prop <- function(){

		PropList <- list(NULL)
		Get <- function()PropList

        ## Funktionen rufen vorherige Funktionen unter 2. auf
		setSdc <- function(...) if (length(list(...))==0){PropList$SDC <<- Sdc_Set()}
						else {PropList$SDC <<- Sdc_Set(...)}
		setIcc <- function(...) if (length(list(...))==0){PropList$ICC <<- Icc_Set()}
						else {PropList$ICC <<- Icc_Set(...)}
		setCompany <- function(...) if (length(list(...))==0){PropList$COMPANY <<- Company_Set()}
						else {PropList$COMPANY <<- Company_Set(...)}
        setCharacList <- function(CompanyProp){PropList$COMPANY <<- CompanyProp}                
		setIccPeriod <- function(...) if (length(list(...))==0){PropList$ICCPeriod <<- Icc_Period_Set()}
						else {PropList$ICCPeriod <<- Icc_Period_Set(...)}
        
        ## Funktion zum Setzen von Standardwerten
        setDefault <- function(){
                        PropList$SDC <<- Sdc_Set()
                        PropList$ICC <<- Icc_Set()
                        PropList$COMPANY <<- Company_Set()
                        PropList$ICCPeriod <<- Icc_Period_Set()
                        }
    
        ## Erstellt Liste
		DataPropList <<- list(Get = Get, setSdc = setSdc, setICC = setIcc, setCompany = setCompany,
					setIccPeriod = setIccPeriod, setDefault = setDefault)	
		}

##  Aufruf von Data_Prop() ->  Liste DataPropList wird erstellt ohne Zuweisung        
##    Data_Prop()
Data_Prop()
##  Setzen von Standardwerten für DataPropList     
    DataPropList$setDefault()

    
    
    
## 3.	Weitere Modifizierung der Datensätze durch Information aus Schritt 2.
    
##3.1	Dataset_Adjust(): Modifiziert die Datentypen für SDC und ICC Datensatz 
#####   sowie die Spaltennamen des SDC Datensatz    
Dataset_Adjust <- function(TableStored,DataPropList){
		
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
        
        IccDatCol <- DataPropList$Get()$ICC$DatCol
        IccTable[,{IccDatCol}:=substr(as.Date(strptime(IccTable[[IccDatCol]],"%Y-%m-%d")),1,7)]     
		
        ## Überschreibung alter Datensätze
        Tables <- TableStored$Get()
		Tables$SDC <- SdcTable
		TableStored$Set(Tables)
		}

##  Aufruf von Dataset_Adjust()        
##    Dataset_Adjust(TableStored,DataPropList)     
Dataset_Adjust(TableStored,DataPropList)          
        
##3.2   Company_Prop_List_Set(): Untersucht, an welchen Positionen im 
#####   Datensatz für Unternehmensdaten welche Unternehmensdaten MV, Sales etc.
#####   auftauchen und gibt eine Liste mit logischen Datensätzen für jeden
#####   Unternehmensdatentyp zurück. Dieser wird an DataPropList angehängt.
#####   Die Aktion soll später bei den Berechnungen Zeit einsparen.
Company_Prop_List_Set <- function(TableStored = TableStored, DataPropList){
		
		CompanyTable <- TableStored$Get()$COMPANY 
		CompProp  <- DataPropList$Get()$COMPANY
		CharacCol <- CompProp$CharacCol
		CharacCol <- CompanyTable[[CharacCol]]          
		CharacCol <- as.factor(CharacCol)		

		Characs <- levels(CharacCol)
		CharacMat <- sapply(1:length(Characs),function(x){Characs[x] == CharacCol })
		colnames(CharacMat) <- Characs
		CharacMat <- as.data.table(CharacMat)	
        
        ## Anhängen an DataPropList
        DataPropList$setCompany(CharacList = CharacMat)
		}
              
##4.1	Sdc_Get(): Entnimmt Daten zu M&A aus SDC Datensatz und berechnet 
#####   gleichzeitig durch Zugriff auf AccessData$retrieveIccPeriod(Datum) 
#####   das Zeitintervall zur ICC Berechnung
Sdc_Get <- function(DataPropList, TableStored, SDCRow,access_data = AccessData){
        ##  Zunächst werden die Spaltennamen des SDC Datensatzes aus DataPropList
        ##  zwischengespeichert, ebenso wie der SDC Datensatz.
        ##  Dann wird durch den Funktionsaufruf access_data$retrieveIccPeriod
        ##  die Berechnung der prae und post Zeitintervalle ausgelöst,
        ##  zum Funktionsende die M&A Daten eingeholt und in einer Liste
        ##  zurückgegeben.
        
        ##  SDCRow: Welche Zeile im SDC Datensatz ausgewertet wird
        ##          im Prinzip der M&A, der ausgewertet wird.
        ##  AccessData: Liste, in der Funktionen zur Datenauswertung zusammen-
        ##              gefasst sind - wird später genauer erläutert.   
        
        SdcProp  <- DataPropList$Get()$SDC   ## Aufruf der Spaltenbenennung des SDC Datensatzes
        SdcTable <- TableStored$Get()$SDC   ## Aufruf des SDC Datensatze
        
        ## Zwischenspeicherung der SDC Spaltennamen
        DatCol 	<- SdcProp$DatCol	
        AcquirorCol <- SdcProp$AcCol
        TargetCol   <- SdcProp$TaCol
        ShareAcCol  <- SdcProp$ShareCol
        SicAcqCol	<- SdcProp$SicAcCol
        SicTarCol   <- SdcProp$SicTaCol
        
        Datum     <- SdcTable[[DatCol]][SDCRow]
        access_data$retrieveIccPeriod(Datum) 
        ## access_data: wird später erklärt. Hier wird eine Funktion aufgerufen,
        ##              die für das M&A Datum die post/prae Zeitintervalle berechnet 
        ##              und speichert.    
        
        
        ## Entnahme der M&A Informationen
        AcquirorDscd <- SdcTable[[AcquirorCol]][SDCRow]   
        TargetDscd   <- SdcTable[[TargetCol]][SDCRow]     
        ShareAc      <- SdcTable[[ShareAcCol]][SDCRow]      
        SicAc		 <- SdcTable[[SicAcqCol]][SDCRow]     
        SicTa		 <- SdcTable[[SicTarCol]][SDCRow] 	    
        
        ## Rückgabe der M&A Informationen
        list( Datum = Datum, AcquirorDscd = AcquirorDscd, 
            TargetDscd = TargetDscd, ShareAc = ShareAc, 
            SicAc = SicAc, SicTa = SicTa)
        }

##4.2	Icc_Period_Get(): Berechnet für ein übergebenes Datum die post und prae
#####   M&A Zeitintervalle
Icc_Period_Get <- function(Datum){
	
        icc_period_data <- DataPropList$Get()$ICCPeriod ## Aufruf der zuvor definierten Parameter für Zeifenster
        
        Datum <- as.POSIXlt(Datum) 
        Datum$mday <- 15
        DatPost <- DatPrae <- rep(Datum,icc_period_data$Far - icc_period_data$Close+1)
                 
        DatPrae$mon <- DatPrae$mon-(icc_period_data$Far:icc_period_data$Close)    ##  Zuweisung des prae Zeitintervalls
        DatPost$mon <- DatPost$mon+(icc_period_data$Close:icc_period_data$Far)    ##  Zuweisung des post Zeitintervalls
        DatPrae <- substr(as.Date(DatPrae),1,7) ##  Entfernung der Wochentage (für Auswertung sind nur Monate von Bedeutung)
        DatPost <- substr(as.Date(DatPost),1,7) ##  Entfernung der Wochentage (für Auswertung sind nur Monate von Bedeutung)
        MinObs <- icc_period_data$MinObs
        Size <- icc_period_data$Size	## Größe der rollierenden Berechnungsfenster

        list(DatPrae = DatPrae, DatPost = DatPost, MinObs = MinObs, Size = Size)
        }



##4.3	Icc_Get(): entnimmt für die Zeitfenster und Datastream Codes der M&A Partner 
#####   die ICC-Werte aus dem ICC Datensatz und berechnet innerhalb der beiden Zeitintervalle
#####   (post/prae M&A) rollierend Mittelwerte unter der Bedingung, dass die geforderte Anzahl
#####   an Mindestbeobachtungen erfüllt ist (-->DataPropList$Get()$ICCPeriod$MinObs<--)
Icc_Get <- function(DataPropList, TableStored, access_data = AccessData, Sample=FALSE){
        ## Nach Aufruf von Spaltenbenennung und ICC Datensatz
        ## werden zunächst prae ICCs für Target- und Acquiror-Unternehmen
        ## sowie post ICCs für Acquiror-Unternehmen aus ICC Datensatz
        ## entnommen und diese in dem data.frame ICC zwischengespeichert.
        ## Danach werden durch rollapply die rollierenden Mittelwerte für alle
        ## Berechnungsfenster im Zeitintervall berechnet.
        
        IccProp <- DataPropList$Get()$ICC	## Aufruf der Spaltenbenennung des ICC Datensatzes
        
        if(Sample){IccTable <- TableStored$Get()$ICCsample} #
        else{IccTable <- TableStored$Get()$ICC}   
        ##  Legt fest, ob gesamter ICC Datensatz durchsucht werden soll oder ein vorher erstellter
        ##  reduzierter Datensatz, der alle notwendigen Daten enthält. Zur Einsparung von Rechenzeit. 
        
        SdcData <- access_data$Get()$SDC        ##  Aufruf der von SDC_get() entnommenen M&A Informationen
        PeriodData <- access_data$Get()$ICCPeriod  ##  Aufruf der von Icc_Period_Get() erstellten Zeitfenster
        
        ## Zwischenspeicherung der ICC Spaltennamen
        DatCol  <- IccProp$DatCol
        DscdCol <- IccProp$DscdCol
        IccCol  <- IccProp$IccCol
        
        ## Zwischenspeicherung der Zeitfenster Daten
        DatPrae <- sort(PeriodData$DatPrae)
        DatPost <- sort(PeriodData$DatPost)
        MinObs  <- PeriodData$MinObs
        Size    <- PeriodData$Size
        
        ## Aufruf des ICC Datensatzes
        IccTable  <- IccTable[,c(DatCol,DscdCol,IccCol),with=F]             ##
        
        ## data.frame zur Speicherung der ICCs
        ICC <- rep(NA,length(DatPrae))
        
        ## Am Ende soll ein Dataframe mit ICC Zeitreihen für das Target vor, den
        ## Acquiror vor und nach M&A im ICC data.frame gespeichert werden.
        ## Der data.frame "ICC" ist die leere Vorlage.
        ICC <- data.frame("TaIcc" = ICC, "AcIccPrae" = ICC,"AcIccPost" = ICC)

            temp  <- IccTable[[DscdCol]] == as.character(SdcData$TargetDscd) ############
            ## Filterung der Target-Datastream Codeeinträge im ICC Datensatz
            
            tempTA  <- IccTable[temp, c(DatCol,IccCol),with=F]          
            ## Neuzuweisung des ICC Datensatzes mit ausschließlich Informationen zum Target 
            
            tempDat <- is.element(tempTA[[DatCol]],DatPrae)
            ## Suche nach Einträgen zum prae Zeitfenster im Target ICC Datensatz
            
            IccExist <- is.element(DatPrae,tempTA[[DatCol]][tempDat])       
            ## Um zu verhindern, dass fehlende Werte (nicht NA, sondern gar 
            ## kein Eintrag zu dem Datum) im Untersuchungszeitraum nicht
            ## verschwinden, müssen auch deren Positionen ermittelt werden und 
            ## beim Eintrag in die ICC Tabelle berücksichtigt werden
            
            ICC[IccExist,"TaIcc"] <- tempTA[tempDat, IccCol,with=F]         
            ## Zuweisung der ICCs zum leeren data.frame "ICC"
            
            ## Analoge Berechnung für ICC des Acquirors
            temp  <- IccTable[[DscdCol]] == SdcData$AcquirorDscd            
            tempAC  <- IccTable[temp, c(DatCol,IccCol),with=F]              
            tempDat <- is.element(tempAC[[DatCol]],DatPrae)                 
            IccExist <- is.element(DatPrae,tempAC[[DatCol]][tempDat])       
            ICC[IccExist,"AcIccPrae"] <- tempAC[tempDat, IccCol,with=F]        

            tempDat <- is.element(tempAC[[DatCol]],DatPost)     
            IccExist <- is.element(DatPost,tempAC[[DatCol]][tempDat])  
            ICC[IccExist,"AcIccPost"] <- tempAC[tempDat, IccCol,with=F]     
          
            ## Im folgenden wird der data.frame "ICC" in ein Objekt vom Typ zoo umgewandelt.
            ## Darin werden für jede Spalte (TaIcc, AcIccPrae, AcIccPost) rollierend Mittelwerte
            ## bestimmt. Sollten pro Berechnungszeitraum weniger Beobachtungen vorhanden sein
            ## als durch "MinObs" definiert, findet keine Berechnung statt, sondern es wird ein
            ## NA zurückgegeben.
		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
	
		ICC <- zoo(ICC)
		ICC <- rollapply(ICC,FUN = CalcMean,width = Size)
		ICC	
		}

        
##4.4	Company_Get: Entnimmt für die Zeitfenster und Datastream Codes der M&A 
#####   Partner die Marktwerte aus dem Datensatz mit Unternehmensdaten und 
#####   berechnet innerhalb der beiden Zeitfenster (post/prae M&A) rollierend 
#####   Mittelwerte unter der Bedingung, dass die geforderte Anzahl Mindest-
#####   beobachtungen erfüllt ist. Mit MV-Mittelwerten können später die gewichteten  
#####   ICCs bestimmt werden.

Company_Get<- function(DataPropList, TableStored, access_data = AccessData){
	## Im Prinzip zu ICC_Get() analoge Bestimmung von Marktwerten
		
        CompProp <- DataPropList$Get()$COMPANY
		CompanyTable <- TableStored$Get()$COMPANY		
		SdcData <-  access_data$Get()$SDC	
        PeriodData <- access_data$Get()$ICCPeriod
    
		CharacCol <- CompProp$CharacList
		CharacCol <- CharacCol$MV
		DscdCol <- CompProp$DscdCol
		DatPrae <- sort(PeriodData$DatPrae)
		DatPost <- sort(PeriodData$DatPost)
		MinObs  <- PeriodData$MinObs
		Size 	<- PeriodData$Size
        
        ## Die Entnahme und Berechnung der MV-Mittelwerte sind ähnlich der der ICC.
        ## Es entfällt aber die Prüfung auf nicht eingetragene Daten, weil im
        ## CompanyData Datensatz jedes Datum mit mindestens einem NA-Wert enthalten
        ## ist. Außerdem sind die Daten selbst Spaltennamen und können ohne Überprüfung
        ## mit "is.element" direkt ausgewählt werden.
		CompanyTablePrae <- CompanyTable[CharacCol ,c(DscdCol, DatPrae),with=F]
		CompanyTablePost <- CompanyTable[CharacCol ,c(DscdCol, DatPost),with=F]
		
		Mv <- rep(NA,length(DatPrae))
		Mv <- data.frame("TaMv" = Mv, "AcMvPrae" = Mv,"AcMvPost" = Mv)

		temp <- CompanyTablePrae[[DscdCol]] == SdcData$TargetDscd               
        Mv$TaMv <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])	            
		## hier direkte Auswahl der Daten über Spaltennamen ohne is.element oder
        ## Überprüfung auf nicht verzeichnete Daten
        
		temp <- CompanyTablePrae[[DscdCol]] == SdcData$AcquirorDscd             
		Mv$AcMvPrae <- as.numeric(CompanyTablePrae[,DatPrae,with=F][temp])	        

        Mv$AcMvPost <- as.numeric(CompanyTablePost[,DatPost,with=F][temp])           
		
        
        ## Berechnung rollierender Mittelwerte analog zu ICC_Get()
		CalcMean<-function(x){
			if((Size-sum(is.na(x))) >= MinObs){mean(x,na.rm=T)}
			else NA}
	
		Mv <- zoo(Mv)
		Mv <- rollapply(Mv,FUN = CalcMean,width = Size)
		Mv
		}        


##4.5	Data_Retrieve(): Auf Sdc_Get(), Icc_Get() und Company_Get() wird
#####   nicht direkt zugegriffen, sondern durch Data_Retrieve() wird eine
#####   Liste der Funktionen erstellt, über die auf die Funktionen 
#####   4.1, 4.3,  4.4 zugegriffen werden kann. Die Liste speichert außerdem
#####   die jeweils zum M&A entnommenen und berechneten Daten in MaaData.
Data_Retrieve <- function(){

        MaaData <- list(NULL)
        retrieveSdc <- function(...){MaaData$SDC <<- Sdc_Get(...)}
        retrieveIcc <- function(...){MaaData$ICC <<- Icc_Get(...)}
        retrieveCompany <- function(...){MaaData$COMPANY <<- Company_Get(...)}
        retrieveIccPeriod <- function(...){MaaData$ICCPeriod <<- Icc_Period_Get(...)}
        Get <- function()MaaData
        
        list(retrieveSdc = retrieveSdc, retrieveIcc = retrieveIcc, retrieveCompany = retrieveCompany, 
            retrieveIccPeriod = retrieveIccPeriod, Get = Get)
        }
        


## 5.   Anwendung der Funktionen aus Schritt 4 zur Erstellung eines finalen
##      Datensatzes, der für jeden M&A die ICCs angibt.        

##5.1	Compute_MandA_Table(): Ruft für jeden M&A Sdc_Get(),Icc_Get() und Company_Get() auf 
#####   (bzw. indirekt über eine von Data_Retrieve() erstellte Liste). 
#####   Berechnet aus den erhaltenen Informationen alle ICCs zum M&A und 
#####   gibt die Daten in einer neuen Tabelle zurück.
Compute_MandA_Table <- function(DataPropList,TableStored){
    ## Zuerst wird ein kleinerer ICC Datensatz erstellt, um die Berechnungen zu
    ## beschleunigen. Dann wird eine leere Tabelle aufgebaut, in der später die
    ## M&A Daten gespeichert werden.
    ## In der for-Schleife wird jeder M&A im SDC Datensatz durchlaufen, durch
    ## die retrieve SDC/ICC/Company Aufrufe die M&A Daten und ICCs entnommen und danach 
    ## im Datensatz gespeichert. Außerdem wird eine Spalte zur Klassifizierung des M&As
    ## nach SIC Unterschieden angehängt (bis zu welcher SIC-Ziffer sich die M&A
    ## Partner ähneln bzw. unterscheiden).
        ## Auswahl der Datensätze
        SdcTable <- TableStored$Get()$SDC
        IccTable <- TableStored$Get()$ICC
        
        laenge <- nrow(SdcTable)
        
        ## Reduzierung des ICC Datensatzes auf relevante Spalten 
        IccTable <- IccTable[,as.character(DataPropList$Get()$ICC),with=F]
        
            ## In den folgenden zehn Zeilen wird der ICC Datensatz auf einen
            ## neuen verkleinert, in dem nur diejenigen Zeilen noch vorhanden sind,
            ## in denen Daten für Unternehmen gespeichert sind, die auch im SDC Datensatz
            ## aufgeführt sind.
            AcquirorDscds <- DataPropList$Get()$SDC$AcCol
            TargetDscds <- DataPropList$Get()$SDC$TaCol
            IccDscdCol <- DataPropList$Get()$ICC$DscdCol
            
            ## Sammeln aller Datastream Codes aus SDC Datensatz
            DscdsList <- c( SdcTable[[AcquirorDscds]][1:laenge], SdcTable[[TargetDscds]][1:laenge])
            IccSampleRows <- is.element(IccTable[[IccDscdCol]],DscdsList)

            IccTable <- IccTable[IccSampleRows,]
            Tables <- TableStored$Get()    
            Tables$ICCsample <- IccTable
            TableStored$Set(Tables)
        
        ## Erstellung eines leeren Datensatze, in dem später die M&A Daten gespeichert werden
        icc_period_data <- DataPropList$Get()$ICCPeriod
        nobs <- ((icc_period_data$Far - icc_period_data$Close+1)-icc_period_data$Size+1)
        maa_table <- as.data.frame(matrix(rep(NA,(7+6*nobs)*laenge),nrow=laenge,ncol=7+6*nobs))
        
        ## Aufruf von Funktion 3.2
        Company_Prop_List_Set(TableStored, DataPropList) 
        
        ## das Rückgabeobjekt von Data_Retrieve() muss vor Verwendung bereits existieren
        AccessData <<- Data_Retrieve ()
        
        
            ## Start der tatsächlichen Datenentnahme und der Berechnung     
            for (i_row in 1:laenge){ ## i_row für jede Zeile des SDC Datensatzes
                print(i_row)
                AccessData$retrieveSdc(DataPropList, TableStored, i_row,AccessData) ## Anwendung von Sdc_Get()
                AccessData$retrieveIcc(DataPropList, TableStored, AccessData, Sample=TRUE) ## Anwendung von Icc_Get()
                AccessData$retrieveCompany(DataPropList, TableStored, AccessData) ## Anwendung von Company_Get()

                ## Zuweisung der Werte zu der M&A Tabelle    
                maa_table[i_row,] <- c(as.character(AccessData$Get()$SDC$Datum), AccessData$Get()$SDC$AcquirorDscd, AccessData$Get()$SDC$TargetDscd, 
                    AccessData$Get()$SDC$ShareAc, AccessData$Get()$SDC$SicAc , AccessData$Get()$SDC$SicTa , NA,
                    AccessData$Get()$ICC$TaIcc, AccessData$Get()$ICC$AcIccPrae, AccessData$Get()$ICC$AcIccPost,
                    AccessData$Get()$COMPANY$TaMv, AccessData$Get()$COMPANY$AcMvPrae, AccessData$Get()$COMPANY$AcMvPost)	
            }
         ##DatEffective = DatEffective, DatAnnounced = DatAnnounced 
     
        ## Benennung der Spalten der finalen Tabelle
        IccColNames <- c(paste("TaIcc",-(nobs:1),sep="_"),paste("AcIccPrae",-(nobs:1),sep="_"),
                       paste("AcIccPost",(1:nobs),sep="_+"))
       

        MvColNames  <- c(paste("TaMv",-(nobs:1),sep="_"),paste("AcMvPrae",-(nobs:1),sep="_"),
                        paste("AcMvPost",(1:nobs),sep="_"))
       

        names(maa_table)<-c("Date","Acquiror_Dscd","Target_Dscd","Perc_Shares_Acquired","Acquiror_Sic","Target_Sic",
                    "SicSep",IccColNames,MvColNames)
        
        ## Änderung der Datentypen der Tabelle auf numeric
        maa_table[,c("Perc_Shares_Acquired",IccColNames,MvColNames)]<-apply(maa_table[,c("Perc_Shares_Acquired",
                       IccColNames,MvColNames)],2,as.numeric)
        
        ## Zuweisung neuer Spalte, die angibt, an welcher Sic Ziffer sich die Targer und Acquiror Sic unterscheiden
        maa_table$SicSep <- Sic_Separation(maa_table)
        maa_table <- Calc_Weighted_Icc_prae(maa_table=maa_table)
        maa_table <- Calc_Icc_Diff (maa_table=maa_table)
        maa_table
        }    
            

##5.2	Sic_Separation(): Wertet aus, bis zu welcher Position die Sic des Targets dem 
#####   Acquiror gleich ist oder ob beide Sics identisch sind
Sic_Separation <- function(SummarySdc, AcSicCol = "Acquiror_Sic", TaSicCol = "Target_Sic")
			{
            ## Überprüfung, bis zu welcher Stelle Acquiror bzw. Target Sic gleich bzw. identisch sind
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
			
            ## Zuweisung entsprechender Bezeichnung
			SicCol <- rep(NA,nrow(SummarySdc))
			SicCol[Sic0] <- "sameSic" 
			SicCol[Sic1] <- "firstDigit"
			SicCol[Sic2] <- "secDigit"
			SicCol[Sic3] <- "thirdDigit"
			SicCol[Sic4] <- "fourthDigit"
			SicCol <- factor(SicCol,order=T,levels=c("sameSic","fourthDigit","thirdDigit","secDigit","firstDigit"))
			SicCol
			}
            

##5.3	Calc_Weighted_Icc_prae(): Berechnet aus MV und ICC des Targets und Acquirors sowie dem
#####   am Target erworbenen Unternehmensanteil die gewichtete ICC vor M&A.
Calc_Weighted_Icc_prae <- function(maa_table = MaATable,acquiror_icc_prae = "AcIccPrae",target_icc_prae = "TaIcc",
                            acquiror_mv_prae="AcMvPrae",target_mv_prae = "TaMv",share_acquired = "Perc_Shares_Acquired"){
            ## Bestimmung, wie viele Berechnungen im Interval vor M&A durchgeführt wurden 
            n_icc_calc <- length(grep( acquiror_icc_prae,colnames(maa_table)))
            n_row <- nrow(maa_table)
            
            ## Erstellung vom leeren Datensatzes als Vorlage
            weighted_icc <- data.frame("WeightedIccPrae_-9"= rep(NA,n_row))
            
            ## Erstellung von Namen der neuen Spalten
            weighted_icc_col_names <- paste("WeightedIccPrae",n_icc_calc:1,sep="_-")
            weighted_icc[,weighted_icc_col_names] <- rep(NA,n_row)
            weighted_icc<-weighted_icc[,-1]
            
            ## Zwischenspeicherung von Spalte, die Information zu erworbenen
            ## Unternehmensanteil beinhaltet
            share <- maa_table[,share_acquired] 
            
            ## Index i ist Zahl, die an die Namen "AcIccPrae" und "TaIcc" angehängt wird, um
            ## so die richtigen Target- und Acquiror- ICC bzw. MV Spalten vom MaATable zu selektieren. 
            for(i in 1:n_icc_calc){
            
                   ## Auswahl der Spalten   
                   weighted_icc_col <- paste("WeightedIccPrae",i,sep="_-")
                   acquiror_icc_col <- maa_table[paste(acquiror_icc_prae,i,sep="_-")]
                   acquiror_mv_col  <- maa_table[paste(acquiror_mv_prae,i,sep="_-")]
                   target_icc_col   <- maa_table[paste(target_icc_prae,i,sep="_-")]
                   target_mv_col    <- maa_table[paste(target_mv_prae,i,sep="_-")]
                           
                   ## Berechnung der gewichteten ICC 
                   weighted_icc[,weighted_icc_col] <- ((acquiror_icc_col*acquiror_mv_col  +  target_icc_col*target_mv_col*share)
                                                                 /       (acquiror_mv_col  +  target_mv_col*share))
                   }
            ## Anhängen der neuen Spalten an MaATable        
            maa_table <- cbind(maa_table,as.data.table(weighted_icc))
            maa_table
            }

##5.4	Calc_Icc_Diff(): Berechnet aus gewichteten ICCs und korrespondierenden post M&A ICC des 
#####   Acquirors die ICC Differenz, die durchc den M&A entstanden ist.            
Calc_Icc_Diff <- function(maa_table = MaATable, weighted_icc_prae ="WeightedIccPrae", acquiror_icc_prae ="AcIccPost"){
            ## Bestimmung, wieviele Berechnungen im Intervall vor M&A durchgeführt wurden
            n_icc_calc <- length(grep(weighted_icc_prae,colnames(maa_table)))
            n_row <- nrow(maa_table)
              
            ## Erstellung eines leeren Datensatzes als Vorlage        
            diff_icc <- data.frame("IccDifference_1"= rep(NA,n_row))
            diff_icc_col_names <- paste("IccDifference",1:n_icc_calc,sep="_")
            diff_icc[,diff_icc_col_names] <- rep(NA,n_row)
            
            ## Analog zu Calc_Weighted_Icc_prae() werden durch Index i die korrespondierenden 
            ## WeightedICC und AcIccPost ausgewählt und aus ihnen die ICC Differenz der M&As
            ## bestimmt    
            ## Index i ist eine Zahl, die an die Namen "AcIccPrae" und "TaIcc" angehängt wird, um
            ## so die richtigen Target- und Acquiror- ICC bzw. MV Spalten vom MaATable zu selektieren.
            for(i in 1:n_icc_calc){
                   
                   ## Auswahl der Spalten    
                   diff_icc_col <- paste("IccDifference",i,sep="_")
                   weighted_icc_col <- maa_table[paste(weighted_icc_prae,i,sep="_-")]
                   acquiror_icc_col <- maa_table[paste(acquiror_icc_prae,i,sep="_+")]
                   
                   ## Berechnung der Differenz
                   diff_icc[,diff_icc_col] <- acquiror_icc_col - weighted_icc_col
                   }
            ## Anhängen der neuen Spalten an MaATable          
            maa_table <- cbind(maa_table,as.data.table(diff_icc))
            maa_table
            }