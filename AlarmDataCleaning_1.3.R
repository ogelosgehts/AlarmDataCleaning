####### Dieses Skript erstellt eine übersichtiliche, auswertbare .csv-Datei. Als input dienen die Log-Daten des
####### Philips IntelliVue patient monitoring system (MX800, version M.00.03; MMS X2, version H.15.41-M.00.04), die als csv.-Datei extrahiert werden können.

####### Für Fragen und Erlaeuterungen bitte eine E-Mail an: mail[aett]mmwunderlich[punkt]de



Sys.setlocale("LC_ALL", 'en_US.UTF-8')

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(scales)

# --------------------------------------------- DATEN IMPORTIEREN -----------------------------------------------------------

#Importieren der Daten als csv

setwd("/…")
data <- read.csv("CSV_ALARM_DATEI.csv", sep = ",", stringsAsFactors = FALSE)
data$Monitor <- data$Geräte.Name


data$Zeit <- as.POSIXct(data$Zeit)       # als Datum statt Faktor formatieren

data <- select(data, Zeit, Bettname, Aktion, Monitor) # relevante Variablen auswählen

# Ein neues Datenset erstellen für die Bereinigung

data.new <- data

# --------------------------------------------- MONITORE BEREINIGEN -----------------------------------------------------------

# Da einige Monitornamen sowohl klein als auch groß geschrieben sind, diese nun zusammenführen, sodass statt zB "gwm101"
# nur noch "GWM101 steht"

data.new$Monitor <- tolower(data.new$Monitor) # erst alle Zeichen lower case machen
data.new$Monitor <- str_replace_all(data.new$Monitor, "piic ix: ", "") # löscht "piic ix: ", da es redundant und unübersichtlich ist

# Macht das erste M der Bettmonitore wieder groß geschrieben wird
data.new[which(grepl("^m", data.new$Monitor)),]$Monitor <- str_to_title(data[which(grepl("^m", data.new$Monitor)),]$Monitor, locale = "en")

# Als Faktor formatieren
data.new$Monitor <- as.factor(data.new$Monitor)


# --------------------------------------------- FEHLENDE BETTNAMEN EINTRAGEN -----------------------------------------------------------

# Leere Einträge in der Spalte Bettname mit dem vorrangehenden Bett ersetzen

# Zunächst leere Einträge in NA umwandeln, denn tidyr::fill wurde für's ersetzen von NAs geschrieben
data.new$Bettname[data.new$Bettname == ""] <- NA

# NAs durch das vorrangehende Bett ersetzen, direction "up", da das Datenset chronologisch von unten nach oben sortiert ist.
data.new %>%
  tidyr::fill(Bettname, .direction = "up") %>%
  {.->> data.new}


# Als Faktor formatieren
data.new$Bettname <- as.factor(data.new$Bettname)

# --------------------------------------------- GENERIERT vs BEENDET -----------------------------------------------------------

data.new$Situation <- NA

data.new$Situation <- ifelse(grepl("generiert", data.new$Aktion), "generiert",
                              ifelse(grepl("beendet", data.new$Aktion) & !grepl("Alarm- oder Störungston wurde beendet.", data.new$Aktion), "beendet",
                                     NA))

# Als Faktor formatieren
data.new$Situation <- as.factor(data.new$Situation)

# --------------------------------------------- ALARMFARBEN EXTRAHIEREN -----------------------------------------------------------

# kreiert eine Spalte mit "gelb" und "rot" je nach Alarm.
# Philips Manual Seite 61: "Die Sternchensymbole (*) neben der Alarmmeldung geben ebenfalls die Alarmpriorität an: *** für rote Alarme, ** für gelbe Alarme,
# * für kurze gelbe Alarme. Standard- Störungsmeldungen haben kein Symbol. Bei roten und gelben Störungsmeldungen befinden sich neben der 
# Alarmmeldung Ausrufezeichen: !!! für rote und !! für gelbe Störungsmeldungen."

# Das letzte ifelse Argument nimmt alle Alarme die ein "generiert" enthalten, aber noch keine Farbe durch die Argumente davor zugewiesen bekommen haben
# und macht sie blau, da dies die technischen Alarme sind, die nicht als rot oder gelb eingestuft wurden.

data.new$Alarmfarbe <- NA

data.new$Alarmfarbe <- ifelse(grepl("\\*\\*\\*", data.new$Aktion), "rot",
                              ifelse(grepl("\\*\\*", data.new$Aktion) & !grepl("\\*\\*\\*", data.new$Aktion), "gelb",
                                     ifelse(grepl("\\*", data.new$Aktion) & !grepl("\\*\\*\\*", data.new$Aktion), "gelb",
                                            ifelse(grepl("!!", data.new$Aktion) & !grepl("!!!", data.new$Aktion), "gelb",
                                                   ifelse(grepl("!!!", data.new$Aktion), "rot",
                                                          ifelse(grepl("generiert",data.new$Aktion) | grepl("beendet",data.new$Aktion) & is.na(data.new$Alarmfarbe), "blau",
                                                                        NA))))))

# Dieser Eintrag erhält fälschlicherweise ebenfalls die Zuweisung "blau" durch den grepl "beendet", das korrigieren.
data.new[which(grepl("Alarm- oder Störungston wurde beendet.", data.new$Aktion)),]$Alarmfarbe <- NA


# Als Faktor formatieren
data.new$Alarmfarbe <- as.factor(data.new$Alarmfarbe)



# --------------------------------------------- ALARME EXTRAHIEREN -----------------------------------------------------------



# Alle Zeichen bis "generiert um"/"beendet" extrahieren, anschließend leading/trailing whitespace, Zahlen weg und * oder ! und andere Sonderzeichen weg.
# Das ist möglich, da JEDER Alarm, egal ob *, **, ***, !!, !!! oder ohne jegliche Zeichen die Phrase "generiert um.."/"beendet" enthält

data.new$Alarm <- NA

# nimmt die erste gematchte Gruppe //1 vom Anfang des strings bis "generiert", oder die erste gematchte Gruppe vom Anfang bis "beendet."
data.new$Alarm <- ifelse(grepl("generiert", data.new$Aktion), sub("^(.*?) *generiert.*", "\\1", data.new$Aktion),
                         ifelse(grepl("beendet", data.new$Aktion), sub("^(.*?) *beendet.*", "\\1", data.new$Aktion), NA))
# löscht diese Sonderzeichen
data.new$Alarm <- str_replace_all(data.new$Alarm, "[\\*,!,>,<,:]", "")
# ersetzt Bindestriche mit Leerzeichen, sodass negative Werte wie -5 < 10 verschwinden und zB PEEP-VERLUST lesbar bleibt
data.new$Alarm <- str_replace_all(data.new$Alarm, "[[-]]", " ")
# löscht alle Zahlen
data.new$Alarm <- str_replace_all(data.new$Alarm, "[[:digit:]]", "")
# löscht Leerzeichen am Anfang und Ende der Strings
data.new$Alarm <- trimws(data.new$Alarm, which = "both")

# Das wurde fälschlicherweise ebenfalls extrahiert und kann NA bleiben
data.new[which(data.new$Alarm == "Alarm  oder Störungston wurde"),]$Alarm <- NA

# Die Extrahierten CO2 relatierten Werte haben bei der bereinigung ihre 2 verloren, das wieder herstellen
data.new$Alarm <- str_replace(data.new$Alarm, "CO TIEF/HOCH", "CO2 TIEF/HOCH")
data.new$Alarm <- str_replace(data.new$Alarm, "SpOr", "SpO2r")
data.new$Alarm <- str_replace(data.new$Alarm, "SpO$", "SpO2") # $ um das Ende des Strings zu markieren, weil sonst das SPO2r in der Zeile darüber noch eine zweite 2 bekommt
data.new$Alarm <- str_replace(data.new$Alarm, "FIO TIEF/HOCH", "FIO2 TIEF/HOCH")
data.new$Alarm <- str_replace(data.new$Alarm, "FIO   HOCH", "FIO2 HOCH")
data.new$Alarm <- str_replace(data.new$Alarm, "etCO$", "etCO2")
data.new$Alarm <- str_replace(data.new$Alarm, "SpOpo", "SpO2po")
data.new$Alarm <- str_replace(data.new$Alarm, "FIO   NIEDRIG", "FIO2 NIEDRIG")
data.new$Alarm <- str_replace(data.new$Alarm, "SPO TIEF/HOCH", "SPO2 TIEF/HOCH") # neu in Datensatz 2
data.new$Alarm <- str_replace(data.new$Alarm, "SpOl", "SpO2l") # neu in Datensatz 2

# manche extrahierten Alarme haben zu viel Whitespace zwischen den Wörtern
data.new$Alarm <- str_replace(data.new$Alarm, "     ", " ")
data.new$Alarm <- str_replace(data.new$Alarm, "   ", " ")
data.new$Alarm <- str_replace(data.new$Alarm, "  ", " ")

# Auch der Alarm "Apnoe > 10min" hat sein Sonderzeichen und seine Zahl verloren
data.new$Alarm <- str_replace(data.new$Alarm, "Apnoe  min", "Apnoe > 10min")
data.new$Alarm <- str_replace(data.new$Alarm, "Apnoe min", "Apnoe > 10min")

# Gleiche Alarme mit unterschiedlichen Bezeichnungen zusammenführen
data.new[which(data.new$Alarm == "SpO2l" | data.new$Alarm == "SpO2r" | data.new$Alarm == "SpO2po"),]$Alarm <- "SpO2"
data.new[which(data.new$Alarm == "ABPs" | data.new$Alarm == "ARTs"),]$Alarm <- "ABPs"
data.new[which(data.new$Alarm == "ABPm" | data.new$Alarm == "ARTm"),]$Alarm <- "ABPm"
data.new[which(data.new$Alarm == "ABPd" | data.new$Alarm == "ARTd"),]$Alarm <- "ABPd"


# Als Faktor formatieren
data.new$Alarm <- as.factor(data.new$Alarm)


# Spalten die fälschlicherweise eine Alarmfarbe zugeordnet bekommen haben, ohne dass es sich um einen Alarm handelt, auf NA zurücksetzen.
# E.g.: "Pat.-Profil auf *Erwachsene - Erweitert gesetzt" und "Alarm verworfen: **CO2 TIEF/HOCH".
data.new[which(is.na(data.new$Alarm)),]$Alarmfarbe <- NA



# --------------------------------------------- WELCHE ALARME GIBT ES? -----------------------------------------------------------

# erstellt ein Dataframe mit allen extrahierten Alarmen
Liste.aller.Alarme <- as.data.frame(unique(data.new$Alarm[which(!is.na(data.new$Alarm))]))
colnames(Liste.aller.Alarme) <- c("Alarme")

# Liste aller technischen Alarme mit ihren zugewiesenen Farbem
tech.gelb <- as.character(unique(data.new[which(
               grepl("!", data.new$Aktion) & data.new$Alarmfarbe == "gelb"
               ),]$Alarm))

tech.rot <- as.character(unique(data.new[which(
               grepl("!", data.new$Aktion) & data.new$Alarmfarbe == "rot"
               ),]$Alarm))

tech.blau <- as.character(unique(data.new[which(
               grepl("generiert",data.new$Aktion) & data.new$Alarmfarbe == "blau"
               ),]$Alarm))

tech.blau <- cbind(tech.blau, Farbe = "blau")
tech.gelb <- cbind(tech.gelb, Farbe = "gelb")
tech.rot <- cbind(tech.rot, Farbe = "rot")

Liste.Techn.Alarme <- as.data.frame(rbind(tech.blau, tech.gelb, tech.rot))
colnames(Liste.Techn.Alarme) <- c("Alarm", "Alarmfarbe")

# entfernen der temporären Variablen aus dem Environment
rm(tech.blau, tech.gelb, tech.rot)

# --------------------------------------------- RICHTUNG DER GRENZWERTVERLETZUNG -----------------------------------------------------------

data.new$Richtung <- NA

data.new$Richtung[grepl(">", data.new$Aktion) ]  <- "überschritten"
data.new$Richtung[grepl("<", data.new$Aktion) ]  <- "unterschritten"
data.new$Richtung[grepl("HOCH", data.new$Aktion) & !grepl("TIEF", data.new$Aktion) ]  <- "überschritten" # um den Alarm **CO2 TIEF/HOCH auszuschließen
data.new$Richtung[grepl("TIEF", data.new$Aktion) & !grepl("HOCH", data.new$Aktion)]  <- "unterschritten" # um den Alarm **CO2 TIEF/HOCH auszuschließen
data.new$Richtung[grepl("NIEDRIG", data.new$Aktion) ]  <- "unterschritten"

# Als Faktor formatieren
data.new$Richtung <- as.factor(data.new$Richtung)


# --------------------------------------------- ALARMPAUSIERUNGEN EINTRAGEN -----------------------------------------------------------

data.new$Alarmpause <- NA

data.new$Alarmpause[grepl("Pause: Alle Alarme", data.new$Aktion)]  <- "Pause Ein"
data.new$Alarmpause[grepl("Fortsetzen: Alle Alarme", data.new$Aktion)]  <- "Pause Aus"

# Als Faktor formatieren
data.new$Alarmpause <- as.factor(data.new$Alarmpause)
data.new$Alarmpause <- factor(data.new$Alarmpause,levels = c("Pause Ein", "Pause Aus"))



# --------------------------------------------- NACH EIN- UND ZWEIBETTZIMMERN SORTIEREN -----------------------------------------------------------

data.new$Bettenanzahl <- NA

data.new$Bettenanzahl[grepl("M101i0[1,2,3,4,5,6,8,9]A", data.new$Bettname) |
                        grepl("M101i15A", data.new$Bettname) ] <- "Einbettzimmer"

data.new$Bettenanzahl[grepl("M101i0[7][A,B]", data.new$Bettname) |
                        grepl("M101i1[0,1,2,3,4][A,B]", data.new$Bettname)] <- "Zweibettzimmer"

# Als Faktor formatieren
data.new$Bettenanzahl <- as.factor(data.new$Bettenanzahl)


# --------------------------------------------- ZIMMERNUMMERN EXTRAHIEREN -----------------------------------------------------------

data.new$Zimmernummer <- NA

data.new$Zimmernummer <- ifelse(grepl("^M101i0[[:digit:]]", data.new$Bettname),
                                str_sub(data.new$Bettname,7,7),
                                
                                ifelse(grepl("^M101i1[[:digit:]]", data.new$Bettname),
                                       str_sub(data.new$Bettname,6,7),
                                       NA))

# Als Faktor formatieren
data.new$Zimmernummer <- as.factor(data.new$Zimmernummer)
data.new$Zimmernummer <- factor(data.new$Zimmernummer,levels = c(as.character(seq(1:15))))

# --------------------------------------------- GRENZWERTANPASSUNGEN FINDEN -----------------------------------------------------------

data.new$Grenzwert <- NA
data.new$Grenzwert <- ifelse(grepl("[G,g]renz", data.new$Aktion)
                             , str_extract(data.new$Aktion, "^[[:alnum:]]+"), NA)

# data.new[which(grepl("Pat.-Profil", data.new$Aktion)),]$Grenzwert <- "Pat.-Profil Angep." # Akira empfiehlt das nicht mit reinzunehmen, da es keinen Sinn ergibt.

# korrigieren
data.new[which(data.new$Grenzwert == "SpO2l" | data.new$Grenzwert == "SpO2r"| data.new$Grenzwert == "SpO2po"),]$Grenzwert <- "SpO2"

# Aus und einschalten von Alarmen extrahieren
data.new$EinAus <- NA

# Welcher Alarm?
data.new$EinAus <- ifelse(grepl("Ein", data.new$Aktion) | grepl("Aus", data.new$Aktion)
                             , str_extract(data.new$Aktion, "^[[:alnum:]]+"), NA)

# korrigieren
data.new[which(data.new$EinAus == "Bei"),]$EinAus <- "Bei Assoziierung: Arrhythmie"
data.new[which(data.new$EinAus == "SpO2l" | data.new$EinAus == "SpO2r"| data.new$EinAus == "SpO2po"),]$EinAus <- "SpO2"

# Ein oder Aus?
data.new$EinAus <- ifelse(grepl("Ein", data.new$Aktion)
                          , paste(data.new[which(!is.na(data.new$EinAus)),]$EinAus, "Ein"),
                          ifelse(grepl("Aus", data.new$Aktion)
                                 , paste0(data.new[which(!is.na(data.new$EinAus)),]$EinAus, "Aus"), NA))


# Die EinAus-Spalte mit der Grenzwertspalte zusammenführen, für sparseness.
data.new <- mutate(data.new, Grenzwert = coalesce(Grenzwert, EinAus))

# Als Faktor formatieren
data.new$Grenzwert <- as.factor(data.new$Grenzwert)

# Die nun überflüssige EinAus-Spalte löschen
data.new <- subset(data.new, select = -EinAus )

# --------------------------------------------- ALARME ZU SENSORGRUPPEN ZUORDNEN -----------------------------------------------------------

data.new$Alarmgruppe <- NA

# Folgende Alarme habe ich mit in die Gruppen genommen, obwohl sie nicht im urspruenglichen Datenset vorkommen,
# aber realisitisch erscheinen (vielleicht kommen sie in zukünftigen Datensets vor):

# - Frequenz zu tief
# - Druck zu tief
# - peep niedrig
# - AFaw niedrig
# - TV niedrig
# - ICPd

# Folgende Alarme werden hier vorerst nicht in Gruppen eingeteilt:
# ZVD Bereich?, ZVDs, ZVDm, etCO2, BIS, Ps, awAF

# Den Alarm "EKG Elektrdn ab" habe ich statt zum EKG zu den technischen Fehlern gezählt, da Philips Manual Seite 59 diesen Alarm unter hellblauen Störungen listet.

Ventilator.Machine <- c("OPERATOR", "MINVOL ZU HOCH", "MINVOL ZU TIEF" , "MinVol NIEDRIG", "MinVol HOCH" ,"FREQUENZ ZU HOCH", "FREQUENZ ZU TIEF" ,
                        "ALLG. ALARM", "PEEP VERLUST", "DISKONNEKT. PAT",
                        "DRUCK ZU HOCH", "DRUCK ZU TIEF", "APNOE", "APNOE VENTILAT.", "DISKONNEKT.VENT",
                        "GASVERSORGUNG", "PEEP HOCH", "PEEP NIEDRIG" , "CO2 TIEF/HOCH", "AFaw HOCH", "AFaw NIEDRIG", "awP HOCH", "awP NIEDRIG" , "FIO2 HOCH", "FIO2 NIEDRIG", "FIO2 TIEF/HOCH", 
                        "TV HOCH", "TV NIEDRIG" ,"DRUCK TV LIMIT", "TV NICHT KONST.", "VENT SCHLAUCH?", "SPO2 TIEF/HOCH")
NIBP <- c("NBPm", "NBPs", "NBPd")
IBP <- c("ABPs", "ABPm", "ABPunterbrochn", "ABP Bereich?" , "ABPd", "ARTs", "ARTm", "ARTd", "ARTunterbrochn", "ART Bereich?", "P Bereich?",
         "PAPs", "PAP Bereich?", "PAPunterbrochn", "UAP Bereich?", "UAPs")
EKG <- c("AF", "HF", "HF unregelmäßig",  "Vent Fib/Tachy", "Asystolie", "xBrady",
         "VTachy", "xTachy", "AFIB", "VES Paar", "Multiform VES", "QRS ausgelassen", "Apnoe", "Apnoe > 10min", "Vent ALARM", "Vent STANDBY", "Paroxysmale VT",
         "VES Salve Hoch", "VES/min", "EndeUnregelm.HF", "ST I", "Pause")
TEMP <- c("TKern", "THaut", "TBlut", "Temp")
Intracranial.Pressure <- c("ICPm", "ICP Bereich?", "ICPs", "ICPd", "CPP")
Pulse.Oximetry <- c("Desat", "Puls", "SpO2", "SpO2po", "SpO2r", "SpO2l")
Tech.Fail <- c("SL Verbindg prüfn", "AkkErw fehlt", "AkkErw schwach", "AkkErw leer", "Akku einlegen", "Akku leer", "Akku schwach", "Fehler Akku Erw.",
               "EKG Elektrdn ab", "Pat. ID überprüf", "MSL Verbindung?")
Thermodilution <- c("kHI")

# Die Alarme nach Sensorgruppen sortieren
data.new$Alarmgruppe <- ifelse(data.new$Alarm %in% Ventilator.Machine,
                               "Respirator",
                               ifelse(data.new$Alarm %in% NIBP,
                                      "NIBP",
                                      ifelse(data.new$Alarm %in% IBP,
                                             "IBP",
                                             ifelse(data.new$Alarm %in% EKG,
                                                    "ECG",
                                                    ifelse(data.new$Alarm %in% TEMP,
                                                           "Temperature",
                                                           ifelse(data.new$Alarm %in% Intracranial.Pressure,
                                                                  "Intracranial pressure",
                                                                  ifelse(data.new$Alarm %in% Pulse.Oximetry,
                                                                         "SpO2",
                                                                         ifelse(data.new$Alarm %in% Tech.Fail,
                                                                                "Technical failure",
                                                                                ifelse(data.new$Alarm %in% Thermodilution,
                                                                                       "Thermodilution",
                                                                                       NA)))))))))
# Entfernen der Sensorgruppen aus dem Environment
rm(Ventilator.Machine, NIBP, IBP, EKG, TEMP, Intracranial.Pressure, Pulse.Oximetry, Tech.Fail, Thermodilution)


# Als Faktor formatieren
data.new$Alarmgruppe <- as.factor(data.new$Alarmgruppe)


# --------------------------------------------- DAUER VON ALARMEN BERECHNEN -----------------------------------------------------------

# Zuerst die "wahren" Zeiten, wann ein Alarm ausgelöst wurde in eine neue Spalte einfügen, da sich die Uhrzeit in der Spalte "Zeit" auf die Zeit des
# Eintragens in das Datenset bezieht. Die tatsächliche Zeit des generierten Alarms steht im String unter "Aktion".


data.new$TrueTime <- NA

# extrahiert den String der Uhrzeit nach "generiert" und vor dem Punkt.
data.new$TrueTime <- ifelse(grepl("generiert", data.new$Aktion), sub("^(.*?) *generiert.* (.*?).$", "\\2", data.new$Aktion), NA)

# Um Zeitdiffernezen zu berechnen muss das Format Posix sein und Posix verlangt neben Uhrzeiten auch ein Datum.
# Also füge ich das Datum aus der original Zeit Spalte an die Uhrzeiten in der TrueTime Spalte ein

dummytimes4 <- as.Date(data.new$Zeit)    # erst Daten ohne Uhrzeit extrahieren
dummytimes4 <- paste(dummytimes4, data.new$TrueTime, sep = " ") # die strings zusammenführen
dummytimes4[grepl("NA", dummytimes4)] <- NA # strings die ein NA enthalten, weil sie keine "wahre" Zeit haben, zurück zu NA umwandeln
dummytimes4 <- as.POSIXct(dummytimes4) # als Posixct formatieren

data.new$TrueTime <- dummytimes4 # ans data frame anhängen
rm(dummytimes4) # dummyvektor aus dem Environment löschen

# Die wahren Alarm generiert Zeiten mit der "Zeit" Spalte zusammengefügen, sodass auch die Alarm-beendet Alarme eine Zeit erhalten.
data.new <- mutate(data.new, TrueTime = coalesce(TrueTime, Zeit))

# In den Daten gibt es viele Duplizierte Zeilen. Es scheint, als hätte vor allem "***DISKONNEKT. PAT" einen Bug, dass immer zweimal eingetragen wird,
# wenn dieser Alarm generiert oder beendet wird. Diese Dupliukate muss ich löschen, da sonst in den Zeitdifferenzen zwischen diesen Duplikaten die Differnezn errechnet wird,
# was immer 0 ergiebt.

# Zeigt die duplizierten Zeilen:
dupli.data.new <- data.new[duplicated(data.new) | duplicated(data.new, fromLast = TRUE),]

# löscht Duplikate
data.new  <-  distinct(data.new, .keep_all = TRUE)

# Nun werde ich das Datenset nach Bettnamen aufsplitten, um zu vermeiden, dass die Zeitdifferenzen zwischen Alarmen generiert und beendet
# von zwei unterscheidlichen Betten ausgerechnet werden.

# Erstellt eine Liste mit Dataframes für die einzelnen Betten
BettBett <- split(data.new, data.new$Bettname)

# diese Funktion läuft für jedes Dataframe in der Liste mit Dataframes sortiert nach Betten einmal durch.
# Zeitlich gesehen kommt Alarm beendet immer nach Alarm generiert.

# Im nächsten Schritt sortiert die Funktion das Datenset des jeweiligen Betts nach Zeit, nach dem Alarmnamen und nach der Alarmfarbe (Es gibt z.B. Fälle,
# wo ABPs gelb generiert wird, dann rot, dann wird der gelbe beendet, dann der rote, z.B. bei Bett M101i07A zwischen 2019-02-04 15:00:19 und 2019-02-04 15:00:42.
# Durch Sortieren nach Farbe erreicht man, dass nicht die Zeitdifferenz zischen dem
# generierten gelben und beendeten roten Alarm berechnet wird). Der beendete Alarm steht somit immer direkt unter seinem generierten Alarm.
# Dann wird die Zeitdifferenz zwischen dem unteren und dem oberen Alarm (dem vorrangehenden, deshalb die lag-Funktion) in Sekunden in der Spalte "Zeitdifferenz" gespeichert.

BettBett.diffTime <- lapply(BettBett, function(x) {
   x %>%

      arrange(Alarm, Alarmfarbe, Zeit) %>%
      mutate(Zeitdifferenz = as.numeric(TrueTime - lag(TrueTime), units = 'secs'))
})

# Nun füge ich die einzelnen Dataframes der Betten wieder zu einem großen Dataframe zusammen
Join <- do.call(rbind,BettBett.diffTime)

# und lösche die Zeitdifferenzen ohne Bedeutung, da nach der obreren Methode z.B. auch die Differenz zischen dem letzten beendeten
# und dem erneut generierten Alarm berechnet wird.
Join[which(Join$Situation == "generiert" | is.na(Join$Situation) ), ]$Zeitdifferenz <- NA

# In den Zeitdifferenzen kommen negative Werte vor.
# Manche sind eher klein, z.b. -16 Sekunden, da hier die TrueTime nach der beendeten Zeit liegt. Dass muss ein Bug im Eintragsystem sein. Diese Werte werde ich löschen,
# da ich nicht darauf rückschließen kann, welcher wahre "beendet" Wert hier vorliegt.

# Es gibt aber auch negative Werte, die z.B. -86377 betragen. Diese Werte kommen zustande, wenn z.B. die TrueTime noch im vorherigen Tag liegt (z.B. 23:59:27)
# aber durch mein Vorgehen weiter oben, das Datum des nächsten Tages aus der Zeit-Spalte in die TrueTime Spalte übernommen wird.
# Diese Werte werde ich in die wahre Dauer umrechnen, in dem ich die Differenz zu der Anzahl Sekunden in einem Tag bilde. Also z.B. -86377s + 86400s = 23s.

# Da es nur sehr große negative Werte und sehr kleine  negative Werte gibt, werte ich den Cut-off bei 1 Stunde vor Mitternacht setzten.
# Heißt: Ich bilde die Differenz für negative Werte < -82800, also 3600 Sekunden vor Ende des Tages. Alle negativen Werte > -82800 < 0 werden gelöscht.
# Auch negative Dauern < -86400s werde ich löschen, z.B. extrem lange Differenzen wie -2580577. Diese entstehen,
# wenn der erste Eintrag eines Alarms "beendet" ist, statt "generiert", sodass Differenzen zu anderen unrelatierten Alarmen, die dann zufällig in der Zeile davor
# stehen, berechnet werden. 

IndexToDelete <- which(Join$Zeitdifferenz > -82800 & Join$Zeitdifferenz < 0 | Join$Zeitdifferenz < -86400)
Join[IndexToDelete,]$Zeitdifferenz <- NA

IndexNegVal <- which(Join$Zeitdifferenz <= -82800)
Join[IndexNegVal,]$Zeitdifferenz <- Join[IndexNegVal,]$Zeitdifferenz + 86400



# --------------------------------------------- DAUER VON PAUSEN BERECHNEN -----------------------------------------------------------

# Um die Pausen sinnvoll zu gruppieren, dürfen sie nicht Pause Ein, Pause Aus heißen, da sonst die Differenzen zwischen Pause Ein und Pause Ein, bzw. Aus
# und Aus berechtnet, wird, da diese beiden zusammengruppiert werden. Heißt: Ich muss eine Dummyspalte machen, die nur Pause einträgt, egal, ob ein oder aus.

# DummyPause kreieren:
Join$DummyPause <- NA

# Pause eintragen wo eine Pause ein oder ausgeschaltet wurde
Join$DummyPause[!is.na(Join$Alarmpause)] <- "Pause"

# als Faktor formatieren
Join$DummyPause <- as.factor(Join$DummyPause)

# Erstellt eine Liste mit Dataframes für die einzelnen Betten
BettBett <- split(Join, Join$Bettname)

# Wie oben sortiert die Funktion die Bettendatensätze nach Zeit und nach "Pause", sodass die beendete Pause immer direkt unter
# der eingeschalteten Pause steht. Hier kann sich zeitlich nichts überschneiden, da man eine Pause am Bett erst auschalten muss, bevor sie wieder eingeschaltet werden kann.
# Dann wird die Zeitdifferenz zwischen dem unteren und dem oberen Pauseneintrag in Sekunden in der Spalte "PausenDauer" gespeichert.

BettBett.diffTime <- lapply(BettBett, function(y) {
   y %>%
      
      arrange(DummyPause, Zeit) %>%
      mutate(PausenDauer = as.numeric(TrueTime - lag(TrueTime), units = 'secs')) 
})


# die Bett-DFs dann wieder zusammenzuführen
Join <- do.call(rbind,BettBett.diffTime)

# die Zeitdifferenzen ohne Bedeutung löschen
Join[is.na(Join$Alarmpause), ]$PausenDauer <- NA
Join[which(Join$Alarmpause == "Pause Ein"), ]$PausenDauer <- NA

# Pausendauern die >190s sind löschen. 180s, bzw 3 Minuten ist die maximale Pausenlänge der Monitore.
# Philips Manual Seite 65: "Je nach Monitorkonfiguration werden die Alarme für eine, zwei oder drei Minuten oder unbegrenzt ausgesetzt
# Eine unbegrenzte Alarmpause entspricht dem Ausschalten der Alarme.

# Ich setzte die Grenze bei 190s statt 180s direkt, da es scheinbar eine 4-7 sekündige Verzögerung zum Eintrag in das Datenset gibt:
# die meisten Pausen dauern 185s. Alle größeren Werte sind dadurch entstanden, dass für ein Bett der erste Eintrag "Pause Aus" ist, oder, 
# das aus irgendeinem Grund "Pause Ein" nicht in das Datenset eingetragen wurde.

IndexToDeletePause <- which(Join$PausenDauer > 190 | Join$PausenDauer < 0)
Join[IndexToDeletePause,]$PausenDauer <- NA

# wieder nach Zeit absteigend sortieren, wie im Originaldatensatz
Join <- Join[order(Join$Zeit, decreasing = TRUE),]


# Join in data.new umwandeln, um die Nachfolgenden Visualisierungen nicht neu schreiben zu müssen.
# Join enthält eine Zeile weniger als data.new, da der letzte Eintrag in Data.new für den Bettnamen NA und somit eh irrelevant ist.

data.new <- Join

# Das Environment bereinigen
rm(BettBett, BettBett.diffTime, Join, IndexToDeletePause, IndexToDelete, IndexNegVal)



# --------------------------------------------- SCHICHTZEITEN EINTRAGEN -----------------------------------------------------------

# Die hier verwendeten Schichtzeiten:
# Früh: 6:36-14:42
# Spät: 14:06-22:24
# Nacht: 21:51-07:09

# Übergabe Früh Spät: 14:06 bis 14:42
# Übergabe Spät Nacht: 21:51 bis 22:24
# Übergabe Nacht Früh: 6:36 bis 07:09

data.new$Schicht <- NA

# Um die Schichtzeiten einzutragen, muss ich zunächst eine Dummy Spalte kreieren, in der jeder Zeiteintrag das gleiche Datum hat.
dummytimes <- as.POSIXlt(data.new$TrueTime)    # convert to 'POSIX list type'
dummytimes$mday <- dummytimes[1]$mday # setzt den Tag des Dummy Datums auf den Tag des ersten Eintrags im DF
dummytimes$mon <- dummytimes[1]$mon # setzt den Monat des Dummy Datums auf den Monat des ersten Eintrags im DF
dummytimes$sec <- 0 # auf 0 setzten, damit auch z.B. 22:24:00 schon als "Nacht" eingetragen wird, statt "SpätNacht"
dummytimes <- as.POSIXct(dummytimes)   # convert back to POSIXc t

# Nun eine Spalte mit dem Dummy Datum ans DF hinzufügen, es unterscheiden sich nur noch die Tageszeiten
data.new$dummytimes <- dummytimes

# Das Dummy Datum ohne Uhrzeit extrahieren, damit es variabel in die Intervallerstellung eingefügt werden kann.
DummyDate <- as.Date(data.new$dummytimes[1])

# Nun Intervalle definieren, die dann die Schichtzeiten bestimmen, mit dem Dummydate variabel eingefügt
## Frühschicht Interval
FR.Anfang <- as.POSIXct(sprintf('%s 06:37:00', DummyDate))
FR.Ende <- as.POSIXct(sprintf('%s 14:41:00', DummyDate))

FR.int <- interval(FR.Anfang, FR.Ende)

## Spätschicht Interval
SP.Anfang <- as.POSIXct(sprintf('%s 14:07:00', DummyDate))
SP.Ende <- as.POSIXct(sprintf('%s 22:23:00', DummyDate))

SP.int <- interval(SP.Anfang, SP.Ende)

## Nachtschicht Interval
NA.Anfang <- as.POSIXct(sprintf('%s 21:52:00', DummyDate))
NA.Ende <- as.POSIXct(sprintf('%s 07:08:00', DummyDate))

NA.int <- interval(NA.Anfang, NA.Ende)

## Übergabe Früh Spät Interval
UFS.Anfang <- as.POSIXct(sprintf('%s 14:06:00', DummyDate))
UFS.Ende <- as.POSIXct(sprintf('%s 14:41:00', DummyDate))

UFS.int <- interval(UFS.Anfang, UFS.Ende)

## Übergabe Spät Nacht Interval
USN.Anfang <- as.POSIXct(sprintf('%s 21:51:00', DummyDate))
USN.Ende <- as.POSIXct(sprintf('%s 22:23:00', DummyDate))

USN.int <- interval(USN.Anfang, USN.Ende)

## Übergabe Nacht Früh Interval
UNF.Anfang <- as.POSIXct(sprintf('%s 06:36:00', DummyDate))
UNF.Ende <- as.POSIXct(sprintf('%s 07:08:00', DummyDate))

UNF.int <- interval(UNF.Anfang, UNF.Ende)


# Nun die Schichten eintragen, basierend auf dem Interval.
data.new$Schicht <- ifelse(data.new$dummytimes %within% FR.int, "Früh",
                           ifelse(data.new$dummytimes %within% SP.int, "Spät",
                                  "Nacht"))


# Und nun die Übergabezeiten
data.new$Schicht[data.new$dummytimes %within% UFS.int] <-  c("FrühSpät")
data.new$Schicht[data.new$dummytimes %within% USN.int] <-  c("SpätNacht")
data.new$Schicht[data.new$dummytimes %within% UNF.int] <-  c("NachtFrüh")


# entfernen der Dummy Spalte aus dem DF
data.new %>% select(-dummytimes) %>% {.->> data.new}

# entfernen der Intervalvariablen
rm(DummyDate, dummytimes,
   FR.Anfang, FR.Ende, FR.int,
   SP.Anfang, SP.Ende, SP.int,
   NA.Anfang, NA.Ende, NA.int,
   UFS.Anfang, UFS.Ende, UFS.int,
   UNF.Anfang, UNF.Ende, UNF.int,
   USN.Anfang, USN.Ende, USN.int)

# Als Faktor formatieren
data.new$Schicht <- as.factor(data.new$Schicht)

# Die Levels des Faktors sinnvoll sortieren
data.new$Schicht <- factor(data.new$Schicht,levels = c("Nacht", "NachtFrüh", "Früh", "FrühSpät", "Spät", "SpätNacht"))



# --------------------------------------------- FINALISIEREN UND EXPORTIEREN -----------------------------------------------------------

# Die Spalten sinnvoll ordnen und Dummy-Spalten weglassen
data.new <- data.new[c("Zeit", "Bettname", "Monitor", "TrueTime", "Aktion", "Alarm", "Situation", "Zeitdifferenz",
                       "Alarmfarbe", "Alarmgruppe", "Alarmpause", "PausenDauer", "Richtung", "Schicht", "Bettenanzahl", "Grenzwert", "Zimmernummer")]

# das Datenset so sortieren wie es Anfangs war, nach Zeit absteigend sortiert und entsprechenden rownames

data.new <- data.new[order(data.new$Zeit , decreasing = TRUE),]
rownames(data.new) <- 1:nrow(data.new)


# exportieren

write.csv(data.new, file = "Alarmdata.csv", row.names = FALSE)



##### Eine Übesicht mit allen Alarmen, ihren Farben und Gruppen
Overview.Alarms <- as.data.frame(unique(data.new[c("Alarm", "Alarmfarbe", "Alarmgruppe")]))

   # Faktoren zu character strings umwandeln um Änderungen zu ermöglichen
   Overview.Alarms$Alarmfarbe <- as.character(Overview.Alarms$Alarmfarbe)
   Overview.Alarms$Alarmgruppe <- as.character(Overview.Alarms$Alarmgruppe)
   
   # Für Sparseness Alarme die gelb und rot sein können zusammenfassen
   Overview.Alarms[which(duplicated(Overview.Alarms$Alarm) | duplicated(Overview.Alarms$Alarm, fromLast = TRUE)),]$Alarmfarbe <- "gelb/rot"
   
   # duplizierte Zeilen löschen
   Overview.Alarms <- Overview.Alarms[!duplicated(Overview.Alarms$Alarm),]
   
   # Alarme die wir bewusst nicht zu Sensoren zugewiesen haben kennzeichnen
   Overview.Alarms[which(!is.na(Overview.Alarms$Alarm) & is.na(Overview.Alarms$Alarmgruppe)),]$Alarmgruppe <- "Ausgeschlossen"
   
   # remove NAs
   Overview.Alarms <- na.omit(Overview.Alarms)
   
   # Nach Sensorgruppe sortieren
   Overview.Alarms <- Overview.Alarms[order(Overview.Alarms$Alarmgruppe, decreasing = FALSE),]
  
   # als .csv exportieren
   write.csv(Overview.Alarms, file = "Overview_Alarms.csv", row.names = FALSE)

   
   
   
          ##########################################################################          #######################################################################
         ############################################################################# ENDE ##########################################################################
          ##########################################################################          #######################################################################