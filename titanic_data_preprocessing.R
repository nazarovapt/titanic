# ============================================================================= 
# Titel:    Titanic-Datensatz – Datenvorverarbeitung
# Datei:    titanic_data_preprocessing.R
# Autoren:  Meico Bastian Heil, Saskia Lapkowski
# Datum:    2026-01-21
# Zweck:    Datenaufbereitung und Auswertung
# ============================================================================= 

#Datensatz einlesen
titanic <- read.csv("~/titanic.csv")

## 1.

# Extrahiert aus dem Namen eine Variable mit der Anrede der Person, d.h.
# „Mr.“, „Mrs.“, „Mse.“ usw., damit später fehlende Werte im Alter ersetzt
# werden können. Beachtet hierbei, dass gewisse Anreden wie „Ms.“, „Miss.“
# oder „Mlle“ inhaltlich gleichbedeutend sind (in diesem Beispiel eine junge,
# unverheiratete Frau). Die Anrede „Master“ bezeichnet einen kleinen Jungen.

?gsub
?regex

# Alles vor dem Komma wird ignoriert, dann beliebig viele Leerzeichen,
# in () der Titel bis zum nächsten Punkt, alles nach dem Punkt wird ignoriert
titanic$Anrede <- gsub(".*,\\s*([A-Za-z ]+\\.).*", "\\1", titanic$Name)
unique(titanic$Anrede)

# Anreden werden nach Alter und Geschlecht gruppiert
young_woman <- titanic$Anrede %in% c("Miss.", "Ms.", "Mlle.")
older_woman <- titanic$Anrede %in% c("Mrs.", "Mme.", "Lady.", "the Countess.")
boys <- titanic$Anrede == "Master."
adult_men <- titanic$Anrede %in% c("Mr.", "Don.", "Sir.", "Jonkheer.", "Rev.",
                                   "Dr.")

# ============================================================================= 

# Codiert die Variablen „Survived“, „Sex“, „Embarked“ als factor um.


?as.factor

# Die drei Spalten "Survived", "Sex", "Embarked" in Faktoren umwandeln
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

# ============================================================================= 

# Überführt die Variable „Pclass“ in einen ordered-factor. (ordinal mit 1>2>3)

titanic$Pclass <- factor(titanic$Pclass,
                         levels = c(3,2,1),
                         ordered = TRUE)

# =============================================================================

# Imputiert fehlende Werte in der Variable „Age“ mithilfe der erzeugten
# Variable „Anrede“ über ein Imputationsverfahren eurer Wahl
# (z.B. arithmetisches Mittel,Median, usw.)

?median

# Medianes Alter für die gruppierten Anreden berechnen
median(titanic$Age[young_woman], na.rm = TRUE)
## 21
median(titanic$Age[older_woman], na.rm = TRUE)
## 35
median(titanic$Age[boys], na.rm = TRUE)
## 3.5
median(titanic$Age[adult_men], na.rm = TRUE)
## 30


# NA-Werte für young_woman ersetzen
# NA-Werte für young_woman ersetzen
titanic$Age[is.na(titanic$Age) & young_woman] <- median(titanic$Age[young_woman]
                                                        , na.rm = TRUE)

# NA-Werte für older_woman ersetzen
titanic$Age[is.na(titanic$Age) & older_woman] <- median(titanic$Age[older_woman]
                                                        ,na.rm = TRUE)

# NA-Werte für boys ersetzen
titanic$Age[is.na(titanic$Age) & boys] <- median(titanic$Age[boys],
                                                 na.rm = TRUE)

# NA-Werte für adult_men ersetzen
titanic$Age[is.na(titanic$Age) & adult_men] <- median(titanic$Age[adult_men], 
                                                      na.rm = TRUE)

# Test ob alle NA-Werte ersetzt wurden
sum(is.na(titanic$Age))

# =============================================================================

# Extrahiert aus der Variable „Cabin“ die folgenden Informationen und erzeugt
# neue Variablen hierfür:
# - Backbord oder Steuerbord? Tipp: Kabinen mit einer ungeraden
#   Nummer liegen auf Steuerbord, die anderen auf Backbord.
# - Deck: Vorangehender Buchstabe der Kabinennummer
# - Einträge mit unbekannter Kabinennummer, d.h. „“ setzt ihr auf NA.


# Es wird in den Cabin-Einträgen nach Zahlen gesucht und nur die werden in
# cabin_nums gespeichert
cabin_nums <- gsub(".*([0-9]+)", "\\1", titanic$Cabin)

# Einträge ohne Kabinennumer werden auf NA gesetzt
cabin_nums[cabin_nums == titanic$Cabin] <- NA

# Char in num umwandeln um rechnen zu können
cabin_nums <- as.numeric(cabin_nums)

# Gerade Zahlen sind backbord, ungerade steuerbord
# In backbord und steuerbord werden die Kabinen gespeichert
backbord <- titanic[cabin_nums %% 2 == 0 & !is.na(cabin_nums), 11]
steuerbord <- titanic[cabin_nums %% 2 == 1 & !is.na(cabin_nums), 11]

# Es werden die Buchstaben extrahiert und in deck gespeichert
deck <- gsub("([A-Z]).*", "\\1", titanic$Cabin)

# Einträge ohne Kabinennummer werden auf NA gesetzt
deck[deck == ""] <- NA

# =============================================================================

# Entfernt am Ende die Variablen „PassengerID“, „Name“, „Ticket“ und „Cabin“
# aus dem Datensatz

titanic <- titanic [, !names(titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin")]


# =============================================================================
# Speichert das R-Skript, sowie den neuen Datensatz in dem GitHub-Repository
# ab.

?write.csv()

# data.frame neu zuweisen
titanic_new <- titanic

# CSV-Datei erstellen, die den Datensatz beinhaltet
write.csv(titanic_new, "titanic_new", row.names = FALSE)

# =============================================================================



