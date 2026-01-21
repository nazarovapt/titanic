# ============================================================================= 
# Titel:    Analyse der Verkaufsdaten
# Datei:    analyse_verkauf.R
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
adult_men <- titanic$Anrede %in% c("Mr.", "Don.", "Sir.", "Jonkheer.")

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
