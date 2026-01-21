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
median(titanic$Age[titanic$Anrede %in% c("Miss.", "Ms.", "Mlle.")],
       na.rm = TRUE)
## 21
median(titanic$Age[titanic$Anrede %in% c("Mrs.", "Mme.", "Lady.",
                                       "the Countess.")], na.rm = TRUE)
## 35
median(titanic$Age[titanic$Anrede %in% "Master."], na.rm = TRUE)
## 3.5
median(titanic$Age[titanic$Anrede %in% c("Mr.", "Don.", "Sir.", "Jonkheer.",
                                         "Rev.", "Dr.")], na.rm = TRUE)
## 30


# NA-Werte für young_woman ersetzen
titanic$Age[is.na(titanic$Age) &
              titanic$Anrede %in% c("Miss.", "Ms.", "Mlle.")
            ] <- median(titanic$Age[titanic$Anrede %in%
                                      c("Miss.", "Ms.", "Mlle.")], na.rm = TRUE)

# NA-Werte für older_woman ersetzen
titanic$Age[is.na(titanic$Age) &
              titanic$Anrede %in% c("Mrs.", "Mme.", "Lady.",
                                    "the Countess.")
            ] <- median(titanic$Age[titanic$Anrede %in%
                          c("Mrs.", "Mme.", "Lady.", "the Countess.")],
                            na.rm = TRUE)

# NA-Werte für boys ersetzen
titanic$Age[is.na(titanic$Age) & titanic$Anrede %in% "Master."
            ] <- median(titanic$Age[titanic$Anrede %in% "Master."],
                        na.rm = TRUE)

# NA-Werte für adult_men ersetzen
titanic$Age[is.na(titanic$Age) &
              titanic$Anrede %in% c("Mr.", "Don.", "Sir.", "Jonkheer.", "Rev.",
                                    "Dr.")
            ] <- median(titanic$Age[titanic$Anrede %in%
                          c("Mr.", "Don.", "Sir.", "Jonkheer.", "Rev.", "Dr.")],
                            na.rm = TRUE)

# Test ob alle NA-Werte ersetzt wurden
sum(is.na(titanic$Age))

# =============================================================================

