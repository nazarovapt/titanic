# =============================================================================
# Titel:    Deskriptive Analyse und Visualisierung des aufgeräumten 
#           Titanic-Datensatzes
# Autoren:  Heetae Kim, CIA, Nazarov Sergei
# =============================================================================


# vorbereiteter Datensatz und Analysefunktionen laden
titanic <- read.csv("../titanic_new.csv", stringsAsFactors = TRUE)
source("analysis_functions.R")

# =============================================================================
# Übersicht "titanic_new.csv":
# =============================================================================
# Der Datensatz enthält bereinigte Passagierdaten der Titanic.
# Enthalten sind sowohl metrische Variablen (z.B. Age, Fare), als auch 
# kategoriale Variablen (z.B. Survived, Sex, Pclass).
# Fehlende Werte in Age wurden imputiert, irrelevante Variablen entfernt.
# =============================================================================
# Geplante Untersuchungen:
# 1. Deskriptive Statistiken für metrische Variablen (Age, Fare, SibSp, Parch)
# 2. Deskriptive Statistiken für kategoriale Variablen (Survived, Sex, Pclass)
# 3. Zusammenhang zwischen zwei kategorialen Variablen (z.B. Survived und Sex)
# 4.
# 5.
# 6.
# =============================================================================

# =============================================================================
# 1. Deskriptive Statistik für metrische Variablen
num_vars <- c("Age", "Fare", "SibSp", "Parch")
num_summary <- numSummary(titanic, num_vars)
num_summary
# Interpretation:
# Die durchschnittliche Altersstruktur der Passagiere liegt bei etwa 29 Jahren,
# wobei die Altersspanne von Säuglingen bis zu sehr alten Personen reicht.
# Der Ticketpreis (Fare) weist eine sehr hohe Streuung auf, was durch die große
# Standardabweichung und den hohen Maximalwert deutlich wird. Dies deutet auf
# eine stark rechtsschiefe Verteilung der Ticketpreise hin.
# Die Variablen SibSp und Parch zeigen niedrige Mittelwerte, was darauf
# hindeutet, dass die meisten Passagiere alleine oder mit wenigen Angehörigen
# gereist sind.

# Histogramm des Ticketpreises
hist(titanic$Fare,
     breaks = 30,
     main = "Verteilung des Ticketpreises (Fare)",
     xlab = "Ticketpreis",
     col = "lightblue",
     xlim = c(0, 150))
# =============================================================================

# =============================================================================
# 2. 
# =============================================================================

# =============================================================================
# 3.
# =============================================================================

# =============================================================================
# 4. 
# =============================================================================

# =============================================================================
# 5. 
# =============================================================================

# =============================================================================
# 6. 
# =============================================================================


