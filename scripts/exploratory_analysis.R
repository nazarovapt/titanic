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
# 2. Deskriptive Statistik für kategoriale Variablen
cat_vars <- c("Survived", "Sex", "Pclass", "Embarked")

cat_results <- lapply(cat_vars, function(v) {
  catSummary(titanic, v)
})

names(cat_results) <- cat_vars

# Ergebnisse anzeigen
cat_results$Survived
cat_results$Sex
cat_results$Pclass
cat_results$Embarked

# Interpretation:
# Insgesamt war der Anteil der Passagiere, die die Katastrophe nicht überlebt 
# haben, höher als der Anteil der Überlebenden.
# In Bezug auf die Geschlechterverteilung befanden sich deutlich mehr männliche 
# als weibliche Passagiere an Bord. Zudem reiste der Großteil der Passagiere 
# in der dritten Klasse, während Passagiere der ersten Klasse vergleichsweise 
# selten vertreten waren.
# Hinsichtlich der Einschiffungshäfen zeigt sich, dass der größte Anteil
# der Passagiere in Southampton eingeschifft ist.

# Überlebensstatus der Passagiere
barplot(
  table(titanic$Survived),
  names.arg = c("Nicht überlebt", "Überlebt"),
  main = "Überlebensstatus der Passagiere",
  xlab = "Überlebensstatus",
  ylab = "Anzahl der Passagiere",
  col = "lightgreen",
  ylim = c(0, max(table(titanic$Survived)) + 50)
)
text(
  x = c(0.7, 1.9),
  y = table(titanic$Survived),
  labels = table(titanic$Survived),
  pos = 3
)

# Geschlecht der Passagiere
barplot(
  table(titanic$Sex),
  main = "Geschlechterverteilung der Passagiere",
  xlab = "Geschlecht",
  ylab = "Anzahl der Passagiere",
  col = c("lightpink", "lightblue"),
  ylim = c(0, max(table(titanic$Sex)) + 50)
)
text(
  x = c(0.7, 1.9),
  y = table(titanic$Sex),
  labels = table(titanic$Sex),
  pos = 3
)

# Passagierklassen der Passagiere
barplot(
  table(titanic$Pclass),
  main = "Passagierklassen der Passagiere",
  xlab = "Passagierklasse",
  ylab = "Anzahl der Passagiere",
  col = c("lightblue", "lightgreen", "lightgray"),
  ylim = c(0, max(table(titanic$Pclass)) + 50)
)

# Anzahl über den Balken anzeigen
text(
  x = c(0.7, 1.9, 3.1),
  y = table(titanic$Pclass),
  labels = table(titanic$Pclass),
  pos = 3
)
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


