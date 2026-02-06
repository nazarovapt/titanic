# =============================================================================
# Titel:    Deskriptive Analyse und Visualisierung des aufgeräumten 
#           Titanic-Datensatzes
# Autoren:  Heetae Kim (i - iii), 
#           Leticia Kwanga (iv - vi), 
#           Nazarov Sergei (LaTeX)
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
# 4. Bivariate deskriptive Statistik: metrisch × dichotom (z.B. Age und Fare)
# 5. Visualisierung von drei oder vier kategorialen Variablen (z.B. Pclass, 
#    Embarked, Survived)
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
# Der Ticketpreis weist eine sehr hohe Streuung auf, was durch die große
# Standardabweichung und den hohen Maximalwert deutlich wird. Dies deutet auf
# eine stark rechtsschiefe Verteilung der Ticketpreise hin.
# Die Variablen SibSp und Parch zeigen niedrige Mittelwerte, was darauf hindeutet, 
# dass die meisten Passagiere alleine oder mit wenigen Angehörigen gereist sind.

# Visualisierung
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

# Visualisierung
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# Überlebensstatus der Passagiere
barplot(
  table(titanic$Survived),
  names.arg = c("Nicht überlebt", "Überlebt"),
  main = "Überlebensstatus der Passagiere",
  xlab = "Überlebensstatus",
  ylab = "Anzahl der Passagiere",
  col = "lightgreen",
  ylim = c(0, max(table(titanic$Survived)) * 1.15)
)
text(seq_along(table(titanic$Survived)), table(titanic$Survived), 
     labels = table(titanic$Survived), pos = 3)

# Geschlecht der Passagiere
barplot(
  table(titanic$Sex),
  main = "Geschlechterverteilung der Passagiere",
  xlab = "Geschlecht",
  ylab = "Anzahl der Passagiere",
  col = c("lightpink", "lightblue"),
  ylim = c(0, max(table(titanic$Sex)) * 1.15)
)
text(seq_along(table(titanic$Sex)), table(titanic$Sex), 
     labels = table(titanic$Sex), pos = 3)

# Passagierklassen der Passagiere
barplot(
  table(titanic$Pclass),
  main = "Passagierklassen der Passagiere",
  xlab = "Passagierklasse",
  ylab = "Anzahl der Passagiere",
  col = c("lightblue", "lightgreen", "lightgray"),
  ylim = c(0, max(table(titanic$Pclass)) * 1.15)
)
text(seq_along(table(titanic$Pclass)), table(titanic$Pclass), 
     labels = table(titanic$Pclass), pos = 3)

# Einschiffungshäfen der Passagiere
barplot(
  table(titanic$Embarked),
  main = "Einschiffungshäfen der Passagiere",
  xlab = "Einschiffungshafen",
  ylab = "Anzahl der Passagiere",
  col = "lightgray",
  ylim = c(0, max(table(titanic$Embarked)) * 1.15)
)
text(seq_along(table(titanic$Embarked)), table(titanic$Embarked), 
     labels = table(titanic$Embarked), pos = 3)

par(mfrow = c(1, 1))
# =============================================================================

# =============================================================================
# 3. Bivariate deskriptive Statistik für zwei kategoriale Variablen

# Überlebensanteile nach Geschlecht
# Survived vs. Sex 
biv_surv_sex <- catBivSummary(titanic, "Survived", "Sex")
biv_surv_sex$counts     # absolute Häufigkeiten
biv_surv_sex$percent    # Zeilenprozente pro Survived-Gruppe
# Interpretation:
# Unter den Überlebenden ist der Anteil der Frauen deutlich höher als der der Männer.
# Unter den Nicht-Überlebenden ist der Anteil der Männer größer.
# Visualisierung
par(mar = c(5, 5, 4, 2), oma = c(4, 0, 0, 0))
barplot(
  t(biv_surv_sex$percent),
  beside = FALSE,
  main = "Überlebensstatus nach Geschlecht",
  xlab = "Geschlecht",
  ylab = "Anteil",
  ylim = c(0, 1),
  col = c("grey80", "grey40"),
  names.arg = colnames(biv_surv_sex$percent),
  border = "white"
)
par(xpd = NA)  
legend(
  x = 0.8, y = -0.15,  
  legend = c("Nicht überlebt", "Überlebt"),
  fill = c("grey80", "grey40"),
  horiz = TRUE,
  bty = "n",
  cex = 1
)
par(xpd = FALSE)


# Überlebensanteile nach Passagierklasse
# Survived vs. Pclass 
biv_class_surv <- catBivSummary(titanic, "Pclass", "Survived")
biv_class_surv$counts     # absolute Häufigkeiten
biv_class_surv$percent    # Zeilenprozente innerhalb jeder Klasse
# Interpretation:
# Die Zeilenprozente zeigen deutliche Unterschiede zwischen den Klassen:
# In der 1. Klasse ist der Anteil der Überlebenden höher als in der 2. und 3. Klasse.
# In der 3. Klasse ist der Anteil der Nicht-Überlebenden am höchsten.
# Visualisierung
par(mar = c(5, 5, 4, 2), oma = c(4, 0, 0, 0))
barplot(
  t(biv_class_surv$percent),
  beside = FALSE,
  main = "Überlebensstatus nach Passagierklasse",
  xlab = "Passagierklasse",
  ylab = "Anteil",
  ylim = c(0, 1),
  col = c("grey80", "grey40"),
  names.arg = rownames(biv_class_surv$percent),
  border = "white"
)
par(xpd = NA)  

legend(
  x = 1.1, y = -0.15,  
  legend = c("Nicht überlebt", "Überlebt"),
  fill = c("grey80", "grey40"),
  horiz = TRUE,
  bty = "n",
  cex = 1
)
par(xpd = FALSE)


# Passagierklassen nach Einschiffungshafen
# Embarked vs. Pclass 
biv_emb_class <- catBivSummary(titanic, "Embarked", "Pclass")
# Leere Einschiffungshäfen als NA behandeln
titanic$Embarked[titanic$Embarked == ""] <- NA
biv_emb_class$counts
biv_emb_class$percent <- biv_emb_class$percent[rownames(biv_emb_class$percent) != "", ]
biv_emb_class$percent
# Interpretation:
# Passagiere aus Cherbourg gehörten häufiger der ersten Klasse an, was auf einen 
# höheren sozioökonomischen Status hindeutet und den höheren Überlebensanteil erklären kann.
# Visualisierung
par(mar = c(5, 5, 4, 2), oma = c(4, 0, 0, 0))
barplot(
  t(biv_emb_class$percent),
  beside = FALSE,
  main = "Passagierklassen nach Einschiffungshafen",
  xlab = "Einschiffungshafen",
  ylab = "Anteil",
  ylim = c(0, 1),
  col = c("lightblue", "lightgreen", "lightgray"),
  names.arg = rownames(biv_emb_class$percent),
  border = "white"
)
par(xpd = NA) 
legend(
  x = 0.9, y = -0.15,
  legend = c("1. Klasse", "2. Klasse", "3. Klasse"),
  fill = c("lightblue", "lightgreen", "lightgray"),
  horiz = TRUE,
  bty = "n"
)
par(xpd = FALSE)
# =============================================================================

# =============================================================================
# 4. Bivariate deskriptive Statistik: metrisch × dichotom
# Unterscheidet sich das Alter und der Ticketpreis der Passagiere in 
# Abhängigkeit vom Überlebensstatus?

# Lokale Kopie
df4 <- titanic

# Typzuweisungen
df4$Survived <- factor(df4$Survived, levels = c(0,1),
                       labels = c("Nicht überlebt","Überlebt"))
df4$Age  <- as.numeric(as.character(df4$Age))
df4$Fare <- as.numeric(as.character(df4$Fare))

# NA lokal entfernen
df4 <- remove_na(df4, c("Age", "Fare", "Survived"))

# Deskriptive Statistik
age_survival_summary  <- metricDichoSummary(df4, "Age", "Survived")
fare_survival_summary <- metricDichoSummary(df4, "Fare", "Survived")

# Boxplots
par(mfrow = c(1,2))
boxplot(Age ~ Survived, data = df4,
        names = levels(df4$Survived),
        col = c("lightgray","lightgreen"),
        main = "Alter nach Überlebensstatus",
        xlab = "Überlebensstatus", ylab = "Alter")

boxplot(Fare ~ Survived, data = df4,
        names = levels(df4$Survived),
        col = c("lightgray","lightgreen"),
        main = "Ticketpreis nach Überlebensstatus",
        xlab = "Überlebensstatus", ylab = "Ticketpreis")
par(mfrow = c(1,1))

age_survival_summary
fare_survival_summary

# =============================================================================

# =============================================================================
# 5. Visualisierung von drei bzw. vier kategorialen Variablen
# Wie hängen Passagierklasse, Einschiffungshafen und Überlebensstatus
# zusammen und unterscheidet sich das nach Geschlecht?

# Lokale Kopie
df5 <- titanic
df5$Pclass   <- factor(df5$Pclass)
df5$Survived <- factor(df5$Survived, levels = c(0,1),
                       labels = c("Nicht überlebt","Überlebt"))
df5$Embarked <- factor(df5$Embarked)
df5$Sex      <- factor(df5$Sex)

# Facettiertes Balkendiagramm: 3 Variablen
plot_categorical_faceted(df5,
                         var_x = "Pclass",
                         var_fill = "Survived",
                         var_facet1 = "Embarked") + 
  ggtitle("Überlebensstatus nach Passagierklasse und Einschiffungshafen")
# =============================================================================

# =============================================================================
# 6. 
# =============================================================================


