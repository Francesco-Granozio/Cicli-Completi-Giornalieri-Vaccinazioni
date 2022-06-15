# Fonte: https://www.rainews.it/ran24/speciali/2020/covid19/vaccini.php
# Numero giornaliero di cicli di vaccinazione completi
# Range date: 01-Aprile-2022 - 01-Giugno-2022

# Inserimento delle date e dei valori in due vettori
# che diventeranno le colonne del datagramma utilizzato per tenere traccia dei dati
date <- c(
  "01-Aprile-2022",
  "02-Aprile-2022",
  "03-Aprile-2022",
  "04-Aprile-2022",
  "05-Aprile-2022",
  "06-Aprile-2022",
  "07-Aprile-2022",
  "08-Aprile-2022",
  "09-Aprile-2022",
  "10-Aprile-2022",
  "11-Aprile-2022",
  "12-Aprile-2022",
  "13-Aprile-2022",
  "14-Aprile-2022",
  "15-Aprile-2022",
  "16-Aprile-2022",
  "17-Aprile-2022",
  "18-Aprile-2022",
  "19-Aprile-2022",
  "20-Aprile-2022",
  "21-Aprile-2022",
  "22-Aprile-2022",
  "23-Aprile-2022",
  "24-Aprile-2022",
  "25-Aprile-2022",
  "26-Aprile-2022",
  "27-Aprile-2022",
  "28-Aprile-2022",
  "29-Aprile-2022",
  "30-Aprile-2022",
  "01-Maggio-2022",
  "02-Maggio-2022",
  "03-Maggio-2022",
  "04-Maggio-2022",
  "05-Maggio-2022",
  "06-Maggio-2022",
  "07-Maggio-2022",
  "08-Maggio-2022",
  "09-Maggio-2022",
  "10-Maggio-2022",
  "11-Maggio-2022",
  "12-Maggio-2022",
  "13-Maggio-2022",
  "14-Maggio-2022",
  "15-Maggio-2022",
  "16-Maggio-2022",
  "17-Maggio-2022",
  "18-Maggio-2022",
  "19-Maggio-2022",
  "20-Maggio-2022",
  "21-Maggio-2022",
  "22-Maggio-2022",
  "23-Maggio-2022",
  "24-Maggio-2022",
  "25-Maggio-2022",
  "26-Maggio-2022",
  "27-Maggio-2022",
  "28-Maggio-2022",
  "29-Maggio-2022",
  "30-Maggio-2022",
  "31-Maggio-2022",
  "01-Giugno-2022"
)

ciclo_completo <-
  c(
    4338,
    4478,
    1041,
    3637,
    3761,
    4070,
    3948,
    4671,
    4355,
    888,
    3776,
    3373,
    3543,
    3470,
    3064,
    2141,
    63,
    148,
    3483,
    3477,
    2836,
    2903,
    2622,
    508,
    183,
    2746,
    2348,
    2340,
    2267,
    2110,
    113,
    1430,
    1570,
    1736,
    1621,
    1676,
    1426,
    404,
    1021,
    1418,
    1466,
    1353,
    1580,
    1353,
    332,
    801,
    1234,
    1366,
    1480,
    1373,
    1299,
    230,
    820,
    1053,
    1347,
    1175,
    1204,
    1195,
    281,
    915,
    982,
    820
  )

# Trasformo i dati raccolti in un dataframe
dataframemio <-
  data.frame(date = date, ciclo_completo = ciclo_completo)
dataframemio


# Frequenze assolute e modalita' del carattere
table(dataframemio$ciclo_completo)
# Si denota che le frequenze assolute e lo modalità del carattere coindidono

# Frequenze relative

# Metodo 1
round(table(dataframemio$ciclo_completo) / length(dataframemio$ciclo_completo),
      digits = 2)
# Metodo 2
round(proportions(table(dataframemio$ciclo_completo)), digits = 2)

# Frequenze assolute cumulate
cumsum(table(dataframemio$ciclo_completo))

# Frequenze relative cumulate
cumsum(table(dataframemio$ciclo_completo) / length(dataframemio$ciclo_completo))

# Istogramma, considerando che le classi di default utilizzate da R sono esaustive non si specificano gli intervalli delle
# classi
hist(dataframemio$ciclo_completo,
     main = "Istogramma",
     xlab = "Cicli completi giornalieri")

# Calcolo della media
media <- mean(dataframemio$ciclo_completo)
media

# Calcolo della moda
getmode <- function(v) {
  uniqv <- unique(v) # vettore senza duplicati
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda <- getmode(dataframemio$ciclo_completo)
moda # L'unico valore che ha frequenza massima è quindi il valore 1353

# Calcolo della mediana
mediana <- median(dataframemio$ciclo_completo)
mediana

# Calcolo dei quartili
quantili <- quantile(dataframemio$ciclo_completo)
quantili

# Quartili specifici
quantile(dataframemio$ciclo_completo, probs = c(0, 0.25)) # Primo quartile
quantile(dataframemio$ciclo_completo, probs = c(0.25, 0.50)) # Secondo quartile
quantile(dataframemio$ciclo_completo, probs = c(0.50, 0.75)) # Terzo quartie
quantile(dataframemio$ciclo_completo, probs = c(0.75, 1)) # Quarto quartile

# Summary dai dati
summary(dataframemio$ciclo_completo)
# Dal summary si evince che:
# l'ultimo quartile e' il numero maggiore del 100% dei dati e coincide con il massimo
# il secondo quartile coincide con la mediana

# Box plot
boxplot(
  dataframemio$ciclo_completo,
  xlab = "Cicli completi giornalieri",
  col = "green",
  ylim = c(63, 5000) # Scegliamo 5000 come limite superiore dato che utilizzando il massimo 
  # nella roccolta dei dati come limite il box plot non forniva una visualizzazione concreta del range dei valori
)
# [1026 - 1.5(2886 - 1026), 2886 + 1.5(2886 - 1026)]

# Calcolo dei cardini
cardine1 <-
  quantile(dataframemio$ciclo_completo)[[2]] - 1.5 * (quantile(dataframemio$ciclo_completo)[[4]] - quantile(dataframemio$ciclo_completo)[[2]])
cardine1

cardine2 <-
  quantile(dataframemio$ciclo_completo)[[4]] + 1.5 * (quantile(dataframemio$ciclo_completo)[[4]] - quantile(dataframemio$ciclo_completo)[[2]])
cardine2

ciclo_completo_new <-
  ifelse((dataframemio$ciclo_completo < cardine1) |
           (dataframemio$ciclo_completo > cardine2),
         0,
         dataframemio$ciclo_completo
  )
ciclo_completo_new
# Si evince che non ci sono valori outliers
# e quindi tutti i valori potranno essere rappresentati all'interno del bloxplot

# Indici di dispersione: calcolo della varianza, deviazione standard e coefficiente variazione
# Varianza
var <- var(dataframemio$ciclo_completo)
var

# Deviazione standard
devst <- sd(dataframemio$ciclo_completo)
devst

# Coefficiente di variazione
cv <- devst / media
cv

# Calcolo dell'asimmetria (skewness)
# Calcolo diretto della skewness campionaria
n1 <- length(dataframemio$ciclo_completo)
skw_1 <-
  sum((dataframemio$ciclo_completo - media) ^ 3) / (n1 * var(dataframemio$ciclo_completo) ^ (3 / 2))
skw_1

# Calcolo diretto della curtosi
curt1 <-
  sum((dataframemio$ciclo_completo - media) ^ 4) / (n1 * var(dataframemio$ciclo_completo) ^ (2)) - 3
curt1

# Calcolo con funzione di libreria della skewness campionaria
library(e1071)

skw_2 <- skewness(dataframemio$ciclo_completo)
skw_2

# Calcolo con funzione di libreria della curtosi
curt <- kurtosis(dataframemio$ciclo_completo)
curt

# Possiamo vedere che la skewness è 0.544, quindi,
# >0 quindi la distribuzione dei dati presenta una asimmetria positiva.
# Avendo invece la curtosi uguale a -0.89, quindi,
# <0 si denota che è presente una carenza dei dati nelle classi centrali
# e che quindi i dati hanno una distribuzione platicurtica.
