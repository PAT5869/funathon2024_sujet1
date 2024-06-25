# On définit l'URL des données
STATIONS_DATA_URL <- "https://www.data.gouv.fr/fr/datasets/r/d22ba593-90a4-4725-977c-095d1f654d28"

stations_data <- read.csv2(STATIONS_DATA_URL)

ok