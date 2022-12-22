############################
#
#   Clasificacion de tws
#
############################

# Packages
library(tidyverse)
library(stringr)
library(stringdist)
library(magrittr)
library(corpus)
library(caret)
library(tidytext)
library(ggplot2)
library(tm)
library(labelled)
library(haven)

# Paths
# path = "G:/pado_sentencing/twitter/datos"

# Setear directory
setwd(path)

# Cargar datos
tws <- read.csv("tws_01012016_31122016.csv")

# Limpiar tws de caracteres 
tws$text = gsub("http.+|\n|\"", "", tws$text)

# Quitar espacios al final
tws$text = gsub(" $", "", tws$text)

# Transformar todo a minusculas
tws$text = tolower(tws$text)

# Quitar stopwords
tws$text <- removeWords(tws$text, stopwords("spanish"))

# Quitar tildes
tws$text = stringi::stri_trans_general(tws$text, "Latin-ASCII")

# Quitar numeros
tws$text = gsub("[0-9]", "", tws$text)

# Formatear datetime y crear date
tws$created_at = as.POSIXct(strptime(tws$created_at, format = "%Y-%m-%dT%H:%M:%S"))
tws$date = as.Date(strptime(tws$created_at, format = "%Y-%m-%d"))

# Keywords
words = "asaltada|mafioso|disparo|prision|rapiñeros|policia|delincuente|narco|delito|tiroteo|detenido|baleado|crimen|detenida|baleada|patrullero|preso|violencia|violar|asesinada|incautan|allanmiento|inseguridad|hurto|mata|atropella|robaron|procesar|homicidio|contrabando|pornografico|denuncia|carcel|copar|cartel|condena|victima|abuso|motin|sospechoso|violador|detuvieron|asesinado|delincuencia|robar|asaltar|ladron|hirio|asalto|robo|sicario|balazos|patrulla|asaltado|mafiosa|sospechosa|narcomenudeo|microtrafico|sicariato"

# Variable crime
tws$crime = ifelse(str_detect(tws$text,
                              words), 1, 0)

### Generar dta con ratio + ponderaciones

base <- tws %>%
  group_by(user_username) %>%
  mutate(followers_mean = mean(unique(user_followers_count)))

followers_total = sum(unique(base$followers_mean))

base <- base %>%
  mutate(crime_pond = (1*like_count + 2*retweet_count + 3*quote_count)*crime,
         crime_pond2 = (followers_mean/followers_total)*crime) %>%
  group_by(date) %>%
  summarise(crime_n = sum(crime),
            crime_ratio = (sum(crime)/length(crime)*100),
            crimepond_ratio = (sum(crime_pond)/length(crime_pond))*100,
            crimepond2_ratio = (sum(crime_pond2)/length(crime_pond))*100) %>%
  mutate(crime_ratio_n = (crime_ratio-min(crime_ratio))/(max(crime_ratio)-min(crime_ratio)),
         crimepond_ratio_n = (crimepond_ratio-min(crimepond_ratio))/(max(crimepond_ratio)-min(crimepond_ratio)),
         crimepond2_ratio_n = (crimepond2_ratio-min(crimepond2_ratio))/(max(crimepond2_ratio)-min(crimepond2_ratio)))

# Agregar label
base <- base %>%
  set_variable_labels(date = "Fecha",
                      crime_n = "Cantidad de tws con contenido de crimen",
                      crime_ratio = "Ratio diario de tws con contenido de crimen",
                      crimepond_ratio = "Ratio diario de tws con contenido de crimen, ponderado por mg, rt y citas",
                      crimepond2_ratio = "Ratio diario de tws con contenido de crimen, ponderado por # de seguidores",
                      crime_ratio_n = "crime_ratio normalizado",
                      crimepond_ratio_n = "crimepond_ratio normalizado",
                      crimepond2_ratio_n = "crimepond2_ratio normalizado")

# Exportar dta
write_dta(base, "tweets_base.dta")

### Analisis descriptivo

## Crime por dia

# Estadísticas
daily = aggregate(crime ~ date, tws, sum)
daily$tweets = aggregate(crime ~ date, tws, FUN=length)
daily$ratio=daily$crime/daily$tweets$crime*100

# Scatter plot
plot(daily$date, daily$ratio)

# Setear cutoff
cutoff <- as.Date("2016/04/11")

# Plot
ggplot(daily, aes(date, ratio)) +
  geom_point() +
  geom_smooth(aes(group = date >= cutoff),
              method = "lm")



