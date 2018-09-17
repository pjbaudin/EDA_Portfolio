# https://www.data.gouv.fr/fr/datasets/indicateurs-des-contrats-h2020-par-pays-2014-2020-1/#_


setwd("C:/Users/pierr/Documents/GitHub/EDA_Portfolio/France Indicateurs des contrats H2020 par pays 2014-2020")

urlfile <- "https://data.enseignementsup-recherche.gouv.fr//explore/dataset/fr-esr-h2020_pcrdt-indicateurs_pays/download?format=csv&timezone=Europe/Berlin&use_labels_for_header=true"

download.file(urlfile, destfile = "./dataset/df-est-h2020_pcrdt-indicateurs_pays.csv")

df <- read.csv("./dataset/df-est-h2020_pcrdt-indicateurs_pays.csv", header = TRUE, sep = ";")
head(df)
str(df)

df$pays_code <- factor(df$pays_code, exclude = NULL)
summary(df)

library(ggplot2)

ggplot(df, aes(x = nb_projet, y = montant_subvention, label = pays_code, color = zone_pays)) +
  geom_point() +
  geom_text() +
  scale_x_log10() +
  facet_grid(zone_pays ~ .) +
  theme_light() +
  theme(legend.position = "none")

library(dplyr)

df %>%
  group_by(zone_pays) %>%
  mutate(#meanSubventionZone = mean(montant_subvention),
            TotalSubventionZone = sum(montant_subvention),
            #meannbrprojectZone = mean(nb_projet),
            TotalnbProjetZone = sum(nb_projet),
            #meanParticipationZone = mean(nb_participation),
            TotalParticipationZone = sum(nb_participation)) %>%
  ungroup() %>%
  mutate(RatioSubventionZone = montant_subvention / TotalSubventionZone,
         RatioNbrProjectZone = nb_projet / TotalnbProjetZone,
         RatioParticipationZone = nb_participation / TotalParticipationZone) %>%
  select(-geolocalisation, -TotalSubventionZone, -TotalnbProjetZone, -TotalParticipationZone) %>%
  ggplot(aes(x = RatioSubventionZone, group = zone_pays, fill = zone_pays)) +
  geom_histogram(position = "stack", bins = 50) +
  scale_x_log10() +
  facet_grid(zone_pays ~ .)