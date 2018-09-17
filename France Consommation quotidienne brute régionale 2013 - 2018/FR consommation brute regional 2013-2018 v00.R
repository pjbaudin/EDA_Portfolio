# https://www.data.gouv.fr/fr/datasets/consommation-quotidienne-brute-regionale-2013-2018/#_

setwd("C:/Users/pierr/Documents/GitHub/EDA_Portfolio/France Consommation quotidienne brute régionale 2013 - 2018")

urlfile <- "https://opendata.reseaux-energies.fr/explore/dataset/consommation-quotidienne-brute-regionale/download?format=csv&timezone=Europe/Berlin&use_labels_for_header=true"


download.file(urlfile, destfile = "./dataset/consommation-quotidienne-brute-regionale.csv")

df <- read.csv("./dataset/consommation-quotidienne-brute-regionale.csv",
               header = TRUE, sep = ";", na.strings=c(" ", ""))

# Date - Heure
# Date
# Heure
# Code INSEE région
# Région
# Consommation brute gaz (MW PCS 0°C) - GRTgaz
# Qualité donnée gaz - GRTgaz
# Consommation brute gaz (MW PCS 0°C) - TIGF
# Qualité donnée gaz - TIGF
# Consommation brute électricité (MW) - RTE
# Qualité donnée électricité - RTE

library(dplyr)
library(lubridate)

df_processed <- df %>%
  select(-Date, -Heure)

df_processed$Date...Heure <- ymd_hms(df_processed$Date...Heure)

names(df_processed) <- c("Time", "Code", "Region",
                         "ConsoBrutGRT", "QA_GTR",
                         "ConsoBrutTIGF", "QA_TIGF",
                         "ConsoBrutElecRTE", "QA_RTE")

head(df_processed)
str(df_processed)

library(reshape2)

dfplot <-  melt(df_processed, id=c("Time", "Code", "Region",
                                   "QA_GTR", "QA_TIGF", "QA_RTE"),
                value.name = "Measure",
                na.rm = TRUE)

library(ggplot2)

dfplot %>%
  filter(variable == "ConsoBrutElecRTE") %>%
  ggplot(aes(Time, Measure, color = QA_RTE)) +
  geom_point() +
  facet_grid(Region ~ .)

dfplot %>%
  filter(Code == 75) %>%
  ggplot(aes(Time, Measure)) +
  geom_line(aes(color = QA_RTE)) +
  facet_grid(variable ~ .)