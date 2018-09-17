# Anki database

# Visualize progress and EDA ####


## Setup and data acquisition ####

# Set working directory
act_wd <- "C:/Users/pierr/Documents/GitHub/EDA_Portfolio/Anki Progress"

if (getwd() != act_wd) {
  setwd(act_wd)
} else {
  print("Already set!")
}

# Import copy of the collection.anki2 database
ankipath <- "C:/Users/pierr/AppData/Roaming/Anki2/User 1/collection.anki2"
if (file.info(ankipath)$mtime != file.info("collection.anki2")$mtime) {
  file.copy(ankipath, act_wd)
  print(file.info(ankipath)$mtime)
} else {
  print("Backup file already set!")
}

# Setup packages
library(RSQLite)
library(DBI)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# Set up connection to Anki database
con <- dbConnect(SQLite(), dbname = "collection.anki2")

# Get the card data
res <- dbSendQuery(con, "SELECT * FROM cards")
card <- dbFetch(res)
dbClearResult(res)

# Get the collection (decks) data
res <- dbSendQuery(con, "SELECT * from col")
col <- dbFetch(res)
dbClearResult(res)

# Get the revision log
res <- dbSendQuery(con, "SELECT * FROM revlog")
revlog = dbFetch(res)
dbClearResult(res)

res <- dbSendQuery(con, "SELECT * FROM notes")
notes <- dbFetch(res)
dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)

## Data Processing ####

# Reassign class on the card data and notes data
card$id <- as.numeric(card$id)
card$nid <- as.numeric(card$nid)
card$did <- as.numeric(card$did)

notes$id <- as.numeric(notes$id)

# Tidy the deck info from col data

# function to get name and id of the deck
deck <- fromJSON(txt = col$decks) %>%
  unname() %>%
  lapply(function(x) {
    temp <- unlist(x)
    data.frame(name = temp[names(temp) == "name"],
               id = temp[names(temp) == "id"],
               row.names = NULL)
  }) %>%
  bind_rows()

# Reclass deck
deck$name <- as.factor(deck$name)
deck$id <- as.numeric(deck$id)

# Mutate revlog id into timestamp and reclass
revlog <- revlog %>%
  mutate(TimeStamp = as.POSIXct(as.numeric(id) / 1000, origin = "1970-01-01"),
         id = as.numeric(id),
         cid = as.numeric(cid),
         ivl = as.numeric(ivl),
         lastIvl = as.numeric(lastIvl)) %>%
  select(-id)

# left_join card with deck to add deck names
card <- card %>%
  left_join(deck, by = c("did" = "id")) %>%
  left_join(notes[, c("id", "sfld")], by = c("nid" = "id"))

# leftjoin revlog with card to complete revlog info
revlog <- left_join(revlog, card, by = c("cid" = "id"))


## Preliminary exploration ####

filt_card <- card %>%
  # select deck name, card id, type, interval,
  #number of reviews and number of lapses correct to incorrect
  select(name, id, type, ivl, reps, lapses, sfld) %>%
  # filter out new cards
  filter(type != 0) %>%
  # reclass variables
  mutate(type = as.numeric(type),
         ivl = as.numeric(ivl),
         reps = as.numeric(reps),
         lapses = as.numeric(lapses))

narrow_card <- filt_card %>%
  filter(ivl < 21, reps > 6)

table(narrow_card$name)







