#Berechnen Sie die Diversitätsindizes Balance und Variety für Ihre persönlichen „Top Tracks“ in Bezug auf Labels,
#Artist-Herkunftsländer und Musikgenres 

rm(list=ls()) # clear workspace


library(haven) # import SPSS Functions
library(performance)  # import statistical functions
library(jmv)   # import jamovi functions
library(tidyverse) # import basic function
library(dplyr)
library(ineq) # Für Gini-Koeffizient

top_tracks<-readRDS("files_ha5/mb_my_top_tracks2023-12-05.rds")
top_artists<-readRDS("files_ha5/mb_my_top_artists2023-12-05.rds")
### hier nicht notwendig
# top_tracks<-readRDS("saved_data/mb_my_top_tracks2023-12-03.rds")
# recent_tracks <- readRDS("saved_data/mb_my_recent_tracks2023-12-03.rds")
# recent_tracks_unnested <- readRDS("saved_data/mb_my_recent_tracks2023-12-03.rds")
# 
 personendaten<-readRDS("files_ha5/personendaten.rds")
# frabo_diskreptoren<-readRDS("files_ha5/frabo_diskreptoren.rds")
###

##
#shorted_infos sollte auch artist.mb.origin enthalten, 
#durch einen Bug werden in musicbrainz_augment.R aber 
#die Artistinfos nur dem Artistvektor hinzugefügt und (nicht top_artists).
#Bis der Bug bereinigt ist, wird stattdessen artist.mb.origin aus top_artists extrahiert.
#Die folge ist, das am Ende 2 Tabellen als Ergebnis gebildet werden müssen
#Da top_tracks und top_artists unterschiedliche Zeilenbeschrifungen/infos haben.
shorted_infos<-select(top_tracks,track.s.title,track.s.firstartist.name,album.s.label,artist.s.topgenre, track.mb.topgenre )
artist_origins<-select(top_artists,artist.mb.origin)

##Bildung neuer Spalte "topgenre":
# enthält artist.s.topgenre, falls kein NA,
# sonst track.mb.topgenre, falls kein NA,
# anderenfalls
#Löschen der Zeile und Anzeige der gelöschten Tracks
tracks_without_genre <- shorted_infos %>%
  filter(is.na(artist.s.topgenre) & is.na(track.mb.topgenre)) %>%
  pull(track.s.title)
shorted_infos <- shorted_infos %>%
  mutate(topgenre = coalesce(artist.s.topgenre, track.mb.topgenre)) %>%
  filter(!is.na(topgenre))  # Remove rows where topgenre is still NA after coalesce

# Print out the titles of tracks with no genre defined
message <- paste(tracks_without_genre, "has no genre defined", sep=": ")
print(message)

#Nominalvariablen als Faktor deklarieren
shorted_infos$album.s.label<-factor(shorted_infos$album.s.label)
artist_origins<-factor(artist_origins <- factor(top_artists$`artist.mb.origin`)) #direkter Zugriff, da Rstudio hier einen Fehler bei 1-Dimensionalen dataframes wirft
shorted_infos$topgenre<-factor(shorted_infos$topgenre)

#Berechnung von Variety nach Bello & Garcia 2021
label_variety <- length(unique(na.omit(shorted_infos$album.s.label)))/nrow(shorted_infos)
origin_variety<- length(unique(na.omit(artist_origins)))/length(artist_origins) #auch hier wieder eine Extrawurst fällig, da nicht auf top_tracks zugriffen werden kann. sonst Berechnung wie bei den beiden anderen Variablen
genre_variety<- length(unique(na.omit(shorted_infos$topgenre)))/nrow(shorted_infos)
  
#Berechnung von Balance (1 - Gini-Koeffizient) nach Bello & Garcia 2021
label_balance<- 1 - Gini(as.numeric(table(shorted_infos$album.s.label)))
origin_balance<- 1 - Gini(as.numeric(table(artist_origins)))
genre_balance<- 1 - Gini(as.numeric(table(shorted_infos$topgenre)))

# Create a data frame from these variables
diversity_indices <- data.frame(
  Variable = c("label_variety", "origin_variety", "genre_variety", 
               "label_balance", "origin_balance", "genre_balance"),
  Value = c(label_variety, origin_variety, genre_variety, 
            label_balance, origin_balance, genre_balance)
)



saveRDS(diversity_indices,"files_ha5/diversity_indices.rds")



