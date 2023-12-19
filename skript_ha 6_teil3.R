rm(list=ls()) # clear workspace

# Aggregat- und Trendstatistiken


library(jmv)   # import jamovi functions
library(flextable) # import table functions
library(countrycode) # import ISO countrycode functions
library(tidyverse) #import count/arrange
library(flextable) #import (flex)tables

source("wrapper_extensions.R", encoding="UTF-8")

de_tracks<-readRDS("germanDailySpotify_full_mb_artistbased.rds")



de_tracks_clean<-distinct(de_tracks,artist.s.id,track.s.chartsDate,.keep_all=T) # if an Artist has more hits on the same day, it only counts once: "duration in charts"
de_tracks_clean_pop <- de_tracks_clean %>%
  filter(grepl("pop", artist.s.topGenre)) # Including only Pop and Subgenres

result <- de_tracks_clean_pop %>%
  count(artist.s.name) %>%
  arrange(desc(n)) # count appearance of artist in charts and sort by counts

top_ten_artists<-head(result,10) #top ten!
top_ten_artists<-select(top_ten_artists,"Artist"=artist.s.name,"Days in Charts"=n)
tabelle <- flextable(top_ten_artists, col_keys = c("Artist", "Days in Charts"))#build table

save_as_html(tabelle,path="top_ten_artists.html") #safe table as .html

