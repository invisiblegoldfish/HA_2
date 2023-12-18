
rm(list=ls()) # clear workspace

# Datenoperationen üben

library(haven) # import SPSS Functions
library(performance)  # import statistical functions
library(jmv)   # import jamovi functions
library(spotifyr)

source("wrapper_extensions.R", encoding="UTF-8")
source("spotify_augment.R")


######################################## mittlere musikalische Profile für beliebteste Artists erstellem

my_top_tracks<-readRDS("mb_my_top_tracks2023-12-04.rds")

my_top3_artists <- head(distinct(my_top_tracks[order(my_top_tracks$artist.s.popularity, decreasing = T), c("artist.s.name", "artist.s.id", "artist.s.popularity")]), n=3)

top_artist_profiles <- tibble()
for (i in c(1:nrow(my_top3_artists))) {
  top_artist_top_tracks <- get_artist_top_tracks(my_top3_artists[i,"artist.s.id"])
  
  top_artist_top_tracks_info <- cbind(track.s.id = top_artist_top_tracks$id,
                                      track.s.name = top_artist_top_tracks$name,
                                      pull_audio_features(top_artist_top_tracks$id, passport))
  
  top_artist_profile <- top_artist_top_tracks_info %>% distinct(track.s.id,.keep_all = T) %>%
    summarize_at(c("track.s.loudness",
                   "track.s.tempo",
                   "track.s.duration",
                   "track.s.danceability",
                   "track.s.energy",
                   "track.s.valence",
                   "track.s.speechiness",
                   "track.s.acousticness",
                   "track.s.instrumentalness",
                   "track.s.liveness"),list(ave=median,dis=IQR,min=min,max=max),na.rm=T)
  
  top_artist_profile <- cbind(artist.s.id = my_top3_artists[i,"artist.s.id"],
                              artist.s.name = my_top3_artists[i,"artist.s.name"],
                              top_artist_profile) %>% as_tibble()
  
  top_artist_profiles <- rbind(top_artist_profiles, top_artist_profile)
}

write_rds(top_artist_profiles, paste0("my_top3_artists_profiles_", Sys.Date(), ".rds"))


############## Mainstreaminess meiner Recent Tracks bestimmen

de_daten<- readRDS("germanDailySpotify_full_mb_artistbased.rds")

my_recent_tracks<-readRDS("mb_my_recent_tracks_unnested2023-12-04.rds")


de_tracks <- de_daten %>% 
  group_by(track.s.id) %>% 
  mutate(track.med_pop=0) %>% 
  ungroup() %>% 
  select(track.s.title, track.s.id, track.med_pop) %>%
  distinct(track.s.id, .keep_all = T)  # deutsche tracks, alle popularity 0
count(de_tracks, track.med_pop) %>% arrange(-track.med_pop) %>% print(n=150)

recent_tracks<-my_recent_tracks %>% 
  group_by(track.s.id) %>% 
  mutate(track.med_pop=n()) %>% 
  ungroup() %>%
  select(track.s.title, track.s.id, track.med_pop) %>%
  distinct(track.s.id, .keep_all = T)     #  Popularty Ranking der eigenen Tracks nach Häufigkeit des Hörens
recent_tracks %>% arrange(-track.med_pop) %>% print(n=150)

my_track_ranking<-full_join(recent_tracks,de_tracks,by=c("track.s.id","track.med_pop","track.s.title")) %>% 
  distinct(track.s.id,.keep_all=T)    # Vergleichsliste für eigenes Hörverhalten

my_track_ranking  %>% arrange(-track.med_pop) %>% print(n=150)  # Mein Ranking aller Tracks (meine+deutsche)


de_tracks<-de_daten %>% 
  group_by(track.s.id) %>% 
  mutate(track.med_pop=track.s.popularity) %>% 
  ungroup() %>% 
  select(track.s.title, track.s.id, track.med_pop) %>%
  distinct(track.s.id, .keep_all = T)  # deutsche tracks, mittlere Popularität
count(de_tracks,track.med_pop) %>% arrange(-track.med_pop) %>% print(n=150)


recent_tracks<-my_recent_tracks %>% 
  group_by(track.s.id) %>% 
  mutate(track.med_pop=0) %>% 
  ungroup() %>%
  select(track.s.title, track.s.id, track.med_pop) %>%
  distinct(track.s.id, .keep_all = T)     #  Popularty Ranking der eigenen Tracks mit 0
recent_tracks %>% arrange(-track.med_pop) %>% print(n=150)


de_track_ranking<-full_join(de_tracks,recent_tracks,by=c("track.s.id","track.med_pop","track.s.title")) %>%   #
  distinct(track.s.id,.keep_all=T)    # Vergleichsliste für eigenes Hörverhalten

de_track_ranking  %>% arrange(-track.med_pop) %>% print(n=150)   # das Ranking der Deutschen aller Tracks (meine+deutsche)


mainstreaminess<-cor(my_track_ranking$track.med_pop,de_track_ranking$track.med_pop,method="kendall")


















