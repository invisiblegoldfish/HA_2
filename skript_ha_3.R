rm(list=ls()) # clear workspace

library(tidyverse) # import basic function
library(spotifyr) # import spotify api wrapper
library(haven)  # import spss function


# load client id and secret
source("my_credentials.R")

# load functions for pulling track/artist/album information
source("spotify_augment.R", encoding="UTF-8")


##### MAIN

##read rds files (datasets)
#recent_tracks<-readRDS("my_recent_tracks_2023-11-06.rds")
recent_tracks<-readRDS("my_recent_tracks.rds")
top_tracks<-readRDS("my_fav_tracks_2023-11-06.rds")
top_artists<-readRDS("fav_artists_par_2023-11-06.rds")


##unnest artists in tracklists
recent_tracks_unnested<-expand_spotify_artists(recent_tracks)
top_tracks_unnested<-expand_spotify_artists(top_tracks)





#codezeile aus isis: recent_tracks_augmented<-cbind(recent_tracks,pull_album_infos(recent_tracks$album.s.id, passport))

recent_tracks_augmented<-cbind(recent_tracks,pull_album_infos(recent_tracks$album.s.id, passport), pull_track_infos(recent_tracks$track.s.id, passport))
top_tracks_augmented<-cbind(top_tracks,pull_album_infos(top_tracks$album.s.id, passport),  pull_track_infos(top_tracks$track.s.id, passport))
#top_artists_augmented<-cbind(top_artists,pull_album_infos(top_artists$album.s.id, passport), pull_artist_info(top_artists$artist.s.id, passport), pull_track_infos(top_artists$track.s.id, passport))

