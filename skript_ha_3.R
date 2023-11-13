#readme:


rm(list=ls()) # clear workspace

library(tidyverse) # import basic function
library(spotifyr) # import spotify api wrapper
library(haven)  # import spss function


# load client id and secret
source("my_credentials.R")


# stream data from spotify-api, save recent_tracks, top_tracks and top_artists as .RDS-Files
#source("own_data_download.R", encoding="UTF-8")

# load functions for pulling track/artist/album information
source("spotify_augment.R", encoding="UTF-8")

##### MAIN


#fetch data
#connect_spotify();


##read rds files (datasets)
recent_tracks<-readRDS("my_recent_tracks_2023-11-13.rds")
top_tracks<-readRDS("my_top_tracks_2023-11-13.rds")
top_artists<-readRDS("my_top_tracks_2023-11-13.rds")




##unnest artists in tracklists
recent_tracks_unnested<-expand_spotify_artists(recent_tracks)
top_tracks_unnested<-expand_spotify_artists(top_tracks)





#codezeile aus isis: recent_tracks_augmented<-cbind(recent_tracks,pull_album_infos(recent_tracks$album.s.id, passport))
#source("spotify_augment.R", encoding="UTF-8")

#tes2t=pull_album_infos(recent_tracks$album.s.id, passport)
#test2=pull_track_infos(recent_tracks$album.s.id,passport)

#pull album-, track-, artistinfos and audiofeatures, binding collums to dataset: recent_tracks
recent_tracks_augmented<-cbind(recent_tracks,
                               pull_album_infos(recent_tracks$album.s.id, passport),
                               pull_track_infos(recent_tracks$track.s.id, passport),
                               #pull_artist_infos(recent_tracks$track.s.id, passport),
                               pull_audio_features(recent_tracks$track.s.id, passport)
                               )

#pull album-, track-, artistinfos and audiofeatures, binding collums to dataset: top_tracks
top_tracks_augmented<-cbind(top_tracks,
                            pull_album_infos(top_tracks$album.s.id, passport),
                            pull_track_infos(top_tracks$track.s.id, passport),
                            #pull_artist_infos(top_tracks$track.s.id, passport),
                            pull_audio_features(top_tracks$track.s.id, passport))
#top_artists_augmented<-cbind(top_artists,pull_album_infos(top_artists$album.s.id, passport), pull_artist_info(top_artists$artist.s.id, passport), pull_track_infos(top_artists$track.s.id, passport))

