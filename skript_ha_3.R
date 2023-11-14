#readme:


rm(list=ls()) # clear workspace

library(tidyverse) # import basic function
library(spotifyr) # import spotify api wrapper
library(haven)  # import spss function





# load client id and secret
source("my_credentials.R")
# load functions for pulling track/artist/album information
source("spotify_augment.R", encoding="UTF-8")
# fetch data from spotify-api, save recent_tracks, top_tracks and top_artists as .RDS-Files
source("own_data_download.R", encoding="UTF-8")


##### MAIN



##read rds files (datasets)
recent_tracks<-readRDS("my_recent_tracks_2023-11-14.rds")
top_tracks<-readRDS("my_top_tracks_2023-11-14.rds")
top_artists<-readRDS("my_top_artists_2023-11-14.rds")




##unnest artists in tracklists
recent_tracks_unnested<-expand_spotify_artists(recent_tracks)
top_tracks_unnested<-expand_spotify_artists(top_tracks)



#pull album-, track-, artistinfos and audiofeatures, binding collums to dataset: recent_tracks
recent_tracks_augmented<-cbind(recent_tracks,
                               pull_album_infos(recent_tracks$album.s.id, passport),
                               pull_track_infos(recent_tracks$track.s.id, passport),
                               pull_artist_infos(recent_tracks$track.s.firstartist.id, passport),
                               pull_audio_features(recent_tracks$track.s.id, passport)
                               )


#pull album-, track-, artistinfos and audiofeatures, binding collums to dataset: top_tracks
top_tracks_augmented<-cbind(top_tracks,
                            pull_album_infos(top_tracks$album.s.id, passport),
                            pull_track_infos(top_tracks$track.s.id, passport),
                            pull_artist_infos(top_tracks$track.s.firstartist.id, passport),
                            pull_audio_features(top_tracks$track.s.id, passport)
                            )

#pull artistinfos binding collums to dataset: top_artists
top_artists_augmented<-cbind(top_artists,
                            pull_artist_infos(top_artists$artist.s.id, passport)
                            )


#pull album-, track-, artistinfos and audiofeatures, binding collums to dataset: recent_tracks_unnested
recent_tracks_unnested_augmented<-cbind(recent_tracks_unnested,
                               pull_album_infos(recent_tracks_unnested$album.s.id, passport),
                               pull_track_infos(recent_tracks_unnested$track.s.id, passport),
                               pull_artist_infos(recent_tracks_unnested$track.s.firstartist.id, passport),
                               pull_audio_features(recent_tracks_unnested$track.s.id, passport)
)

#pull album-, track-, artistinfos and audiofeatures, binding collums to dataset: top_tracks_unnested
top_tracks_unnested_augmented<-cbind(top_tracks_unnested,
                                        pull_album_infos(top_tracks_unnested$album.s.id, passport),
                                        pull_track_infos(top_tracks_unnested$track.s.id, passport),
                                        pull_artist_infos(top_tracks_unnested$track.s.firstartist.id, passport),
                                        pull_audio_features(top_tracks_unnested$track.s.id, passport)
)


#saving datasets:
write_rds(recent_tracks_augmented, paste0("a_my_recent_tracks_", Sys.Date(), ".rds"))
write_rds(top_tracks_augmented, paste0("a_my_top_tracks_", Sys.Date(), ".rds"))
write_rds(top_artists_augmented, paste0("a_my_top_artists_", Sys.Date(), ".rds"))
write_rds(top_tracks_unnested_augmented, paste0("ua_my_top_tracks_", Sys.Date(), ".rds"))
write_rds(recent_tracks_unnested_augmented, paste0("ua_my_recent_tracks_", Sys.Date(), ".rds"))



