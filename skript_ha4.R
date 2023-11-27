rm(list=ls()) # clear workspace

source("musicbrainz_augment.R")


# read files
top_artists<-readRDS("a_my_top_artists_2023-11-14.rds")
top_tracks<-readRDS("a_my_top_tracks_2023-11-14.rds")
top_tracks_unnested<-readRDS("ua_my_top_tracks_2023-11-14.rds")
recent_tracks <- readRDS("a_my_recent_tracks_2023-11-14.rds")
recent_tracks_unnested <- readRDS("ua_my_recent_tracks_2023-11-14.rds")



# add info to data frames
mb_top_artists <- add_musicbrainz_artist_infos(top_artists)

mb_top_tracks <- add_musicbrainz_album_infos(top_tracks)
mb_top_tracks <- add_musicbrainz_track_infos(top_tracks)

mb_top_tracks_unnested <- add_musicbrainz_album_infos(top_tracks)
mb_top_tracks_unnested <- add_musicbrainz_track_infos(top_tracks)

mb_recent_tracks <- add_musicbrainz_album_infos(top_tracks)
mb_recent_tracks <- add_musicbrainz_track_infos(top_tracks)

mb_recent_tracks_unnested <- add_musicbrainz_album_infos(top_tracks)
mb_recent_tracks_unnested <- add_musicbrainz_track_infos(top_tracks)

# write RDS files

write_rds(mb_top_artists, paste0("mb_my_top_artists", Sys.Date(), ".rds"))
write_rds(mb_top_tracks, paste0("mb_my_top_tracks", Sys.Date(), ".rds"))
write_rds(mb_top_tracks_unnested, paste0("mb_my_top_tracks_unnested", Sys.Date(), ".rds"))
write_rds(mb_recent_tracks, paste0("mb_my_recent_tracks", Sys.Date(), ".rds"))
write_rds(mb_recent_tracks_unnested, paste0("mb_my_recent_tracks_unnested", Sys.Date(), ".rds"))



