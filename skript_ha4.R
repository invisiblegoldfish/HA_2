#skript to add musicbrainz-features to an existing spotify-info-dataframe

rm(list=ls()) # clear workspace

source("musicbrainz_augment.R")

max_rowEntries_per_api_call<-50;

# read files
top_artists<-readRDS("saved_data/a_my_top_artists_2023-12-04.rds")
top_tracks<-readRDS("saved_data/a_my_top_tracks_2023-12-04.rds")
top_tracks_unnested<-readRDS("saved_data/ua_my_top_tracks_2023-12-04.rds")
recent_tracks <- readRDS("saved_data/a_my_recent_tracks_2023-12-04.rds")
recent_tracks_unnested <- readRDS("saved_data/ua_my_recent_tracks_2023-12-04.rds")



# add info to data frames
mb_top_artists <- add_musicbrainz_artist_infos(top_artists)


mb_top_tracks <- api_call_albums_mb(top_tracks,max_rowEntries_per_api_call)
mb_top_tracks <- api_call_tracks_mb(mb_top_tracks,max_rowEntries_per_api_call)
mb_top_tracks <- mb_top_tracks[, unique(names(mb_top_tracks))] # Remove duplicate columns


mb_top_tracks_unnested <- api_call_albums_mb(top_tracks_unnested,max_rowEntries_per_api_call)
mb_top_tracks_unnested <- api_call_tracks_mb(mb_top_tracks_unnested,max_rowEntries_per_api_call)
mb_top_tracks_unnested <- mb_top_tracks_unnested[, unique(names(mb_top_tracks_unnested))] # Remove duplicate columns


mb_recent_tracks <- api_call_albums_mb(recent_tracks,max_rowEntries_per_api_call)
mb_recent_tracks <- api_call_tracks_mb(mb_recent_tracks,max_rowEntries_per_api_call)
mb_recent_tracks <- mb_recent_tracks[, unique(names(mb_recent_tracks))] # Remove duplicate columns


mb_recent_tracks_unnested <- api_call_albums_mb(recent_tracks_unnested,max_rowEntries_per_api_call)
mb_recent_tracks_unnested <- api_call_tracks_mb(mb_recent_tracks_unnested,max_rowEntries_per_api_call)
mb_recent_tracks_unnested <- mb_recent_tracks_unnested[, unique(names(mb_recent_tracks_unnested))] # Remove duplicate columns

# write RDS files

write_rds(mb_top_artists, paste0("mb_my_top_artists", Sys.Date(), ".rds"))
write_rds(mb_top_tracks, paste0("mb_my_top_tracks", Sys.Date(), ".rds"))
write_rds(mb_top_tracks_unnested, paste0("mb_my_top_tracks_unnested", Sys.Date(), ".rds"))
write_rds(mb_recent_tracks, paste0("mb_my_recent_tracks", Sys.Date(), ".rds"))
write_rds(mb_recent_tracks_unnested, paste0("mb_my_recent_tracks_unnested", Sys.Date(), ".rds"))



