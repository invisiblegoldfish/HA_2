
rm(list=ls()) # clear workspace

library(tidyverse) # import basic function
library(spotifyr) # import spotify api wrapper
library(haven)  # import spss function

# set credentials

source("my_credentials.r")

# connect to Spotify API

connect_spotify<-function(){
  cat("Connecting with Spotify API\n")
  Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secrect)
  api_token <- get_spotify_access_token()
  return(api_token)
}

# parse track results, 
# Input variables:  
#                   tracks: data.frame containing table of tracks; 
#                   mode: describing mode of "tracks", 1=recent_tracks, 2=fav_tracks, 3=fav_artists

track_parser<-function(tracks,mode){
  if (mode=="recent_tracks"){
    
  tracks<-hoist(tracks,"track.artists",
                track.s.firstartist.name=list("name",1),
                track.s.firstartist.id=list("id",1),.remove=F)
  tracks<-distinct(tracks,track.id,.keep_all=T)
  final<-select(tracks,track.s.id=track.id,track.s.title=track.name,
                track.s.artists=track.artists,
                track.s.firstartist.id,
                track.s.firstartist.name,
                album.s.id=track.album.id,
                album.s.name=track.album.name)
  }
  if (mode=="top_tracks"){
    
    tracks<-hoist(tracks,"artists",
                  track.s.firstartist.name=list("name",1),
                  track.s.firstartist.id=list("id",1),.remove=F)
    tracks<-distinct(tracks,id,.keep_all=T)
    final<-select(tracks,track.s.id=id,track.s.title=name,
                  track.s.artists=album.artists,
                  track.s.firstartist.id,
                  track.s.firstartist.name,
                  album.s.id=album.id,
                  album.s.name=album.name)
    
  }
  
  if (mode=="top_artists"){
    final<-select(tracks,artist.s.id=id,
                  artist.s.name=name,
                  artist.s.genres=genres)
    
  }
  return(final)

}

##### MAIN PROGRAM

connect_spotify() # connect with API

auth<-get_spotify_authorization_code(scope="user-read-recently-played user-top-read") # authentify user profile access


recent_tracks<-get_my_recently_played(limit=50,authorization=auth) # get recently played tracks
recent_tracks_par<-track_parser(recent_tracks,"recent_tracks") # parse recently played tracks, parsed tracks temp. stored

fav_tracks<-get_my_top_artists_or_tracks(type= "tracks", limit=50, authorization=auth, time_range = "short_term") # get favorite tracks (shortterm)
fav_tracks_par<-track_parser(fav_tracks,"top_tracks") # parse favorite tracks (shortterm), parsed tracks temp. stored

fav_artists<-get_my_top_artists_or_tracks(type= "artists", limit=50, authorization=auth, time_range = "short_term") # get favorite artists (shortterm)
fav_artists_par<-track_parser(fav_artists,"top_artists") # parse favorite artists (shortterm), parsed tracks temp. stored
 

write_rds(recent_tracks_par, paste0("my_recent_tracks_", Sys.Date(), ".rds"))
write_rds(fav_tracks_par, paste0("my_fav_tracks_", Sys.Date(), ".rds"))
write_rds(fav_artists_par, paste0("fav_artists_par_", Sys.Date(), ".rds"))

