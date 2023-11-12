
# Extension des Spotifyr Wrappers

library(tidyverse) # import basic function
library(spotifyr) # import spotify api wrapper

# connect to Spotify API

connect_spotify<-function(pass){
  cat("Connecting with Spotify API\n")
  Sys.setenv(SPOTIFY_CLIENT_ID = pass[1])
  Sys.setenv(SPOTIFY_CLIENT_SECRET = pass[2])
  api_token <- get_spotify_access_token()
  return(api_token)
}

# parse track results

track_parser<-function(tracks){
  tracks<-hoist(tracks,"track.artists",
                track.s.firstartist.name=list("name",1),
                track.s.firstartist.id=list("id",1),.remove=F)
  tracks<-distinct(tracks,track.id,.keep_all=T)
  final<-select(tracks,track.s.id=track.id,track.s.title=track.name,
                track.s.artists=track.artists,track.s.firstartist.id,track.s.firstartist.name)
  return(final)
}
