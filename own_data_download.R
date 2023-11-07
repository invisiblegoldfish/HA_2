# connect to Spotify API

connect_spotify<-function(){
  cat("Connecting with Spotify API")
  Sys.setenv(SPOTIFY_CLIENT_ID = '4e46be4ec1834ac78e04e98bd2d2d7a2')
  Sys.setenv(SPOTIFY_CLIENT_SECRET = 'd63823deb0fa495e9f502c4e9fb97b18')
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
