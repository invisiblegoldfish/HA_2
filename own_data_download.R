
# load client id and secret
source("my_credentials.R")
source("wrapper_extensions.R")

# connect to Spotify API

connect_spotify(passport)

auth<-get_spotify_authorization_code(scope="user-read-recently-played user-top-read") # authentify user profile access


#Download tracklists:

#   parse track results

#Lecture Version
#track_parser<-function(tracks){
#  tracks<-hoist(tracks,"track.artists",
#                track.s.firstartist.name=list("name",1),
#                track.s.firstartist.id=list("id",1),.remove=F)
#  tracks<-distinct(tracks,track.id,.keep_all=T)
#  final<-select(tracks,track.s.id=track.id,track.s.title=track.name,
#                track.s.artists=track.artists,track.s.firstartist.id,track.s.firstartist.name)
#  return(final)

#Own Version incl. mode selection (recent_tracks,top_tracks_top_artists:
track_parser<-function(tracks,mode){
  if (mode=="recent_tracks"){
    tracks<-hoist(tracks,"track.artists",
                  track.s.firstartist.name=list("name",1),
                  track.s.firstartist.id=list("id",1),.remove=F)
    tracks<-distinct(tracks,track.id,.keep_all=T)
    final_dataset<-select(tracks,track.s.id=track.id,track.s.title=track.name,
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
    final_dataset<-select(tracks,track.s.id=id,track.s.title=name,
                  track.s.artists=album.artists,
                  track.s.firstartist.id,
                  track.s.firstartist.name,
                  album.s.id=album.id,
                  album.s.name=album.name)
  }
  if (mode=="top_artists"){
    final_dataset<-select(tracks,artist.s.id=id,
                          artist.s.name=name,
                          artist.s.genres=genres)
  }
  return(final_dataset)
}


##Download, parse and save tracklists:
recent_tracks<-get_my_recently_played(limit=50,authorization=auth) # get recently played tracks
recent_tracks_par<-track_parser(recent_tracks,"recent_tracks") # parsed recently played tracks, parsed tracks temp. stored

top_tracks<-get_my_top_artists_or_tracks(type= "tracks", limit=50, authorization=auth, time_range = "short_term") # get favorite tracks (shortterm)
top_tracks_par<-track_parser(top_tracks,"top_tracks") # parsed favorite tracks (shortterm), parsed tracks temp. stored

top_artists<-get_my_top_artists_or_tracks(type= "artists", limit=50, authorization=auth, time_range = "short_term") # get favorite artists (shortterm)
top_artists_par<-track_parser(top_artists,"top_artists") # parsed favorite artists (shortterm), parsed tracks temp. stored

write_rds(recent_tracks_par, paste0("my_recent_tracks_", Sys.Date(), ".rds"))
write_rds(top_tracks_par, paste0("my_top_tracks_", Sys.Date(), ".rds"))
write_rds(top_tracks_par, paste0("my_top_artists_", Sys.Date(), ".rds"))

