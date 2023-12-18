# Augmentierung von Musikdaten mit Spotify-API

source("wrapper_extensions.R", encoding="UTF-8")
source("my_credentials.R")



pull_album_infos<-function(albumidvector,pass){
  cat('Pull Album Infos','\n')
  visa<-connect_spotify(pass) # connect with API
  albuminfos<-c()
  anfang<-1
  stepsize<-20
  repeat {
    ende<-min(c(anfang+stepsize-1,length(albumidvector)))
    cat("Getting album infos for track",anfang,"-",ende,"\n")
    repeat {
      tryCatch(
        expr = {
          ergebnis<-get_albums(albumidvector[anfang:ende],market=NULL,authorization=visa)
          break},
        error = function(e){
          cat('Web access failure:',as.character(e))
          cat('waiting 3 seconds, then trying again..','\n')
          Sys.sleep(3)
        },
        warning = function(w){
          cat('Web access warning:',w)
          break;
        }
      )
    }
    albuminfos<-rbind(albuminfos,ergebnis)
    anfang<-anfang+stepsize
    if (anfang>length(albumidvector)) {break}
  }
  cat('Write Album Infos','\n')
  albuminfos$release_year<-substr(albuminfos$release_date,1,4) %>% as.integer()
  albuminfos$release_date<-as.Date(albuminfos$release_date,"%Y-%m-%d")
  albuminfos<-select(albuminfos,
                     album.s.type=album_type,
                     album.s.label=label,
                     album.s.popularity=popularity,
                     album.s.upc=external_ids.upc,
                     album.s.total_tracks=tracks.total,
                     album.s.release_date=release_date,
                     album.s.release_year=release_year
                     )
  return(albuminfos)
}


pull_track_infos<-function(trackidvector, pass){
  cat('Pull Track Infos','\n')
  visa<-connect_spotify(pass) # connect with API
  trackinfos<-c()
  anfang<-1
  stepsize<-20
  repeat {
    ende<-min(c(anfang+stepsize-1,length(trackidvector)))
    cat("Getting track infos for track",anfang,"-",ende,"\n")
    repeat {
      tryCatch(
        expr = {
          ergebnis<-get_tracks(trackidvector[anfang:ende],market=NULL,authorization=visa)
          break},
        error = function(e){
          cat('Web access failure:',as.character(e))
          cat('waiting 3 seconds, then trying again..','\n')
          Sys.sleep(3)
        },
        warning = function(w){
          cat('Web access warning:',w)
          break;
        }
      )
    }
    trackinfos<-rbind(trackinfos,ergebnis)
    anfang<-anfang+stepsize
    if (anfang>length(trackidvector)) {break}
  }
  trackinfos<-select(trackinfos,
                     track.s.popularity=popularity, 
                     track.s.albumPosition=track_number, 
                     track.s.explicit=explicit, 
                     track.s.isrc=external_ids.isrc)
  return(trackinfos)
}


pull_artist_infos<-function(artistidvector,pass){
  cat('Pull Artist Infos','\n')
  visa<-connect_spotify(pass) # connect with API
  artistinfos<-c()
  anfang<-1
  stepsize<-20
  repeat {
    ende<-min(c(anfang+stepsize-1,length(artistidvector)))
    cat("Getting artist infos for track",anfang,"-",ende,"\n")
    repeat {
      tryCatch(
        expr = {
          ergebnis<-get_artists(artistidvector[anfang:ende],authorization=visa)
          break},
        error = function(e){
          cat('Web access failure:',as.character(e))
          cat('waiting 3 seconds, then trying again..','\n')
          Sys.sleep(3)
        },
        warning = function(w){
          cat('Web access warning:',w)
          break;
        }
      )
    }
    artistinfos<-rbind(artistinfos,ergebnis)
    anfang<-anfang+stepsize
    if (anfang>length(artistidvector)) {break}
  }
  cat('Write Artist Infos','\n')
  
  artistinfos$topGenre <- sapply(artistinfos$genres , "[", 1) # assuming that the first called element is the top genre.
                                                              # further classification needs access to other music-info-library like musicbrainz
                                                              # which is not provided here at this point.
  artistinfos<-select(artistinfos,
                      artist.s.popularity=popularity,
                      artist.s.followers=followers.total,
                      artist.s.genres=genres,
                      artist.s.topgenre=topGenre,
                      artist.s.id=id, #how is there only 1 ID and 1 Name, when you have more then 1 artists?
                      artist.s.name=name #think it just picks the 1st element
                      )
                    
  return(artistinfos)
}


pull_audio_features<-function(trackidvector,pass){
  cat('Pull Audio Features','\n')
  visa<-connect_spotify(pass) # connect with API
  audiofeatures<-c()
  anfang<-1
  stepsize<-20
  repeat {
    ende<-min(c(anfang+stepsize-1,length(trackidvector)))
    cat("Getting audio features for track",anfang,"-",ende,"\n")
    repeat {
      tryCatch(
        expr = {
          ergebnis<-get_track_audio_features(trackidvector[anfang:ende],authorization=visa)
          break},
        error = function(e){
          cat('Web access failure:',as.character(e))
          cat('waiting 3 seconds, then trying again..','\n')
          Sys.sleep(3)
        },
        warning = function(w){
          cat('Web access warning:',w)
          break;
        }
      )
    }
    cat('Write Audio Features','\n')
    audiofeatures<-rbind(audiofeatures,ergebnis)
    anfang<-anfang+stepsize
    if (anfang>length(trackidvector)) {break}
  }
  
  audiofeatures$duration_s<-audiofeatures$duration_ms/1000 %>% as.double()
  
  #select and name features
  audiofeatures<-select(audiofeatures,
                        track.s.key=key, 
                        track.s.loudness=loudness,
                        track.s.mode=mode, 
                        track.s.tempo=tempo,
                        track.s.duration=duration_s,
                        track.s.duration_ms=duration_ms,
                        track.s.time_signature=time_signature, 
                        track.s.danceability=danceability,
                        track.s.energy=energy,
                        track.s.valence=valence,
                        track.s.speechiness=speechiness,
                        track.s.acousticness=acousticness,
                        track.s.instrumentalness=instrumentalness,
                        track.s.liveness=liveness)
  
#convert features
  
  audiofeatures$track.s.key <- factor(audiofeatures$track.s.key)
  audiofeatures$track.s.mode <- factor(audiofeatures$track.s.mode)
  audiofeatures$track.s.time_signature <- factor(audiofeatures$track.s.time_signature)
  
  
  return(audiofeatures)
}




# unnest column track.s.artist

expand_spotify_artists<-function(trackframe){
  trackframe$track.s.artists.unnested <- trackframe$track.s.artists
  trackframe<-unnest_legacy(trackframe,track.s.artists.unnested, .preserve=track.s.artists)
  trackframe$track.s.firstartist.name<-trackframe$name
  return(trackframe)
}
