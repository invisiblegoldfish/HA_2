# AUgmentierung von Musikdaten mit Spotify-API

source("wrapper_extensions.R", encoding="UTF-8")


pull_album_infos<-function(albumidvector,apitoken){
  visa<-connect_spotify(apitoken) # connect with API
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
    albuminfos<-rbind(albuminfos,ergebnis)
    anfang<-anfang+stepsize
    if (anfang>length(albumidvector)) {break}
    }
    
  #read albumrelease year and date
  albuminfos$release_year<-substr(albuminfos$release_date,1,4) %>% as.integer()
  albuminfos$release_date<-as.Date(albuminfos$release_date,"%Y-%m-%d")
  
  #read Spotify Album Label
  #albuminfos$label<-as.character(albuminfos$label)
  #read Spotify Album Popularity
  
  
  #create outputvector
  albuminfos<-select(albuminfos,album_type,label, popularity,album.s.upc=external_ids.upc,tracks.total,release_date,release_year )
  return(albuminfos)
  }}


pull_track_infos<-function(trackidvector,apitoken){
  visa<-connect_spotify(apitoken) # connect with API
  trackinfos<-c()
  anfang<-1
  stepsize<-20
  repeat {
    ende<-min(c(anfang+stepsize-1,length(trackidvector)))
    cat("Getting album infos for track",anfang,"-",ende,"\n")
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
      trackinfos<-rbind(trackinfos,ergebnis)
      anfang<-anfang+stepsize
      if (anfang>length(trackidvector)) {break}
    }
    
    #read trackrelease year and date
    #trackinfos$release_year<-substr(trackinfos$release_date,1,4) %>% as.integer()
    #trackinfos$release_date<-as.Date(trackinfos$release_date,"%Y-%m-%d")
    
    #read Spotify Track Info about Explicit Lyrics
    trackinfos$track.s.explicit<-as.logical(trackinfos$explicit)
    #read Spotify Album Popularity
    
    
    #create outputvector
    trackinfos<-select(trackinfos,explicit,track_number, popularity)
    return(trackinfos)
  }
}


pull_artist_infos<-function(artistidvector,apitoken){
  
}

# unnest column track.s.artist

expand_spotify_artists<-function(trackframe){
  trackframe<-unnest_legacy(trackframe,track.s.artists,.preserve=track.s.artists) # warum behalten wir die Varialbe?
  trackframe$track.s.firstartist.name<-trackframe$name
  return(trackframe)
}