# Augmentierung von Musikdaten mit MusicBrainz API

source("wrapper_extensions.R", encoding="UTF-8")

library(musicbrainz)
library(stringdist)


pull_musicbrainz_artist_ids<-function(artistvector){
  idlist<-c()
  for (i in 1:length(artistvector)){
    cat("Searching for",artistvector[i],"\n")
    ergebnis<-search_artists(artistvector[i], limit=1)
    ergebnis<-cbind(ergebnis,quality=stringsim(artistvector[i],ergebnis$name,"jw"))
    idlist<-rbind(idlist,ergebnis)
  }
  idlist<-select(idlist,artist.mb.name=name,artist.mb.id=mbid,artist.mb.quality=quality)
  return(idlist)
}

pull_musicbrainz_track_ids<-function(trackidvector){ # i should do this with ids but my songs don't have those
  idlist<-c()
  for (i in 1:length(trackidvector)){
    cat("Searching for",trackidvector[i],"\n")
    ergebnis<-search_recordings(trackidvector[i], limit=1)
    ergebnis<-cbind(ergebnis,quality=stringsim(trackidvector[i],ergebnis$title,"jw"))
    idlist<-rbind(idlist,ergebnis)
  }
  idlist<-select(idlist,track.mb.title=title, track.mb.id=mbid, track.mb.quality=quality)
  return(idlist)
}

pull_musicbrainz_album_ids<-function(albumidvector){ 
  idlist<-c()
  for (i in 1:length(albumidvector)){
    cat("Searching for",albumidvector[i],"\n")
    ergebnis<-search_releases(albumidvector[i], limit=1)
    ergebnis<-cbind(ergebnis,matchquality=stringsim(albumidvector[i],ergebnis$title,"jw"))
    idlist<-rbind(idlist,ergebnis)
  }
  idlist<-select(idlist,album.mb.title=title, album.mb.id=mbid, album.mb.quality=matchquality)
  return(idlist)
}

pull_musicbrainz_artist_infos<-function(artistidvector){
  erglist<-c()
  for (i in 1:length(artistidvector)){
    cat("Searching genre for id",artistidvector[i],"\n")
    result<-lookup_artist_by_id(artistidvector[i], includes="tags")
    topgenre<-result[["tags"]][[1]]$tag_name[which.max(result[["tags"]][[1]]$tag_count)]
    print(topgenre)
    if (!is.null(topgenre)) {
      result$topgenre<-topgenre
    } else {
      result$topgenre<-NA
    }
    erglist<-rbind(erglist,result)
  }
  erglist$life_span_begin_year <- substr(erglist$life_span_begin,1,4) %>% as.integer()
  erglist$life_span_begin_date <- as.Date(erglist$life_span_begin, "%Y-%m-%d")
  
  erglist$life_span_end_year <- substr(erglist$life_span_end,1,4) %>% as.integer()
  erglist$life_span_end_date <- as.Date(erglist$life_span_end, "%Y-%m-%d")
  
  erglist<-select(erglist,
                  artist.mb.id = mbid,
                  artist.mb.name = name,
                  artist.mb.type = type,
                  artist.mb.gender = gender,
                  artist.mb.origin = country,
                  artist.mb.area = area_iso,
                  artist.mb.birth = life_span_begin_date,
                  artist.mb.birthyear = life_span_begin_year,
                  artist.mb.death = life_span_end_date,
                  artist.mb.deathyear = life_span_end_year,
                  artist.mb.dead = life_span_ended,
                  artist.mb.genres=tags,
                  artist.mb.topgenre=topgenre)
  return(erglist)
}

pull_musicbrainz_track_infos<-function(trackidvector){
  erglist<-c()
  for (i in 1:length(trackidvector)){
    result<-lookup_recording_by_id(trackidvector[i], includes = c("artists", "releases", "tags"))
    result$firstartist<-result[["artists"]][[1]]$name[1] # gets first artist in list
    result$firstartistid<-result[["artists"]][[1]]$artist_mbid[1]
    topgenre<-result[["tags"]][[1]]$tag_name[which.max(result[["tags"]][[1]]$tag_count)]
    if (!is.null(topgenre)) {
      result$topgenre<-topgenre
    } else {
      result$topgenre<-NA
    }
    erglist<-rbind(erglist,result)
  }
  erglist<-select(erglist,
                  track.mb.id = mbid,
                  track.mb.title = title,
                  track.mb.artistlist = artists,
                  track.mb.firstartist.name = firstartist,
                  track.mb.firstartist.id = firstartistid,
                  track.mb.releases = releases,
                  track.mb.genres = tags,
                  track.mb.topgenre = topgenre)
  return(erglist)
}


pull_musicbrainz_album_infos<-function(albumidvector){
  erglist<-c()
  for (i in 1:length(albumidvector)){
    result<-lookup_release_by_id(albumidvector[i], includes = "tags")
    topgenre<-result[["tags"]][[1]]$tag_name[which.max(result[["tags"]][[1]]$tag_count)]
    if (!is.null(topgenre)) {
      result$topgenre<-topgenre
    } else {
      result$topgenre<-NA
    }
    erglist<-rbind(erglist,result)
  }
  erglist<-select(erglist,
                  album.mb.id = mbid,
                  album.mb.title = title,
                  track.mb.genres = tags,
                  track.mb.topgenre = topgenre)
  return(erglist)
}



add_musicbrainz_track_infos <- function(dataframe) { # all of those cut ff after 50, so i need to do this in repeat loops and i definitely will. tomorrow.
  tracktitles <- dataframe$track.s.title
  tracktitles <- pull_musicbrainz_track_ids(tracktitles)
  trackids <- tracktitles$track.mb.id
  ergebnis <- cbind(dataframe, pull_musicbrainz_track_infos(trackids))
  return(ergebnis)
}

add_musicbrainz_album_infos <- function(dataframe) { # i tried only checking the unique albums but merging didn't work,
                                                    # but here would be a good place to do that
  albumtitles <- dataframe$album.s.title
  albumtitles <- pull_musicbrainz_album_ids(albumtitles)
  albumids <- albumtitles$album.mb.id
  ergebnis <- cbind(dataframe, pull_musicbrainz_album_infos(albumids))
  return(ergebnis)
}

add_musicbrainz_artist_infos <- function(dataframe) { # see above
  artistnames <- dataframe$artist.s.name
  artistnames <- pull_musicbrainz_artist_ids(artistnames)
  artistids <- artistnames$artist.mb.id
  ergebnis <- cbind(dataframe, pull_musicbrainz_artist_infos(artistids))
  return(ergebnis)
}

