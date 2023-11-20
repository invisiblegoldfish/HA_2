rm(list=ls()) # clear workspace

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

pull_musicbrainz_artist_infos<-function(artistidvector){
  erglist<-c()
  for (i in 1:length(artistidvector)){
    cat("Searching genre for id",artistidvector[i],"\n")
    result<-lookup_artist_by_id(artistidvector[i], includes="tags")
    topgenre<-result[["tags"]][[1]]$tag_name[which.max(result[["tags"]][[1]]$tag_count)]
    result<-select(result,tags)
    print(topgenre)
    if (!is.null(topgenre)) {
      result$topgenre<-topgenre
    } else {
      result$topgenre<-NA
    }
    erglist<-rbind(erglist,result)
  }
  erglist<-select(erglist,artist.mb.genres=tags,artist.mb.topgenre=topgenre)
  return(erglist)
}


##### MAIN
top_artists<-readRDS("a_my_top_artists_2023-11-14.rds")

top_artists2<-cbind(top_artists,pull_musicbrainz_artist_ids(top_artists$artist.s.name)) # Search

top_artists2<-select(top_artists2,artist.s.name,artist.mb.name,artist.mb.quality,everything()) # Sortierung der Variablen

summarize(top_artists2,average_artist_quality=mean(artist.mb.quality)) # Auswertung der Qualität

top_artists3<-cbind(top_artists2,pull_musicbrainz_artist_infos(top_artists2$artist.mb.id)) # Lookup


