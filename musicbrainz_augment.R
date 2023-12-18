# Augmentierung von Musikdaten mit MusicBrainz API

source("wrapper_extensions.R", encoding="UTF-8")

library(musicbrainz)
library(stringdist)
library(httr)



#get Artist Infos:
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

add_musicbrainz_artist_infos <- function(dataframe) { # see above
 
  artistnames <- dataframe$artist.s.name
  
  artistnamesids <- pull_musicbrainz_artist_ids(artistnames)
  artistids <- artistnamesids$artist.mb.id
  ergebnis <- cbind(dataframe, pull_musicbrainz_artist_infos(artistids))
  return(ergebnis)
}

pull_musicbrainz_artist_infos<-function(artistidvector){
  erglist<-c()
  
  for (i in 1:length(artistidvector)){
    cat("Searching genre for id",artistidvector[i],"\n")
    cat(i)
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

api_call_artists_mb <- function(dataframe, max_rowEntries_per_api_call) {
  no_rows <- nrow(dataframe)
  
  # Return an empty data frame if the input dataframe is empty
  if (no_rows == 0) {
    return(data.frame())
  }
  
  # Process the entire dataframe directly if it's smaller than max_rowEntries_per_api_call
  if (no_rows <= max_rowEntries_per_api_call) {
    return(add_musicbrainz_artist_infos(dataframe))
  }
  
  quotient <- no_rows %/% max_rowEntries_per_api_call
  remainder <- no_rows %% max_rowEntries_per_api_call
  
  # List to hold results from each chunk
  results_list <- list()
  
  for (i in 0:(quotient - 1)) {
    start_row <- i * max_rowEntries_per_api_call + 1
    end_row <- start_row + max_rowEntries_per_api_call - 1
    
    subset_dataframe <- dataframe[start_row:end_row, ]
    processed_chunk <- add_musicbrainz_artist_infos(subset_dataframe)
    
    # Check if processed_chunk is not empty and has the correct structure
    if (nrow(processed_chunk) > 0 && ncol(processed_chunk) == ncol(dataframe)) {
      results_list[[length(results_list) + 1]] <- processed_chunk
    }
  }
  
  # Handle the remainder if it exists
  if (remainder > 0) {
    start_row <- quotient * max_rowEntries_per_api_call + 1
    subset_dataframe <- dataframe[start_row:no_rows, ]
    
    processed_chunk <- add_musicbrainz_artist_infos(subset_dataframe)
    
    # Check if processed_chunk is not empty and has the correct structure
    if (nrow(processed_chunk) > 0 && ncol(processed_chunk) == ncol(dataframe)) {
      results_list[[length(results_list) + 1]] <- processed_chunk
    }
  }
  
  # Combine all processed chunks
  if (length(results_list) > 0) {
    final_result <- do.call(rbind, results_list)
  } else {
    final_result <- data.frame(matrix(ncol = ncol(dataframe), nrow = 0))  # Return an empty data frame with the correct number of columns
  }
  
  return(final_result)
}




# get Track Infos:
pull_musicbrainz_track_ids<-function(trackidvector){ # i should do this with ids but my songs don't have those
  idlist<-c()
  
  
  for (i in 1:length(trackidvector[,1])){
    quality <- 0.0
    
    #search for irsc first, then check if succesfull-> quality=1; else search for combination of tracktitle and artistname
    tryCatch({
      isrc_code <- trackidvector[i, 1]
      query_string <- paste0("isrc:", isrc_code)
      
      result <- search_recordings(query_string, limit=1)
      cat("Searching for",trackidvector[i,2],"using ISCR \n")
      if (nrow(result) > 0) {
        print("Search for ISCR was successful")
        quality <- 1
      } else {
        
        # create query string using Apache Lucene search syntax, suited for musicbrainz
       # apache_query=paste0("title:(", trackidvector[i,"track.s.title"], ") AND artist:(", trackidvector[i,"artist.s.name"], ")")
        apache_query=paste0(trackidvector[i,"track.s.title"]," ",trackidvector[i,"artist.s.name"]," ")
        cat("Search for ISCR not successful: Serching for:",apache_query,"\n")
        result <- search_recordings(apache_query, limit=1)
        
        if (nrow(result)>0){
          print("Search for Tracktitle+Artistname was successful")
          quality<-stringsim(trackidvector[i,2],result$title,"jw")
    
        } else {
          print("Search Failed", "/n")
        }
        # Handle no results
      }
    }, error = function(e) {
      print(paste("Error during search:", e$message))
      # Handle the error scenario
    })
    
    
    result<-cbind(result,quality)
    idlist<-rbind(idlist,result)
  }
  
  
  idlist<-select(idlist,track.mb.title=title, track.mb.id=mbid, track.mb.quality=quality)
  return(idlist)
}

add_musicbrainz_track_infos <- function(dataframe) { 
  
 

 #trackinfos contains necassery infos for 'pull_musicbrainz_track_ids(trackidvector)'
  trackinfos<-data.frame()
  trackinfos<-select(dataframe, track.s.isrc, track.s.title, artist.s.name)

  tracktitles <- pull_musicbrainz_track_ids(trackinfos)
  trackids <- tracktitles$track.mb.id
  
  ergebnis <- cbind(dataframe, pull_musicbrainz_track_infos(trackids), tracktitles$track.mb.quality )
  #renaming track.mb.quality, since its calculated inside pull_track_infos
  names(ergebnis)[names(ergebnis) == "tracktitles$track.mb.quality"] <- "track.mb.quality"
  
  return(ergebnis)
  
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

api_call_tracks_mb <- function(dataframe, max_rowEntries_per_api_call) {
  quotient <- nrow(dataframe) %/% max_rowEntries_per_api_call
  remainder <- nrow(dataframe) %% max_rowEntries_per_api_call
  no_rows<-nrow(dataframe)
  # List to hold results from each chunk
  results_list <- list()
  if (no_rows<max_rowEntries_per_api_call){
    final_result<-add_musicbrainz_track_infos(dataframe)
  }else {
    for (i in 0:(quotient - 1)) {
      start_row <- i * max_rowEntries_per_api_call + 1
      end_row <- start_row + max_rowEntries_per_api_call - 1
      
      # Extract the subset of dataframe for processing
      subset_dataframe <- dataframe[start_row:end_row, ]
      
      # Process each chunk of the dataframe
      processed_chunk <- add_musicbrainz_track_infos(subset_dataframe)
      
      # Store the result
      results_list[[length(results_list) + 1]] <- processed_chunk
    }
    
    # Handle the remainder if it exists
    if (remainder > 0) {
      start_row <- quotient * max_rowEntries_per_api_call + 1
      subset_dataframe <- dataframe[start_row:nrow(dataframe), ]
      
      # Process the remainder of the dataframe
      processed_chunk <- add_musicbrainz_track_infos(subset_dataframe)
      
      # Store the result
      results_list[[length(results_list) + 1]] <- processed_chunk
    }
    
    # Combine all processed chunks
    final_result <- do.call(rbind, results_list)
  }
  return(final_result)
}






# get Album Infos:
pull_musicbrainz_album_ids<-function(albumidvector){
  idlist<-c()
  for (i in 1:length(albumidvector[,1])){
    albumquality <- 0.0
    #search for UPC first, then check if succesfull-> quality=1; else search for combination of albumtitle and artistname
    tryCatch({
      upc_code <- albumidvector[i, 1]
      query_string <- paste0("barcode:", upc_code)
      result <- search_releases(query_string, limit=1)
      cat("Searching for",albumidvector[i,2],"using UPC \n")
      if (nrow(result) > 0) {
        cat("Search for UPC:",albumidvector[i,1]," was successful! \n")
        albumquality <- 1
      } else {
        # create query string using Apache Lucene search syntax, suited for musicbrainz
        apache_query=paste0(albumidvector[i,"album.s.title"]," ",albumidvector[i,"artist.s.name"]," ")
        cat("Search for UPC not successful: Serching for:",apache_query,"\n")
        result <- search_releases(apache_query, limit=1)
        
        if (nrow(result)>0){
          print("Search for Albumtitle+Artistname was successful")
          albumquality<-stringsim(albumidvector[i,2],result$title,"jw")
        } else {
          print("Search Failed", "/n")
        }
        # Handle no results
      }
    }, error = function(e) {
      print(paste("Error during search:", e$message))
      # Handle the error scenario
    })
    
    result<-cbind(result,albumquality)
    idlist<-rbind(idlist,result)
  }
  
  
  idlist<-select(idlist,album.mb.title=title, album.mb.id=mbid, album.mb.quality=albumquality)
  return(idlist)
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

add_musicbrainz_album_infos <- function(dataframe) { # i tried only checking the unique albums but merging didn't work,
                                                    # but here would be a good place to do that
  
  #albuminfos contains necassery infos for 'pull_musicbrainz_album_ids(albumidvector)'
  albuminfos<-data.frame()
  albuminfos<-select(dataframe, album.s.upc, album.s.title, artist.s.name)
  
  albumtitles <- pull_musicbrainz_album_ids(albuminfos)
  albumids <- albumtitles$album.mb.id
  
  ergebnis <- cbind(dataframe, pull_musicbrainz_album_infos(albumids), albumtitles$album.mb.quality)
  #renaming track.mb.quality, since its calculated inside pull_track_infos
  names(ergebnis)[names(ergebnis) == "albumtitles$album.mb.quality"] <- "album.mb.quality"
  return(ergebnis)
}

api_call_albums_mb <- function(dataframe, max_rowEntries_per_api_call) {
  quotient <- nrow(dataframe) %/% max_rowEntries_per_api_call
  remainder <- nrow(dataframe) %% max_rowEntries_per_api_call
  no_rows<-nrow(dataframe)
  # List to hold results from each chunk
  results_list <- list()
  if (no_rows<max_rowEntries_per_api_call){
    final_result<-add_musicbrainz_album_infos(dataframe)
  }else {
  for (i in 0:(quotient - 1)) {
    start_row <- i * max_rowEntries_per_api_call + 1
    end_row <- start_row + max_rowEntries_per_api_call - 1
    
    # Extract the subset of dataframe for processing
    subset_dataframe <- dataframe[start_row:end_row, ]
    
    # Process each chunk of the dataframe
    processed_chunk <- add_musicbrainz_album_infos(subset_dataframe)
    
    # Store the result
    results_list[[length(results_list) + 1]] <- processed_chunk
  }
  
  # Handle the remainder if it exists
  if (remainder > 0) {
    start_row <- quotient * max_rowEntries_per_api_call + 1
    subset_dataframe <- dataframe[start_row:nrow(dataframe), ]
    
    # Process the remainder of the dataframe
    processed_chunk <- add_musicbrainz_album_infos(subset_dataframe)
    
    # Store the result
    results_list[[length(results_list) + 1]] <- processed_chunk
  }
  
  # Combine all processed chunks
  final_result <- do.call(rbind, results_list)
  }
  return(final_result)
}

