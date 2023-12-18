rm(list=ls()) # clear workspace

# Aggregat- und Trendstatistiken - Teil1


library(jmv)   # import jamovi functions
library(flextable) # import table functions
library(countrycode) # import ISO countrycode functions
library(ggplot2) # import plot_functionality

source("wrapper_extensions.R", encoding="UTF-8")

short_tracks<-readRDS("mb_my_recent_tracks2023-12-04.rds")


short_tracks<-short_tracks %>% distinct(track.s.id,.keep_all=T)




##berechnung desktriptoren der metrischen features
metric_feature_des<-descriptives(short_tracks,vars=c("track.s.popularity",
                                                     "track.s.duration_ms",
                                                     "track.s.albumPosition",
                                                     "track.s.danceability",
                                                     "track.s.energy",
                                                     "track.s.loudness",
                                                     "track.s.speechiness",
                                                     "track.s.acousticness",
                                                     "track.s.instrumentalness",
                                                     "track.s.liveness",
                                                     "track.s.valence",
                                                     "track.s.tempo",
                                                     "album.s.total_tracks",
                                                     "album.s.release_year",
                                                     "album.s.popularity",
                                                     "artist.s.popularity",
                                                     "artist.s.followers",
                                                     "artist.mb.birthyear",
                                                     "artist.mb.deathyear"),
             hist=T, iqr=T,skew=T,kurt=T,dens=T)

######berechnung desktriptoren der nominalen features
## wenig Ausprägungen

key_plot<-ggplot(data=short_tracks,aes(x=track.s.key))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="My Personal Short Term Tracks: Key of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),x="key",y="frequency")
key_plot
ggsave("key_plot.png")

explicit_plot <- ggplot(data=short_tracks,aes(x=track.s.explicit))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="My Personal Short Term Tracks: Explicit Lyrics",
       subtitle=paste0("n = ",nrow(short_tracks)),x="explicit",y="frequency")
explicit_plot

mode_plot <- ggplot(data=short_tracks,aes(x=track.s.mode))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="My Personal Short Term Tracks: Mode of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),x="mode",y="frequency")
mode_plot

time_signature_plot <- ggplot(data=short_tracks,aes(x=track.s.time_signature))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="My Personal Short Term Tracks: Time Signature of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),x="time signature",y="frequency")
time_signature_plot

# everything is of album type "album"
# ggplot(data=short_tracks,aes(x=album.s.type))+
#   geom_bar(aes(y=..count..))+
#   geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
#             stat="count",vjust=1.5,color="white",size=3)+
#   labs(title="My Personal Short Term Tracks: Album Type that Tracks Appear on",
#        subtitle=paste0("n = ",nrow(short_tracks)),x="album type",y="frequency")


## viele Ausprägungen

label_plot <- ggplot(data=drop_na(short_tracks,album.s.label),
                     aes(y=fct_lump_prop(fct_infreq(album.s.label),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="My Personal Short Term Tracks: Label of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),y="label",x="frequency")
label_plot

artist_top_genre_plot <- ggplot(data=drop_na(short_tracks,artist.s.topgenre),
                                aes(y=fct_lump_prop(fct_infreq(artist.s.topgenre),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="My Personal Short Term Tracks: Top Genre of Artists",
       subtitle=paste0("n = ",nrow(short_tracks)," tracks"),y="genre",x="frequency")
artist_top_genre_plot # not sure about this one, feels wrong to group by songs, then analyse artists...





