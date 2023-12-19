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
                                                     "artist.s.followers"),
             hist=T, iqr=T,skew=T,kurt=T,dens=T)

metric_feature_des$plots[[i]]
ggsave(paste0("metric_features_", i, ".png"))


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
ggsave("explicit_plot.png")

mode_plot <- ggplot(data=short_tracks,aes(x=track.s.mode))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="My Personal Short Term Tracks: Mode of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),x="mode",y="frequency")
mode_plot
ggsave("mode_plot.png")

time_signature_plot <- ggplot(data=short_tracks,aes(x=track.s.time_signature))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="My Personal Short Term Tracks: Time Signature of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),x="time signature",y="frequency")
time_signature_plot
ggsave("time_signature_plot.png")

album_type_plot <- ggplot(data=short_tracks,aes(x=album.s.type))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="My Personal Short Term Tracks: Album Type that Tracks Appear on",
       subtitle=paste0("n = ",nrow(short_tracks)),x="album type",y="frequency")
album_type_plot
ggsave("album_type_plot.png")


## viele Ausprägungen

artist_plot <- ggplot(data=drop_na(short_tracks,track.s.firstartist.name),
                      aes(y=fct_lump_prop(fct_infreq(track.s.firstartist.name),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="My Personal Short Term Tracks: Frequency of Artists",
       subtitle=paste0("n = ",nrow(short_tracks)),y="artist",x="frequency")
artist_plot
ggsave("artist_plot.png")

# top_genre_mb_plot <- ggplot(data=drop_na(short_tracks,track.mb.topgenre),
#                       aes(y=fct_lump_prop(fct_infreq(track.mb.topgenre),prop=.01,other_level="other")))+
#   geom_bar(aes(x=..count..))+
#   labs(title="My Personal Short Term Tracks: Frequency of Genres (Musicbrainz)",
#        subtitle=paste0("n = ",nrow(short_tracks)),y="genre",x="frequency")
# top_genre_mb_plot

album_plot <- ggplot(data=drop_na(short_tracks,album.s.title),
                      aes(y=fct_lump_prop(fct_infreq(album.s.title),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="My Personal Short Term Tracks: Frequency of Albums",
       subtitle=paste0("n = ",nrow(short_tracks)),y="album",x="frequency")
album_plot
ggsave("album_plot.png")

label_plot <- ggplot(data=drop_na(short_tracks,album.s.label),
                     aes(y=fct_lump_prop(fct_infreq(album.s.label),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="My Personal Short Term Tracks: Label of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),y="label",x="frequency")
label_plot
ggsave("label_plot.png")

artist_top_genre_plot <- ggplot(data=drop_na(short_tracks,artist.s.topgenre),
                                aes(y=fct_lump_prop(fct_infreq(artist.s.topgenre),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="My Personal Short Term Tracks: Top Genre of Artists",
       subtitle=paste0("n = ",nrow(short_tracks)," tracks"),y="genre",x="frequency")
artist_top_genre_plot 
ggsave("artist_top_genre_plot.png")

