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
metric_feature_des<-descriptives(short_tracks,vars=c("track.s.energy",
                                 "track.s.valence",
                                 "track.s.danceability",
                                 "track.s.tempo",
                                 "track.s.liveness",
                                 "track.s.instrumentalness",
                                 "track.s.speechiness",
                                 "track.s.duration_ms",
                                 "track.s.acousticness",
                                 "track.s.key",
                                 "track.s.mode",
                                 "track.s.loudness"),
             hist=T, iqr=T,skew=T,kurt=T,dens=T)

##berechnung desktriptoren der nominalen features
key_plot<-ggplot(data=short_tracks,aes(x=track.s.key))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="Shorttermtracks: Key of Tracks",
       subtitle=paste0("n = ",nrow(short_tracks)),x="key",y="frequency")
key_plot
ggsave("key_plot.png")


ggplot(data=drop_na(short_tracks,artist.s.topgenre),
       aes(y=fct_lump_prop(fct_infreq(artist.s.topgenre),prop=.005,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(y="genre",x="frequency")
