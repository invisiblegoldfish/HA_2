
rm(list=ls()) # clear workspace

# Aggregat- und Trendstatistiken


library(jmv)   # import jamovi functions
library(flextable) # import table functions
library(countrycode) # import ISO countrycode functions

source("wrapper_extensions.R", encoding="UTF-8")

de_tracks<-readRDS("germanDailySpotify_full_mb_artistbased.rds")

de_tracks_small <- de_tracks %>% head(n=10000)

### Aggregatstatistiken für metrische Variablen

all_tracks<-de_tracks %>% distinct(track.s.id,.keep_all=T)


# data that doesn't have to be cumulated
metric_descriptives <- descriptives(all_tracks,vars=c("track.s.popularity",
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
                                                      "album.s.popularity",
                                                      "artist.s.popularity",
                                                      "artist.s.followers"),
                       hist=T, iqr=T,skew=T,kurt=T,dens=T)
metric_descriptives

### Aggregatstatistiken für nominale Variablen

## wenig Ausprägungen

key_plot<-ggplot(data=all_tracks,aes(x=track.s.key))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Key of Tracks",
       subtitle=paste0("n = ",nrow(all_tracks)),x="key",y="frequency")
key_plot
ggsave("key_plot.png")

explicit_plot <- ggplot(data=all_tracks,aes(x=track.s.explicit))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Explicit Lyrics",
       subtitle=paste0("n = ",nrow(all_tracks)),x="explicit",y="frequency")
explicit_plot

mode_plot <- ggplot(data=all_tracks,aes(x=track.s.mode))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Mode of Tracks",
       subtitle=paste0("n = ",nrow(all_tracks)),x="mode",y="frequency")
mode_plot

time_signature_plot <- ggplot(data=all_tracks,aes(x=track.s.time_signature))+
  geom_bar(aes(y=..count..))+
  geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
            stat="count",vjust=1.5,color="white",size=3)+
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Time Signature of Tracks",
       subtitle=paste0("n = ",nrow(all_tracks)),x="time signature",y="frequency")
time_signature_plot

# everything is of album type "album"
# ggplot(data=all_tracks,aes(x=album.s.type))+
#   geom_bar(aes(y=..count..))+
#   geom_text(aes(label=scales::percent(..count../sum(..count..),accuracy = 0.1)),
#             stat="count",vjust=1.5,color="white",size=3)+
#   labs(title="Spotify German Daily Top 200 Charts 2017-2023: Album Type that Tracks Appear on",
#        subtitle=paste0("n = ",nrow(all_tracks)),x="album type",y="frequency")


## viele Ausprägungen

combinedgenre_plot <- ggplot(data=drop_na(all_tracks,track.mb.combinedgenre),
                             aes(y=fct_lump_prop(fct_infreq(track.mb.combinedgenre),prop=.005,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Genre of Tracks",
       subtitle=paste0("n = ",nrow(all_tracks)),y="genre",x="frequency")
combinedgenre_plot


label_plot <- ggplot(data=drop_na(all_tracks,album.s.label),
                     aes(y=fct_lump_prop(fct_infreq(album.s.label),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Label of Tracks",
       subtitle=paste0("n = ",nrow(all_tracks)),y="label",x="frequency")
label_plot

artist_top_genre_plot <- ggplot(data=drop_na(all_tracks,artist.s.topGenre),
                                aes(y=fct_lump_prop(fct_infreq(artist.s.topGenre),prop=.01,other_level="other")))+
  geom_bar(aes(x=..count..))+
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Top Genre of Artists",
       subtitle=paste0("n = ",nrow(all_tracks)," tracks"),y="genre",x="frequency")
artist_top_genre_plot # not sure about this one, feels wrong to group by songs, then analyse artists...



######################## Top Ten Tabellen für Nominalvariablen

all_artists<-de_tracks %>% distinct(artist.s.id,.keep_all=T)

top_artists_de<-de_tracks %>%
  count(artist.s.id) %>%
  arrange(-n) %>%
  head(20) %>%
  left_join(all_artists) %>%
  select(days=n,artist.s.id,artist.s.name,artist.mb.type,artist.mb.gender,artist.mb.birthyear,
         artist.mb.origin,artist.mb.topGenre) %>%
  mutate(URL=paste0("https://open.spotify.com/artist/",artist.s.id))

top_artists_table<-top_artists_de %>%
  flextable(col_keys = c("days","artist.s.name","artist.mb.type","artist.mb.gender","artist.mb.birthyear",
                         "artist.mb.origin","artist.mb.topGenre")) %>%
  autofit() %>%
  compose(j=2,value=as_paragraph(hyperlink_text(x=artist.s.name,url=URL))) %>%
  print(preview="pptx") %>%
  save_as_html(path="top20artistsde.html")

#########



############ Trendgrafiken

trends_mean<-de_tracks %>%
  group_by(track.s.chartsDate) %>%
  summarise_at(c("track.s.energy","track.s.valence","track.s.danceability","track.s.tempo"),
               list(average=~mean(.x,na.rm=T),upper=~mean(.x,na.rm=T)+sd(.x,na.rm=T),
                    lower=~mean(.x,na.rm=T)-sd(.x,na.rm=T),
                    min=~min(.x),max=~max(.x))) %>%
  ungroup()


ggplot(data=trends_mean,aes(x=track.s.chartsDate))+
  geom_line(aes(y=track.s.tempo_average),size=0.5)+
  geom_line(aes(y=track.s.tempo_min),linetype="dotted")+
  geom_line(aes(y=track.s.tempo_max),linetype="dotted")+
  geom_ribbon(aes(ymin=track.s.tempo_lower,ymax=track.s.tempo_upper),alpha=0.1)+
  scale_x_date(expand=c(0,15),date_minor_breaks = "month")+
  labs(y="tempo",x="date")

trends_features_mean<-select(trends_mean,track.s.chartsDate,ends_with("average")) %>%
  pivot_longer(cols=c("track.s.energy_average","track.s.valence_average","track.s.danceability_average"),
               names_to="feature")

ggplot(data=trends_features_mean,aes(x=track.s.chartsDate,y=value,color=feature))+
  geom_line()+
  geom_smooth(method="loess",span=.5,size=2,se=F)


######################### Nominaltrends



trends_gender <- de_tracks %>% 
  group_by(track.s.chartsDate) %>% 
  count(artist.mb.gender) %>%  
  mutate(prop = prop.table(n)) %>% 
  ungroup () %>%
  complete(artist.mb.gender, track.s.chartsDate, fill = list(prop = 0)) %>%   # fill unmentioned categories per observation with NAs
  mutate(perc=prop*100)


trendchart_gender<-ggplot(data=trends_gender,aes(x=track.s.chartsDate,y=perc,fill=artist.mb.gender))+
  geom_area()+
  scale_x_date(expand=c(0,15),date_minor_breaks = "1 month")+
  scale_y_continuous(expand=c(0,1))+
  scale_fill_brewer(palette="Paired")+
  labs(title="Spotify German Daily Charts 2017-2023: Artist Gender Across Time", x="date", y="percentage", fill="Gender",
       subtitle=paste0("n=",nrow(trends_mean)," time points, relative frequencies within 200 tracks per observation (=day)"))
trendchart_gender














