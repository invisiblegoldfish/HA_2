
rm(list=ls()) # clear workspace

# Teil 2 Trendstatistiken


library(jmv)   # import jamovi functions
library(flextable) # import table functions
library(countrycode) # import ISO countrycode functions
library(ggplot2) # import plot_functionality

de_tracks<-readRDS("germanDailySpotify_full_mb_artistbased.rds")
de_tracks_verysmall <- de_tracks %>% head(n=100)


############ Trendgrafiken

#metrisches Feature: streamCound und dessen Log für Übersichtlichkeit
de_tracks$track.s.streamCountLOG <- log(de_tracks$track.s.streamCount + 1)

trends_mean<-de_tracks %>%
  group_by(track.s.chartsDate) %>%
  summarise_at(c("track.s.energy","track.s.valence","track.s.danceability","track.s.tempo","track.s.streamCount","track.s.streamCountLOG"),
               list(average=~mean(.x,na.rm=T),upper=~mean(.x,na.rm=T)+sd(.x,na.rm=T),
                    lower=~mean(.x,na.rm=T)-sd(.x,na.rm=T),
                    min=~min(.x),max=~max(.x))) %>%
  ungroup()



ggplot(data=trends_mean,aes(x=track.s.chartsDate))+
  geom_line(aes(y=track.s.streamCount_average),size=0.5)+
  geom_line(aes(y=track.s.streamCount_min),linetype="dotted")+
  geom_line(aes(y=track.s.streamCount_max),linetype="dotted")+
  geom_ribbon(aes(ymin=track.s.streamCount_lower,ymax=track.s.streamCount_upper),alpha=0.1)+
  scale_x_date(expand=c(0,15),date_minor_breaks = "month")+
  labs(y="No of Streams",x="date")

ggplot(data=trends_mean,aes(x=track.s.chartsDate))+
  geom_line(aes(y=track.s.streamCountLOG_average),size=0.5)+
  geom_line(aes(y=track.s.streamCountLOG_min),linetype="dotted")+
  geom_line(aes(y=track.s.streamCountLOG_max),linetype="dotted")+
  geom_ribbon(aes(ymin=track.s.streamCountLOG_lower,ymax=track.s.streamCountLOG_upper),alpha=0.1)+
  scale_x_date(expand=c(0,15),date_minor_breaks = "month")+
  labs(y="No of Streams (LOG)",x="date")

##non parametrisches Feature: höchste Chartposition jeweils des Genres: Pop,Rock, Rap (und subgenres)


# nach Genre vorfiltern
de_tracks_pop <- de_tracks %>%
  filter(grepl("pop", artist.s.topGenre)) # Including only Pop and Subgenres
de_tracks_pop$artist.s.topGenre <- "Pop" # define general genre to aggregate easier later
de_tracks_rock <- de_tracks %>%
  filter(grepl("rock", artist.s.topGenre)) # Including only Rock and Subgenres
de_tracks_rock$artist.s.topGenre <- "Rock" 
de_tracks_rap <- de_tracks %>%
  filter(grepl("rap|hiphop|hip-hop", artist.s.topGenre)) # Including only Rap,Hiphop and Subgenres
de_tracks_rap$artist.s.topGenre <- "Rap" 

de_vorauswahl<-rbind(de_tracks_pop,de_tracks_rock,de_tracks_rap)
de_vorauswahl <- de_vorauswahl %>%arrange(track.s.chartsDate)
de_vorauswahl_small <-  de_vorauswahl %>% head(n=100)


# calculate best entry for each genre
highest_positions <- aggregate(track.s.chartPos ~ track.s.chartsDate + artist.s.topGenre, data = de_vorauswahl, min)
highest_positions <- highest_positions %>%arrange(track.s.chartsDate)
highest_positions_small <-  highest_positions %>% head(n=100)

highest_positions <- highest_positions %>% # combine 3 rows per date into 1 row per date with 3 genre collumns
  # First, we'll spread the entries to wide format based on genre
  pivot_wider(names_from = artist.s.topGenre, values_from = track.s.chartPos) %>%
  # Rename the columns to have the desired names
  rename(PopBestEntry = Pop, RockBestEntry = Rock, RapBestEntry = Rap) %>%
  # Ensure there's one row per date
  group_by(track.s.chartsDate) %>%
  summarize(across(everything(), ~ .[1]))

highest_positions_small <-  highest_positions %>% head(n=100)

highest_positions_obj<- highest_positions %>%
  pivot_longer(cols=c("PopBestEntry","RockBestEntry","RapBestEntry"),
               names_to="feature")


highest_positions_plot <- ggplot(data=highest_positions_obj, aes(x=track.s.chartsDate, y=value, colour=feature)) +
  geom_line() +
  geom_smooth(method="loess", span=.5, linetype="dashed", se=FALSE) +
  labs(title="Spotify German Daily Top 200 Charts 2017-2023: Highest Placed Pop/Rap/Rock Songs by Date",
       subtitle=paste0("n = ",nrow(highest_positions_obj)),x="Date",y="Highest Chart Place in Genre")


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
