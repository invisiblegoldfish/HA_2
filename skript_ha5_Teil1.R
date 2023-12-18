#Bereiten Sie die Fragebogendaten auf:
rm(list=ls()) # clear workspace



library(haven) # import SPSS Functions
library(performance)  # import statistical functions
library(jmv)   # import jamovi functions

source("wrapper_extensions.R", encoding="UTF-8")
source("ownTools.R", encoding="UTF-8")

frabo_daten<-read_sav("files_ha5/befragungsdaten.sav")

#indices bestimmen für "musical training MT" und "active engagement AE"
mt_columns <- grep("MT", names(frabo_daten), value = TRUE)
mt_columns_string <- paste(mt_columns, collapse = ", ")
print(mt_columns_string)

ae_columns <- grep("AE", names(frabo_daten), value = TRUE)
ae_columns_string <- paste(ae_columns, collapse = ", ")
print(ae_columns_string)


##### Scores berechnen
frabo_fertig<-frabo_daten %>%  
  mutate_at(vars(neoFFI30_item6, neoFFI30_item11, neoFFI30_item21, neoFFI30_item26, neoFFI30_item41, neoFFI30_item51,
                 neoFFI30_item2, neoFFI30_item7, neoFFI30_item22, neoFFI30_item32, neoFFI30_item37, neoFFI30_item52,
                 neoFFI30_item8, neoFFI30_item13, neoFFI30_item23, neoFFI30_item43, neoFFI30_item48, neoFFI30_item58,
                 neoFFI30_item9, neoFFI30_item14, neoFFI30_item24, neoFFI30_item39, neoFFI30_item49, neoFFI30_item59,
                 neoFFI30_item5, neoFFI30_item10, neoFFI30_item20, neoFFI30_item40, neoFFI30_item50, neoFFI30_item55,
                 gmsi_MT03, gmsi_MT07, gmsiMT01, gmsiMT06, gmsiMT02,
                 gmsi_AE01, gmsi_AE02, gmsi_AE05, gmsi_AE07
                 ),
            ~replace_na(., mean(., na.rm = TRUE))) %>%
  rowwise()%>%
  mutate(
    neuroticism = mean(c(neoFFI30_item6, neoFFI30_item11, neoFFI30_item21,
                         neoFFI30_item26, neoFFI30_item41, neoFFI30_item51)),
    extraversion = mean(c(neoFFI30_item2, neoFFI30_item7, neoFFI30_item22,
                          neoFFI30_item32, neoFFI30_item37, neoFFI30_item52)),  
    openness = mean(c(neoFFI30_item8, neoFFI30_item13, neoFFI30_item23,
                      neoFFI30_item43, neoFFI30_item48, neoFFI30_item58)),  
    intolerance = mean(c(neoFFI30_item9, neoFFI30_item14, neoFFI30_item24,
                         neoFFI30_item39, neoFFI30_item49, neoFFI30_item59)),  
    conscientiousness = mean(c(neoFFI30_item5, neoFFI30_item10, neoFFI30_item20,
                               neoFFI30_item40, neoFFI30_item50, neoFFI30_item55)),  
    active_engangement = mean(c(gmsi_MT03, gmsi_MT07, gmsiMT01, gmsiMT06, gmsiMT02)),  
    musical_training = mean(c(gmsi_AE01, gmsi_AE02, gmsi_AE05, gmsi_AE07))
    
  )

### Scores inspizieren

#### Reliabilität 

cA_neuroticism<-frabo_daten %>% select(neoFFI30_item6,neoFFI30_item11,neoFFI30_item21,         
                       neoFFI30_item26,neoFFI30_item41,neoFFI30_item51) %>% cronbachs_alpha()
cA_extraversion<-frabo_daten %>% select(neoFFI30_item2, neoFFI30_item7, neoFFI30_item22,
                                        neoFFI30_item32, neoFFI30_item37, neoFFI30_item52) %>% cronbachs_alpha()
cA_openness<-frabo_daten %>% select(neoFFI30_item8, neoFFI30_item13, neoFFI30_item23,
                                    neoFFI30_item43, neoFFI30_item48, neoFFI30_item58) %>% cronbachs_alpha()
cA_intolerance<-frabo_daten %>% select(neoFFI30_item9, neoFFI30_item14, neoFFI30_item24,
                                       neoFFI30_item39, neoFFI30_item49, neoFFI30_item59) %>% cronbachs_alpha()
cA_conscientiousness<-frabo_daten %>% select(neoFFI30_item5, neoFFI30_item10, neoFFI30_item20,
                                             neoFFI30_item40, neoFFI30_item50, neoFFI30_item55) %>% cronbachs_alpha()
cA_active_engangement<-frabo_daten %>% select(gmsi_MT03, gmsi_MT07, gmsiMT01, gmsiMT06, gmsiMT02) %>% cronbachs_alpha()
cA_musical_training<-frabo_daten %>% select(gmsi_AE01, gmsi_AE02, gmsi_AE05, gmsi_AE07) %>% cronbachs_alpha()


###### Verteilung
des_neuroticism<-descriptives(frabo_fertig,neuroticism, hist=T,qq=T,sw=T) 
ks_neuroticism<-ks.test(frabo_fertig$neuroticism,"pnorm",exact=T)
des_neuroticism_df <- as.data.frame(des_neuroticism$descriptives)
des_neuroticism_df["Cronbach's Alpha"] <- cA_neuroticism
ks_neuroticism_df<-ks_to_df(ks_neuroticism)

des_extraversion<-descriptives(frabo_fertig,extraversion, hist=T,qq=T,sw=T) 
ks_extraversion<-ks.test(frabo_fertig$extraversion,"pnorm",exact=T)
des_extraversion_df <- as.data.frame(des_extraversion$descriptives)
des_extraversion_df["Cronbach's Alpha"] <- cA_extraversion
ks_extraversion_df<-ks_to_df(ks_extraversion)

des_openness<-descriptives(frabo_fertig,openness, hist=T,qq=T,sw=T) 
ks_openness<-ks.test(frabo_fertig$openness,"pnorm",exact=T)
des_openness_df <- as.data.frame(des_openness$descriptives)
des_openness_df["Cronbach's Alpha"] <- cA_openness
ks_openness_df<-ks_to_df(ks_openness)

des_intolerance<-descriptives(frabo_fertig,intolerance, hist=T,qq=T,sw=T) 
ks_intolerance<-ks.test(frabo_fertig$intolerance,"pnorm",exact=T)
des_intolerance_df <- as.data.frame(des_intolerance$descriptives)
des_intolerance_df["Cronbach's Alpha"] <- cA_intolerance
ks_intolerance_df<-ks_to_df(ks_intolerance)

des_conscientiousness<-descriptives(frabo_fertig,conscientiousness, hist=T,qq=T,sw=T) 
ks_conscientiousness<-ks.test(frabo_fertig$conscientiousness,"pnorm",exact=T)
des_conscientiousness_df <- as.data.frame(des_conscientiousness$descriptives)
des_conscientiousness_df["Cronbach's Alpha"] <- cA_conscientiousness
ks_conscientiousness_df<-ks_to_df(ks_conscientiousness)

des_active_engangement<-descriptives(frabo_fertig,active_engangement, hist=T,qq=T,sw=T) 
ks_active_engangement<-ks.test(frabo_fertig$active_engangement,"pnorm",exact=T)
des_active_engangement_df <- as.data.frame(des_active_engangement$descriptives)
des_active_engangement_df["Cronbach's Alpha"] <- cA_active_engangement
ks_active_engangement_df<-ks_to_df(ks_active_engangement)

des_musical_training<-descriptives(frabo_fertig,musical_training, hist=T,qq=T,sw=T) 
ks_musical_training<-ks.test(frabo_fertig$musical_training,"pnorm",exact=T)
des_musical_training_df <- as.data.frame(des_musical_training$descriptives)
des_musical_training_df["Cronbach's Alpha"] <- cA_musical_training
ks_musical_training_df<-ks_to_df(ks_musical_training)

##### build dataframe for all desctriptors

variable_names <- c("neuroticism", "extraversion", "openness", "intolerance", "conscientiousness", "active_engagement", "musical_training")

des_neuroticism_df<-remove_prefix(des_neuroticism_df,"neuroticism")
des_extraversion_df<-remove_prefix(des_extraversion_df,"extraversion")
des_openness_df<-remove_prefix(des_openness_df,"openness")
des_intolerance_df<-remove_prefix(des_intolerance_df,"intolerance")
des_conscientiousness_df<-remove_prefix(des_conscientiousness_df,"conscientiousness")
des_active_engangement_df<-remove_prefix(des_active_engangement_df,"engangement")
des_musical_training_df<-remove_prefix(des_musical_training_df,"musical_training")

column_names <- colnames(des_neuroticism_df)
colnames(des_extraversion_df) <- column_names
colnames(des_openness_df) <- column_names
colnames(des_intolerance_df) <- column_names
colnames(des_conscientiousness_df) <- column_names
colnames(des_active_engangement_df) <- column_names
colnames(des_musical_training_df) <- column_names

# combinde descriptors
descriptive_dfs <- rbind(
  des_neuroticism_df,
  des_extraversion_df,
  des_openness_df,
  des_intolerance_df,
  des_conscientiousness_df,
  des_active_engangement_df,
  des_musical_training_df
)

# combine ks results
ks_dfs <- rbind(
  ks_neuroticism_df,
  ks_extraversion_df,
  ks_openness_df,
  ks_intolerance_df,
  ks_conscientiousness_df,
  ks_active_engangement_df,
  ks_musical_training_df
)

all_desctriptors<-cbind(descriptive_dfs,ks_dfs)
rownames(all_desctriptors) <- variable_names


## Fragebogendaten mit Spotify_Daten vereinen

spotify_daten<-read_delim("files_ha5/Spotivey_Results.csv",delim=";") %>% rename(id=participant_id)

combined_data<-inner_join(frabo_fertig,spotify_daten,by="id")

saveRDS(combined_data,"files_ha5/personendaten.rds")
saveRDS(all_desctriptors,"files_ha5/frabo_diskreptoren.rds")
