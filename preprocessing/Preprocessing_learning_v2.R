library(tidyverse)


# Daten laden

load(file = "/home/nils/ICP_data/ICP_all_processed/ICP_all.RData")  

load(file = "/home/nils/ICP_data/ICP_all_processed/ICP_data_all.RData")




# Targets  ----------------------------------------------------------------------------  
# Größe < 30 wird auf NaN
# Outcome Haus wird dedroppt


ICP_data_all_target <-
  ICP_data_all %>%
  mutate(Pat_ID = paste0(DB,Pat_ID)) %>% #filter(is.na(Geschlecht)) %>%
  mutate(Geschlecht = ifelse(is.na(Geschlecht), rbinom(2, 1, 0.5), Geschlecht)) %>%
  mutate(Geschlecht = case_when(Geschlecht == '0' ~'Männlich',
                                Geschlecht == '1' ~'Weiblich',
                                TRUE ~ Geschlecht)) %>% #filter(!is.na(Geschlecht)) %>%
  mutate(Geschlecht = as.factor(Geschlecht)) %>% group_by(Geschlecht) %>% 
  mutate(Alter = ifelse(Alter < 5 ,NA,Alter)) %>%
  mutate(Größe = ifelse(Größe < 50 ,NA,Größe)) %>%
  mutate(Alter = ifelse(is.na(Alter),mean(Alter,na.rm = TRUE),Alter)) %>%
  mutate(Größe = ifelse(is.na(Größe),mean(Größe,na.rm = TRUE),Größe)) %>%
  mutate(Gewicht = ifelse(is.na(Gewicht),mean(Gewicht,na.rm = TRUE),Gewicht)) %>% 
  select(Pat_ID,DB,Diagnose_txt,Outcome,Outcome_Haus,Geschlecht,Alter,Größe,Gewicht) %>%
  ungroup()



summary(ICP_data_all_target)

rm(ICP_data_all)



#  Targets -  PatID mit fortlaufender Zahl ändern      ########################
ID_ICP <- 
  ICP_all %>% ungroup() %>%
  distinct(Pat_ID, DB)

ID_Pat <- ICP_data_all_target %>% 
  select(Pat_ID)%>% pull()


# gibt es Patienten zu denen noch Information aus der Datenbank fehlt?
ID_ICP %>% 
  mutate(Pat_ID = paste0(DB, Pat_ID)) %>%
  filter(!Pat_ID %in% ID_Pat) %>%
  count(DB)


ID_Number <-
  ID_ICP %>% 
  mutate(Pat_ID = paste0(DB, Pat_ID)) %>%
  filter(Pat_ID %in% ID_Pat) %>% 
  select(Pat_ID) %>%
  mutate(ID_Nr = 1:n())

save(ID_Number, file = "/home/nils/ICP_data/ICP_all_processed/ID_Number.RData")


load(file = "/home/nils/ICP_data/ICP_all_processed/ID_Number.RData")



# IDs werden abgeglichen mit ICP_data abgegelichen
ICP_all <-
  ICP_all %>%
  mutate(Pat_ID = paste0(DB, Pat_ID)) %>%
  filter(Pat_ID %in% ID_Pat) %>%
  left_join(ID_Number, by = 'Pat_ID') %>%
  select(-Pat_ID) %>% rename(Pat_ID = ID_Nr) %>%
  select(names(ICP_all))



# IDs vereinfachen
ICP_data_all_target <-
  ICP_data_all_target %>%
  left_join(ID_Number) %>%
  filter(!is.na(ID_Nr)) %>% 
  select(-Pat_ID) %>%
  rename(Pat_ID = ID_Nr) %>%
  select(names(ICP_data_all_target))



# Targets - one hot encoding  ######################
library(caret)
# Diagnose als one hot encoding
# Diganose_TBI 1 wenn es zutrifft
# DB als one hot encoding 
# DB_MIMIC 

ICP_data_all_target <-
  ICP_data_all_target %>%
  rename(Diagnose = Diagnose_txt) 

one_hot <- ICP_data_all_target %>% select(Pat_ID,DB,Diagnose)

one_hot <-
  one_hot %>% 
  mutate(DB = as.factor(DB), Diagnose = as.factor(Diagnose))

dmy <- caret::dummyVars(" ~ .", data = one_hot, sep = "_")
trsf <- data.frame(predict(dmy, newdata = one_hot))

trsf <-
  as_tibble(trsf)


trsf %>%
  filter(DB_UKE == 1) %>%
  distinct(Pat_ID) %>%
  ungroup() %>%
  summarise(max(Pat_ID))


# Z Normalisierung der numerischen Targets

ICP_targets_numeric <-
  ICP_data_all_target %>%
  select(Pat_ID,Alter,Größe,Gewicht) %>%
  mutate(Alter = (Alter - mean(Alter, na.rm = TRUE))/ sd(Alter, na.rm = TRUE)) %>%
  mutate(Größe = (Größe - mean(Größe, na.rm = TRUE))/ sd(Größe, na.rm = TRUE)) %>%
  mutate(Gewicht = (Gewicht - mean(Gewicht, na.rm = TRUE))/ sd(Gewicht, na.rm = TRUE)) 



# exitus = 1 und survived = 0
# Geschlecht als feature

ICP_targets <-
  ICP_data_all_target %>%
  select(Pat_ID, Geschlecht, Outcome) %>%
  mutate(Geschlecht = ifelse(str_detect(Geschlecht,'Männ'),1,0)) %>%
  mutate(Outcome = ifelse(str_detect(Outcome,'exit'),1,0))

# Übrigen Listen (Numerisch und One Hot werden hinzugefühgt)
ICP_targets <-
  ICP_targets %>% left_join(ICP_targets_numeric) %>%
  left_join(trsf)

rm(one_hot,trsf,dmy, ICP_data_all, ICP_targets_numeric, Pat_ID_too_less)



save(ICP_targets , file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_targets.RData"))

rm(ICP_targets)



# ICP_all - 5 min Abstände ------------

load(file = "/home/nils/ICP_data/ICP_all_processed/ICP_all.RData")  


# IDs werden abgeglichen mit ICP_data abgegelichen
# Erstmal laden

load(file = "/home/nils/ICP_data/ICP_all_processed/ID_Number.RData")

ID_Number$Pat_ID

# Dann anpassen
ICP_all <-
  ICP_all %>%
  mutate(Pat_ID = paste0(DB, Pat_ID)) %>%
  filter(Pat_ID %in% ID_Number$Pat_ID) %>%
  left_join(ID_Number, by = 'Pat_ID') %>%
  select(-Pat_ID) %>% rename(Pat_ID = ID_Nr) %>%
  select(names(ICP_all))

# hier sollte nichts auftauchen
ICP_all %>%
  filter(is.na(Pat_ID))

# Verkleinern der Zeitabstände (Ursprünglich jetzt auf Minuten)


Faktor <- 5


ICP_all_woMED <- 
  ICP_all %>%
  filter(ID != 'Med') %>%
  mutate(rel_time = floor(rel_time/Faktor)*Faktor) %>%
  group_by(Pat_ID, DB, ID, Maßnahme, Maßnahme_norm, rel_time) %>%
  summarise(Wert = mean(Wert, na.rm = TRUE)) %>%
  ungroup()



ICP_all_med <-   
  ICP_all %>%
  filter(ID == 'Med') %>%
  mutate(rel_time = floor(rel_time/Faktor)*Faktor) %>%
  group_by(Pat_ID, DB, ID, Maßnahme, Maßnahme_norm, rel_time) %>%
  summarise(Wert = max(Wert, na.rm = TRUE)) %>%
  ungroup()



ICP_all <- rbind(ICP_all_woMED,ICP_all_med)




rm(ICP_all_woMED, ICP_all_med) 



save(ICP_all, file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_", Faktor,".RData"))



# Outlier  -------------------------------------------------------------------------------------
# Outlier - Negativen Werte werden rausgefiltert -----------------------------------------------

Faktor <- 5

load(ICP_all, file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_", Faktor,".RData"))


ICP_all %>%
  filter(Wert < 0) %>%
  count(Maßnahme_norm)


ICP_all <-
  ICP_all %>%
  mutate(Wert = ifelse(!str_detect(Maßnahme,"ICP|CPP|SBE") & Wert < 0, NA, Wert)) %>%
  filter(!is.na(Wert)) 


ICP_all %>%
  filter(DB  == 'eICU') %>%
  filter(Maßnahme == 'ICP')


# SpO2, sO2 müssen zwischen 0 und 100 liegen
# und Werte 

ICP_all <-
  ICP_all %>%
  mutate(Wert = ifelse(str_detect(Maßnahme,"SpO2|sO2") & Wert > 100, 100, Wert)) %>%
  filter(!is.na(Wert)) 



# Outlier - rel_time / negative rel_time wird gefiltert ---------------------------------------
ICP_all  <-
  ICP_all %>%
  filter(rel_time > 0)


# 0,90 quantile kann herausgefiltert
ICP_all %>%
  group_by(Pat_ID) %>%
  summarise(max_time = max(rel_time)) %>%
  summarise(quantile(max_time,probs = 0.90))


# Alles über die 90% Quantile wird gefiltert
ICP_all  <-
  ICP_all %>%
  filter(rel_time < 48153)



ICP_all %>%
  filter(ID == 'BGA') %>%
  filter(Maßnahme_norm == 'Glu') %>%
  group_by(Pat_ID, Maßnahme_norm) %>%
  mutate(Wert_lag = lag(Wert)) %>%
  mutate(Wert_lag = Wert_lag-Wert) %>%
  mutate(Outlier = quantile(Wert_lag, probs = 1, na.rm = TRUE)) %>%
  filter(Wert_lag < max(Outlier))



#  Outlier - insgesamt      ---------------------------------------
#  pro Datenbank
#  pro Feature/Column


ICP_all <-
  ICP_all %>%
  # filter(ID == 'Vital') %>%
  # filter(Maßnahme_norm == 'ICP') %>%
  group_by(DB,Maßnahme_norm) %>%
  # mutate(Outlier = quantile(Wert, probs = 0.999, na.rm = TRUE)) %>%
  # filter(Wert < max(Outlier)) %>% select(names(ICP_all))
  mutate(OUTLIER = outliers::scores(Wert, type = "chisq", prob = 1)) %>%
  ungroup() %>%
  # filter(OUTLIER < 1) %>% select(names(ICP_all)) %>%
  # Clipping
  group_by(DB,Maßnahme_norm,Pat_ID) %>%
  mutate(Wert = ifelse(OUTLIER >= 1,max(Wert, na.rm = T), Wert)) %>% select(names(ICP_all))




#   Outlier - über die Zeit      ---------------------------------------
#   Der Einfluss auf die Modelle sollte gecheckt werden

ICP_all %>%
  # filter(DB == 'UKE') %>%
  # filter(ID == 'Med') %>%
  # filter(Maßnahme == 'Katecholamin') %>%
  # filter(str_detect(Maßnahme_norm, 'Ketamin')) %>%
  # filter(Wert > 0) %>%
  group_by(Pat_ID, Maßnahme_norm) %>%
  mutate(Wert_lead = lead(Wert)) %>%
  mutate(Wert_lag = lag(Wert)) %>%
  mutate(Wert_lag = abs(Wert_lag-Wert)) %>%
  mutate(Wert_lead = abs(Wert_lead-Wert)) %>%
  mutate(Wert_lag = ifelse(is.na(Wert_lag),0, Wert_lag)) %>% 
  mutate(Wert_lead = ifelse(is.na(Wert_lead),0, Wert_lead)) %>% 
  mutate(Wert_lag = Wert_lag + Wert_lead) %>%
  group_by(DB, Maßnahme_norm) %>%
  mutate(Outlier = quantile(Wert_lag, probs = 0.999, na.rm = TRUE)) %>%
  mutate(Outlier = ifelse(Wert_lag <= max(Outlier),'Normal','Outlier')) %>%
  group_by(DB) %>%
  count(Outlier)


#   CAVE CAVE CAVE CAVE

ICP_all <-
  ICP_all %>%
  group_by(Pat_ID, Maßnahme_norm) %>%
  mutate(Wert_lead = lead(Wert)) %>%
  mutate(Wert_lag = lag(Wert)) %>%
  mutate(Wert_lag = abs(Wert_lag-Wert)) %>%
  mutate(Wert_lead = abs(Wert_lead-Wert)) %>%
  mutate(Wert_lag = ifelse(is.na(Wert_lag),0, Wert_lag)) %>% 
  mutate(Wert_lead = ifelse(is.na(Wert_lead),0, Wert_lead)) %>% 
  mutate(Wert_lag = Wert_lag + Wert_lead) %>%
  group_by(DB,Maßnahme_norm) %>%
  mutate(Outlier = quantile(Wert_lag, probs = 0.999, na.rm = TRUE)) %>%
  filter(Wert_lag <= max(Outlier)) %>% select(names(ICP_all))


# COMMENT: ggf. noch einbauen
# Multivariate Outlier filtern? 


# Outlier - Patienten mit wenig Datenpunkten filtern ###########

ICP_all %>%
  group_by(Pat_ID) %>% count(ID) %>% ungroup() %>% filter(ID== 'Vital') %>%
  arrange(n) %>% filter(n<=100) %>%
  ggplot(.,aes(n))+geom_density()

# Patienten mit weniger als 50 Vital Parametern werden herausgefiltert

Pat_ID_too_less <-
  ICP_all %>%
  group_by(Pat_ID) %>% count(ID) %>% ungroup() %>% filter(ID== 'Vital') %>%
  arrange(n) %>% filter(n<= 50) %>% pull(Pat_ID)


ICP_all <- 
  ICP_all %>%
  filter(!Pat_ID %in% Pat_ID_too_less)


rm(Pat_ID_too_less)


# COMMENT


# Dinge die hier noch ergänzt werden können:
# Death and Release Time
# Stirbt in 6 Stunden


Faktor <- 5 # bleibt hier starr



save(ICP_all, file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_", Faktor,"outlier.RData"))



load(file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_", Faktor,"outlier.RData"))


ICP_all %>%
  filter(DB == 'eICU') %>%
  filter(Maßnahme == 'ICP')


# I -----
# Kritische Phasen    -------------------------------------------

# Wide Format erstellen um alle missing values für ICP zu bekommen 
# und auch das richtige Phasen Ende zu berechnen

ICP_all_wide <-
  ICP_all %>%
  ungroup() %>%
  select(-Maßnahme_norm, -DB) %>%
  group_by(Pat_ID, rel_time, ID, Maßnahme) %>%
  summarise(Wert = mean(Wert, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(Maßnahme = gsub(' ','_',Maßnahme)) %>%
  mutate(Maßnahme = gsub('-','_',Maßnahme)) %>%
  pivot_wider(names_from = c('ID','Maßnahme'), values_from = 'Wert') 



# Aufpusten
ICP_all_wide <-
  ICP_all_wide %>%
  group_by(Pat_ID) %>%
  complete(rel_time = seq(0 , max(rel_time, na.rm = T), by = Faktor))


# save wide format (5 MINUTEN!!)

save(ICP_all_wide, file = "/home/nils/ICP_data/ICP_all_processed/ICP_all_wide_5min.RData")




# Falls die anderen Sachen schon berechnet wurden

library(tidyverse)

load(file = "/home/nils/ICP_data/ICP_all_processed/ICP_all_wide_5min.RData")  

load(file = "/home/nils/ICP_data/ICP_all_processed/ICP_data_all_target.RData")



# Nur den ICP_values herausfiltern
ICP_value <-
  ICP_all_wide %>%
  select(Pat_ID,rel_time,Vital_ICP)



# Ansschauen welches die Werte sind die überhalb der 95ten Percentile liegen
ICP_value %>%
  ungroup() %>%
  filter(!is.na(Vital_ICP)) %>%
  left_join(ICP_data_all_target) %>%
  group_by(DB) %>%
  filter(Outcome == 'survived') %>%
  summarise(quantile(Vital_ICP,probs = 0.95, na.rm = T))



ICP_value %>%
  ungroup() %>%
  filter(!is.na(Vital_ICP)) %>%
  left_join(ICP_data_all_target, by = 'Pat_ID') %>%
  group_by(DB) %>%
  ggplot(.,aes(Vital_ICP,color = DB)) + 
  geom_density()



ICP_value %>% 
  left_join(ICP_data_all_target) %>%
  filter(DB == 'MIMIC') %>%
  filter(!is.na(Vital_ICP))


# Kritische Phasen - berechnen    -------------------------------------------
# Kritische Phasen pro Stunde berechnen
# und schauen wie lang sie sind


ICP_crit <-
  ICP_value %>%
  filter(!is.na(Vital_ICP)) %>%
  mutate(rel_time_hour = floor(rel_time/60)) %>%
  group_by(Pat_ID,rel_time_hour) %>%
  mutate(max_ICP_hour = max(Vital_ICP, na.rm = T)) %>%
  group_by(Pat_ID) %>%
  mutate(ICP_critical = ifelse(max_ICP_hour >= 22, 1 , 0)) %>% # WICHTIG!!!! Wert!!!
  mutate(test = rel_time_hour - min(rel_time_hour)) %>%
  distinct(Pat_ID, rel_time_hour, ICP_critical) %>%
  group_by(Pat_ID, ICP_critical) %>%
  mutate(n = 1 : n()) %>%
  mutate(test = rel_time_hour - n) %>%
  select(-n)


# Label lang und Label Kurz werden hinzugefügt

ICP_crit <-
ICP_crit %>%
  # filter(Pat_ID == 2229) %>%
  group_by(Pat_ID, test, ICP_critical) %>%
  add_count() %>%
  mutate(ICP_critical_short = ifelse(max(n,na.rm = T) <= 2 & ICP_critical == 1 , 1, 0)) %>%
  mutate(ICP_critical_long = ifelse(max(n,na.rm = T) > 2 & ICP_critical == 1, 1, 0)) %>%
  ungroup() %>%
  select(-ICP_critical,-n,-test)

# Wie viel Phasen Shifts sollen angesehen werden

x <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,24)

ICP_crit_list <- list()


for (i in x) {
  ICP_crit_list[[paste0(i,'h')]] <- 
    ICP_crit %>%
    group_by(Pat_ID) %>%
    complete(rel_time_hour = seq(min(rel_time_hour,na.rm = TRUE) - i, 
                                 max(rel_time_hour,na.rm = TRUE), by = 1 )) %>%
    mutate(ICP_critical_short = lead(ICP_critical_short, n = i)) %>%
    mutate(ICP_critical_long = lead(ICP_critical_long, n = i))
}




ICP_pred <- list()

for (i in x) {
  ICP_pred[[paste0(i,'h')]] <- 
    ICP_value %>%
    mutate(rel_time_hour = floor(rel_time/60)) %>%
    left_join( ICP_crit_list[[paste0(i,'h')]] ) %>%
    # Vital_ICP muss als Target umbenannt werden
    rename(ICP_pred = Vital_ICP)
}



# Kritische Phasen - Verteilungen der Gruppen ansehen    ------------------------------------


ICP_pred[['0h']] %>%
  left_join(ICP_data_all_target) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome,DB) %>%
  summarise(across(starts_with("ICP"), ~sum(.x, na.rm = TRUE)),
            n = n()) %>%
  mutate(ICP_critical_short_pro = (ICP_critical_short/n)*100,
         ICP_critical_long_pro = (ICP_critical_long/n)*100)



# Einzelner Patient nach dem Verlauf plotten
#test_join <- 
  ICP_pred[['0h']] %>%
  filter(Pat_ID == '12') %>%
  mutate(rel_time = floor(rel_time/5)*5) %>%
  group_by(Pat_ID,rel_time) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup()


#test_join <- 
  ICP_pred %>%
  ungroup() %>%
  filter(Pat_ID == '15')


summary(ICP_pred)



# Speichern!

save(ICP_pred, file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_list.RData")

save(ICP_crit_list, file = "/home/nils/ICP_data/ICP_all_processed/ICP_crit_list.RData")


rm(#ICP_pred,
   ICP_crit_list,
   ICP_crit,ICP_value,i,x)



# I --------
# ICP_all - gwünschte Abstände mit  --------------------------

library(tidyverse)

# es werden die 5 minuten und outlier berechneten Datensätze geladen

Faktor <- 5 # bleibt hier starr


load(file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_", Faktor,"outlier.RData"))


# Verkleinern der Zeitabstände (Ursprünglich jetzt auf Minuten)

# ICP_all - Faktor aktuell --------------------------
# CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE

Faktor <- 60

# CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE CAVE


ICP_all_woMED <- 
  ICP_all %>%
  filter(ID != 'Med') %>%
  mutate(rel_time = floor(rel_time/Faktor)*Faktor) %>%
  group_by(Pat_ID, ID, DB, Maßnahme, Maßnahme_norm, rel_time) %>%
  summarise(Wert = mean(Wert, na.rm = TRUE)) %>%
  ungroup()


ICP_all_med <-   
  ICP_all %>%
  filter(ID == 'Med') %>%
  mutate(rel_time = floor(rel_time/Faktor)*Faktor) %>%
  group_by(Pat_ID, DB, ID, Maßnahme, Maßnahme_norm, rel_time) %>%
  summarise(Wert = max(Wert, na.rm = TRUE)) %>%
  ungroup() %>% rbind(ICP_all_woMED)




ICP_all <- rbind(ICP_all_woMED,ICP_all_med)




rm(ICP_all_woMED, ICP_all_med) 


save(ICP_all, file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_", Faktor,".RData"))


# Wenn es geladen werden soll


load(file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_", Faktor,".RData"))



# Normalisierung      ---------------------------------------

#  Maßnahme Norm benutzen und dann eine Maßnahme herausschmeißen

#  Normalisierung - Z Normaliseriung  -----------------------

ICP_all_znorm <- 
  ICP_all %>%
  filter(ID != 'Med') %>%
  group_by(DB, Maßnahme_norm) %>%
  # Z Normalisierung
  mutate(Wert = (Wert - mean(Wert, na.rm = TRUE))/ sd(Wert, na.rm = TRUE)) %>%
  ungroup() %>% select(-Maßnahme_norm) %>%
  group_by(Pat_ID, rel_time, DB, ID, Maßnahme) %>% # Duplikate werden entfernt
  summarise(Wert = mean(Wert, na.rm = TRUE)) %>% ungroup()


# Medikamente werden min max normalisiert
ICP_all_znorm <-   
  ICP_all %>%
  filter(ID == 'Med') %>%
  filter(!str_detect(Maßnahme_norm, 'Manni')) %>%
  group_by(DB, Maßnahme_norm) %>%
  # Min Max Normalisierung
  replace(is.na(.), 0) %>%
  mutate(Wert = (Wert - min(Wert, na.rm = TRUE)) / 
           (max(Wert, na.rm = TRUE) - min(Wert, na.rm = TRUE)) ) %>%
  ungroup() %>% select(-Maßnahme_norm) %>%
  group_by(DB,Pat_ID,ID,Maßnahme, rel_time) %>% # Duplikate werden entfernt
  summarise(Wert = max(Wert, na.rm = TRUE)) %>% 
  ungroup() %>% rbind(ICP_all_znorm)



save(ICP_all_znorm, file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_znorm_",Faktor,"min.RData"))


# Normalisierung - Min Max  -----------------------

ICP_all_minmax <-
  ICP_all %>%
  filter(ID != 'Med') %>%
  group_by(DB, Maßnahme_norm) %>%
  # Min Max Normalisierung
  mutate(Wert = (Wert - min(Wert, na.rm = TRUE))/ (max(Wert, na.rm = TRUE)- min(Wert, na.rm = TRUE))) %>%
  ungroup() %>% select(-Maßnahme_norm) %>%
  group_by(DB,Pat_ID,ID,Maßnahme, rel_time) %>% # Duplikate werden entfernt
  summarise(Wert = mean(Wert, na.rm = TRUE)) %>% ungroup()

# Medikamente werden min max normalisiert
ICP_all_minmax <-   
  ICP_all %>%
  filter(ID == 'Med') %>%
  filter(!str_detect(Maßnahme_norm, 'Manni')) %>%
  group_by(DB, Maßnahme_norm) %>%
  # Min Max Normalisierung
  replace(is.na(.), 0) %>%
  mutate(Wert = (Wert - min(Wert, na.rm = TRUE)) / 
           (max(Wert, na.rm = TRUE) - min(Wert, na.rm = TRUE)) ) %>%
  ungroup() %>% select(-Maßnahme_norm) %>%
  group_by(DB,Pat_ID,ID,Maßnahme, rel_time) %>% # Duplikate werden entfernt
  summarise(Wert = max(Wert, na.rm = TRUE)) %>% 
  ungroup() %>% rbind(ICP_all_minmax)


save(ICP_all_minmax, file = paste0( "/home/nils/ICP_data/ICP_all_processed/ICP_all_minmax_",Faktor,"min.RData"))


# Normalisierung - YeoJohnson  -----------------------

ICP_all_yeojo <-
  ICP_all %>%
  filter(ID != 'Med') %>%
  group_by(DB, Maßnahme_norm) %>%
  # YeoJohnson
  mutate(Wert = bestNormalize::yeojohnson(Wert)$x.t) %>%
  ungroup() %>% select(-Maßnahme_norm) %>%
  group_by(DB, Pat_ID, ID, Maßnahme, rel_time) %>% # Duplikate werden entfernt
  summarise(Wert = mean(Wert, na.rm = TRUE)) %>% ungroup()


# Medikamente werden min max normalisiert
ICP_all_yeojo <-   
  ICP_all %>%
  filter(ID == 'Med') %>%
  filter(!str_detect(Maßnahme_norm, 'Manni')) %>%
  group_by(DB, Maßnahme_norm) %>%
  # Min Max Normalisierung
  replace(is.na(.), 0) %>%
  mutate(Wert = (Wert - min(Wert, na.rm = TRUE)) / 
           (max(Wert, na.rm = TRUE) - min(Wert, na.rm = TRUE)) ) %>%
  ungroup() %>% select(-Maßnahme_norm) %>%
  group_by(DB,Pat_ID,ID,Maßnahme, rel_time) %>% # Duplikate werden entfernt
  summarise(Wert = max(Wert, na.rm = TRUE)) %>% 
  ungroup() %>% rbind(ICP_all_yeojo)


save(ICP_all_yeojo, file = paste0( "/home/nils/ICP_data/ICP_all_processed/ICP_all_yeojo_",Faktor,"min.RData"))


# loading

load(file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_targets.RData"))




# ZNORM - für den Export vorbereiten ----------------

Faktor <- 5

load(file = paste0( "/home/nils/ICP_data/ICP_all_processed/ICP_all_znorm_",Faktor,"min.RData"))


ICP_znorm_wide <-
  ICP_all_znorm %>%
  ungroup() %>%
  # filter(DB == 'UKE') %>%
  select( -DB) %>%
  ungroup() %>%
  mutate(Maßnahme = gsub(' ','_',Maßnahme)) %>%
  mutate(Maßnahme = gsub('-','_',Maßnahme)) %>%
  pivot_wider(names_from = c('ID','Maßnahme'), values_from = 'Wert') 


ICP_znorm_wide <-
  ICP_znorm_wide %>%
  group_by(Pat_ID) %>%
  complete(rel_time = seq(0 , max(rel_time, na.rm = T), by = Faktor))


ICP_znorm_wide <-
  ICP_znorm_wide %>%
  mutate_at(vars(contains('Med')), ~replace_na(., 0))


# Thrombinzeit muss raus
ICP_znorm_wide <-
ICP_znorm_wide %>%
  select(-Labor_TZ)

# Predictions hinzufügen

load( file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_6h.RData")
load( file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_3h.RData")
load( file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_1h.RData")


ICP_znorm_wide_6h <-
  ICP_znorm_wide %>%
  left_join(ICP_pred_6h) %>%
  select(-rel_time_hour) %>%
  ungroup() %>%
  # Zeit wird znorm normalisiert
  # Z Normalisierung
  # mutate(rel_time = (rel_time - mean(rel_time, na.rm = TRUE))/ sd(rel_time, na.rm = TRUE)) %>%
  # Min Max
  mutate(rel_time = (rel_time - min(rel_time, na.rm = TRUE)) / 
           (max(rel_time, na.rm = TRUE) - min(rel_time, na.rm = TRUE)))%>%
  # Targets hinzufügen
  left_join(ICP_targets)


ICP_znorm_wide_3h <-
  ICP_znorm_wide %>%
  left_join(ICP_pred_3h) %>%
  select(-rel_time_hour) %>%
  ungroup() %>%
  # Zeit wird znorm normalisiert
  # Z Normalisierung
  # mutate(rel_time = (rel_time - mean(rel_time, na.rm = TRUE))/ sd(rel_time, na.rm = TRUE)) %>%
  # Min Max
  mutate(rel_time = (rel_time - min(rel_time, na.rm = TRUE)) / 
           (max(rel_time, na.rm = TRUE) - min(rel_time, na.rm = TRUE)))%>%
  # Targets hinzufügen
  left_join(ICP_targets)


ICP_znorm_wide_1h <-
  ICP_znorm_wide %>%
  left_join(ICP_pred_1h) %>%
  select(-rel_time_hour) %>%
  ungroup() %>%
  # Zeit wird znorm normalisiert
  # Z Normalisierung
  # mutate(rel_time = (rel_time - mean(rel_time, na.rm = TRUE))/ sd(rel_time, na.rm = TRUE)) %>%
  # Min Max
  mutate(rel_time = (rel_time - min(rel_time, na.rm = TRUE)) / 
           (max(rel_time, na.rm = TRUE) - min(rel_time, na.rm = TRUE)))%>%
  # Targets hinzufügen
  left_join(ICP_targets)


rm(ICP_pred_1h,ICP_pred_3h,ICP_pred_6h)




# Verteiung und Prozente an der Gesamtdauer anschauen

ICP_znorm_wide_6h %>%
  left_join(ICP_data_all_target[,c('Pat_ID','DB')]) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome, DB) %>%
  count(ICP_critical_long,ICP_critical_short) %>%
  mutate(Prozent = (n/sum(n))*100) %>%
  filter(ICP_critical_long == 1 | ICP_critical_short == 1 )


ICP_znorm_wide_3h %>%
  left_join(ICP_data_all_target[,c('Pat_ID','DB')]) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome, DB) %>%
  count(ICP_critical_long,ICP_critical_short) %>%
  mutate(Prozent = (n/sum(n))*100) %>%
  filter(ICP_critical_long == 1 | ICP_critical_short == 1 )


ICP_znorm_wide_1h %>%
  left_join(ICP_data_all_target[,c('Pat_ID','DB')]) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome, DB) %>%
  count(ICP_critical_long,ICP_critical_short) %>%
  mutate(Prozent = (n/sum(n))*100) %>%
  filter(ICP_critical_long == 1 | ICP_critical_short == 1 )



# Schauen ob die max Pat_ID auch die richtige ist

ICP_znorm_wide_3h %>%
  filter(DB_UKE == 1) %>%
  distinct(Pat_ID) %>%
  ungroup() %>%
  summarise(max(Pat_ID))


ICP_znorm_wide_3h %>%
  filter(DB_UKE == 1) %>%
  ggplot(.,aes(x = ICP_pred)) +
  geom_density()




# mit dem richtigen Faktor abspeichern

save(ICP_znorm_wide_6h, 
     file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_znorm_wide_",Faktor,"min_6h.RData"))
save(ICP_znorm_wide_3h, 
     file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_znorm_wide_",Faktor,"min_3h.RData"))
save(ICP_znorm_wide_1h, 
     file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_znorm_wide_",Faktor,"min_1h.RData"))


write.csv(ICP_znorm_wide_6h, 
          file = paste0("/home/nils/ICP_data/ICP_all_CSV/Datenbank_znorm_",Faktor,"min_6h.csv"), 
          row.names=FALSE, na="")
write.csv(ICP_znorm_wide_3h, 
          file = paste0("/home/nils/ICP_data/ICP_all_CSV/Datenbank_znorm_",Faktor,"min_3h.csv"), 
          row.names=FALSE, na="")
write.csv(ICP_znorm_wide_1h, 
          file = paste0("/home/nils/ICP_data/ICP_all_CSV/Datenbank_znorm_",Faktor,"min_1h.csv"), 
          row.names=FALSE, na="")


rm(ICP_znorm_wide,ICP_znorm_wide_1h,ICP_znorm_wide_3h,ICP_znorm_wide_6h)




# YeoJo - für den Export vorbereiten ------------

Faktor <- 5

load(file = paste0( "/home/nils/ICP_data/ICP_all_processed/ICP_all_yeojo_",Faktor,"min.RData"))

load(file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_targets.RData"))


ICP_yeojo_wide <-
  ICP_all_yeojo %>%
  ungroup() %>%
  # filter(DB == 'UKE') %>%
  select( -DB) %>%
  ungroup() %>%
  mutate(Maßnahme = gsub(' ','_',Maßnahme)) %>%
  mutate(Maßnahme = gsub('-','_',Maßnahme)) %>%
  pivot_wider(names_from = c('ID','Maßnahme'), values_from = 'Wert') 


ICP_yeojo_wide <-
  ICP_yeojo_wide %>%
  group_by(Pat_ID) %>%
  complete(rel_time = seq(0 , max(rel_time, na.rm = T), by = Faktor))


ICP_yeojo_wide <-
  ICP_yeojo_wide %>%
  mutate_at(vars(contains('Med')), ~replace_na(., 0))


# Thrombinzeit muss raus
ICP_yeojo_wide <-
  ICP_yeojo_wide %>%
  select(-Labor_TZ)


ICP_yeojo_wide %>%
  select(Labor_TZ)


# Predictions hinzufügen

load( file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_list.RData")

ICP_yeojo_wide_export <- list()

x <- c(1)



for (i in x) {
  ICP_yeojo_wide_export[[paste0(i,'h')]] <-
    ICP_yeojo_wide %>%
    left_join(ICP_pred[[paste0(i,'h')]]) %>%
    select(-rel_time_hour) %>%
    ungroup() %>%
    # Zeit wird minmax normalisiert
    # Z Normalisierung
    # mutate(rel_time = (rel_time - mean(rel_time, na.rm = TRUE))/ sd(rel_time, na.rm = TRUE)) %>%
    # Min Max
    mutate(rel_time = (rel_time - min(rel_time, na.rm = TRUE)) / 
             (max(rel_time, na.rm = TRUE) - min(rel_time, na.rm = TRUE)))%>%
    # Targets hinzufügen
    left_join(ICP_targets)
}


# Verteiung und Prozente an der Gesamtdauer anschauen

ICP_yeojo_wide_export[['1h']] %>%
  left_join(ICP_data_all_target[,c('Pat_ID','DB')]) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome, DB) %>%
  count(ICP_critical_long, ICP_critical_short) %>%
  mutate(Prozent = (n/sum(n))*100) %>%
  filter(ICP_critical_long == 1 | ICP_critical_short == 1 )



# Schauen ob die max Pat_ID auch die richtige ist

x <- 1

ICP_yeojo_wide_export[[paste0(x,'h')]] %>%
  filter(DB_UKE == 1) %>%
  distinct(Pat_ID) %>%
  ungroup() %>%
  summarise(max(Pat_ID))

ICP_yeojo_wide_export[[paste0(x,'h')]] %>%
  filter(DB_eICU == 1) %>%
  distinct(Pat_ID) %>%
  ungroup() %>%
  summarise(max(Pat_ID))

ICP_yeojo_wide_export[[paste0(x,'h')]] %>%
  filter(DB_MIMIC == 1) %>%
  distinct(Pat_ID) %>%
  ungroup() %>%
  summarise(max(Pat_ID))



# Welche Zeitschritte sollen exportiert werden?

x <- c(2,4,5,7,8,9,10,11,12)

x <- c(0,1)


for (i in x) {
  ICP_yeojo_wide_final <- ICP_yeojo_wide_export[[paste0(i,'h')]]
  save(ICP_yeojo_wide_final, 
       file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_yeojo_wide_",Faktor,"min_",i,"h.RData"))
}



for (i in x) {
  ICP_yeojo_wide_final <- ICP_yeojo_wide_export[[paste0(i,'h')]]
  
  write.csv(ICP_yeojo_wide_final, 
            file = paste0("/home/nils/ICP_data/ICP_all_CSV/Datenbank_yeojo_",Faktor,"min_",i,"h.csv"), 
            row.names=FALSE, na="")
}


# Schauen ob alles identisch ist

ICP_yeojo_wide_final <- ICP_yeojo_wide_export[[paste0(i,'h')]]

load(file = "/home/nils/ICP_data/ICP_all_processed/ICP_all_yeojo_wide_60min_1h.RData")

table(ICP_yeojo_wide_final == ICP_yeojo_wide_1h)


rm(ICP_yeojo_wide,ICP_yeojo_wide_1h,ICP_yeojo_wide_3h,ICP_yeojo_wide_6h)






# minmax - für den Export vorbereiten ------------


load(file = paste0( "/home/nils/ICP_data/ICP_all_processed/ICP_all_minmax_",Faktor,"min.RData"))


ICP_minmax_wide <-
  ICP_all_minmax %>%
  ungroup() %>%
  # filter(DB == 'UKE') %>%
  select( -DB) %>%
  ungroup() %>%
  mutate(Maßnahme = gsub(' ','_',Maßnahme)) %>%
  mutate(Maßnahme = gsub('-','_',Maßnahme)) %>%
  pivot_wider(names_from = c('ID','Maßnahme'), values_from = 'Wert') 


ICP_minmax_wide <-
  ICP_minmax_wide %>%
  group_by(Pat_ID) %>%
  complete(rel_time = seq(0 , max(rel_time, na.rm = T), by = Faktor))


ICP_minmax_wide <-
  ICP_minmax_wide %>%
  mutate_at(vars(contains('Med')), ~replace_na(., 0))


# Thrombinzeit muss raus
ICP_minmax_wide <-
  ICP_minmax_wide %>%
  select(-Labor_TZ)


# Predictions hinzufügen

load( file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_6h.RData")
load( file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_3h.RData")
load( file = "/home/nils/ICP_data/ICP_all_processed/ICP_pred_1h.RData")


ICP_minmax_wide_6h <-
  ICP_minmax_wide %>%
  left_join(ICP_pred_6h) %>%
  select(-rel_time_hour) %>%
  ungroup() %>%
  # Zeit wird minmax normalisiert
  # Z Normalisierung
  # mutate(rel_time = (rel_time - mean(rel_time, na.rm = TRUE))/ sd(rel_time, na.rm = TRUE)) %>%
  # Min Max
  mutate(rel_time = (rel_time - min(rel_time, na.rm = TRUE)) / 
           (max(rel_time, na.rm = TRUE) - min(rel_time, na.rm = TRUE)))%>%
  # Targets hinzufügen
  left_join(ICP_targets)


ICP_minmax_wide_3h <-
  ICP_minmax_wide %>%
  left_join(ICP_pred_3h) %>%
  select(-rel_time_hour) %>%
  ungroup() %>%
  # Zeit wird minmax normalisiert
  # Z Normalisierung
  # mutate(rel_time = (rel_time - mean(rel_time, na.rm = TRUE))/ sd(rel_time, na.rm = TRUE)) %>%
  # Min Max
  mutate(rel_time = (rel_time - min(rel_time, na.rm = TRUE)) / 
           (max(rel_time, na.rm = TRUE) - min(rel_time, na.rm = TRUE)))%>%
  # Targets hinzufügen
  left_join(ICP_targets)


ICP_minmax_wide_1h <-
  ICP_minmax_wide %>%
  left_join(ICP_pred_1h) %>%
  select(-rel_time_hour) %>%
  ungroup() %>%
  # Zeit wird minmax normalisiert
  # Z Normalisierung
  # mutate(rel_time = (rel_time - mean(rel_time, na.rm = TRUE))/ sd(rel_time, na.rm = TRUE)) %>%
  # Min Max
  mutate(rel_time = (rel_time - min(rel_time, na.rm = TRUE)) / 
           (max(rel_time, na.rm = TRUE) - min(rel_time, na.rm = TRUE)))%>%
  # Targets hinzufügen
  left_join(ICP_targets)


rm(ICP_pred_1h,ICP_pred_3h,ICP_pred_6h)



# Verteiung und Prozente an der Gesamtdauer anschauen


ICP_minmax_wide_6h %>%
  left_join(ICP_data_all_target[,c('Pat_ID','DB')]) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome, DB) %>%
  count(ICP_critical_long,ICP_critical_short) %>%
  mutate(Prozent = (n/sum(n))*100) %>%
  filter(ICP_critical_long == 1 | ICP_critical_short == 1 )


ICP_minmax_wide_3h %>%
  left_join(ICP_data_all_target[,c('Pat_ID','DB')]) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome, DB) %>%
  count(ICP_critical_long,ICP_critical_short) %>%
  mutate(Prozent = (n/sum(n))*100) %>%
  filter(ICP_critical_long == 1 | ICP_critical_short == 1 )


ICP_minmax_wide_1h %>%
  left_join(ICP_data_all_target[,c('Pat_ID','DB')]) %>%
  select(Outcome, DB, ICP_critical_short, ICP_critical_long) %>%
  group_by(Outcome, DB) %>%
  count(ICP_critical_long,ICP_critical_short) %>%
  mutate(Prozent = (n/sum(n))*100) %>%
  filter(ICP_critical_long == 1 | ICP_critical_short == 1 )





# Schauen ob die max Pat_ID auch die richtige ist

ICP_minmax_wide_3h %>%
  filter(DB_UKE == 1) %>%
  distinct(Pat_ID) %>%
  ungroup() %>%
  summarise(max(Pat_ID))

ICP_minmax_wide_3h %>%
  filter(DB_UKE == 1) %>%
  ggplot(.,aes(x = ICP_pred)) +
  geom_density()




# mit dem richtigen Faktor abspeichern

save(ICP_minmax_wide_6h, 
     file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_minmax_wide_",Faktor,"min_6h.RData"))
save(ICP_minmax_wide_3h, 
     file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_minmax_wide_",Faktor,"min_3h.RData"))
save(ICP_minmax_wide_1h, 
     file = paste0("/home/nils/ICP_data/ICP_all_processed/ICP_all_minmax_wide_",Faktor,"min_1h.RData"))


write.csv(ICP_minmax_wide_6h, 
          file = paste0("/home/nils/ICP_data/ICP_all_CSV/Datenbank_minmax_",Faktor,"min_6h.csv"), 
          row.names=FALSE, na="")
write.csv(ICP_minmax_wide_3h, 
          file = paste0("/home/nils/ICP_data/ICP_all_CSV/Datenbank_minmax_",Faktor,"min_3h.csv"), 
          row.names=FALSE, na="")
write.csv(ICP_minmax_wide_1h, 
          file = paste0("/home/nils/ICP_data/ICP_all_CSV/Datenbank_minmax_",Faktor,"min_1h.csv"), 
          row.names=FALSE, na="")


rm(ICP_minmax_wide,ICP_minmax_wide_1h,ICP_minmax_wide_3h,ICP_minmax_wide_6h)
