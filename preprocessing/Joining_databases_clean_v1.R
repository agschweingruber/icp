library(tidyverse)

# Joining preprocessed databases

# eICU loading                ################################################

load(file = "eicu_processed_clean/EICU_med.RData")
load(file = "eicu_processed_clean/EICU_bga.RData")
load(file = "eicu_processed_clean/EICU_labor.RData")
load(file = "eicu_processed_clean/EICU_vital.RData")


names(ICP_BGA_outlier)
names(ICP_Labor_outlier)
names(ICP_vital_outlier)
names(ICP_Med_total)


ICP_total_eicu <- rbind(ICP_BGA_outlier, ICP_Labor_outlier, ICP_vital_outlier)

ICP_Med_total <- 
  ICP_Med_total %>% 
  select(-Wert_norm) %>% 
  rename( Maßnahme = Klasse, Maßnahme_norm = Maßnahme)%>%
  select(Pat_ID,ID,Maßnahme,Maßnahme_norm,rel_time,Wert)


ICP_total_eicu <-
  ICP_total_eicu %>% 
  mutate( Maßnahme_norm = Maßnahme)%>%
  select(names(ICP_Med_total))


ICP_total_eicu <- rbind(ICP_total_eicu, ICP_Med_total)


ICP_total_eicu <- 
  ICP_total_eicu %>%
  mutate(DB = "eICU")


rm(ICP_BGA_outlier,ICP_Labor_outlier,ICP_Med_total,ICP_vital_outlier)


# MIMIC loading                ################################################

load(file = "mimic_processed_clean/ICP_med.RData")
load(file = "mimic_processed_clean/ICP_BGA.RData")
load(file = "mimic_processed_clean/ICP_labor.RData")
load(file = "mimic_processed_clean/ICP_vital.RData")

names(ICP_BGA)
names(ICP_Labor_select)
names(ICP_vital_outlier)
names(Medikamente_total_norm)

ICP_vital_outlier <-
  ICP_vital_outlier %>%
  select(names(ICP_BGA))


ICP_total_mimic <- rbind(ICP_BGA, ICP_Labor_select, ICP_vital_outlier)


Medikamente_total_norm <- 
  Medikamente_total_norm %>% 
  select(-Wert_norm) %>% 
  rename( Maßnahme = Klasse, Maßnahme_norm = Maßnahme) %>%
  select(PAT_ID,ID,Maßnahme,Maßnahme_norm,rel_time,Wert)


ICP_total_mimic <-
  ICP_total_mimic %>% 
  mutate( Maßnahme_norm = Maßnahme)%>%
  select(names(Medikamente_total_norm))

ICP_total_mimic <- rbind(ICP_total_mimic, Medikamente_total_norm)


ICP_total_mimic <- 
  ICP_total_mimic %>%
  mutate(DB = "MIMIC")


ICP_total_mimic <- 
  ICP_total_mimic %>%
  rename(Pat_ID = PAT_ID)

rm(ICP_BGA, ICP_Labor_select, Medikamente_total_norm, ICP_vital_outlier)



# UKE loading               ################################################

load(file = "UKE_processed_clean/ICP_med.RData")
load(file = "UKE_processed_clean/ICP_Labor.RData")
load(file = "UKE_processed_clean/ICP_Vital.RData")
load(file = "UKE_processed_clean/ICP_BGA.RData")

names(ICP_BGA)
names(ICP_Labor)
names(ICP_Vital)
names(Med_total)



ICP_total_uke <- rbind(ICP_BGA, ICP_Labor, ICP_Vital)



Med_total <- 
  Med_total %>% 
  mutate(ID = 'Med') %>%
  rename( Maßnahme = Klasse, Maßnahme_norm = Maßnahme) %>%
  select(-Wert) %>% rename(Wert = Menge) %>%
  select(Fallnummer,ID,Maßnahme,Maßnahme_norm,rel_time,Wert)


ICP_total_uke <-
  ICP_total_uke %>% 
  mutate( Maßnahme_norm = Maßnahme)%>%
  select(names(Med_total))


ICP_total_uke <- rbind(ICP_total_uke,Med_total)


ICP_total_uke <- 
  ICP_total_uke %>%
  mutate(DB = "UKE")

ICP_total_uke <- 
  ICP_total_uke %>%
  rename(Pat_ID = Fallnummer)

rm(ICP_BGA,ICP_Labor, Med_total, ICP_Vital)




# Plotting Overview               ################################################
# of data distribution

ICP_all <- rbind(ICP_total_uke,ICP_total_eicu,ICP_total_mimic)


ICP_all %>%
  filter(ID=="BGA") %>%
  select(Wert,Maßnahme,DB) %>% ggplot(.,aes(Wert,color=DB))+geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free") +
  theme_bw() +
  ggsave("Figures/Übersicht_Gesamt_BGA.pdf", width=20,height = 15, device = "pdf")



ICP_all %>%
  filter(ID=="Med") %>%
  select(Wert,Maßnahme,DB) %>% ggplot(.,aes(Wert,color=DB))+geom_density()+
  facet_wrap(vars(Maßnahme), scales = "free") +
  theme_bw()+
  ggsave("Figures/Übersicht_Gesamt_Med.pdf", width=20, height = 15, device = "pdf")



ICP_all %>%
  filter(ID=="Vital") %>%
  select(Wert,Maßnahme,DB) %>% ggplot(.,aes(Wert,color=DB))+geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free") +
  theme_bw()+
  ggsave("Figures/Übersicht_Gesamt_Vital.pdf",width=20,height = 15,device = "pdf")


ICP_all %>%
  filter(ID=="Labor") %>%
  select(Wert,Maßnahme,DB) %>% ggplot(.,aes(Wert,color=DB))+geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free") +
  theme_bw()+
  ggsave("Figures/Übersicht_Gesamt_Labor.pdf",width=20,height = 15,device = "pdf")


ICP_all %>%
  filter(ID=="Med") %>%
  ggplot(.,aes(rel_time,color=DB))+geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free") +
  theme_bw()+
  ggsave("Figures/Übersicht_Med_Zeiten.pdf",width=20,height = 15,device = "pdf")


ICP_all %>%
  filter(ID=="Vital") %>%
  ggplot(.,aes(rel_time,color=DB))+geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free") +
  theme_bw()+
  ggsave("Figures/Übersicht_Vital_Zeiten.pdf",width=20,height = 15,device = "pdf")


ICP_all %>%
  filter(ID=="BGA") %>%
  ggplot(.,aes(rel_time,color=DB))+geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free") +
  theme_bw()+
  ggsave("Figures/Übersicht_BGA_Zeiten.pdf", width=20,height = 15, device = "pdf")


# einzelne Werte anschauen um die DB mit Ausreißern zu sehen
ICP_all %>%
  filter(ID == "Labor") %>%
  filter(Maßnahme == "Erythrocyten") %>%
  # filter(DB == "MIMIC") %>%
  ggplot(.,aes(Wert,color=DB))+
  geom_density()+
  theme_bw()


# Plotten von ICP Abhängig von der Diagnose und Facet mit Zentrum

ICP_all %>%
  filter(Maßnahme == "ICP") %>%
  left_join(ICP_data_all) %>%
  filter(Diagnose_txt %in% c("TBI","SAH", "ICH", "Stroke")) %>%
  filter(rel_time <= 240, rel_time >= 0) %>%
  ggplot(., aes(rel_time, Wert, color = Diagnose_txt)) +
  geom_smooth(fill = "grey90", size = 0.5) +  
  facet_wrap(vars(DB),scales = "free") +
  #guides(fill=FALSE, color=FALSE) +
  labs(x = "hours", y = "Intracranial pressure (mmHg)") +
  theme_light() +
  scale_colour_brewer(palette = "Set1")


ICP_all %>%
  filter(Maßnahme == "ICP") %>%
  left_join(ICP_data_all) %>%
  #filter(Diagnose_txt %in% c("TBI","SAH", "ICH", "Stroke")) %>%
  filter(rel_time <= 240, rel_time >= 0) %>%
  ggplot(., aes(rel_time, Wert, color = Outcome)) +
  geom_smooth(fill = "grey90", size = 0.5) +  
  facet_wrap(vars(DB),scales = "free") +
  #guides(fill=FALSE, color=FALSE)+
  labs(x = "hours", y = "Intracranial pressure (mmHg)") +
  theme_light() +
  scale_colour_brewer(palette = "Set1")


ICP_all %>%
  filter(Maßnahme == "GCS_total") %>%
  left_join(ICP_data_all) %>%
  filter(Diagnose_txt %in% c("TBI","SAH", "ICH", "Stroke")) %>%
  filter(rel_time <= 240, rel_time >= 0) %>%
  ggplot(., aes(rel_time, Wert, color = Diagnose_txt)) +
  geom_smooth(fill = "grey90", size = 0.5) +  
  facet_wrap(vars(DB),scales = "free") +
  #guides(fill=FALSE, color=FALSE)+
  labs(x = "hours", y = "GCS total") +
  theme_light()+
  scale_colour_brewer(palette = "Set1")

ICP_all %>%
  filter(Maßnahme == "Katecholamin") %>%
  left_join(ICP_data_all) %>%
  group_by(Maßnahme_norm) %>%
  mutate(Wert = bestNormalize::yeojohnson(Wert)$x.t) %>%
  ungroup() %>%
  filter(Diagnose_txt %in% c("TBI","SAH", "ICH", "Stroke")) %>%
  filter(rel_time <= 240, rel_time >= 0) %>%
  ggplot(., aes(rel_time, Wert, color = Outcome)) +
  geom_smooth(fill = "grey90", size = 0.5) +  
  facet_wrap(vars(DB),scales = "free") +
  #guides(fill=FALSE, color=FALSE)+
  labs(x = "hours", y = "GCS total") +
  theme_light()+
  scale_colour_brewer(palette = "Set1")


# ICP_all Overview of distribution of DB -----------
# This part is important


ICP_all <- rbind(ICP_total_uke,ICP_total_eicu,ICP_total_mimic)

# Which DB hhas the lowest count of features
ICP_all %>%
  group_by(DB, Maßnahme) %>%
  summarise(n = n()) %>%
  group_by(DB) %>%
  summarise(n = n()) 

Maßnahmen_eICU <-
  ICP_all %>% filter(DB == 'eICU') %>%
  select(Maßnahme) %>% distinct() %>% pull

Maßnahmen_MIMIC <-
  ICP_all %>% filter(DB == 'MIMIC') %>%
  select(Maßnahme) %>% distinct() %>% pull

Maßnahmen_UKE <-
  ICP_all %>% filter(DB == 'UKE') %>%
  select(Maßnahme) %>% distinct() %>% pull

# which feature ist different
ICP_all %>% filter(DB == 'MIMIC') %>%
  filter(!Maßnahme %in% Maßnahmen_eICU) %>%
  select(Maßnahme) %>% distinct() %>% pull


ICP_all %>% filter(DB == 'MIMIC') %>%
  filter(!Maßnahme %in% Maßnahmen_UKE) %>%
  select(Maßnahme) %>% distinct() %>% pull


ICP_all %>% filter(DB == 'eICU') %>%
  filter(!Maßnahme %in% Maßnahmen_UKE) %>%
  select(Maßnahme) %>% distinct() %>% pull



ICP_all %>%
  filter(DB == 'UKE') %>%
  filter(ID == 'Labor') %>%
  distinct(Maßnahme)


# a few labels need to be adapted

ICP_total_uke <-
  ICP_total_uke %>%
  mutate(Maßnahme = ifelse(str_detect(Maßnahme,'AST'), 'AST', Maßnahme)) %>%
  mutate(Maßnahme = ifelse(str_detect(Maßnahme,'ALT') , 'ALT', Maßnahme)) %>%
  mutate(Maßnahme = ifelse(str_detect(Maßnahme,'Alk') , 'Alk', Maßnahme)) %>%
  mutate(Maßnahme = ifelse(str_detect(Maßnahme,'Tropo'), 'Troponin', Maßnahme)) %>%
  mutate(Maßnahme = ifelse(str_detect(Maßnahme,'pankreas') , 'pankreasspez', Maßnahme)) %>%
  mutate(Maßnahme = ifelse(str_detect(Maßnahme,'Phospha') , 'Phosphat', Maßnahme)) %>%
  mutate(Maßnahme = ifelse(str_detect(Maßnahme,'FO2') , 'FiO2', Maßnahme))



ICP_all <- 
  ICP_all %>%
  mutate(Maßnahme = ifelse(Maßnahme == "FO2", 'FiO2', Maßnahme))



# All features are aligned to the db with the lowest amount of features
ICP_all <-
  ICP_all %>% 
  filter(Maßnahme %in% Maßnahmen_eICU) 


# Still features different?
last_Filter <- 
  ICP_all %>% filter(DB == 'eICU') %>%
  filter(!Maßnahme %in% Maßnahmen_UKE) %>%
  select(Maßnahme) %>% distinct() %>% pull


ICP_all <-
  ICP_all %>% 
  filter(!Maßnahme %in% last_Filter) 


# Data - Joining everything    ##################


# Data - UKE     ##################

load(file = "UKE_processed_clean/ICP_data.RData")


ICP_data_uke <- ICP_data


rm(ICP_data,ICP_ID)

# Filtern und modifizieren

ICP_data_uke <-
  ICP_data_uke %>%
  select(Fallnummer,Alter, Diagnose_txt,
         Outcome, Geschlecht, Größe, Gewicht,
         `Aufenthaltsdauer in Tagen`) %>%
  rename(Pat_ID = Fallnummer) %>%
  mutate(Outcome=as.character(Outcome),Geschlecht=as.character(Geschlecht)) %>%
  mutate(Outcome_Haus = Outcome) %>%
  mutate(`Aufenthaltsdauer in Tagen` = as.numeric(`Aufenthaltsdauer in Tagen`)) %>%
  mutate(`Aufenthaltsdauer in Tagen` = 
           ifelse(`Aufenthaltsdauer in Tagen`<= 100,`Aufenthaltsdauer in Tagen`,NA)) %>%
  mutate(DB = "UKE")

ICP_data_uke <-
  ICP_data_uke %>%
  mutate(Outcome = as.factor(Outcome),Outcome_Haus = as.factor(Outcome_Haus)) 

ICP_data_uke <-
  ICP_data_uke %>%
  filter(!is.na(Geschlecht)) 

summary(ICP_data_uke)



# Data - eICU   #############

load(file = "eicu_processed_clean/EICU_data.RData")

ICP_data_eicu <- ICP_data

rm(ICP_data)


ICP_data_eicu <-
  ICP_data_eicu %>%
  mutate(gender = as.character(gender))%>%
  mutate(gender = case_when(gender == "Male" ~ "Männlich",
                            gender == "Female" ~ "Weiblich"))%>%
  rename(Geschlecht = gender)

ICP_data_eicu <-
  ICP_data_eicu %>%
  mutate(Alter = as.character(Alter),Pat_ID = as.character(Pat_ID)) %>%
  mutate(Alter = as.numeric(Alter)) %>%
  select(Pat_ID,Alter,Diagnose_txt,Outcome,Geschlecht,
         Größe,Gewicht,'Aufenthaltsdauer in Tagen',Outcome_Haus) %>%
  mutate(DB = "eICU")

summary(ICP_data_eicu)

# Data - MIMIC  #############

load(file = "mimic_processed_clean/MIMIC_ICP_data.RData")

ICP_data_mimic <- ICP_data

rm(ICP_data)

ICP_data_mimic <-
  ICP_data_mimic %>%
  mutate(`Aufenthaltsdauer in Tagen`= as.numeric(difftime(DISCHTIME,ADMITTIME, units = "days"))) %>%
  arrange(desc(`Aufenthaltsdauer in Tagen`)) %>% 
  mutate(Geschlecht = ifelse(GENDER == "F", "Weiblich", "Männlich")) %>%
  rename(Alter = AGE, Größe = mean_height, Gewicht = mean_weight) %>%
  #mutate(Outcome_Haus = as.character(NA)) %>%
  select(PAT_ID,Alter,DIAGNOSE,Outcome,Outcome_Haus, Geschlecht,
         Größe,Gewicht,'Aufenthaltsdauer in Tagen') %>%
  rename(Diagnose_txt = DIAGNOSE, Pat_ID = PAT_ID) %>%
  mutate(Outcome = as.character(Outcome),Outcome_Haus = as.character(Outcome_Haus)) %>%
  mutate(DB = "MIMIC") 



ICP_data_mimic %>%
  mutate(Outcome = as.factor(Outcome),Outcome_Haus = as.factor(Outcome_Haus)) %>%
  summary

ICP_data_mimic <-
  ICP_data_mimic %>%
  mutate(Outcome = as.factor(Outcome),Outcome_Haus = as.factor(Outcome_Haus))


ICP_data_eicu %>% distinct(Diagnose_txt,.keep_all = TRUE) 
ICP_data_mimic %>% distinct(Diagnose_txt,.keep_all = TRUE) 
ICP_data_uke %>% distinct(Diagnose_txt,.keep_all = TRUE) 



# Data -Joining  ###############################

ICP_data_all <- full_join(ICP_data_eicu,ICP_data_uke)

ICP_data_all <- full_join(ICP_data_all,ICP_data_mimic)

# Export ###############################

dir.create("ICP_all_clean")
dir.create("ICP_all_clean/csv")


write.csv(ICP_data_all, file = "ICP_all_clean/csv/Datenbank_Pat_ID.csv", row.names=FALSE, na="")

write.csv(ICP_all, file = "ICP_all_clean/csv/Datenbank_Werte.csv", row.names=FALSE, na="")


save(ICP_data_all, file = "ICP_all_clean/ICP_data_all.RData")

save(ICP_all, file = "ICP_all_clean/ICP_all.RData")


rm(list=ls())


ID_wo_ICP <-
ICP_all %>%
  filter(Maßnahme == 'ICP') %>%
  distinct(Pat_ID) %>%
  pull()

ID_wo_ICP <-
ICP_all %>%
  filter(!Pat_ID %in% ID_wo_ICP) %>%
  distinct(Pat_ID) %>%
  pull()

ICP_data_all %>%
  filter(Pat_ID %in% ID_wo_ICP) %>%
  count(DB)



