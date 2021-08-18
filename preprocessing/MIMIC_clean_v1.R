
# MIMIC Import clean ------

# required packages:
library(tidyverse)
library(readxl)
library(lubridate)

dir.create('mimic_processed_clean')
dir.create('mimic_processed_clean/csv')


#  ICP ID erstellen    #####################################################

load(file = "mimic_processed/MIMIC_ICP_chartevent.RData")

MIMIC_items <- as_tibble(read.csv(gzfile("mimic-iii/D_ITEMS.csv.gz"), stringsAsFactors=FALSE))

MIMIC_items <- MIMIC_items %>% select(ITEMID,LABEL)

ICP_chartevent <- ICP_chartevent %>% left_join(MIMIC_items)


ICP_ID <- 
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Intra Cranial Pressure|ICP")) %>%
  distinct(SUBJECT_ID,HADM_ID) %>%
  mutate(PAT_ID = paste(SUBJECT_ID,HADM_ID, sep = "_"))

Pat_ID <- ICP_ID %>% distinct(PAT_ID) %>% pull

ICP_ID <- ICP_ID %>% distinct(SUBJECT_ID) %>% pull


save(ICP_ID , file = "mimic_processed_clean/MIMIC_ICP_ID.RData")
save(Pat_ID , file = "mimic_processed_clean/MIMIC_Pat_ID.RData")



#----------------------------- Datenbank ------------------------------------
# PatID = Subject ID und Hospital Admitance ID
# Diagnosen
# Die Primary Diagnose ist wahrscheinlich die wichtigste

MIMIC_diagnosis <- 
  as_tibble(read.csv(gzfile("mimic-iii/DIAGNOSES_ICD.csv.gz"), stringsAsFactors=FALSE))

load(file = "mimic_processed_clean/MIMIC_Pat_ID.RData")

MIMIC_diagnosis <-
  MIMIC_diagnosis %>% mutate(PAT_ID = paste(SUBJECT_ID,HADM_ID, sep = "_"))

ICP_diagnosis <- MIMIC_diagnosis %>% filter(PAT_ID %in% Pat_ID)

rm(MIMIC_diagnosis)

# DIE ICD LISTE VON MIMIC
diagnosis_ICD <- 
  as_tibble(read.csv(gzfile("mimic-iii/D_ICD_DIAGNOSES.csv.gz"), stringsAsFactors=FALSE))

ICP_diagnosis <- 
  ICP_diagnosis %>% 
  left_join(diagnosis_ICD %>% select(ICD9_CODE,SHORT_TITLE)) %>%
  mutate(DIAGNOSE = case_when(str_detect(ICD9_CODE,"^433|^434") ~ "Stroke",
                              str_detect(SHORT_TITLE,"CV|Crbl") ~ "Stroke",
                              str_detect(ICD9_CODE,"^80") ~ "TBI",
                              str_detect(SHORT_TITLE,"Traum|scalp|contus") ~ "TBI",
                              str_detect(ICD9_CODE,"^430") ~ "SAH",
                              str_detect(SHORT_TITLE,"Subarach") ~ "SAH",
                              str_detect(ICD9_CODE,"^431|^432") ~ "ICH",
                              str_detect(SHORT_TITLE,"Subdural|Brain hem") ~ "ICH",
                              str_detect(ICD9_CODE,"^19") ~ "Tumor",
                              str_detect(SHORT_TITLE,"Tumor|neoplasm|Ben neo|neo brain|Benign neo") ~ "Tumor",
                              str_detect(ICD9_CODE,"^421|^322") ~ "MISC",
                              TRUE ~ "MISC")) %>%
  mutate(STROKE = case_when(str_detect(DIAGNOSE,"SAH") ~ "2",
                            str_detect(DIAGNOSE,"ICH") ~ "4",
                            str_detect(DIAGNOSE,"Stroke") ~ "3",
                            str_detect(DIAGNOSE,"TBI") ~ "1",
                            str_detect(DIAGNOSE,"MISC") ~ "5",
                            TRUE ~ "1")) %>%
  group_by(PAT_ID) %>%
  arrange(STROKE) %>%
  slice(1) %>% select(-STROKE)


# Zur Zusammenfassung kann das folgende genutzt werden
ICP_diagnosis  %>% #filter(DIAGNOSE == "MISC") %>% select(SUBJECT_ID) %>% pull
  group_by(SUBJECT_ID, DIAGNOSE) %>%
  summarise(n=n()) %>%
  group_by(DIAGNOSE) %>%
  summarise(n=n()) %>% #filter(n >3) %>% 
  arrange(desc(n))


# Datenbank - Basisinformationen #######################

# ICP Patients zusammenstellen... hier werden die Basisinformationen herausgefiltert. Alter fehlt noch
# ICP_data (Zentrale Übersichtsdatei erstellen mit Gewicht, Alter, Outcome und Diagnosen)


MIMIC_patient <-as_tibble(read.csv(gzfile("mimic-iii/PATIENTS.csv.gz"), stringsAsFactors=FALSE))
MIMIC_items <-as_tibble(read.csv(gzfile("mimic-iii/D_ITEMS.csv.gz"), stringsAsFactors=FALSE))


ICP_weight <-
  ICP_chartevent %>%
  filter(str_detect(VALUEUOM,"kg")) %>%
  filter(!str_detect(VALUEUOM,"m|/")) %>%
  filter(VALUENUM >= 25) %>%
  filter(VALUENUM < 400) %>%
  filter(!str_detect(LABEL,"Feed|Ideal|Change")) %>%
  group_by(SUBJECT_ID) %>%
  summarise(mean_weight = mean(VALUENUM, na.rm = TRUE))

ICP_height <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Heigh")) %>%
  filter(str_detect(VALUEUOM,"cm")) %>%
  filter(VALUENUM < 250) %>%
  group_by(SUBJECT_ID) %>%
  summarise(mean_height = mean(VALUENUM, na.rm = TRUE))


ICP_weight %>% summary()
ICP_height %>% summary()

# Aufnahmezeiten werden hinzugefüg auch um das Alter zu brechnen
# Wichtig ist hier das Erstellen einer einzigartigen PAT_ID für später
#

MIMIC_admissiondx <- as_tibble(read.csv(gzfile("mimic-iii/ADMISSIONS.csv.gz"), stringsAsFactors=FALSE))

ICP_admittime <-
  MIMIC_admissiondx %>%
  mutate(PAT_ID = paste(SUBJECT_ID,HADM_ID, sep = "_")) %>%
  filter(PAT_ID %in% Pat_ID) %>%
  mutate(ADMITTIME = ymd_hms(ADMITTIME)) %>%
  #mutate(DEATHTIME = ymd_hms(DEATHTIME)) %>%
  mutate(DISCHTIME = ymd_hms(DISCHTIME)) %>%
  select(PAT_ID, SUBJECT_ID, HADM_ID, ADMITTIME, DISCHTIME)

rm(MIMIC_admissiondx)

# Diagnosen werden auf das Relevante reduziert
ICP_diagnosis <-
  ICP_diagnosis %>%
  select(SUBJECT_ID,SHORT_TITLE,DIAGNOSE)

ICP_data <-
  MIMIC_patient %>%
  filter(SUBJECT_ID %in% ICP_ID) %>%
  select(SUBJECT_ID, GENDER, DOB, DOD_HOSP, DOD, EXPIRE_FLAG) %>%
  left_join(ICP_weight) %>%
  left_join(ICP_height) %>%
  left_join(ICP_diagnosis) %>%
  mutate(DOB = ymd_hms(DOB), 
         DOD_HOSP = ymd_hms(DOD_HOSP),
         DOD = ymd_hms(DOD))

summary(ICP_data)

ICP_data <-
  ICP_data %>%
  right_join(ICP_admittime) %>%
  mutate(AGE = as.numeric(ADMITTIME - DOB)/365) %>%
  mutate(AGE = ifelse(AGE > 200 , 89,AGE)) %>%
  mutate(Outcome_Haus = ifelse(is.na(DOD_HOSP),"survived","exitus")) %>%
  mutate(Outcome_Haus = as.factor(Outcome_Haus)) # %>% summary()

ICP_data <-
  ICP_data %>%
  mutate(Expire_ICU = DOD_HOSP <= DISCHTIME) %>%
  mutate(Outcome = ifelse(Expire_ICU == TRUE ,"exitus","survived")) %>%
  mutate(Outcome = ifelse(is.na(Expire_ICU) ,"survived", Outcome)) %>%
  select( -Expire_ICU ) %>%
  mutate(Outcome = as.factor(Outcome)) # %>% summary()


# Datenbank - Exportieren #####################

# Übersicht
ICP_data %>% summary()

save(ICP_data,file = "mimic_processed_clean/MIMIC_ICP_data.RData")

write.csv(ICP_data, file = "mimic_processed_clean/csv/MIMIC_data.csv", row.names = FALSE, na = "")


# Für das Weiterarbeiten wird die Subject ID nicht mehr benötigt
# Dies ist wichtig für die relativen Zeiten

ICP_ID <- ICP_data %>% select(PAT_ID) %>% pull

# IDs speichern

save(ICP_ID, file = "mimic_processed_clean/MIMIC_ICP_ID.RData")

# Alles unwichtige löschen
rm(ICP_weight,ICP_diagnosis,ICP_admittime,MIMIC_patient,diagnosis_ICD)

# Workspace aufräumen:
rm(list=ls())


# Vital   ###################################################################################

load(file = "mimic_processed/MIMIC_ICP_chartevent.RData")

MIMIC_items <- as_tibble(read.csv(gzfile("mimic-iii/D_ITEMS.csv.gz"), stringsAsFactors=FALSE))

MIMIC_items <- MIMIC_items %>% select(ITEMID,LABEL)

ICP_chartevent <- ICP_chartevent %>% left_join(MIMIC_items)



# Pat_ID erstellen (Subject ID und Hospital ID)
ICP_chartevent <- ICP_chartevent %>% mutate(PAT_ID = paste(SUBJECT_ID,HADM_ID, sep = "_")) 

load(file = "mimic_processed_clean/MIMIC_Pat_ID.RData")

# Nur die relevanten Aufenthalte herausfiltern
ICP_chartevent <- 
  ICP_chartevent %>% filter(PAT_ID %in% PAT_ID)

rm(MIMIC_items)


#Um sich eine Übersicht zu schaffen ist folgendes ganz sinnvoll
ALL_CHARTEVENTS_sorted <-
  ICP_chartevent %>% 
  filter(!str_detect(LABEL,'Alarm')) %>%
  filter(!is.na(VALUENUM)) %>%
  group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))


ALL_CHARTEVENTS_sorted %>%
  filter(str_detect(LABEL,"Man"))



# Vital - Relevante Feature herausfiltern ###########################################


ICP_vital_1 <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Heart|Respi|SpO2|O2 saturation|Resp Rate")) %>%
  filter(!str_detect(LABEL,'Alarm|Rhythm|Sound|Pattern|Set|Effort|PAR-|Desat')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Heart'), 'HF', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Resp') & str_detect(LABEL,'Total') , 'Freq gesamt', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Resp') & str_detect(LABEL,'spontan') , 'Freq spontan', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Resp') & str_detect(LABEL,'Rate') , 'AF', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'O2 saturation') , 'SpO2', LABEL)) 


ICP_vital_2 <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Arterial") & str_detect(LABEL,"BP|Pressure")) %>%
  filter(!str_detect(LABEL,'Alarm|CO2|IABP')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Arterial') & str_detect(LABEL,'Systolic|syst') , 'syst', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Arterial') & str_detect(LABEL,'Diast|diast') , 'diast', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Arterial') & str_detect(LABEL,'Mean|mean') , 'mittl', LABEL)) 


ICP_vital_3 <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Intra Cra|ICP|Cereb|CPP")) %>%
  filter(!str_detect(LABEL,'Line|Alarm|-ventr')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Intra') & str_detect(LABEL,'Press') , 'ICP', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Cereb') & str_detect(LABEL,'Press') , 'CPP', LABEL)) %>%
  filter(LABEL %in% c('ICP','CPP')) 


ICP_vital_4 <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Airway|Peak|Inspired|PEEP") & str_detect(LABEL,"Pressure|Fract|PEEP")) %>%
  filter(!str_detect(LABEL,'Cuff|Auto|Total')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'PEEP'), 'PEEP', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Mean') & str_detect(LABEL,'Press') , 'Pmean', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Peak') & str_detect(LABEL,'Press') , 'Ppeak', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Insp') & str_detect(LABEL,'O2') , 'FiO2', LABEL))


ICP_vital_GCS <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"GCS|Eye|Verba|Motor Res|Total")) %>%
  filter(!str_detect(LABEL,'Level|Care|Rate')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Eye'), 'GCS_auge', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Verb') , 'GCS_verbal', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Motor'), 'GCS_motor', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'GCS') & str_detect(LABEL,'Total') , 'GCS_total', LABEL)) %>%
  filter(str_detect(LABEL,"GCS"))




ICP_VITAl_Temp_NBP <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Temperature C|NBP|Non Invasive Blood")) %>%
  filter(!str_detect(LABEL,'CCO|Alarm')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Temp'), 'Temp', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'NBP|Non') & str_detect(LABEL,'mean|Mean') , 'mittl_NBD', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'NBP|Non') & str_detect(LABEL,'syst|Syst') , 'syst_NBD', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'NBP|Non') & str_detect(LABEL,'diast|Diast') , 'diast_NBD', LABEL))


ICP_VITAl_Beatmung <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"Minute Volume|Volum|Tida|Insp")) %>%
  filter(!str_detect(LABEL,'High|Alarm|Stroke|Lumen|Gas|O2')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Minute'), 'AMV total', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Tidal') & str_detect(LABEL,'Volume') , 'AZV', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Insp') & str_detect(LABEL,'Time') , 't insp', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Insp') & str_detect(LABEL,'Ratio') , 'I_E', LABEL)) %>%
  filter(str_detect(LABEL,"AZV|AMV|t insp|I_E")) 


ICP_VITAl_RASS_Pup <-
  ICP_chartevent %>%
  filter(str_detect(LABEL,"RAS|Pup")) %>%
  # filter(!str_detect(LABEL,'High|Alarm|Stroke|Lumen|Gas|O2')) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Resp') & str_detect(LABEL,'Right') , 'Lichtreaktion re', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Resp') & str_detect(LABEL,'Left') , 'Lichtreaktion li', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Size') & str_detect(LABEL,'Right') , 'Pupille re', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Size') & str_detect(LABEL,'Left') , 'Pupille li', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'Goal') , 'RASS_Ziel', LABEL)) %>%
  mutate(LABEL = ifelse(str_detect(LABEL,'RAS') & !str_detect(LABEL,'Ziel') , 'RASS', LABEL)) %>%
  # mutate(LABEL = ifelse(str_detect(LABEL,'Insp') & str_detect(LABEL,'Time') , 't insp', LABEL)) %>%
  # mutate(LABEL = ifelse(str_detect(LABEL,'Insp') & str_detect(LABEL,'Ratio') , 'I_E', LABEL)) %>%
  filter(!str_detect(LABEL,"Pupils")) 


ICP_VITAl_RASS_Pup <-
  ICP_VITAl_RASS_Pup %>%
  filter(str_detect(LABEL,"Lichtreaktion")) %>%
  mutate(VALUENUM = ifelse(str_detect(LABEL,'Licht') & str_detect(VALUE,'Brisk|Slug') , 1, VALUENUM)) %>%
  mutate(VALUENUM = ifelse(str_detect(LABEL,'Licht') & str_detect(VALUE,'Non') , 2, VALUENUM)) %>%
  mutate(VALUEUOM = ifelse(str_detect(LABEL,'re') , 'rechts', VALUEUOM)) %>%
  mutate(VALUEUOM = ifelse(str_detect(LABEL,'li') , 'links', VALUEUOM)) %>%
  mutate(LABEL = 'Lichtreaktion') %>%
  rbind(ICP_VITAl_RASS_Pup %>% filter(!str_detect(LABEL,"Lichtreaktion")))



# load(file = "/home/nils/ICP_data/UKE_processed/ICP_Vital.RData")


ICP_vital <- rbind(ICP_vital_1,ICP_vital_2,ICP_vital_3,ICP_vital_4,ICP_vital_GCS,ICP_VITAl_Temp_NBP, ICP_VITAl_Beatmung,
                   ICP_VITAl_RASS_Pup)


ICP_LABEL <- 
  ICP_vital %>% distinct(LABEL) %>% pull(LABEL)


rm(ICP_vital_1,ICP_vital_2,ICP_vital_3,ICP_vital_4,ICP_vital_GCS,ICP_VITAl_Temp_NBP, ICP_VITAl_Beatmung,
   ICP_VITAl_RASS_Pup)


ICP_vital %>%
  filter(LABEL == 'ICP')


ICP_vital %>%
  filter(!Maßnahme %in% ICP_LABEL)%>% 
  count(Maßnahme) %>% arrange(desc(n)) %>% as.data.frame()


ICP_chartevent %>%
  filter(str_detect(LABEL,"Puls")) %>%
  # filter(!str_detect(LABEL,'High|Alarm|Stroke|Lumen|Gas|O2')) %>%
  count(LABEL) %>% arrange(desc(n))



# Vital - Umrechnungen  ######################################################################

ICP_vital %>%
  mutate(VALUENUM = ifelse(is.na(VALUENUM),  parse_number(VALUE), VALUENUM)) %>%
  filter(is.na(VALUENUM)) %>% count(LABEL)


ICP_vital <-
  ICP_vital %>%
  mutate(VALUENUM = ifelse(is.na(VALUENUM),  parse_number(VALUE), VALUENUM))


ICP_vital <-
  ICP_vital %>%
  rename(Zeit = CHARTTIME, Maßnahme=LABEL, Wert = VALUENUM) 


ICP_vital <-
  ICP_vital %>%
  mutate(Wert = ifelse(grepl(":00",Wert),'0',Wert)) %>%
  mutate(Wert = gsub(',','.',Wert)) %>%
  mutate(Wert = parse_number(Wert)) %>%
  filter(!is.na(Wert))



# Vital - Zeiten relativieren und normieren  #######################################################

# Aufnahmezeit laden
load(file = "mimic_processed/MIMIC_ICP_data.RData")

ICP_admit <- ICP_data %>% select(PAT_ID,ADMITTIME)

rm(ICP_data)

ICP_vital <-
  ICP_vital %>% mutate(ID = "Vital")

ICP_vital_outlier <-
  ICP_vital %>%
  left_join(ICP_admit) %>% 
  mutate(Zeit = ymd_hms(Zeit)) %>%
  mutate(rel_time = difftime(Zeit, ADMITTIME, units = 'min')) %>%  
  mutate(rel_time =  floor(as.numeric(rel_time))) %>%
  group_by(ID, PAT_ID, Maßnahme, VALUEUOM, rel_time) %>% 
  summarise(Wert = mean(Wert,na.rm = TRUE)) %>%
  ungroup() 


# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_vital_outlier, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()

# Falls das nicht der Fall ist Ausreißer mit einem selektiven Filter entfernen
ICP_vital_outlier <-
  ICP_vital_outlier %>%
  mutate(Wert = ifelse(str_detect(Maßnahme,"auge|motor|verbal") & rel_time < -300, NA, Wert)) %>%
  filter(!is.na(Wert))

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_vital_outlier, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()



# Vital - Speichern .csv und RData  #######################################################

write.csv(ICP_vital_outlier, file = "mimic_processed_clean/csv/MIMIC_vital.csv",
          row.names=FALSE, na="")

save(ICP_vital_outlier, file = "mimic_processed_clean/ICP_vital.RData")


# Workspace aufräumen:

rm(list=ls())


# Medikamente ############################################################################

MIMIC_inputevents_cv <- as_tibble(read.csv(gzfile("mimic-iii/INPUTEVENTS_CV.csv.gz"), stringsAsFactors=FALSE))

MIMIC_inputevents_mv <- as_tibble(read.csv(gzfile("mimic-iii/INPUTEVENTS_MV.csv.gz"), stringsAsFactors=FALSE))

ICP_inputevents_cv <- MIMIC_inputevents_cv %>% filter(SUBJECT_ID %in% ICP_ID)
ICP_inputevents_mv <- MIMIC_inputevents_mv %>% filter(SUBJECT_ID %in% ICP_ID)

save(ICP_inputevents_cv, file = "mimic-iii/MIMIC_ICP_inputevents_cv.RData")
save(ICP_inputevents_mv, file = "mimic-iii/MIMIC_ICP_inputevents_mv.RData")

rm(MIMIC_inputevents_cv,MIMIC_inputevents_mv)


# MIMIC/ICP Medikamente laden

load( file = "mimic-iii/MIMIC_ICP_inputevents_cv.RData")
load( file = "mimic-iii/MIMIC_ICP_inputevents_mv.RData")
load( file = "mimic_processed_clean/MIMIC_ICP_data.RData") # für das Gewicht
load( file = "mimic_processed_clean/MIMIC_ICP_ID.RData")


MIMIC_items <- as_tibble(read.csv(gzfile("mimic-iii/D_ITEMS.csv.gz"), stringsAsFactors=FALSE))

ICP_inputevents_mv <- ICP_inputevents_mv %>% left_join(MIMIC_items %>% select(ITEMID,LABEL))
ICP_inputevents_cv <- ICP_inputevents_cv %>% left_join(MIMIC_items %>% select(ITEMID,LABEL))

# Medikamente - PAT ID Erstellen und Filtern ###################################

load(file = "mimic_processed_clean/MIMIC_Pat_ID.RData")
# Pat_ID erstellen (Subject ID und Hospital ID)

ICP_inputevents_mv <- ICP_inputevents_mv %>% mutate(PAT_ID = paste(SUBJECT_ID,HADM_ID, sep = "_"))
ICP_inputevents_mv <- ICP_inputevents_mv %>% filter(PAT_ID %in% Pat_ID)

# Pat_ID erstellen (Subject ID und Hospital ID)

ICP_inputevents_cv <- ICP_inputevents_cv %>% mutate(PAT_ID = paste(SUBJECT_ID,HADM_ID, sep = "_"))
ICP_inputevents_cv <- ICP_inputevents_cv %>% filter(PAT_ID %in% Pat_ID)


# Medikamente - Substanzklassen Feature definieren ###################################

Opioide <- c("Fent","fent","Morph","morph","Code","Codein","Dilaudid")

Benzodiazepine <- c("Lora","Mida","mida","lora","Ativa")

Katecholamine <- c("Neosynephrine","Norep","Phenylephrine","Epineph","Dopamin","Dobuta")

Calcium_Blocker <- c("Nicard")

Alpha_Blocker <- c("Clonid","Labet","Dexmed")

Direkter_Vasodilatator <- c("Dihydral","Nitro","nitro","Apresoline","apresol")

Barbiturate <- c("Pentobarb","Allonal","Amytal Sodium","Brevital","Butabarb",
                 "Butalan","Buticaps","Butisol Sodium","Luminal", "Mebaral",
                 "Mephyltaletten","Nembutal","Nembutal Sodium","Oramon","Pentothal",
                 "Phemiton","Prominal","Sarisol","Seconal","Somnifaine","Surital","Brevi","Metho")

Narkotikum <- c("Propo","Ketam")



# Medikamente - Übersicht LABEL VOR Filter   ####################################

ALL_INPUTEVENTS_sorted <-
  ICP_inputevents_mv %>% 
  select(PAT_ID, LABEL)

names(ICP_inputevents_cv)


ALL_INPUTEVENTS_sorted <-
  ICP_inputevents_cv %>% 
  select(PAT_ID, LABEL) %>%
  rbind(ALL_INPUTEVENTS_sorted) %>%
  group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))

ALL_INPUTEVENTS_sorted %>%
  filter(str_detect(LABEL,"Na"))


# Medikamente - Filter  ##################################################

Medikamente_mv <-
  ICP_inputevents_mv %>%
  filter(str_detect(LABEL,paste(Barbiturate,collapse = '|'))|
           str_detect(LABEL,paste(Narkotikum,collapse = '|'))|  
           str_detect(LABEL,paste(Opioide,collapse = '|'))|
           str_detect(LABEL,paste(Katecholamine,collapse = '|'))|
           str_detect(LABEL,paste(Alpha_Blocker,collapse = '|'))|
           str_detect(LABEL,paste(Calcium_Blocker,collapse = '|'))|
           str_detect(LABEL,paste(Direkter_Vasodilatator,collapse = '|'))|
           str_detect(LABEL,paste(Benzodiazepine,collapse = '|'))) %>%
  filter(!is.na(RATE)) %>% 
  select(PAT_ID,STARTTIME,ENDTIME,LABEL,RATE,RATEUOM,ITEMID,ORDERCATEGORYDESCRIPTION)


Medikamente_cv <-
  ICP_inputevents_cv %>%
  filter(str_detect(LABEL,paste(Barbiturate,collapse = '|'))|
           str_detect(LABEL,paste(Narkotikum,collapse = '|'))|  
           str_detect(LABEL,paste(Opioide,collapse = '|'))|
           str_detect(LABEL,paste(Katecholamine,collapse = '|'))|
           str_detect(LABEL,paste(Alpha_Blocker,collapse = '|'))|
           str_detect(LABEL,paste(Calcium_Blocker,collapse = '|'))|
           str_detect(LABEL,paste(Direkter_Vasodilatator,collapse = '|'))|
           str_detect(LABEL,paste(Benzodiazepine,collapse = '|'))) %>%
  filter(!is.na(RATE)) %>% 
  select(PAT_ID,CHARTTIME,LABEL,RATE,RATEUOM,ITEMID,ORIGINALROUTE,STOPPED)

# Workspace aufräumen
rm(MIMIC_items,ICP_ID)

# Medikamente - Übersicht LABEL NACH Filter   ################################

# Um sich eine Übersicht zu schaffen ist folgendes ganz sinnvoll 
ALL_MEDIKAMENTE_sorted <-
  Medikamente_mv %>% 
  filter(!is.na(RATE)) %>% select(PAT_ID, LABEL)

ALL_MEDIKAMENTE_sorted <-
  Medikamente_cv %>% 
  filter(!is.na(RATE)) %>% select(PAT_ID, LABEL) %>%
  rbind(ALL_MEDIKAMENTE_sorted) %>%
  group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))

ALL_MEDIKAMENTE_sorted %>%
  filter(str_detect(LABEL,"Fent"))

rm(ALL_INPUTEVENTS_sorted,ALL_MEDIKAMENTE_sorted)

# Medikamente - Auf eine Stunde mitteln #####################################

Medikamente_mv <-
  Medikamente_mv %>%
  mutate(STARTTIME = ymd_hms(STARTTIME)) %>%
  mutate(ENDTIME = ymd_hms(ENDTIME))

# Damit das Medikament am Ende auf 0 gesetzt wird
Medikamente_mv_ende <-
  Medikamente_mv %>% 
  group_by(PAT_ID,LABEL,RATEUOM,ITEMID,ORDERCATEGORYDESCRIPTION) %>%
  summarise(ENDTIME = max(ENDTIME)) %>%
  ungroup() %>%
  mutate(STARTTIME = ENDTIME) %>%
  mutate(RATE = 0) %>%
  select(names(Medikamente_mv))

Medikamente_mv <-
  Medikamente_mv %>%
  rbind(Medikamente_mv_ende) %>%
  group_by(PAT_ID,LABEL) %>%
  complete(STARTTIME = seq.POSIXt(min(STARTTIME), 
                                  max(ENDTIME), by = "min")) %>%
  arrange(STARTTIME) %>%
  fill(ENDTIME,RATE,RATEUOM,ITEMID,ORDERCATEGORYDESCRIPTION) %>%
  mutate(NEWDATE = floor_date(STARTTIME, unit = "min")) %>%
  group_by(PAT_ID,NEWDATE,LABEL,RATEUOM,ITEMID,ORDERCATEGORYDESCRIPTION) %>%
  summarise(RATE = mean(RATE,na.rm = TRUE)) %>%
  ungroup()

Medikamente_cv <-
  Medikamente_cv %>%
  mutate(CHARTTIME = ymd_hms(CHARTTIME)) %>%
  group_by(PAT_ID,LABEL) %>%
  complete(CHARTTIME = seq.POSIXt(min(CHARTTIME), 
                                  max(CHARTTIME), by = "min")) %>%
  arrange(CHARTTIME) %>%
  fill(RATE,RATEUOM,ITEMID,ORIGINALROUTE) %>%
  mutate(NEWDATE = floor_date(CHARTTIME, unit = "min")) %>%
  group_by(PAT_ID,NEWDATE,LABEL,RATEUOM,ITEMID,ORIGINALROUTE) %>%
  summarise(RATE = mean(RATE,na.rm = TRUE)) %>%
  ungroup()

# Alles zusammenzufassen

names(Medikamente_cv)
names(Medikamente_mv)

Medikamente_mv <-
  Medikamente_mv %>% rename(ORIGINALROUTE=ORDERCATEGORYDESCRIPTION)

Medikamente_total <- rbind(Medikamente_cv,Medikamente_mv)


# Medikamente - Laufraten (LABELS) anpassen  ##################################### 
# Raten der einzelnen Labels werden angepasst und einzelne Labels auch

Medikamente_total <-
  Medikamente_total %>%
  mutate(RATEUOM = gsub("\\/","",RATEUOM),
         RATEUOM = gsub("hour","hr",RATEUOM))%>%
  mutate(LABEL = gsub("Labetalol","Labetolol",LABEL),
         LABEL = gsub("Pentobarbitol","Pentobarbital",LABEL),
         LABEL = gsub("-k","",LABEL),) 

# Es sollte nur EIN! LABEL zu jeder RATE existieren

Medikamente_total %>%
  mutate(RATEUOM = gsub("\\/","",RATEUOM),
         RATEUOM = gsub("hour","hr",RATEUOM)) %>%
  group_by(LABEL,RATEUOM) %>%
  mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n)) %>%
  mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))




# Medikamente - Klassen werden definiert ########################################

Medikamente_total <-
  Medikamente_total %>%
  mutate(Klasse = case_when(str_detect(LABEL,paste(Narkotikum,collapse = '|')) ~ "Narkotikum",
                            str_detect(LABEL,paste(Opioide,collapse = '|')) ~ "Opioid",
                            str_detect(LABEL,paste(Benzodiazepine,collapse = '|')) ~ "Benzodiazepin",
                            str_detect(LABEL,paste(Katecholamine,collapse = '|')) ~ "Katecholamin",
                            str_detect(LABEL,paste(Alpha_Blocker,collapse = '|')) ~ "Alpha Blocker",
                            str_detect(LABEL,paste(Calcium_Blocker,collapse = '|')) ~ "Calcium Blocker",
                            str_detect(LABEL,paste(Direkter_Vasodilatator,collapse = '|')) ~ "Direkter Vasodilatator",
                            str_detect(LABEL,paste(Barbiturate,collapse = '|')) ~ "Barbiturat",
                            TRUE ~ "NaN")) %>%
  group_by(Klasse) # %>%
#mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))

save(Medikamente_total, file = "mimic_processed_clean/ICP_med_total.RData")

# Zwischenspeichern

load(file = "mimic_processed_clean/ICP_med_total.RData")

rm(Alpha_Blocker, Barbiturate, Benzodiazepine, Calcium_Blocker, Katecholamine,
   Narkotikum, Opioide, Direkter_Vasodilatator)


# Medikamente - Normalisieren der Werte ###############################

Medikamente_total_norm <-
  Medikamente_total  %>%
  group_by(LABEL,RATEUOM) %>%
  mutate(mean = mean(RATE, na.rm = TRUE), sd = sd(RATE,na.rm = TRUE)) %>%
  mutate(RATENORM = (RATE-mean)/sd) 


ggplot(Medikamente_total_norm, aes(RATENORM))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()




#  Medikamente - Mannitol      #######################################

# muss auch aus den INPUTEVENTS geladen werden
# da in den prescriptions zwar mehr sind aber
# keine genauen Zeitpunkte was etwas ärgerlich ist


MIMIC_items  <- 
  as_tibble(read.csv(gzfile("mimic-iii/D_ITEMS.csv.gz"), stringsAsFactors=FALSE))

Manni_ID <-
  MIMIC_items %>%
  filter(str_detect(LABEL,"Mann|mann")) %>% select(ITEMID) %>% pull

ICP_manni <-
  ICP_inputevents_cv %>%
  filter(ITEMID %in% Manni_ID) %>%
  select(PAT_ID,CHARTTIME) %>%
  rename(STARTTIME = CHARTTIME) %>%
  mutate(LABEL = "Mannitol",RATE = 1)

ICP_manni <-
  ICP_inputevents_mv %>%
  filter(ITEMID %in% Manni_ID) %>%
  select(PAT_ID,STARTTIME) %>%
  mutate(LABEL = "Mannitol",RATE = 1) %>%
  rbind(ICP_manni)


# Einzelgabe wird auf 2h Wirkung ausgeweitet
ICP_manni <-
  ICP_manni %>%
  mutate(STARTTIME = ymd_hms(STARTTIME)) %>%
  mutate(STARTTIME = floor_date(STARTTIME, unit = "min")) %>%
  group_by(PAT_ID,LABEL) %>%
  complete(STARTTIME = seq.POSIXt(min(STARTTIME), 
                                  min(STARTTIME)+hours(2), by = "min")) %>%
  ungroup() %>%
  mutate(LABEL = "Mannitol",RATE = 1,RATENORM = 1, Klasse = "Mannitol")

ICP_manni <-
  ICP_manni %>%
  rename(Zeit = STARTTIME,Maßnahme=LABEL,Wert = RATE, Wert_norm = RATENORM)

Medikamente_total_norm <-
  Medikamente_total_norm %>%
  rename(Zeit = NEWDATE,Maßnahme=LABEL,Wert = RATE, Wert_norm = RATENORM) %>%
  ungroup() %>%
  select(names(ICP_manni)) %>%
  rbind(ICP_manni)

# ID Med hinzufügen
Medikamente_total_norm <- Medikamente_total_norm %>% mutate(ID = "Med")


# Zur Übersicht plotten
ggplot(Medikamente_total_norm,aes(Wert_norm))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()


# Medikamente - Zeiten relativieren und normieren  #######################################################

# Aufnahmezeit laden
load(file = "mimic_processed_clean/MIMIC_ICP_data.RData")

ICP_admit <- ICP_data %>% select(PAT_ID,ADMITTIME)

rm(ICP_data)

Medikamente_total_norm <-
  Medikamente_total_norm %>%
  left_join(ICP_admit) %>% 
  mutate(Zeit = ymd_hms(Zeit)) %>%
  mutate(rel_time = difftime(Zeit, ADMITTIME, units = 'min')) %>%  
  mutate(rel_time =  floor(as.numeric(rel_time))) %>%
  group_by(PAT_ID,Maßnahme,ID,rel_time,Klasse) %>% 
  summarise(Wert = mean(Wert,na.rm = TRUE),Wert_norm = mean(Wert_norm,na.rm = TRUE)) %>%
  ungroup() 

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(Medikamente_total_norm, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()


# Medikamente - Exportieren  ####################################################

write.csv(Medikamente_total_norm, file = "mimic_processed_clean/csv/MIMIC_med.csv",
          row.names=FALSE, na="")

save(Medikamente_total_norm, file = "mimic_processed_clean/ICP_med.RData")


# Workspace aufräumen:

rm(list=ls())



# Labor + BGA - Laden der Wichtigen Dateien ##############################

MIMIC_labevents <- as_tibble(read.csv(gzfile("mimic-iii/LABEVENTS.csv.gz"), stringsAsFactors=FALSE))

load(file = "mimic_processed_clean/MIMIC_ICP_ID.RData")

# Pat_ID erstellen (Subject ID und Hospital ID)
MIMIC_labevents <- MIMIC_labevents %>% mutate(PAT_ID = paste(SUBJECT_ID,HADM_ID, sep = "_"))
ICP_labevents <- MIMIC_labevents %>% filter(PAT_ID %in% ICP_ID)

LAB_items <- as_tibble(read.csv(gzfile("mimic-iii/D_LABITEMS.csv.gz"), stringsAsFactors=FALSE))
LAB_items <- LAB_items %>% select(ITEMID,LABEL)

ICP_labevents <- ICP_labevents %>% left_join(LAB_items)

save(ICP_labevents, file = "mimic-iii/MIMIC_ICP_labevents.RData")

rm(MIMIC_labevents,ICP_ID,LAB_items)



#  BGA - Start   #######################################################


load(file = "mimic-iii/MIMIC_ICP_labevents.RData")


# Schauen was es alles so für Werte gibt
ALL_LABEVENTS_sorted <-
  ICP_labevents %>% 
  filter(!is.na(VALUENUM)) %>%
  group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))

# und wieder entfernen
rm(ALL_LABEVENTS_sorted)


BGA <- c("Glucose","pH","Sodium","Potassium","Hemoglobin","Chloride",
         "Base Excess","pCO2","pO2","Bicarbonate","Anion Gap","Carbox",
         "Calcium","Lac","Bili","Oxygen")


ICP_BGA <-
  ICP_labevents %>%
  filter(str_detect(LABEL,paste(BGA,collapse = '|'))) %>%
  mutate(LABEL = gsub("Base Excess","SBE",LABEL),
         LABEL = gsub("Calcium, Total","Ca",LABEL),
         LABEL = gsub("Oxygen Saturation","sO2",LABEL),
         LABEL = gsub("Anion Gap","Anionen_Lücke",LABEL)) %>%
  filter(!grepl(" ",LABEL)) %>%
  #group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))
  mutate(LABEL = gsub("Carboxyhemoglobin","FCOHb",LABEL),
         LABEL = gsub("Bilirubin","Bili",LABEL),
         LABEL = gsub("Potassium","K",LABEL),
         LABEL = gsub("Sodium","Na",LABEL),
         LABEL = gsub("pCO2","PCO2",LABEL),
         LABEL = gsub("pO2","PO2",LABEL),
         LABEL = gsub("Oxygen Saturation","sO2",LABEL),
         LABEL = gsub("Oxygen","FO2",LABEL),
         LABEL = gsub("Chloride","Cl",LABEL),
         LABEL = gsub("Glucose","Glu",LABEL),
         LABEL = gsub("Hemoglobin","Hb_BGA",LABEL),
         LABEL = gsub("Bicarbonate","HCO3",LABEL),
         LABEL = gsub("Lactate","Lac",LABEL)) #%>%
#group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))

ICP_BGA <-
  ICP_BGA %>%
  rename(Zeit = CHARTTIME, Maßnahme=LABEL,Wert = VALUENUM) %>%
  select(PAT_ID,Zeit,Maßnahme,Wert)

rm(BGA)



# BGA - Zeiten relativieren und normieren  #######################################################


ICP_BGA <-
  ICP_BGA %>% mutate(ID = "BGA")

# Aufnahmezeit laden
load(file = "mimic_processed_clean/MIMIC_ICP_data.RData")

ICP_admit <- ICP_data %>% select(PAT_ID,ADMITTIME)

rm(ICP_data)

ICP_BGA <-
  ICP_BGA %>%
  left_join(ICP_admit) %>% 
  mutate(Zeit = ymd_hms(Zeit)) %>%
  mutate(rel_time = difftime(Zeit, ADMITTIME, units = 'min')) %>%  
  mutate(rel_time =  floor(as.numeric(rel_time))) %>%
  group_by(PAT_ID,Maßnahme,ID,rel_time) %>% summarise(Wert = mean(Wert,na.rm = TRUE)) %>%
  ungroup() 

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_BGA, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()

rm(ICP_admit)

# BGA - Speichern .csv und RData  ################################################################

write.csv(ICP_BGA, file = "mimic_processed_clean/csv/MIMIC_BGA.csv",
          row.names=FALSE, na="")

save(ICP_BGA, file = "mimic_processed_clean/ICP_BGA.RData")

# Workspace aufräumen:

rm(ICP_BGA)




#  Labor       ################################################################


#  Blutbild und Gerinnung

Labor <- c("MC","Hct","RBC","RDW","WBC","INR","PTT","Baso",
           "Chole","Eos","Fibri","Mono","Lymphs","Polys","Amyl",
           "Phosph","Plat","Thy","Leuk")

ICP_Labor_select <-
  ICP_labevents %>%
  filter(str_detect(LABEL,paste(Labor,collapse = '|'))) %>%
  mutate(LABEL = gsub("Fibrinogen, Functional","Fibrinogen",LABEL),
         LABEL = gsub("Cholesterol, Total","Cholesterin",LABEL),
         LABEL = gsub("Thyroid Stimulating Hormone","TSH",LABEL),
         LABEL = gsub("Platelet Count","Thrombocyten",LABEL)) %>%
  filter(!grepl(" |7|NRBC|siderin|Thy",LABEL)) %>%
  filter(!is.na(VALUENUM))%>%
  #group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))
  mutate(LABEL = gsub("INR\\(PT\\)","INR",LABEL),
         LABEL = gsub("WBC","Leukocyten",LABEL),
         LABEL = gsub("RBC","Erythrocyten",LABEL),
         LABEL = gsub("Basophils","Basophile",LABEL),
         LABEL = gsub("Eosinophils","Eosinophile",LABEL),
         LABEL = gsub("Lymphs","Lymphocyten",LABEL),
         LABEL = gsub("RDW","EVB",LABEL),
         LABEL = gsub("fibrinogen","Fibrinogen",LABEL),
         LABEL = gsub("PTT","aPTT",LABEL),
         LABEL = gsub("Hct","Hk",LABEL),
         LABEL = gsub("Monocytes","Monocyten",LABEL),
         LABEL = gsub("Monos","Monocyten",LABEL),
         LABEL = gsub("Polys","Neutrophile",LABEL),
         LABEL = gsub("Amylase","pankreasspez",LABEL),
         LABEL = gsub("Phosphate","Phosphat",LABEL))  #%>%
#group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))



# Zum Suchen der gewünschten Laborwerte
ALL_LABEVENTS_sorted %>%
  filter(str_detect(LABEL,"Ant"))


Labor <- c("Albu","C-","Alk","ALT","AST","Crea","Dim","Prot",
           "T3","T4","Dehy","Lipase","Magn","Triglycerid","Trop",
           "Urea","Glut","PT","Hema")

ICP_Labor_select <-
  ICP_labevents %>%
  filter(str_detect(LABEL,paste(Labor,collapse = '|'))) %>%
  mutate(LABEL = gsub("C-Reactive Protein","CRP",LABEL),
         LABEL = gsub("Triiodothyronine \\(T3\\)","fT3",LABEL),
         LABEL = gsub("INR\\(PT\\)","INR",LABEL),
         LABEL = gsub("Thyroxine \\(T4\\)","fT4",LABEL),
         LABEL = gsub("Alanine Aminotransferase \\(ALT\\)","ALT",LABEL),
         LABEL = gsub("Asparate Aminotransferase \\(AST\\)","AST",LABEL),
         LABEL = gsub("Lactate Dehydrogenase \\(LD\\)","LDH",LABEL),
         LABEL = gsub("Creatine Kinase \\(CK\\)","CK",LABEL),
         LABEL = gsub("Creatine Kinase, MB Isoenzyme","CK-MB",LABEL),
         LABEL = gsub("Troponin T","Troponin",LABEL),
         LABEL = gsub("Urea Nitrogen","Harnstoff-N",LABEL),
         LABEL = gsub("Gamma Glutamyltransferase","GGT",LABEL),
         LABEL = gsub("Alkaline Phosphatase","Alk",LABEL)) %>%
  filter(!grepl(" |<|PTT",LABEL)) %>%
  filter(!is.na(VALUENUM)) %>%
  mutate(LABEL = gsub("Albumin","Albumin",LABEL),
         LABEL = gsub("Lipase","Lipase",LABEL),
         LABEL = gsub("Magnesium","Magnesium",LABEL),
         LABEL = gsub("Hematocrit","Hk",LABEL),
         LABEL = gsub("PT","TZ",LABEL),
         LABEL = gsub("Creatinine","Kreatinin",LABEL),
         LABEL = gsub("D-Dimer","D-Dimere",LABEL),
         LABEL = gsub("Protein","Eiweiß",LABEL),
         LABEL = gsub("Triglycerides","Triglyceride",LABEL)) %>%
  #group_by(LABEL) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n)) %>% as.data.frame()
  rbind(ICP_Labor_select)


# Labor umrechnen  ################################


#CRP wird auf dl -> L umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("CRP",LABEL)) %>%
  mutate(Faktor = ifelse(grepl("/dL|/dl",VALUEUOM),10,1)) %>%
  mutate(VALUENUM = VALUENUM*Faktor) %>%
  mutate(LABEL = "CRP") %>% select(-Faktor) %>%
  rbind(ICP_Labor_select%>%
          filter(!grepl("CRP",LABEL)))

# Eiweiß, Albumin und Fibrinogen werden auf dl -> l umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Albumin|Eiweiß|Fibrin",LABEL)) %>%
  mutate(Faktor = ifelse(grepl("/dL|/dl",VALUEUOM),10,1)) %>%
  mutate(VALUENUM = VALUENUM*Faktor) %>%
  select(-Faktor) %>%
  rbind(ICP_Labor_select %>% filter(!grepl("Albumin|Eiweiß|Fibrin",LABEL)))


# Eiweiß, Albumin und Fibrinogen werden auf mg -> umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Albumin|Eiweiß|Fibrin",LABEL)) %>%
  mutate(Faktor = ifelse(grepl("mg|mG|MG",VALUEUOM),1000,1)) %>%
  mutate(VALUENUM = VALUENUM/Faktor) %>%
  select(-Faktor) %>%
  rbind(ICP_Labor_select %>%
          filter(!grepl("Albumin|Eiweiß|Fibrin",LABEL)))  

# Magnesium werden auf mmol/L umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Magnesium",LABEL)) %>%
  mutate(VALUENUM = VALUENUM*0.441) %>%
  rbind(ICP_Labor_select %>%
          filter(!grepl("Magnesium",LABEL)))

# Phosphat werden auf mmol/L umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Phosphat",LABEL)) %>%
  mutate(VALUENUM = VALUENUM*0.323) %>%
  rbind(ICP_Labor_select %>%
          filter(!grepl("Phosphat",LABEL)))


ICP_Labor_select %>%
  filter(LABEL == "Fibrinogen") %>%
  ggplot(.,aes(VALUENUM))+
  geom_density()+
  theme_bw()


# Nur relevante Spalten mitnehmen

ICP_Labor_select <-
  ICP_Labor_select %>%
  rename(Zeit = CHARTTIME, Maßnahme=LABEL,Wert = VALUENUM) %>%
  select(PAT_ID,Zeit,Maßnahme,Wert)

rm(Labor)


# Labor - Zeiten relativieren und normieren  #######################################################

ICP_Labor_select <-
  ICP_Labor_select %>% mutate(ID = "Labor")

# Aufnahmezeit laden
load(file = "mimic_processed_clean/MIMIC_ICP_data.RData")

ICP_admit <- ICP_data %>% select(PAT_ID,ADMITTIME)

rm(ICP_data)

ICP_Labor_select <-
  ICP_Labor_select %>%
  left_join(ICP_admit) %>% 
  mutate(Zeit = ymd_hms(Zeit)) %>%
  mutate(rel_time = difftime(Zeit, ADMITTIME, units = 'min')) %>%  
  mutate(rel_time =  floor(as.numeric(rel_time))) %>%
  group_by(PAT_ID,Maßnahme,ID,rel_time) %>% summarise(Wert = mean(Wert,na.rm = TRUE)) %>%
  ungroup() 


# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_Labor_select, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()

rm(ICP_admit)


# Labor - Speichern .csv und RData  ################################################################

write.csv(ICP_Labor_select, file = "mimic_processed_clean/csv/MIMIC_Labor.csv",
          row.names=FALSE, na="")

save(ICP_Labor_select, file = "mimic_processed_clean/ICP_labor.RData")


rm(ICP_Labor_select, ICP_labevents, ALL_LABEVENTS_sorted)




