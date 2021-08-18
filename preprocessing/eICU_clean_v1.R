
# eICU Import clean ------

# required packages:
library(tidyverse)
library(readxl)
library(lubridate)

dir.create('eicu_processed_clean')
dir.create('eicu_processed_clean/csv')


#  ICP ID    #####################################################

#  only Patients with valid ICP measurment are selected
#  this requires some time and memory to load the whole eICU-
#  dataset skip this part if ICP_ID is already existing


EICU_vitalperiodic <- read.csv(gzfile("eicu/vitalPeriodic.csv.gz"))
EICU_vitalperiodic <- as.data.frame(EICU_vitalperiodic)  

ICP_ID <- EICU_vitalperiodic %>%
  filter(!is.na(icp)) %>%
  distinct(patientunitstayid) %>% pull

save(ICP_ID,file = "eicu_processed/EICU_ICP_ID.RData")

# START -----
# if ICP_ID exists please start from here

load(file = "eicu_processed/EICU_ICP_ID.RData")

save(EICU_vitalperiodic, file =  'eicu/vitalPeriodic.RData')


#----------------------------- Datenbank ------------------------------------
# PatID
EICU_patient <- read.csv(gzfile("eicu/patient.csv.gz"), stringsAsFactors=FALSE)

load(file = "eicu_processed/EICU_ICP_ID.RData")

ICP_data <- EICU_patient %>% filter(patientunitstayid %in% ICP_ID) %>% as_tibble()

ICP_data <-
  ICP_data %>%
  as_tibble %>%
  mutate(apacheadmissiondx = as.character(apacheadmissiondx))%>%
  mutate(diagnosis = case_when(str_detect(apacheadmissiondx,"stroke") ~ "Stroke",
                               str_detect(apacheadmissiondx,"trauma|Trauma") ~ "TBI",
                               str_detect(apacheadmissiondx,"hematoma") ~ "ICH",
                               str_detect(apacheadmissiondx,"Subarachnoid") ~ "SAB",
                               str_detect(apacheadmissiondx,"subdural") ~ "SDH",
                               str_detect(apacheadmissiondx,"Neoplasm|tumor|Tumor") ~ "Tumor",
                               str_detect(apacheadmissiondx,"Sepsis|sepsis") ~ "Sepsis",
                               str_detect(apacheadmissiondx,"aortic|Aortic|myocardial") ~ "Aorta",
                               TRUE ~ "other")) 


ICP_data <-
  ICP_data %>%
  mutate(unitdischargeoffset = unitdischargeoffset/60/24)%>%
  rename('Aufenthaltsdauer in Tagen'= unitdischargeoffset)


# Welche Diagnosen fehlen noch, welche sind angesetzt
ICP_data %>%
  #select(diagnosis) %>% table %>% barplot
  filter(diagnosis == "other") %>%
  select(apacheadmissiondx) %>% table %>% as.data.frame %>%  arrange(Freq)  

ICP_data %>%
  as_tibble %>%
  mutate(apacheadmissiondx = as.character(apacheadmissiondx))%>%
  select(apacheadmissiondx) %>% table %>% as.data.frame %>%  arrange(desc(Freq))  

ICP_data <-
  ICP_data %>%
  select(patientunitstayid,gender,age,diagnosis,unitdischargestatus,uniquepid,hospitaldischargestatus,admissionheight,admissionweight,
         'Aufenthaltsdauer in Tagen') %>%
  rename(Pat_ID = patientunitstayid, Alter = age, Diagnose = diagnosis, Outcome = unitdischargestatus, 
         Outcome_Haus = hospitaldischargestatus, Größe = admissionheight, Gewicht = admissionweight)


#####################    Datenbank -  Aligning Diagnosis       ###################################

ICP_data <-
  ICP_data %>%
  mutate(Diagnose_txt = case_when(Diagnose == "Stroke" ~ "Stroke",
                                  Diagnose == "TBI" ~ "TBI",
                                  Diagnose == "SAB" ~ "SAH",
                                  Diagnose == "ICH" ~ "ICH",
                                  Diagnose == "Sepsis" ~ "MISC",
                                  Diagnose == "Aorta" ~ "MISC",
                                  Diagnose == "Tumor" ~ "Tumor",
                                  Diagnose == "other" ~ "MISC",
                                  Diagnose == "SDH"~ "ICH")) 

ICP_data <-
  ICP_data %>%
  mutate(Diagnose = case_when(Diagnose_txt == "Stroke" ~ 1,
                              Diagnose_txt == "TBI" ~ 2,
                              Diagnose_txt == "SAH" ~ 3,
                              Diagnose_txt == "ICH" ~ 4,
                              Diagnose_txt == "Tumor" ~ 5, 
                              Diagnose_txt == "MISC" ~ 6))


#####################    Datenbank -  Outcome alignment       ###################################


ICP_data <-
  ICP_data %>%
  mutate(Outcome = as.character(Outcome)) %>%
  mutate(Outcome = case_when(Outcome == "Alive" ~ "survived",
                             Outcome == "Expired" ~ "exitus")) 

ICP_data <-
  ICP_data %>%
  mutate(Outcome_Haus = as.character(Outcome_Haus))%>%
  mutate(Outcome_Haus = case_when(Outcome_Haus == "Alive" ~ "survived",
                                  Outcome_Haus == "Expired" ~ "exitus")) 


ICP_data <-
  ICP_data %>%
  mutate(Outcome_Haus = ifelse(is.na(Outcome_Haus),Outcome,Outcome_Haus))


ICP_data <-
  ICP_data %>%
  mutate(Outcome = as.factor(Outcome),Outcome_Haus = as.factor(Outcome_Haus)) 


ICP_data %>%
  summary()



#####################    Datenbank -  Export     ###################################

write.csv(ICP_data, file = "eicu_processed_clean/csv/EICU_data.csv",
          row.names=FALSE, na="")


save(ICP_data,file = "eicu_processed_clean/EICU_data.RData")



##############################   Vital - LOAD    ####################################
#
# loading vital parameter 
# Memory and time consuming step
# filtering for ICP_ID's

load(file =  'eicu/vitalPeriodic.RData')
load(file = "eicu_processed_clean/EICU_ICP_ID.RData")

ICP_vital <- EICU_vitalperiodic %>% filter(patientunitstayid %in% ICP_ID)%>% as_tibble()

save(ICP_vital, file = "eicu_processed_clean/EICU_vital_pre.RData")

rm(EICU_vitalperiodic,ICP_ID)

# Vital - START ##########################################################
# if the process of filtering is already done please proceed here

load(file = "eicu_processed_clean/EICU_vital_pre.RData")

ICP_vital <-
  ICP_vital %>%
  rename(HF=heartrate, ICP = icp , mittl= systemicmean, diast = systemicdiastolic,
         syst = systemicsystolic, Temp = temperature , SpO2 = sao2, AF = respiration) %>%
  pivot_longer(4:19) %>%
  rename(Maßnahme = name, Wert = value)


ICP_vital %>%
  filter(str_detect(Maßnahme, 'sys|mittl|diast')) %>%
  filter(!str_detect(Maßnahme, 'NBD')) %>%
  ggplot(.,aes(Wert)) +
  stat_density() +
  facet_grid(vars(Maßnahme))

# Time will be left as it is

ICP_vital <-
  ICP_vital %>%
  filter(!is.na(Wert)) %>%
  rename(Zeit = observationoffset)

barplot(table(ICP_vital$Maßnahme), las=2, cex.names=.5)

names(ICP_vital)

ICP_vital <-
  ICP_vital %>%
  select(patientunitstayid, Maßnahme, Zeit, Wert) %>%
  mutate(ID = "Vital") %>%
  select(5,1,2,3,4) 



# Vital - LOAD aperiodic ############################################################

# From scratch:
# EICU_vitalaperiodic <- read.csv(gzfile("eicu/vitalAperiodic.csv.gz"), stringsAsFactors=FALSE)
# EICU_vitalaperiodic <- as.data.frame(EICU_vitalaperiodic)
# save(EICU_vitalaperiodic, file = "eicu/vitalAperiodic.RData")

load(file = "eicu/vitalAperiodic.RData")


EICU_vitalaperiodic <-
EICU_vitalaperiodic %>%
  as_tibble() %>%
  rename(mittl_NBD = noninvasivemean, diast_NBD = noninvasivediastolic,
         syst_NBD = noninvasivesystolic) %>%
  pivot_longer(4:13) %>%
  rename(Maßnahme = name, Wert = value)


EICU_vitalaperiodic <-
EICU_vitalaperiodic %>%
  filter(str_detect(Maßnahme, 'NBD')) %>%
  filter(!is.na(Wert)) %>%
  rename(Zeit = observationoffset)

barplot(table(EICU_vitalaperiodic$Maßnahme), las=2, cex.names=.5)

names(EICU_vitalaperiodic)

ICP_vital <-
  EICU_vitalaperiodic %>%
  select(patientunitstayid, Maßnahme, Zeit, Wert) %>%
  mutate(ID = "Vital") %>%
  select(names(ICP_vital))%>%
  rbind(ICP_vital)


# Vital - LOAD respiratory ############################################################

# From scratch:
# EICU_respiratoryCharting <- read.csv(gzfile("eicu/respiratoryCharting.csv.gz"), stringsAsFactors=FALSE)
# EICU_respiratoryCharting <- as.data.frame(EICU_respiratoryCharting)
# save(EICU_respiratoryCharting, file = "eicu/respiratoryCharting.RData")

load(file = "eicu/respiratoryCharting.RData")

EICU_respiratoryCharting <-
EICU_respiratoryCharting %>%
  as_tibble()

ICP_vital_1 <-
EICU_respiratoryCharting %>%
  filter(str_detect(respchartvaluelabel,"PEEP|RR |RASS|FiO|Total RR")) %>%
  filter(!str_detect(respchartvaluelabel,"Respiratory|CPAP|Set|Spont|above|Unable")) %>%
  mutate(respchartvaluelabel = ifelse(str_detect(respchartvaluelabel,'RASS'), 'RASS', respchartvaluelabel)) %>%
  mutate(respchartvaluelabel = ifelse(str_detect(respchartvaluelabel,'RR') & 
                                        str_detect(respchartvaluelabel,'patient') , 'Freq spontan', respchartvaluelabel)) %>%
  mutate(respchartvaluelabel = ifelse(str_detect(respchartvaluelabel,'Total RR'), 'Freq gesamt', respchartvaluelabel)) 


ICP_vital_2 <-
EICU_respiratoryCharting %>%
  filter(str_detect(respchartvaluelabel,"Mean Airway Pressure|Peak Insp. Pressure")) %>%
  #filter(!str_detect(respchartvaluelabel,"Respiratory|CPAP|Set|Spont|above|Unable")) %>%
  mutate(respchartvaluelabel = ifelse(str_detect(respchartvaluelabel,'Mean Airway Pressure'), 'Pmean', respchartvaluelabel)) %>%
  mutate(respchartvaluelabel = ifelse(str_detect(respchartvaluelabel,'Peak Insp. Pressure'), 'Ppeak', respchartvaluelabel)) 
  

EICU_respiratoryCharting %>%
  filter(!str_detect(respchartvaluelabel,"PEEP|RR |RASS|FiO|Total RR")) %>%
  filter(!str_detect(respchartvaluelabel,"Mean Airway Pressure|Peak Insp. Pressure")) %>%
  filter(!str_detect(respchartvaluelabel,"Respiratory|CPAP|Set|Spont|above|Unable")) %>%
  count(respchartvaluelabel) %>%
  arrange(desc(n))  

ICP_vital_1 <- rbind(ICP_vital_1,ICP_vital_2)

ICP_vital_1 <-
ICP_vital_1 %>%
  mutate(ID = "Vital") %>%
  select(ID, patientunitstayid, respchartvaluelabel, respchartoffset, respchartvalue) %>%
  rename(Maßnahme = respchartvaluelabel, Zeit = respchartoffset, Wert = respchartvalue) %>%
  mutate(Wert = parse_number(Wert))
  
ICP_vital <- rbind(ICP_vital,ICP_vital_1)

rm(ICP_vital_1, ICP_vital_2)

# Vital - LOAD Nurse Charting für GCS ############################################################
# and other Scores

# From scratch:
# EICU_nursecharting <- read.csv(gzfile("eicu/nurseCharting.csv.gz"), stringsAsFactors=FALSE)
# EICU_nursecharting <- as.data.frame(EICU_nursecharting)
# save(EICU_nursecharting, file = "eicu/nurseCharting.RData")

# EICU_physicalexam <- read.csv(gzfile("eicu/physicalExam.csv.gz"), stringsAsFactors=FALSE)
# EICU_physicalexam <- as_tibble(as.data.frame(EICU_physicalexam))
# save(EICU_physicalexam, file = "eicu/physicalExam.RData")
# load( file = "eicu/physicalExam.RData")

# EICU_nurseassesment <- read.csv(gzfile("eicu/nurseAssessment.csv.gz"), stringsAsFactors=FALSE)
# EICU_nurseassesment <- as_tibble(as.data.frame(EICU_nurseassesment))
# save(EICU_nurseassesment, file = "eicu/nurseAssessment.RData")


# Pupillengröße
load( file = "eicu/nurseAssessment.RData")

ICP_pupille <-
EICU_nurseassesment %>%
  filter(str_detect(celllabel,'Pup')) %>%
  filter(str_detect(cellattribute,'Left|Right')) %>%
  mutate(cellattributevalue = parse_number(cellattributevalue)) %>%
  filter(!is.na(cellattributevalue)) %>%
  mutate(Maßnahme = ifelse(str_detect(cellattribute,'Right'), 'Pupille re', 'Pupille li')) %>%
  mutate(ID = "Vital") %>%
  select(ID, patientunitstayid, Maßnahme, nurseassessoffset, cellattributevalue)%>%
  rename(Zeit = nurseassessoffset, Wert = cellattributevalue) 

# Andocken an die alten Daten
ICP_vital <-
ICP_pupille %>%
  select(names(ICP_vital)) %>%
  rbind(ICP_vital)

  
# If the faster RData is already created:

load(file = "eicu/nurseCharting.RData")
load(file = "eicu_processed/EICU_ICP_ID.RData")

ICP_nursecharting <- EICU_nursecharting %>% filter(patientunitstayid %in% ICP_ID) %>% as_tibble()

save(ICP_nursecharting, file = "eicu_processed_clean/EICU_nursecharting.RData")

rm(EICU_nursecharting,ICP_ID)


# Vital - START Nurse Charting für GCS ######################################################

load(file = "eicu_processed_clean/EICU_nursecharting.RData")

ICP_nursecharting <- 
  ICP_nursecharting %>%
  mutate(ID = "Vital") %>%
  select(ID,patientunitstayid, nursingchartcelltypevalname, nursingchartoffset, nursingchartvalue)%>%
  rename(Maßnahme = nursingchartcelltypevalname, Zeit = nursingchartoffset, Wert = nursingchartvalue) %>%
  mutate(Wert = parse_number(Wert))

ICP_nursecharting <- 
  ICP_nursecharting %>%
  filter(!is.na(Wert))

Nursecharting <-
ICP_nursecharting %>%
  count(Maßnahme) %>%
  arrange(desc(n))

GCS_eyes <-
  ICP_nursecharting %>%
  filter(grepl("Eyes",Maßnahme)) %>%
  mutate(Maßnahme = "GCS_auge")

GCS_verbal <-
  ICP_nursecharting %>%
  filter(grepl("Verbal",Maßnahme))%>%
  mutate(Maßnahme = "GCS_verbal")

GCS_motor <-
  ICP_nursecharting %>%
  filter(grepl("Motor",Maßnahme))%>%
  mutate(Maßnahme = "GCS_motor")

GCS_total <-
  ICP_nursecharting %>%
  filter(grepl("GCS Total",Maßnahme)) %>%
  mutate(Maßnahme = "GCS_total")


ICP_vital <- rbind(ICP_vital, GCS_eyes, GCS_motor, GCS_verbal, GCS_total)

rm(GCS_eyes, GCS_motor, GCS_verbal, GCS_total, ICP_nursecharting)


# ungenutzte Parameter herausfiltern (Hier leider auch etco2)
ICP_vital <-
  ICP_vital %>%
  filter(!Maßnahme %in% c("cvp","padiastolic","pamean","pasystolic","st1","st2","st3","etco2"))


# PatID umbenennen
ICP_vital <-
  ICP_vital %>%
  rename(Pat_ID = patientunitstayid)


# CPP wird noch berechnet zur Vergleichbarkeit
ICP_vital <-
  ICP_vital %>%
  filter(Maßnahme %in% c("ICP","mittl")) %>%
  distinct(ID,Pat_ID,Maßnahme,Zeit, .keep_all = TRUE) %>%
  spread(Maßnahme,Wert) %>%
  mutate(Maßnahme = "CPP", Wert = mittl - ICP) %>%
  select(ID,Pat_ID,Maßnahme,Zeit,Wert) %>%
  filter(!is.na(Wert)) %>%
  rbind(ICP_vital)


# testweise die Verteilung plotten
ICP_vital %>%
  select(Wert,Maßnahme) %>% ggplot(.,aes(Wert))+geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()


ICP_vital %>%
  filter(Maßnahme == 'ICP')


# Vital - Time keeping minutes  ############################################
# duplicated values are removed

ICP_vital_outlier <-
  ICP_vital %>%
  mutate(rel_time = Zeit) %>% mutate(rel_time = floor(rel_time)) %>%
  group_by(ID,Pat_ID,Maßnahme,rel_time) %>% summarise(Wert = mean(Wert, na.rm = TRUE)) %>%
  ungroup()



# where is the time starting?

ggplot(ICP_vital_outlier, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()



# if the time needs to be adjusted selective filtering

ICP_vital_outlier <-
  ICP_vital_outlier %>%
  mutate(Wert = ifelse(str_detect(Maßnahme,"auge|motor|verbal|total") & rel_time < -300, NA, Wert)) %>%
  filter(!is.na(Wert))


# plotting the density

ggplot(ICP_vital_outlier, aes(Wert))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()



# Vital - Saving .csv und RData  ################################################################

write.csv(ICP_vital_outlier, file = "eicu_processed_clean/csv/EICU_vital.csv",
          row.names=FALSE, na="")

save(ICP_vital_outlier, file = "eicu_processed_clean/EICU_vital.RData")


# Workspace aufräumen:

rm(list=ls())




#################################      BGA - LOAD       #######################

# Again loading the data from scratch
# EICU_lab <- read.csv(gzfile("eicu/lab.csv.gz"), stringsAsFactors=FALSE)
# EICU_lab <- as.data.frame(EICU_lab)
# save(EICU_lab, file = "eicu/lab.RData")

load(file = "eicu/lab.RData")
load(file = "eicu_processed_clean/EICU_ICP_ID.RData")

ICP_Labor <- EICU_lab %>% filter(patientunitstayid %in% ICP_ID)%>% as_tibble()

save(ICP_Labor, file = "eicu_processed_clean/EICU_labor_pre.RData")

rm(EICU_lab, ICP_ID)

#  BGA - START #######

load(file = "eicu_processed/EICU_labor_pre.RData")


#  Um sich eine Übersicht zu schaffen ist folgendes ganz sinnvoll
ALL_LABOR_sorted <-
  ICP_Labor %>% 
  filter(!is.na(labresult)) %>%
  group_by(labname) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))


ALL_CHARTEVENTS_sorted %>%
  filter(str_detect(LABEL,"Man"))

BGA <- c("Carbox","calc","chlori","HCO","bili","glucose","Hgb","Base",
         "potass","sodiu","O2","pH","lactat","bicar","anion")


ICP_BGA <-
  ICP_Labor %>%
  filter(str_detect(labname,paste(BGA,collapse = '|'))) %>%
  filter(!grepl("-hs|pre|direct| in | ratio|urina|Tot|LPM|Conten|CSF|Defic",labname)) %>%
  mutate(labname = gsub("Carboxyhemoglobin","FCOHb",labname),
         labname = gsub("calcium","Ca",labname),
         labname = gsub("ionized Ca","Ca",labname),
         labname = gsub("total bilirubin","Bili",labname),
         labname = gsub("potassium","K",labname),
         labname = gsub("bicarbonate","HCO3",labname),
         labname = gsub("anion gap","Anionen_Lücke",labname),
         labname = gsub("sodium","Na",labname),
         labname = gsub("paCO2","PCO2",labname),
         labname = gsub("paO2","PO2",labname),
         labname = gsub("O2 Sat","sO2",labname),
         labname = gsub("\\ \\(%\\)","",labname),
         labname = gsub("FiO2","FO2",labname),
         labname = gsub("chloride","Cl",labname),
         labname = gsub("bedside ","",labname),
         labname = gsub("glucose","Glu",labname),
         labname = gsub("Hgb","Hb_BGA",labname),
         labname = gsub("Base Excess","SBE",labname),
         labname = gsub("lactate","Lac",labname)) # %>%
# group_by(labname) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))


ICP_BGA <-
  ICP_BGA %>%
  select(patientunitstayid,labname,labresult,labresultoffset)%>%
  mutate(ID = "BGA")%>%
  rename(Wert = labresult, Zeit = labresultoffset,Maßnahme = labname) %>%
  select(5,1,2,4,3)

ICP_BGA <-
  ICP_BGA %>%
  rename(Pat_ID = patientunitstayid)


# BGA - Zeiten auf Minuten lassen (Stunden anpassen) und mitteln ############################################
# hier werden automatisch auch Duplikate entfernt

ICP_BGA_outlier <-
  ICP_BGA %>%
  mutate(rel_time = Zeit) %>% mutate(rel_time = floor(rel_time)) %>%
  group_by(ID,Pat_ID,Maßnahme,rel_time) %>% summarise(Wert = mean(Wert, na.rm = TRUE)) %>%
  ungroup()

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_BGA_outlier, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()

# testweise die Verteilung plotten
ggplot(ICP_BGA_outlier, aes(Wert))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()


# Falls das nicht der Fall ist Ausreißer mit einem selektiven Filter entfernen
ICP_BGA_outlier <-
  ICP_BGA_outlier %>%
  mutate(Wert = ifelse(rel_time < -300, NA, Wert)) %>%
  filter(!is.na(Wert))

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_BGA_outlier, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()

# BGA - Speichern .csv und RData  ###########################################################


write.csv(ICP_BGA_outlier, file = "eicu_processed_clean/csv/EICU_bga.csv",row.names=FALSE, na="")

save(ICP_BGA_outlier, file = "eicu_processed_clean/EICU_bga.RData")

# Workspace aufräumen:

rm(ICP_BGA, ICP_BGA_outlier,BGA)


#############################      Labor      ###############################################


Labor <- c("MC","Hct","RBC","RDW","WBC","INR","PTT","baso","chole","eos",
           "fibri","mono","lymphs","poly","amyl","phosph","plat","TSH")



#  Blutbild und Gerinnung
ICP_Labor_select <-
  ICP_Labor %>%
  filter(str_detect(labname,paste(Labor,collapse = '|'))) %>%
  filter(!grepl("ionized|-hs|pre|direct| in | ratio",labname)) %>%
  mutate(labname = gsub("PT - INR","INR",labname),
         labname = gsub("WBC x 1000","Leukocyten",labname),
         labname = gsub("RBC","Erythrocyten",labname),
         labname = gsub("-basos","Basophile",labname),
         labname = gsub("total cholesterol","Cholesterin",labname),
         labname = gsub("-eos","Eosinophile",labname),
         labname = gsub("-lymphs","Lymphocyten",labname),
         labname = gsub("RDW","EVB",labname),
         labname = gsub("fibrinogen","Fibrinogen",labname),
         labname = gsub("PTT","aPTT",labname),
         labname = gsub("Hct","Hk",labname),
         labname = gsub("-monos","Monocyten",labname),
         labname = gsub("-polys","Neutrophile",labname),
         labname = gsub("amylase","pankreasspez",labname),
         labname = gsub("phosphate","Phosphat",labname),
         labname = gsub("platelets x 1000","Thrombocyten",labname)) #%>%
#group_by(labname) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n)) %>% as.data.frame()


Labor <- c("albumin","CRP","alk","ALT","AST","crea","CPK","dim","total prot","CPK-MB",
           "T3","T4","LDH","lipase","magn","trig","trop","BUN","PT")

ALL_LABOR_sorted %>%
  filter(str_detect(labname,"PT"))


# Sonstige Blutwerte
ICP_Labor_select <-
  ICP_Labor %>%
  filter(str_detect(labname,paste(Labor,collapse = '|'))) %>%
  filter(!grepl("ionized|pre|direct| in | ratio|INDEX|urin|3RU|free | - T|-hs|- INR|PTT",labname)) %>%
  mutate(labname = gsub("albumin","Albumin",labname),
         labname = gsub("alkaline phos.","Alk",labname),
         labname = gsub("\\ \\(SGPT\\)|\\ \\(SGOT\\)","",labname),
         labname = gsub("CPK","CK",labname),
         labname = gsub("creatinine","Kreatinin",labname),
         labname = gsub("total protein","Eiweiß",labname),
         labname = gsub("T3","fT3",labname),
         labname = gsub("T4","fT4",labname),
         labname = gsub("lipase","Lipase",labname),
         labname = gsub("magnesium","Magnesium",labname),
         labname = gsub("triglycerides","Triglyceride",labname),
         labname = gsub("PT","TZ",labname),
         labname = gsub("BUN","Harnstoff-N",labname),
         labname = gsub("troponin - I","Troponin",labname)) %>%
  #group_by(labname) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))
  rbind(ICP_Labor_select)

# Labor umrechnen  ################################

#CRP wird auf mg/L umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("CRP",labname)) %>%
  mutate(Faktor = ifelse(grepl("/dL|/dl",labmeasurenameinterface),10,1)) %>%
  mutate(labresult = as.numeric(gsub("<|>","",labresulttext))*Faktor) %>%
  mutate(labname = "CRP")%>%select(-Faktor) %>%
  rbind(ICP_Labor_select%>%
          filter(!grepl("CRP",labname)))

# Eiweiß und Albumin werden auf dl -- > L umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Albumin",labname)) %>%
  mutate(Faktor = ifelse(grepl("/dL|/dl",labmeasurenameinterface),10,1)) %>%
  mutate(labresult = labresult*Faktor) %>%
  select(-Faktor) %>%
  rbind(ICP_Labor_select %>% filter(!grepl("Albumin|Eiweiß|Fibrin",labname)))

# Eiweiß und Albumin werden auf mg -- > g umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Albumin|Eiweiß|Fibrin",labname)) %>%
  mutate(Faktor = ifelse(grepl("mg|mG|MG|Mg",labmeasurenameinterface),1000,1)) %>%
  mutate(labresult = labresult/Faktor) %>%
  select(-Faktor) %>%
  rbind(ICP_Labor_select %>% filter(!grepl("Albumin|Eiweiß|Fibrin",labname)))

# Magnesium werden auf mmol/L umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Magnesium",labname)) %>%
  mutate(labresult = labresult*0.441) %>%
  rbind(ICP_Labor_select %>% filter(!grepl("Magnesium",labname)))

# Phosphat werden auf mmol/L umgerechnet
ICP_Labor_select <-
  ICP_Labor_select %>%
  filter(grepl("Phosphat",labname)) %>%
  mutate(labresult = labresult*0.323) %>%
  rbind(ICP_Labor_select %>% filter(!grepl("Phosphat",labname)))


ICP_Labor_select %>%
  filter(grepl("Albumin",labname))


names(table(ICP_Labor_select$labname))


ICP_Labor_select <-
  ICP_Labor_select %>%
  select(patientunitstayid,labname,labresult,labresultoffset)%>%
  mutate(ID = "Labor")%>%
  rename(Wert = labresult, Zeit = labresultoffset, Maßnahme = labname) %>%
  select(5,1,2,4,3)

ICP_Labor_select <-
  ICP_Labor_select %>%
  rename(Pat_ID = patientunitstayid)

# testweise die Verteilung plotten
ICP_Labor_select %>%
  select(Wert,Maßnahme) %>% ggplot(.,aes(Wert))+geom_density() +
  facet_wrap(vars(Maßnahme),scales = "free") +
  theme_bw()



# Labor - Zeiten auf Minuten lassen (Stunden anpassen) und mitteln ############################################
# hier werden automatisch auch Duplikate entfernt

ICP_Labor_outlier <-
  ICP_Labor_select %>%
  mutate(rel_time = Zeit) %>% mutate(rel_time = floor(rel_time)) %>%
  group_by(ID,Pat_ID,Maßnahme,rel_time) %>% summarise(Wert = mean(Wert, na.rm = TRUE)) %>%
  ungroup()

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_Labor_outlier, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()

# testweise die Verteilung plotten
ggplot(ICP_Labor_outlier, aes(Wert))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()


# Falls das nicht der Fall ist Ausreißer mit einem selektiven Filter entfernen
ICP_Labor_outlier <-
  ICP_Labor_outlier %>%
  mutate(Wert = ifelse(rel_time < -300, NA, Wert)) %>%
  filter(!is.na(Wert))

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_Labor_outlier, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Maßnahme),scales = "free")+
  theme_bw()


# Labor - exportieren ################################################################

write.csv(ICP_Labor_outlier, file = "eicu_processed_clean/csv/EICU_labor.csv",row.names=FALSE, na="")

save(ICP_Labor_outlier, file = "eicu_processed_clean/EICU_labor.RData")


rm(ICP_Labor_select,ICP_Labor,ALL_LABOR_sorted,ICP_Labor_outlier, BGA, Labor, ICP_BGA, ICP_BGA_outlier)




##################################   Medikamente    #####################################


# Medikamente und deren Zeitpunkte
EICU_infusionmed <- read.csv(gzfile("eicu/infusionDrug.csv.gz"), stringsAsFactors=FALSE)
EICU_medication <- read.csv(gzfile("eicu/medication.csv.gz"), stringsAsFactors=FALSE)

EICU_infusionmed <- as.data.frame(EICU_infusionmed)
EICU_medication <- as.data.frame(EICU_medication)

save(EICU_infusionmed, file = "eicu/infusionDrug.RData")
save(EICU_medication, file = "eicu/medication.RData")

#

load(file = "eicu/infusionDrug.RData")
load(file = "eicu/medication.RData")
load(file = "eicu_processed_clean/EICU_ICP_ID.RData")

ICP_infusionmed <- EICU_infusionmed %>% filter(patientunitstayid %in% ICP_ID) %>% as_tibble()
ICP_medication <- EICU_medication %>% filter(patientunitstayid %in% ICP_ID) %>% as_tibble()

save(ICP_infusionmed, file = "eicu_processed/EICU_infusionmed.RData")
save(ICP_medication, file = "eicu_processed/EICU_medication.RData")

rm(EICU_infusionmed,EICU_medication)

# Medikamente - Von hier beginnen geht schneller bei Bedarf ####################

load(file = "eicu_processed/EICU_infusionmed.RData")
load(file = "eicu_processed/EICU_medication.RData")


# Medikamente - Substanzklassen Feature definieren ###############################

Opioide <- c("Fent","fent","Morph","morph","Code","Codein","Dilaudid")

Benzodiazepine <- c("Lora","Mida","mida","lora","Ativa")

Katecholamine <- c("Neosynephrine","Norep","Phenylephrine","Epineph","Dopamin","Dobuta","ephrine")

Calcium_Blocker <- c("Nicard")

Alpha_Blocker <- c("Clonid","Labet","Dexmed")

Direkter_Vasodilatator <- c("Dihydral","Nitro","nitro","Apresoline","apresol")

Barbiturate <- c("Pentobarb","Allonal","Amytal Sodium","Brevital","Butabarb",
                 "Butalan","Buticaps","Butisol Sodium","Luminal", "Mebaral",
                 "Mephyltaletten","Nembutal","Nembutal Sodium","Oramon","Pentothal",
                 "Phemiton","Prominal","Sarisol","Seconal","Somnifaine","Surital","Brevi","Metho")

Narkotikum <- c("Propo","Ketam")


# Um sich eine Übersicht zu schaffen ist folgendes ganz sinnvoll 

ALL_MEDICATION_sorted <-
  ICP_infusionmed %>% 
  select(patientunitstayid, drugname) %>%
  mutate(data = "infusionmed")

ALL_MEDICATION_sorted <-
  ICP_medication %>% 
  select(patientunitstayid, drugname) %>%
  mutate(data = "medication") %>%
  rbind(ALL_MEDICATION_sorted) %>%
  group_by(drugname,data) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))

ALL_MEDICATION_sorted %>%
  filter(str_detect(drugname,"P"))



# Medikamente - Filter werden angewandt ################################################################

Medikamente_iv <-
  ICP_infusionmed %>%
  filter(str_detect(drugname,paste(Barbiturate,collapse = '|'))|
           str_detect(drugname,paste(Narkotikum,collapse = '|'))|  
           str_detect(drugname,paste(Opioide,collapse = '|'))|
           str_detect(drugname,paste(Katecholamine,collapse = '|'))|
           str_detect(drugname,paste(Alpha_Blocker,collapse = '|'))|
           str_detect(drugname,paste(Calcium_Blocker,collapse = '|'))|
           str_detect(drugname,paste(Direkter_Vasodilatator,collapse = '|'))|
           str_detect(drugname,paste(Benzodiazepine,collapse = '|'))) %>%
  filter(!is.na(drugrate)) #%>%
#group_by(drugname) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n)) %>% as.data.frame()



# Auf eine Minute lassen (Stunde normieren) und den mean bilden

Medikamente_iv <-
  Medikamente_iv %>%
  group_by(patientunitstayid,drugname) %>%
  complete(infusionoffset = seq.int(min(infusionoffset),max(infusionoffset),by = 1)) %>%
  mutate(rel_time = infusionoffset) %>% mutate(rel_time = floor(rel_time)) %>%
  fill(drugrate) %>% mutate(drugrate = as.numeric(drugrate)) %>%
  group_by(rel_time,patientunitstayid,drugname) %>% summarise(drugrate = mean(drugrate, na.rm = TRUE)) %>%
  ungroup()


Medikamente_iv %>%
  group_by(drugname) %>% mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n)) %>% as.data.frame()


# Medikamente - Klassen werden definiert ######################################################################

Medikamente_total <-
  Medikamente_iv %>%
  mutate(Klasse = case_when(str_detect(drugname,paste(Narkotikum,collapse = '|')) ~ "Narkotikum",
                            str_detect(drugname,paste(Opioide,collapse = '|')) ~ "Opioid",
                            str_detect(drugname,paste(Benzodiazepine,collapse = '|')) ~ "Benzodiazepin",
                            str_detect(drugname,paste(Katecholamine,collapse = '|')) ~ "Katecholamin",
                            str_detect(drugname,paste(Alpha_Blocker,collapse = '|')) ~ "Alpha Blocker",
                            str_detect(drugname,paste(Calcium_Blocker,collapse = '|')) ~ "Calcium Blocker",
                            str_detect(drugname,paste(Direkter_Vasodilatator,collapse = '|')) ~ "Direkter Vasodilatator",
                            str_detect(drugname,paste(Barbiturate,collapse = '|')) ~ "Barbiturat",
                            TRUE ~ "NaN")) %>%
  group_by(Klasse) # %>%
# mutate(n = 1) %>% summarise(n=sum(n)) %>% arrange(desc(n))



# Medikamente - Normalisieren der Werte #####################################################################



Medikamente_total_norm <-
  Medikamente_total  %>%
  group_by(drugname) %>%
  mutate(mean = mean(drugrate, na.rm = TRUE),sd = sd(drugrate,na.rm = TRUE)) %>%
  mutate(drugrate_norm = (drugrate-mean)/sd) 

ggplot(Medikamente_total_norm, aes(drugrate_norm))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()

ggplot(Medikamente_total_norm, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()

Medikamente_total_norm <-
  Medikamente_total_norm %>%
  mutate(ID = "Med") %>%
  select(ID,patientunitstayid,Klasse,drugname,rel_time,drugrate,drugrate_norm)


# Medikamente - Mannitol  ################################################################
# Prozeduren und deren Zeitpunkte

EICU_treatment <- read.csv(gzfile("eicu/treatment.csv.gz"), stringsAsFactors=FALSE)
EICU_treatment <- as.data.frame(EICU_treatment)
save(EICU_treatment, file = "eicu/treatment.RData")

# START

load(file = "eicu/treatment.RData")
load(file = "eicu_processed_clean/EICU_ICP_ID.RData")

# Prozeduren zum OPs sollten noch gefunden werden
ICP_treatment <- EICU_treatment %>% filter(patientunitstayid %in% ICP_ID) %>% as_tibble()

head(ICP_treatment)

ICP_manni <-
  ICP_treatment %>%
  filter(grepl("manni",treatmentstring)) %>%
  rename(infusiondrugid = treatmentid,
         infusionoffset = treatmentoffset) %>%
  select(infusiondrugid,patientunitstayid,infusionoffset)%>%
  mutate(drugname = "Mannitol",drugrate = 1,
         infusionrate = NA, drugamount = NA, 
         volumeoffluid = NA, patientweight = NA)

# Einzelgabe wird auf 2h Wirkung ausgeweitet

ICP_manni <-
  ICP_manni %>%
  mutate(infusionoffsetend = infusionoffset + 120)%>%
  select(-infusionoffset)%>%
  rename(infusionoffset=infusionoffsetend) %>%
  rbind(ICP_manni) %>% 
  group_by(patientunitstayid,infusiondrugid) %>%
  complete(infusionoffset = seq.int(min(infusionoffset),max(infusionoffset),by = 1)) %>%
  mutate(rel_time = infusionoffset) %>% mutate(rel_time = floor(rel_time)) %>%
  group_by(patientunitstayid,infusiondrugid,rel_time) %>%
  summarise(drugrate = max(drugrate, na.rm = TRUE)) %>%
  filter(!is.infinite(drugrate)) %>%
  ungroup() 


ICP_manni <-
  ICP_manni %>%
  mutate(ID = "Med",Klasse = "Mannitol",drugname = "Mannitol",drugrate_norm =1, OUTLIER = NA) %>%
  select(names(Medikamente_total_norm))

# Einzelnen Patienten plotten
ICP_manni %>%
  filter(patientunitstayid == 251885) %>%
  ggplot(.,aes(rel_time,drugrate))+
  geom_point()

# Hier wird Mannit dann in den gesamt Medikamente Schwung eingebaut
ICP_Med_total <-
  Medikamente_total_norm %>% ungroup() %>% 
  rbind(ICP_manni)

ICP_Med_total <-
  ICP_Med_total %>%
  rename(Maßnahme = drugname, Wert = drugrate, Wert_norm = drugrate_norm) 

ICP_Med_total <-
  ICP_Med_total %>%
  rename(Pat_ID = patientunitstayid)

# Plotten der Klassen
ggplot(ICP_Med_total ,aes(Wert_norm))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()

# Plotten der Zeiten
ggplot(ICP_Med_total ,aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()

# Medikamente - Zeiten anpassen sind schon gemittelt ########################################
# hier werden automatisch auch Duplikate entfernt

# Falls das nicht der Fall ist Ausreißer mit einem selektiven Filter entfernen
ICP_Med_total <-
  ICP_Med_total %>%
  mutate(Wert = ifelse(rel_time < -200, NA, Wert)) %>%
  filter(!is.na(Wert))

# Alle Zeiten sollten in der Nähe von 0 Anfangen
ggplot(ICP_Med_total, aes(rel_time))+
  geom_density()+
  facet_wrap(vars(Klasse),scales = "free")+
  theme_bw()



# Medikamente - exportieren ################################################################
write.csv(ICP_Med_total, file = "eicu_processed_clean/csv/EICU_med.csv",
          row.names=FALSE, na="")

save(ICP_Med_total, file = "eicu_processed_clean/EICU_med.RData")


# Workspace aufräumen:

rm(list=ls())
