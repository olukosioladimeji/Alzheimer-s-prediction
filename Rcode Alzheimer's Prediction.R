#Rcode Ola

install.packages('caret')
install.packages('partykit')
library(RODBC)
library(tcltk)
library(tidyverse)
library(dplyr)
library(lubridate)
library(rpart)
library(party)
library(pROC)
library(caret)
library(partykit)
library(randomForest)
library(glmnet)

getlogin <- function(userName=''){
  wnd <- tktoplevel()
  user <- tclVar(userName)
  passvar <- tclVar('')
  
  tkgrid(tklabel(wnd,text='Username:'))
  passBox <- tkentry(wnd,textvariable = user)
  tkgrid(passBox)
  
  tkgrid(tklabel(wnd,text='Password:'))
  passBox <- tkentry(wnd,textvariable=passvar,show='*')
  tkgrid(passBox)
  
  # Hitting return will also submit password.
  tkbind(passBox, '<Return>', function() tkdestroy(wnd))
  
  # OK button.
  tkgrid(tkbutton(wnd,text='OK',command=function() tkdestroy(wnd)))
  
  # Wait for user to click OK.
  tkwait.window(wnd)
  
  password <- tclvalue(passvar)
  userName <- tclvalue(user)
  
  db <- odbcConnect('PR_SAIL', userName, password)
  return(db)
}

channel <- getlogin()

# All dementia patient and getting the first time they were diagnose of dementia
DEMENTIA_PATIENTS <- sqlQuery(channel,"
              SELECT * FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE EVENT_CD = 'Eu01.' OR EVENT_CD ='Eu010'
              OR EVENT_CD = 'Eu011' OR EVENT_CD = 'Eu025'
              OR EVENT_CD ='Eu012.' OR EVENT_CD = 'Eu013'  
              OR EVENT_CD = 'Eu01y' OR EVENT_CD = 'Eu023'
              OR EVENT_CD= 'Eu01z'  OR  EVENT_CD ='Eu00.'  
              OR EVENT_CD ='Eu000' OR EVENT_CD = 'Eu022'
              OR EVENT_CD ='Eu001'  OR  EVENT_CD ='Eu002' 
              OR EVENT_CD ='Eu00z' OR EVENT_CD = 'Eu021'
              OR EVENT_CD = 'E0010' OR EVENT_CD = 'E0011' 
              OR EVENT_CD = 'E0012.' OR EVENT_CD = 'F1130'
              OR EVENT_CD = 'E0013' OR EVENT_CD ='E001z'   
              OR EVENT_CD = 'E002. 'OR EVENT_CD = 'F1100'
              OR EVENT_CD = 'E0020' OR EVENT_CD = 'E0021'  
              OR EVENT_CD = 'E002z.' OR EVENT_CD = 'F110.'
              OR EVENT_CD = 'E003.' OR EVENT_CD ='E004.'   
              OR EVENT_CD = 'E0040' OR EVENT_CD = 'E041.'
              OR EVENT_CD = 'E0041' OR EVENT_CD = 'E0042'  
              OR EVENT_CD = 'E0043' OR EVENT_CD ='Fy3000'
              OR EVENT_CD = 'E004z' OR EVENT_CD = 'E012.'  
              OR EVENT_CD = 'E02y1'OR EVENT_CD = '1461'
              OR EVENT_CD = 'F1101' OR EVENT_CD ='Eu107' 
              OR EVENT_CD ='Eu843'  OR EVENT_CD = 'F118.'
              OR EVENT_CD = '2B42.' OR EVENT_CD ='2B43.'   
              OR EVENT_CD = '2B44.'OR EVENT_CD = '2B45.'
              OR EVENT_CD = '2B46.' OR EVENT_CD = '2B47.'  
              OR EVENT_CD = '2B48.'OR EVENT_CD ='2B4A.'
              OR EVENT_CD = '2B4Z.' OR EVENT_CD = 'R043.'  
              OR EVENT_CD = 'E2F30'OR EVENT_CD = 'Eu802'
              OR EVENT_CD = 'Eu801' OR EVENT_CD = 'F11y2'")

# WOB converted to age
date_wob<-as.Date(DEMENTIA_PATIENTS$WOB)
last_date<- as.Date('2023-04-01')
age_wob <-round(last_date-date_wob, 0)


DEMENTIA_PATIENTS<- DEMENTIA_PATIENTS%>%
  mutate(AGE=as.numeric(paste0(round(age_wob/365))))%>%
  filter(AGE>=65) # i filter for only age 65 and above


# getting the index case of Dementia(first time diagnose)
DEMENTIA_PATIENTS_INDEX_DIAG <- DEMENTIA_PATIENTS%>%
  arrange(ALF_PE, EVENT_DT)%>%
  group_by(ALF_PE)%>%
  mutate(EVENT_DT_INDEX=row_number())%>%
  filter(EVENT_DT_INDEX==1)

#having a look at the summary of the datasets again
summary(DEMENTIA_PATIENTS_INDEX_DIAG)


# Menopause
MENOPAUSE_PATIENTS <- sqlQuery(channel,"
              SELECT ALF_PE,
              EVENT_CD as MENOPAUSE
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE  EVENT_CD LIKE 'K5A%'OR EVENT_CD= 'C1634' 
              OR EVENT_CD ='C1631' OR EVENT_CD ='1512.'
              OR EVENT_CD LIKE '66U%'OR EVENT_CD LIKE '8B64%' 
              OR EVENT_CD LIKE'7G2A%'
              OR EVENT_CD = 'E2003' OR EVENT_CD ='28G..' OR EVENT_CD = '1AD..'
              OR EVENT_CD LIKE 'K594%' OR EVENT_CD='Fy00.' OR EVENT_CD = '1662.'
              OR EVENT_CD = 'R0084' OR EVENT_CD = '7D1B3' OR EVENT_CD ='1B0..'
              OR EVENT_CD LIKE 'R0071' OR EVENT_CD LIKE 'R031.' 
              OR EVENT_CD LIKE'fk5B.'
              OR EVENT_CD LIKE 'fk5A.'OR EVENT_CD LIKE 'fkF%' 
              OR EVENT_CD LIKE'N063%'
              OR EVENT_CD = 'N3302'")




#  we join table menopause and dementia together using left join
TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG <-left_join(MENOPAUSE_PATIENTS,
                                                DEMENTIA_PATIENTS_INDEX_DIAG,
                                                      by= 'ALF_PE')

TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG <-TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG%>%
  mutate(EVENT_CD_NEW=ifelse(is.na(EVENT_CD),0,1))%>%
  filter(EVENT_CD_NEW==1)

#dropping Na's
TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG<-TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG%>%
  drop_na(ALF_PE,EVENT_YR)

#removing duplicates
TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG<-TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG%>%
  distinct(ALF_PE,.keep_all = TRUE)




#removing irrelevant columns
TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG<-
  TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG[,c(-3,-4,-5,-6,-7,-8,-9,-10,-12,-13,-15,
                                         -16,-17,-18,-19,-20,-21,-23,-24)]

summary(TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG)


# first diagnose of Alzheimer
TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG<-TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG%>%
  mutate(ALZHEIMER_OR_NOT=ifelse(EVENT_CD =='Eu00.'| EVENT_CD =='Eu000'|
                                   EVENT_CD =='Eu001'| EVENT_CD =='Eu002'|
                                   EVENT_CD =='Eu00z'| EVENT_CD =='F110.'|
                                   EVENT_CD =='Fyu3000'|EVENT_CD =='F1100'|
                                   EVENT_CD =='F1100'| EVENT_CD == 'F118.'|
                                   EVENT_CD =='2B42.'| EVENT_CD =='2B43.'| 
                                   EVENT_CD =='2B44.'| EVENT_CD =='2B45.' |
                                   EVENT_CD =='2B46.'| EVENT_CD =='2B47.'|
                                   EVENT_CD =='2B48.'| EVENT_CD =='2B4A.'| 
                                   EVENT_CD =='2B4Z.'| EVENT_CD =='R043.'| 
                                   EVENT_CD =='E2F30'| EVENT_CD == 'Eu802'|
                                   EVENT_CD =='Eu801'|EVENT_CD =='Eu011'|
                                   EVENT_CD =='Eu013'| EVENT_CD == 'F11y2',1,0))


num_AD_DM <- table(TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG$ALZHEIMER_OR_NOT)
print(num_AD_DM) # 10280 cases of Alzheimer and 5706 not Alzheimer




########################   ANALYSIS OF RISK FACTORS

#hypertension
HYPERTENSION_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE,
              COUNT(EVENT_DT) as HYPERTENSION_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE'G20%' OR EVENT_CD LIKE 'G21%' 
              OR EVENT_CD LIKE 'G22%' 
              OR EVENT_CD LIKE'G23%' OR EVENT_CD LIKE 'G24%'
              OR EVENT_CD LIKE 'G25%'
              OR EVENT_CD LIKE 'G26%' OR EVENT_CD ='G28..'
              OR EVENT_CD ='G2y..' OR EVENT_CD = 'G2...')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")
HYPERTENSION_PATIENT <-HYPERTENSION_PATIENT%>%
  drop_na(ALF_PE)

TABLE1 <-left_join(TABLE_DEMENTIA_MENOPAUSE_INDEX_DIAG, 
                   HYPERTENSION_PATIENT, by= 'ALF_PE')


TABLE1<-TABLE1%>%
  mutate(HYPERTENSION_STATUS=replace(HYPERTENSION_STATUS,
                                     is.na(HYPERTENSION_STATUS),0))




# diabetes
DIABETES_PATIENTS <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as DIABETES_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'C135%' OR EVENT_CD LIKE 'C10F%'
              OR EVENT_CD LIKE 'C10E%')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

DIABETES_PATIENTS <-DIABETES_PATIENTS%>%
  drop_na(ALF_PE)

TABLE2 <-left_join(TABLE1, DIABETES_PATIENTS, by= 'ALF_PE')

TABLE2<-TABLE2%>%  mutate(DIABETES_STATUS=replace(DIABETES_STATUS,
                                                  is.na(DIABETES_STATUS),0))



# obesity + fh of obesity
OBESITY_PATIENTS <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT(EVENT_DT) as OBESITY_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'C380%' OR EVENT_CD LIKE '22K%'
              OR EVENT_CD = '1266.')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

OBESITY_PATIENTS <-OBESITY_PATIENTS%>%
  drop_na(ALF_PE)

TABLE3 <-left_join(TABLE2, OBESITY_PATIENTS, by= 'ALF_PE')

TABLE3<-TABLE3%>% mutate(OBESITY_STATUS=replace(OBESITY_STATUS,
                                                is.na(DIABETES_STATUS),0))



# smokers and ex-smoker and family history of smoking
PATIENTS_SMOKES <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as SMOKING_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD = '137R.' OR EVENT_CD = '137S.'
              OR EVENT_CD LIKE '13WF%')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")
PATIENTS_SMOKES<-PATIENTS_SMOKES%>%
  drop_na(ALF_PE)

TABLE4 <-left_join(TABLE3, PATIENTS_SMOKES, by= 'ALF_PE')


TABLE4<-TABLE4%>%  
  mutate(SMOKING_STATUS=replace(SMOKING_STATUS,is.na(SMOKING_STATUS),0))


 

# high cholesterol
HIGH_CHOLESTEROL_PATIENTS <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT(EVENT_DT) as HIGH_CHOLESTEROL_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD = '44P4.' OR EVENT_CD = '44P3.'
              OR EVENT_CD = '1262.' OR EVENT_CD = 'C329.'
              OR EVENT_CD LIKE 'C320%')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

HIGH_CHOLESTEROL_PATIENTS <-HIGH_CHOLESTEROL_PATIENTS%>%
  drop_na(ALF_PE)

TABLE5 <-left_join(TABLE4, HIGH_CHOLESTEROL_PATIENTS, by= 'ALF_PE')

TABLE5<-TABLE5%>%  
  mutate(HIGH_CHOLESTEROL_STATUS=replace(HIGH_CHOLESTEROL_STATUS,
                                         is.na(HIGH_CHOLESTEROL_STATUS),0))



# Heart disease
HEART_DISEASE_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT(EVENT_DT) as HEART_DISEASE_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'G0%' OR EVENT_CD LIKE 'G1%' 
              OR EVENT_CD LIKE 'G3%'
              OR EVENT_CD LIKE 'G5%' OR EVENT_CD LIKE 'GA%'
              OR EVENT_CD LIKE 'G4%'
              OR EVENT_CD = '12C8.' OR EVENT_CD = 'L186.' OR EVENT_CD = 'A98y4'
              OR EVENT_CD = 'L185.' OR EVENT_CD = '8B3K.' OR EVENT_CD = 'ZV173'
              OR EVENT_CD = 'A8600' OR EVENT_CD = 'L1281' OR EVENT_CD = 'L1280')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

HEART_DISEASE_PATIENT <-HEART_DISEASE_PATIENT%>%
  drop_na(ALF_PE)

TABLE6 <-left_join(TABLE5, HEART_DISEASE_PATIENT, by= 'ALF_PE')

TABLE6<-TABLE6%>%
  mutate(HEART_DISEASE_STATUS=replace(HEART_DISEASE_STATUS,
                                      is.na(HEART_DISEASE_STATUS),0))




#GENETICS AND FAMILY HISTORY
# family history of mental disorder and Dementia/Apolipoprotein
FAMILY_HISTORY_MENTAL_DISORDER <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as FAM_HIST_GEN_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'ZV1A%' OR EVENT_CD LIKE '128%'
              OR EVENT_CD = '44xA.' OR EVENT_CD = '44c5.'
              OR EVENT_CD = '4L13.' OR EVENT_CD = 'C3205')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

FAMILY_HISTORY_MENTAL_DISORDER <-FAMILY_HISTORY_MENTAL_DISORDER%>%
  drop_na(ALF_PE)

TABLE7 <-left_join(TABLE6, FAMILY_HISTORY_MENTAL_DISORDER, by= 'ALF_PE')

TABLE7<-TABLE7%>%
  mutate(FAM_HIST_GEN_STATUS=replace(FAM_HIST_GEN_STATUS,
                                     is.na(FAM_HIST_GEN_STATUS),0))



# BRAIN INJURY/HEAD INJURY
BRAIN_HEAD_INJURY_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT(EVENT_DT) as BRAIN_HEAD_INJURY_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'S64%' OR EVENT_CD = 'Q2004'
              OR EVENT_CD = 'SC201' OR EVENT_CD LIKE 'SFO%' OR EVENT_CD ='14J1.'
              OR EVENT_CD='SK18.' OR EVENT_CD = 'SJ70.' OR EVENT_CD = 'SCX..'
              OR EVENT_CD LIKE 'SC20%'
              OR EVENT_CD = '8CAP.' OR EVENT_CD = 'SC37.' OR EVENT_CD = 'S73y1' 
              OR EVENT_CD = 'S73x1' 
              OR EVENT_CD ='8A31.')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

BRAIN_HEAD_INJURY_PATIENT <-BRAIN_HEAD_INJURY_PATIENT%>%
  drop_na(ALF_PE)

TABLE8 <-left_join(TABLE7, BRAIN_HEAD_INJURY_PATIENT, by= 'ALF_PE')

TABLE8<-TABLE8%>%
  mutate(BRAIN_HEAD_INJURY_STATUS=replace(BRAIN_HEAD_INJURY_STATUS,
                                          is.na(BRAIN_HEAD_INJURY_STATUS),0))




# Neurological disorder
NEURO_DISORDER_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT(EVENT_DT) as NEURO_INJURY_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE '666%' OR EVENT_CD LIKE '7P21%'
              OR EVENT_CD LIKE '8A3%'
              OR EVENT_CD LIKE '68A%' OR EVENT_CD LIKE '311%'OR EVENT_CD ='1BZ2.'
              OR EVENT_CD='8H46.' OR EVENT_CD = '38710' OR EVENT_CD = '2816.'
              OR EVENT_CD = '8HVL.'
              OR EVENT_CD = 'ZV7A0' OR EVENT_CD = '8TON.' OR EVENT_CD = 'C10F2'
              OR EVENT_CD = 'C0A0' 
              OR EVENT_CD ='U625.' OR EVENT_CD = 'C10A4')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

NEURO_DISORDER_PATIENT <-NEURO_DISORDER_PATIENT%>%
  drop_na(ALF_PE)

TABLE9 <-left_join(TABLE8, NEURO_DISORDER_PATIENT, by= 'ALF_PE')

TABLE9<-TABLE9%>%
  mutate(NEURO_INJURY_STATUS=replace(NEURO_INJURY_STATUS,
                                     is.na(NEURO_INJURY_STATUS),0))



# Depression
DEPRESSION_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT(EVENT_DT) as DEPRESSION_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD = '1285.'OR EVENT_CD = '1465.'OR EVENT_CD = 'E11Z2'
              OR EVENT_CD = 'Eu32z' OR EVENT_CD ='E2B1.' OR EVENT_CD = 'E135.'
              OR EVENT_CD = 'E204.' OR EVENT_CD = 'E2B0.' OR EVENT_CD = 'IJJ..'
              OR EVENT_CD = 'PE03.' OR EVENT_CD = '9HA0.' OR EVENT_CD = '1B1U.'
              OR EVENT_CD = 'Eu341' OR EVENT_CD = 'E2003' OR EVENT_CD = '12G3.'
              OR EVENT_CD = '1287.' OR EVENT_CD = 'Eu412' OR EVENT_CD = 'Eu32z'
              OR EVENT_CD = 'Q4820' OR EVENT_CD = 'R007z' OR EVENT_CD = 'Eu530'
              OR EVENT_CD = '9H91.' OR EVENT_CD = 'E130.' OR EVENT_CD = 'E115.'
              OR EVENT_CD = 'E113.' OR EVENT_CD = 'Eu313' OR EVENT_CD = '9kQ..')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

DEPRESSION_PATIENT <-DEPRESSION_PATIENT%>%
  drop_na(ALF_PE)

TABLE10 <-left_join(TABLE9, DEPRESSION_PATIENT, by= 'ALF_PE')

TABLE10<-TABLE10%>%
  mutate(DEPRESSION_STATUS=replace(DEPRESSION_STATUS,
                                   is.na(DEPRESSION_STATUS),0))



# Stress
STRESS_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as STRESS_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE '134H%' OR EVENT_CD LIKE '13JM%'
              OR EVENT_CD = '13HT1'
              OR EVENT_CD = '1B1T.' OR EVENT_CD = 'K198.' OR EVENT_CD = '1B1L.'
              OR EVENT_CD LIKE 'E28%' OR EVENT_CD= 'ZV4B2' OR EVENT_CD = 'K586.'
              OR EVENT_CD= 'R00zW' OR EVENT_CD LIKE 'Eu4%' OR EVENT_CD= 'Ry15.')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

STRESS_PATIENT <-STRESS_PATIENT%>%
  drop_na(ALF_PE)

TABLE11 <-left_join(TABLE10, STRESS_PATIENT, by= 'ALF_PE')

TABLE11<-TABLE11%>%
  mutate(STRESS_STATUS=replace(STRESS_STATUS,is.na(STRESS_STATUS),0))



# Chemotheraphy and Radiation Theraphy
CHEM_RAD_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT(EVENT_DT) as CHEM_RAD_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE '8BAD%' OR EVENT_CD LIKE '863%' 
              OR EVENT_CD LIKE '7Q0J%'
              OR EVENT_CD = '8BA5.' OR EVENT_CD = '8HB7.' OR EVENT_CD = '8BAL.'
              OR EVENT_CD = '70462' OR EVENT_CD = '7L161' OR EVENT_CD = '7L193'
              OR EVENT_CD LIKE '65N%' OR EVENT_CD = '7L182'OR EVENT_CD = '8BAa.'
              OR EVENT_CD = 'ZV581' OR EVENT_CD = '8CRC.' OR EVENT_CD = 'ZV662'
              OR EVENT_CD = '7L102' OR EVENT_CD = 'ZV588' OR EVENT_CD = 'Q0078'
              OR EVENT_CD = 'ZV1C3' OR EVENT_CD ='ZV153' OR EVENT_CD ='ZV672'
              OR EVENT_CD ='ZV6B1' OR EVENT_CD ='ZV678')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

CHEM_RAD_PATIENT <-CHEM_RAD_PATIENT%>%
  drop_na(ALF_PE)

TABLE12 <-left_join(TABLE11, CHEM_RAD_PATIENT, by= 'ALF_PE')

TABLE12<-TABLE12%>%
  mutate(CHEM_RAD_STATUS=replace(CHEM_RAD_STATUS,is.na(CHEM_RAD_STATUS),0))




# Urinary track Infection
UTI_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as UTI_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'K190%' OR EVENT_CD = '8CMWE'
              OR EVENT_CD = 'SP07Q')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

UTI_PATIENT <-UTI_PATIENT%>%
  drop_na(ALF_PE)

TABLE13 <-left_join(TABLE12, UTI_PATIENT, by= 'ALF_PE')

TABLE13<-TABLE13%>%
  mutate(UTI_STATUS=replace(UTI_STATUS,is.na(UTI_STATUS),0))



# Lost of Libido
LOST_LIBIDO_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as LOST_LIBIDO_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'E227%' OR EVENT_CD = 'Eu520')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

LOST_LIBIDO_PATIENT <-LOST_LIBIDO_PATIENT%>%
  drop_na(ALF_PE)

TABLE14 <-left_join(TABLE13, LOST_LIBIDO_PATIENT, by= 'ALF_PE')

TABLE14<-TABLE14%>%
  mutate(LOST_LIBIDO_STATUS=replace(LOST_LIBIDO_STATUS,
                                    is.na(LOST_LIBIDO_STATUS),0))





HRT_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as HRT_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD= '8B64.' OR EVENT_CD = '8M40.'OR EVENT_CD ='66U..'
              OR EVENT_CD = '66U7.' OR EVENT_CD = '66U8.'OR EVENT_CD = '66UL.'
              OR EVENT_CD = '66UK.' OR EVENT_CD = '66UJ.' OR EVENT_CD = '66UH.' 
              OR EVENT_CD = '66U9.' OR EVENT_CD = '66UI.')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

HRT_PATIENT <-HRT_PATIENT%>%
  drop_na(ALF_PE)

TABLE15 <-left_join(TABLE14, HRT_PATIENT, by= 'ALF_PE')

TABLE15<-TABLE15%>%
  mutate(HRT_STATUS=replace(HRT_STATUS,is.na(HRT_STATUS),0))



MENTAL_HEALTH_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as MENTAL_HEALTH_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD = 'Euz..' OR EVENT_CD = 'zV4By'
              OR EVENT_CD LIKE 'Eu1%' OR EVENT_CD LIKE '8Hc%' 
              OR EVENT_CD LIKE '8BM0%' OR EVENT_CD = '9NN7.' 
              OR EVENT_CD = '8CQ..' OR EVENT_CD='9NK6.' )
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")

MENTAL_HEALTH_PATIENT <-MENTAL_HEALTH_PATIENT%>%
  drop_na(ALF_PE)

TABLE16 <-left_join(TABLE15, MENTAL_HEALTH_PATIENT, by= 'ALF_PE')

TABLE16<-TABLE16%>%
  mutate(MENTAL_HEALTH_STATUS=replace(MENTAL_HEALTH_STATUS,
                                       is.na(MENTAL_HEALTH_STATUS),0))



FATIGUE_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as FATIGUE_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD = 'R0071' OR EVENT_CD = 'Eu460'
              OR EVENT_CD ='SN26.' OR EVENT_CD LIKE 'F286%'OR EVENT_CD ='N1y1.')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")
FATIGUE_PATIENT <-FATIGUE_PATIENT%>%
  drop_na(ALF_PE)

TABLE17 <-left_join(TABLE16, FATIGUE_PATIENT, by= 'ALF_PE')

TABLE17<-TABLE17%>%
  mutate(FATIGUE_STATUS=replace(FATIGUE_STATUS,is.na(FATIGUE_STATUS),0))



IRR_PAIN_PERIOD_PATIENT <- sqlQuery(channel,"
              SELECT ALF_PE, 
              COUNT (EVENT_DT) as IRR_PAIN_PERIOD_STATUS
              FROM SAIL1448V.WLGP_GP_EVENT_CLEANSED_20230401
              WHERE (EVENT_CD LIKE 'K58%' OR EVENT_CD = 'J035.'
              OR EVENT_CD LIKE 'K59%' OR EVENT_CD = 'J0280'
              OR EVENT_CD LIKE 'J03%%' OR EVENT_CD = 'C3738'
              OR EVENT_CD LIKE 'J026%' OR EVENT_CD = 'C737A'
              OR EVENT_CD LIKE 'F393.' OR EVENT_CD = 'J024'
              OR EVENT_CD LIKE 'F4K01' OR EVENT_CD = 'F2625')
              AND EVENT_DT  BETWEEN '2000-01-01' AND '2015-01-01'
              GROUP BY ALF_PE
              ")
IRR_PAIN_PERIOD_PATIENT <-IRR_PAIN_PERIOD_PATIENT%>%
  drop_na(ALF_PE)


TABLE18 <-left_join(TABLE17, IRR_PAIN_PERIOD_PATIENT , by= 'ALF_PE')

TABLE18<-TABLE18%>%
  mutate(IRR_PAIN_PERIOD_STATUS=replace(IRR_PAIN_PERIOD_STATUS,
                                        is.na(IRR_PAIN_PERIOD_STATUS),0))




# data for ML(we remove those that are not features)
TABLE18<-TABLE18[,c(-1,-2,-3,-4)]

TABLE18<- na.omit(TABLE18)

# ML ALGORITHIM

## writing the table into csv file
write.csv(TABLE18, file='data_alzh_pre.csv', row.names = FALSE)

#read the csv file
data <- read.csv('data_alzh_pre.csv')


head(data)
length(data)

glimpse(data)


# using ggplot to see histogram of the data table
data%>%
  #gather()%>%
  pivot_longer(cols=everything())%>%
  ggplot(aes(value)) +
  facet_wrap(~name,scale="free") + geom_histogram()



# target features is ALZHEIMER_OR_NOT

#We preprocess our data
set.seed(123)

train_indices <- sample(1:nrow(data),0.6*nrow(data))

train_data <- data[train_indices,]
test_data <- data[-train_indices,]

#we now build a decision tree model
train_data$ALZHEIMER_OR_NOT <- as.factor(train_data$ALZHEIMER_OR_NOT)
test_data$ALZHEIMER_OR_NOT <- as.factor(test_data$ALZHEIMER_OR_NOT)

model_dtree <- ctree(ALZHEIMER_OR_NOT~., 
                     data = train_data,
                     control = ctree_control(minsplit = 5,minbucket = 2,
                                             mincriterion = 0.98))

                     
# model_dtree
summary(model_dtree)
plot(model_dtree)


dtree_predictions <- predict(model_dtree, test_data)

confusionMatrix(dtree_predictions,test_data$ALZHEIMER_OR_NOT)

dtree_predicted_prob <- predict(model_dtree, test_data, type="prob")[,"1"]

#lets check model performance
roc_curve_dtree <-roc(test_data$ALZHEIMER_OR_NOT, dtree_predicted_prob)
auc_score<- auc(roc_curve_dtree)

plot(roc_curve_dtree,col='red', 
     main='ROC curve for AD Prediction using Decision tree model')
legend('bottomright',legend=paste('AUC=',round(auc_score,2)),col='red',lty = 0)




#ALGORITHM 2
#Let us see how logistic regression model is doing

data <- read.csv('data_alzh_pre.csv')


set.seed(123)

train_indices <- sample(1:nrow(data),0.6*nrow(data))

train_data <- data[train_indices,]
test_data <- data[-train_indices,]

# we now build a logistic regression model
model_logistic <-glm(ALZHEIMER_OR_NOT~., data = train_data)
summary(model_logistic)

predictions_glm <- predict(model_logistic, test_data)

#confusion_matrix<-table(predictions_glm,test_data$ALZHEIMER_OR_NOT)
#summary(confusion_matrix)
# confusion matrix for GLM was done using different threshold and 
# the one with the best accuracy was choosen

#lets check model performance
roc_curve_glm <-roc(test_data$ALZHEIMER_OR_NOT,predictions_glm)
auc_score<- auc(roc_curve_glm )

plot(roc_curve_glm ,col='blue',
     main='ROC curve for AD Prediction using GLM')
legend('bottomright',legend=paste('AUC=',round(auc_score,2)),col='blue',lty = 0)




#ALGORITHM 3
#Let us see how random forest is doing

data <- read.csv('data_alzh_pre.csv')


set.seed(123)

train_indices <- sample(1:nrow(data),0.6*nrow(data))

train_data <- data[train_indices,]
test_data <- data[-train_indices,]


train_data$ALZHEIMER_OR_NOT <- as.factor(train_data$ALZHEIMER_OR_NOT)
test_data$ALZHEIMER_OR_NOT <- as.factor(test_data$ALZHEIMER_OR_NOT)



# we now have randomforest model
model_ran_forest <- randomForest(ALZHEIMER_OR_NOT ~., data = train_data)

random_forest_predictions <- predict(model_ran_forest, test_data)

confusionMatrix(random_forest_predictions, test_data$ALZHEIMER_OR_NOT)

random_forest_predicted_prob <- predict(model_ran_forest, test_data, 
                                        type="prob")[,"1"]


#lets check model performance
roc_curve_random_forest <-roc(test_data$ALZHEIMER_OR_NOT,
                              random_forest_predicted_prob)
auc_score<- auc(roc_curve_random_forest)

plot(roc_curve_random_forest,col='green',
     main='ROC curve for AD Prediction using Random Forest model')
legend('bottomright',legend=paste('AUC=',round(auc_score,2)),col='green',lty =0)



# I now plot the three ROC curve together
roc_curve_dtree <-roc(test_data$ALZHEIMER_OR_NOT, dtree_predicted_prob)
roc_curve_glm <-roc(test_data$ALZHEIMER_OR_NOT,predictions_glm)
roc_curve_random_forest <-roc(test_data$ALZHEIMER_OR_NOT
                              ,random_forest_predicted_prob)

plot(roc_curve_dtree,col="red")
plot(roc_curve_glm,col="blue", add=TRUE)
plot(roc_curve_random_forest,col="green", add=TRUE)
legend('bottomright',legend=c('ROC_DTREE', 'ROC_GLM','ROC_RF'),
       col=c('red','blue','green'),lty = 1)



# important features for random forest
important_features <- importance(model_ran_forest)
print(important_features)


# hyperparameter tuning
data <- read.csv('data_alzh_pre.csv')


data$ALZHEIMER_OR_NOT <- as.factor(data$ALZHEIMER_OR_NOT)

set.seed(123)

train_indices <- sample(1:nrow(data),0.6*nrow(data))

train_data <- data[train_indices,]
test_data <- data[-train_indices,]

x_train = subset(train_data, select=-c(ALZHEIMER_OR_NOT))
y_train = train_data$ALZHEIMER_OR_NOT

x_test= subset(test_data,select=-c(ALZHEIMER_OR_NOT))
y_test = test_data$ALZHEIMER_OR_NOT


# Using five fold Validation
control <- trainControl(method = "cv", number=5, verboseIter = FALSE,
                        classProbs = TRUE)

randomForest_new <-train(x_train, y_train,method="rf",trcontrol=control,
                         tuneGrid=expand.grid(
                           mtry=floor(sqrt(length(x_train)))
                         ))

print(randomForest_new)
confusionMatrix(randomForest_new,norm = "none")
plot(varImp(randomForest_new))




































