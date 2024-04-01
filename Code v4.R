rm(list = ls())

#install.packages("dplyr")
#install.packages("readxl")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("tidyr")
#install.packages("psych")
#install.packages("janitor")
#install.packages("lubridate")
#install.packages("gridExtra")
#install.packages("scales")
#install.packages("stringr")
#install.packages("RColorBrewer")


library(dplyr)
library(readxl)
library(ggplot2)
library(scales)
library(tidyr)
library(psych)
library(janitor)
library(lubridate)
library(gridExtra)
library(scales)
library(stringr)
library(RColorBrewer)


setwd("C:/Users/CAF4577/OneDrive - CNA/Documents/Data analytics/Data/")

#----------------------------------------------------------------------------------------------------

###################################
########## DATA UPLOADS ##########
###################################


#APRA uploads
##CDMP uploads
apra_cdmp_insured_persons <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "CDMP by age",
    range = "A3:v67"
  )

apra_cdmp_programs <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "CDMP by age",
    range = "A201:v265"
  )

apra_cdmp_benefits <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "CDMP by age",
    range = "A795:v859"
  )

#This is the total amount charged by providers before medicare recovery
apra_cdmp_fees_excl_medicare <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "CDMP by age",
    range = "A993:v1057"
  )


##PHI uploads
apra_phi_ht_insured_persons <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "HT by age",
    range = "A3:V106"
  )

apra_phi_gt_insured_persons <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "GT Ancillary by age",
    range = "A3:V86"
  )

apra_phi_ht_episodes <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "HT by age",
    range = "A318:V421"
  )

apra_phi_ht_medical_benefits <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "HT by age",
    range = "A1146:V1210"
  )

apra_phi_ht_prostheses_benefits <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "HT by age",
    range = "A1344:V1408"
  )

apra_phi_ht_hospaccom_benefits <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "HT by age",
    range = "A948:V1012"
  )


apra_phi_ht_total_benefits <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "HT by age",
    range = "A1542:V1645"
  )


apra_phi_gt_services <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "GT Ancillary by age",
    range = "A258:V341"
  )

apra_phi_gt_benefits <-
  read_xlsx(
    "Quarterly Private Health Insurance Benefit Trends March 2023.xlsx",
    sheet = "GT Ancillary by age",
    range = "A513:V596"
  )

#HT and GT trends? How can I get either or? Just do them both on the same chart (SEU's? No, do insured persons so you can compare to population)

#ABS population data upload
abs_population <-
  read_xlsx("ABS Population data.xlsx", sheet = "Data1")

#Medicare incidence trend info
chronic_hosps_raw <-
  read_xlsx(
    "aihw-phc-11-Supplementary-codes-for-chronic-conditions-data-tables.xlsx",
    sheet = "Table S5",
    skip = 9
  )

disease_burden_data <-
  read_xlsx("AIHW-BOD-37-ABDS-2022-disease-burden-data-tables.xlsx",
            sheet = "1C_Disease")


#Assessing data quality
glimpse(apra_phi_gt_insured_persons)
glimpse(apra_phi_gt_insured_persons)
glimpse(apra_cdmp_benefits) #All APRA data is wide, need to convert to long for plotting/analysis

glimpse(disease_burden_data) #data is long, useful. yll = year lives lost, yld = year lost to disability. yld is more appropriate for my analysis since I'm trying to identify instance of treatable disease which could impact PHI membership
glimpse(abs_population) #names are messy, I just need column 1, 13

#Converting APRA data to long and amalgamating for analysis
apra_mship_long_ht <- apra_phi_ht_insured_persons %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="HT insured persons")

apra_mship_long_gt <- apra_phi_gt_insured_persons %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="GT insured persons")

apra_episodes_long_ht <- apra_phi_ht_episodes %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="HT episodes")

apra_benefits_long_ht_tot <- apra_phi_ht_total_benefits %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="HT total benefits")

apra_benefits_long_ht_med <- apra_phi_ht_medical_benefits %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="HT medical benefits")

apra_benefits_long_ht_pros <- apra_phi_ht_prostheses_benefits %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="HT prostheses benefits")

apra_benefits_long_ht_hospaccom <- apra_phi_ht_hospaccom_benefits %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="HT hospital accommodation benefits")

apra_benefits_long_gt <- apra_phi_gt_benefits %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="GT benefits")

apra_services_long_gt <- apra_phi_gt_services %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="GT services")


#----------------------------------------------------------------------------------------------------

###################################
########## DATA CLEANING ##########
###################################

apra_master <- rbind(apra_mship_long_ht, 
                     apra_mship_long_gt,
                     apra_services_long_gt,
                     apra_episodes_long_ht,
                     apra_benefits_long_ht_tot,
                     apra_benefits_long_ht_med,
                     apra_benefits_long_ht_pros,
                     apra_benefits_long_ht_hospaccom,
                     apra_benefits_long_gt
                     )
apra_master$QUARTER <- as.Date(apra_master$QUARTER)


#Converting CDMP datasets to long and amalgamating for analysis
apra_cdmp_insured_persons_long <- apra_cdmp_insured_persons %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="CDMP insured persons")

apra_cdmp_programs_long <- apra_cdmp_programs %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="CDMP programs")

apra_cdmp_benefits_long <- apra_cdmp_benefits %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="CDMP benefits")

apra_cdmp_fees_excl_medicare_long <- apra_cdmp_fees_excl_medicare %>%
  gather(key=age, value, -QUARTER) %>%
  mutate(category="CDMP fees excl. medicare")

cdmp_master <- rbind(apra_cdmp_insured_persons_long,
                     apra_cdmp_programs_long,
                     apra_cdmp_benefits_long,
                     apra_cdmp_fees_excl_medicare_long)

#Cleaning remaining datasets
aus_population <- abs_population %>% 
  select(1, 12) %>% slice(10:n()) %>% 
  rename(QUARTER = '...1', pop_est = 'Estimated Resident Population (ERP) ;  Australia ;')

aus_population <- aus_population %>% 
  mutate(pop_est = as.numeric(pop_est)) %>% 
  mutate(pop_est = 1000 * pop_est)
  
aus_population <- aus_population %>%
  mutate(QUARTER=as.numeric(QUARTER)) %>%
  mutate(QUARTER = as.Date(QUARTER, origin = "1899/12/30")) %>%
  mutate(QUARTER=as.Date(QUARTER, format="%y-%m-%d"))
  
disease_burden <-
  disease_burden_data %>% filter(sex == "Persons") %>% select(-4, -14)
disease_burden_long <- disease_burden %>%
  gather(key=measurement_type, value, -data_year, -disease_group, -disease) %>%
  group_by(data_year, disease_group, measurement_type) %>%
  summarize(value=sum(value))


chronic_hosps <-
  chronic_hosps_raw %>% select(1, 83:93) %>% rename(chronic_conditions_deep = 'Supplementary code chronic conditions')
chronic_hosps <-
  chronic_hosps %>% slice(-1,-nrow(chronic_hosps_raw)) %>% select(1, 4, 6, 8, 10, 12) %>% rename(
    '2016' = "% of total hospitalisations...85",
    '2017' = "% of total hospitalisations...87",
    '2018' = "% of total hospitalisations...89",
    '2019' = "% of total hospitalisations...91",
    '2020' = "% of total hospitalisations...93"
  )

chronic_conditions_map <-
  read_xlsx("chronic disease mapping.xlsx") #Import mapping - sourced from my local GP
chronic_hosps <-
  left_join(chronic_hosps,
            chronic_conditions_map,
            join_by(chronic_conditions_deep)) #Join mapping onto AIHW data

chronic_hosps_long <-
  chronic_hosps %>% pivot_longer(
    cols = c('2016', '2017', '2018', '2019', '2020'),
    names_to = "year",
    values_to = "prop_hosps"
  ) #Clean and select table of interest
chronic_hosps_long[is.na(chronic_hosps_long)] <-  0 #Replace NA's with 0
chronic_hosps_summ <-
  chronic_hosps_long %>% 
  group_by(year, chronic_disease_summary) %>%
  summarise(prop_hosps = sum(prop_hosps)) #Group variables to simplify dataset


#clean up workspace
rm(list= setdiff(ls(), c("apra_master","cdmp_master","aus_population", "disease_burden_long","chronic_hosps_summ")))

#----------------------------------------------------------------------------------------------------

##############################################################
########## DATA ADJUSTMENTS - POST INITIAL ANALYSIS ##########
##############################################################


#ancilliary data adjustments
#Grouping ages by 25-35, 35-50, 50-65, 65+
apra_master <- apra_master %>%
  filter(!(age %in% c("0-4","5-9","10-14","15-19","20-24"))) %>% #excluding anyone who is dependent on a parents policy - standalone policies under 25 are negligible
  mutate(
    age_group=case_when(
      age %in% c("25-29","30-34") ~ "25-34",
      age %in% c("35-39","40-44","45-49") ~ "35-49",
      age %in% c("50-54","55-59","60-64") ~ "50-64",
      age %in% c("Total") ~ "Total",
      age %in% c("65-69","70-74","75-79","80-84","85-89","90-94","95+") ~ "65+"
    )
  )
apra_master$age_group <- factor(apra_master$age_group)
summary(apra_master$age_group)
apra_master$age <- factor(apra_master$age)
glimpse(apra_master)

apra_master_summary <- apra_master %>% 
  group_by(QUARTER, age_group, category) %>%
  summarize(value = sum(value))
glimpse(apra_master_summary)
apra_master_summary$value <- as.numeric(apra_master_summary$value)

cdmp_master <- cdmp_master %>%
  filter(!(age %in% c("0-4","5-9","10-14","15-19","20-24"))) %>% #excluding anyone who is dependent on a parents policy - standalone policies under 25 are negligible
  mutate(
    age_group=case_when(
      age %in% c("25-29","30-34") ~ "25-34",
      age %in% c("35-39","40-44","45-49") ~ "35-49",
      age %in% c("50-54","55-59","60-64") ~ "50-64",
      age %in% c("Total") ~ "Total",
      age %in% c("65-69","70-74","75-79","80-84","85-89","90-94","95+") ~ "65+"
    )
  )
cdmp_master$age_group <- factor(cdmp_master$age_group)
summary(cdmp_master$age_group)
cdmp_master$age <- factor(cdmp_master$age)
glimpse(cdmp_master)

cdmp_master_summary <- cdmp_master %>% 
  group_by(QUARTER, age_group, category) %>%
  summarize(value = sum(value))
glimpse(cdmp_master_summary)
cdmp_master_summary$value <- as.numeric(cdmp_master_summary$value)

disease_burden_long_summary <- disease_burden_long %>%
  filter(measurement_type == "yld_asr", disease_group %in% c("Cardiovascular diseases","Mental and substance use disorders", "Endocrine disorders", "Respiratory diseases")) %>%
  group_by(data_year, disease_group) %>%
  summarize(value=sum(value)) %>%
  rename(year=data_year) %>%
  mutate(category = "Years lived with disability - age standardised rate")

disease_burden_long_summary$year <- as.numeric(disease_burden_long_summary$year)

chronic_hosps_summ_long <- chronic_hosps_summ %>%
  filter(!(chronic_disease_summary %in% c("Renal medicine","Psychiatry","Gastroenterology","Genetic medicine")))%>%
  rename(value=prop_hosps, disease_group=chronic_disease_summary) %>%
  mutate(category="Proportion of total hospitalisations")

chronic_hosps_summ_long$year <- as.numeric(chronic_hosps_summ_long$year)  

#----------------------------------------------------------------------------------------------------

#########################################
########## DATA INVESTIGATIONS ##########
#########################################


#Investigating APRA data
dim(apra_master)
class(apra_master)
glimpse(apra_master) #I want QUARTER to be a date variable
apra_master$QUARTER <- as.Date(apra_master$QUARTER)
glimpse(apra_master) #happy with formats
table(apra_master$category) #HT latest info from 2007, GT latest info from 2002

apra_master %>% filter(age!="Total", (category %in% c("GT benefits", "HT total benefits"))) %>%
ggplot(aes(QUARTER, value, color=age)) +
  geom_line() +
  facet_wrap(.~ category)
#Seasonal movement in GT benefits due to annual reset of limits on a "use it or lose it" basis
#Seasonal movement in HT benefits as elective surgeries postponed during big holiday periods e.g. christmas
#Seasonal noise accentuated during the COVID periods as restrictions on elective surgery and lockdowns limiting access to healthcare impacted HT/GT claiming

apra_master %>% filter(age!="Total", !(category %in% c("HT total benefits", "HT hospital accommodation benefits", "GT benefits", "HT prostheses benefits", "HT medical benefits"))) %>%
  ggplot(aes(QUARTER, value, color=age)) +
  geom_line() +
  facet_wrap(.~ category)
#Similar seasonal movement observed
#Difficult to identify age based trends so grouping the data - see above

apra_master_summary %>% filter(age_group!="Total", (category %in% c("GT benefits", "HT medical benefits", "HT hospital accommodation benefits", "HT prostheses benefits"))) %>%
  ggplot(aes(QUARTER, value, color=age_group)) +
  geom_line() +
  facet_wrap(.~ category)
#All HT benefits primarily driven by older groups - particularly hospital accomodation
#GT benefits driven by a mixture of all ages 35+, pregnancy claiming comes into play here


apra_master %>% filter(age=="Total", category %in% c("HT insured persons", "GT insured persons")) %>%
  ggplot(aes(QUARTER, value)) +
  geom_line() +
  facet_wrap(.~ category)
#insured persons was flattening/decreasing until 2020 then significant increase 

apra_master_summary %>% filter(age_group!="Total", category %in% c("HT insured persons", "GT insured persons")) %>%
  ggplot(aes(QUARTER, value, color=age_group)) +
  geom_line() +
  facet_wrap(.~ category)
#very aggressive increase in 65+ membership for both GT and HT
#flattening mship trend for 35-49 & 50-64 with resurgence post-covid
#declining membership 25-34 and resurgence post covid

#proportion of benefits stacked by age
apra_master_summary %>% filter(age_group!="Total", category %in% c("GT benefits", "HT total benefits","HT medical benefits", "HT hospital accommodation benefits", "HT prostheses benefits")) %>%
  ggplot(aes(QUARTER, value, fill=age_group)) +
  geom_col() +
  facet_wrap(.~category)
#over 65's claim a much larger proportion of total HT benefits than GT benefits
#GT benefits are spread more evenly across age with the excpetion og 25-34's not claiming a lot

apra_master_summary %>% filter(age_group!="Total", (category %in% c("GT benefits", "HT total benefits","HT insured persons", "GT insured persons"))) %>%
  ggplot(aes(QUARTER, value, fill=age_group)) +
  geom_col() +
  facet_wrap(.~category, scales="free")
#Each age group claims their fair share of GT benefits based on their membership share
#This is with the exception of 25-34 who are claiming a lower proportion than their membership share
#HT benefits are disproportionately swayed to 65+ but membership share is braodly even across age groups
#Trend of proportional benefit/mship share between ages appears stable although chart below will confirm

apra_master_summary %>% filter(age_group!="Total", (category %in% c("GT benefits", "HT total benefits","HT insured persons", "GT insured persons"))) %>%
  ggplot(aes(QUARTER, value, fill=age_group)) +
  geom_col(position="fill") +
  facet_wrap(.~category, scales="free")
#65+ are becoming a greater proportion of mship in both GT and HT
#65+ are claiming more than 60% of HT benefits but represent less than 30% of mship
#This trend has been increasing since 2000


#Investigating CDMP data
dim(cdmp_master)
class(cdmp_master)
glimpse(cdmp_master) #I want QUARTER to be a date variable, and age to be factor
cdmp_master$QUARTER <- as.Date(cdmp_master$QUARTER)
cdmp_master$age <- factor(cdmp_master$age)
glimpse(apra_master) #happy with formats
table(cdmp_master$category) #CDMP latest info from 2007

cdmp_master %>% filter(age=="Total") %>%
  ggplot(aes(QUARTER, value)) +
  geom_line() +
  facet_wrap(.~category, scales="free")
#Big lump in CDMP programs in 2019? Whats driving this? Legislation or increased incidence of chronic diseases?
#CDMP insured persons increased from 2020
#Although benefits/fees excl. medicare have decreased since 2010-2020 and stayed flat since COVID

#using age_group because without grouping is too noisy
cdmp_master_summary %>% filter(age_group!="Total", category %in% c("CDMP benefits", "CDMP fees excl. medicare")) %>%
  ggplot(aes(QUARTER, value, color=age_group)) +
  geom_line() +
  facet_wrap(.~category, scales="free")
#Recent benefit spikes driven by 65+
#majority of benefits billed to 65+, progressively less with age

cdmp_master_summary %>% filter(age_group!="Total", !(category %in% c("CDMP benefits", "CDMP fees excl. medicare"))) %>%
  ggplot(aes(QUARTER, value, color=age_group)) +
  geom_line() +
  geom_point() +
  facet_wrap(.~category, scales="free")
#2019 program spike driven by all ages, is this a data issue or is there a legislative reasoning behind this?
#insured persons post-covid increasing for 35-49 & 50-64
#relatively flat post covid for 24-34


#Investigating population data
dim(aus_population)
class(aus_population)
glimpse(aus_population) #happy with formats
summary(aus_population$QUARTER) #data available from, 1981 but only until Dec 2022
summary(aus_population$pop_est) #min/max appear aligned with a-priori estimates of Aus population growth

ggplot(data=aus_population, aes(x=QUARTER, y=pop_est)) +
  geom_line()
#COVID dip in population growth because of restricted migration/birth rates

#Investigating disease data from AIHW - deciding which to use
#Investigating proportion of hospitalisations
summary(chronic_hosps_summ)
chronic_hosps_summ %>%
  ggplot(aes(chronic_disease_summary, prop_hosps))+
  geom_boxplot()
#appears to be the most volatility in metabolic

ggplot(chronic_hosps_summ, aes(x=year, y=prop_hosps, fill=chronic_disease_summary)) +
  geom_col()+
  geom_text(aes(label=prop_hosps), position=position_stack(vjust=0.5))

#exclude diseases wtih 0% hospitalisation
chronic_hosps_summ %>% filter(!(chronic_disease_summary %in% c("Renal medicine","Psychiatry","Gastroenterology","Genetic medicine","Mental health")))%>%
  ggplot(aes(x=year, y=prop_hosps, fill=chronic_disease_summary)) +
  geom_col()+
  geom_text(aes(label=prop_hosps), position=position_stack(vjust=0.5))
#This is showing that mental health is not resulting in hospitalisations, but other chronic diseases (mainly cardiovasc, metabolic) are increasingly resulting in hospitalisations

ggplot(chronic_hosps_summ, aes(x=year, y=prop_hosps, fill=chronic_disease_summary)) +
  geom_col(position="dodge")
#increasing trends driven by metabolic (obesity)
ggplot(chronic_hosps_summ, aes(x=year, y=prop_hosps, fill=chronic_disease_summary)) +
  geom_col(position="fill")

chronic_hosps_summ %>% filter(chronic_disease_summary %in% c("Cardiovascular","Endocrinology","Mental Health","Metabolic","Pulmonary","Rheumatology")) %>%
  ggplot(aes(x=year, y=prop_hosps, fill=chronic_disease_summary)) +
  geom_col(position="fill")

#invetigating disease burden
summary(disease_burden_long) #yll is total years lost, age standardised (asr) is more appropriate to control for their study sample
unique(disease_burden_long$data_year) #data is not collected every year, gap between 2018 - 2022 during COVID

ggplot(disease_burden_long, aes(data_year, value, color=disease_group)) +
  geom_line() +
  facet_wrap(.~measurement_type, scale="free")
#I believe yld_asr is the most appropriate to use as it represents living people impacted by a disease, and therefore able to use CDMP's and PHI.
#Death burden (yll) represents the prevalence of the disease in the whole population, not particularly relevant to this study
#ASR controls for the ages of the study population used by the AIHW

disease_burden_long %>% filter(measurement_type == "yld_asr") %>%
  ggplot(aes(data_year, value, color=disease_group)) +
  geom_line() +
  facet_wrap(.~disease_group)

unique(disease_burden_long$disease_group)

#reducing to the chronic diseases of interest to PHI
disease_burden_long %>% filter(measurement_type=="yld_asr", disease_group %in% c("Cardiovascular diseases","Mental and substance use disorders", "Endocrine disorders", "Respiratory diseases")) %>%
  ggplot(aes(data_year, value, color=disease_group)) +
  geom_line()+
  geom_point()
#This is telling me that mental health and substance abuse is increasing whilst other chronic disease impacts have been stable
#Hence an opportunity for CDMP's to add value in mental health in particualar - but also the big ones (cardiovasc, diabetes (endocrine), respiratory diseases)
#In contrast to the hosps chart - this is telling me the prevalence across the country whereas the hosps chart is indicating the severity resulting in hospitalisation

#Deciding to use both hospitalisations and disease burden



#----------------------------------------------------------------------------------------------------

##############################################
########## PREPARING MASTER DATASET ##########
##############################################

#Combining: apra_master, cdmp_master, aus_population, disease_burden, chronic_hosps. Be careful of dates and structures
#Keeping AIHW data separate from APRA/ABS data

aihw_master <-
  rbind(chronic_hosps_summ_long, 
        disease_burden_long_summary)

phi_master <-
  rbind(apra_master_summary, 
        cdmp_master_summary)
phi_master$QUARTER <- as.Date(phi_master$QUARTER)


#clean up workspace
rm(list= setdiff(ls(), c("phi_master","aihw_master", "aus_population")))

#widen phi_master to calculate additional frequency, severity, correlation measures
phi_master_wide <- phi_master %>%
  pivot_wider(names_from=category, values_from=value) %>%
  mutate(`HT episodes per insured person` = `HT episodes` / `HT insured persons`,
         `HT average total benefits per insured person` = `HT total benefits` / `HT insured persons`,
         `HT average hospital accommodation benefits per insured person` = `HT hospital accommodation benefits` / `HT insured persons`,
         `HT average medical benefits per insured person` = `HT medical benefits` / `HT insured persons`,
         `HT average prostheses benefits per insured person` = `HT prostheses benefits` / `HT insured persons`,
         `GT average benefits per insured person` = `GT benefits` / `GT insured persons`,
         `GT average services per insured person` = `GT services` / `GT insured persons`,
         `CDMP average benefits per insured person` = `CDMP benefits` / `CDMP insured persons`,
         `CDMP average benefits per program` = `CDMP benefits` / `CDMP programs`,
         `CDMP average programs per insured person` = `CDMP programs` / `CDMP insured persons`)
phi_master_wide$QUARTER <- as.Date(phi_master_wide$QUARTER)

#attaching population estimates and calculating as proportion of population
phi_master_wide <- left_join(phi_master_wide, aus_population, by=join_by(QUARTER))
phi_master_wide <- phi_master_wide %>%
  mutate(`HT mship as prop of Aus pop` = `HT insured persons`/`pop_est`,
         `GT mship as prop of Aus pop` = `GT insured persons`/`pop_est`,
         `CDMP mship as prop of Aus pop` = `CDMP insured persons`/`pop_est`,)

#Defining pre/post COVID periods
phi_master_wide %>% filter(age_group=="Total") %>%
  select(QUARTER, `HT mship as prop of Aus pop`) %>%
  ggplot(aes(QUARTER, `HT mship as prop of Aus pop`)) +
  geom_line() +
  geom_point() +
  scale_x_date(breaks = date_breaks("1 year"),
               date_minor_breaks = "3 months",
               limits=as.Date(c("2010-12-31", "2023-31-03")))
#the spiral starts somewhere between June 2014 and June 2015
#the spiral ends somewhere between June 2019 and June 2021

#Finding the beginning of the death spiral
start_date <- as.Date("2013-06-30")
end_date <- as.Date("2018-06-30")
subset <- phi_master_wide %>%
  filter(age_group=="Total") %>%
  select(QUARTER, `HT mship as prop of Aus pop`) %>%
  subset(QUARTER >= start_date & QUARTER <= end_date)
index <- which.max(subset$`HT mship as prop of Aus pop`)
spiral_start_date <- subset$QUARTER[index]

#Finding the end of the death spiral
start_date <- as.Date("2019-06-30")
end_date <- as.Date("2021-06-30")
subset <- phi_master_wide %>%
  filter(age_group=="Total") %>%
  select(QUARTER, `HT mship as prop of Aus pop`) %>%
  subset(QUARTER >= start_date & QUARTER <= end_date)
index <- which.min(subset$`HT mship as prop of Aus pop`)
spiral_end_date <- subset$QUARTER[index]

#Segmenting data for further analysis of pre and post death spiral characteristics
phi_master_wide <- phi_master_wide %>%
  mutate(spiral_indicator = case_when(
    QUARTER>=spiral_end_date ~ "Post death spiral",
    QUARTER < spiral_end_date & QUARTER >=spiral_start_date ~ "During death spiral",
    TRUE ~ "Pre death spiral"
  ))

#creating long data for plots
phi_master <- phi_master_wide %>%
  select(-26) %>%
  gather(key=category, value=value, -QUARTER, -age_group, -spiral_indicator)





#----------------------------------------------------------------------------------------------------

###########################################
########## ANALYSING MASTER DATA ##########
###########################################

       
#analysis of correlations
mship_corr_table <- phi_master_wide %>%
  filter(age_group=="Total") %>%
  select(1:2,4,7,14,15)
mship_corr <- pairs(mship_corr_table[, 3:6])
#Suggests that there is some kind of relationship between CDMP insured members and PHI membership
#CDMP programs appear to have a limited impact on PHI membership with some outliers present

bens_corr_table <- phi_master_wide %>%
  filter(age_group=="Total") %>%
  select(1:2,5,6,12,13)
  #select(1:2,5,6,9,10,11,12,13)
bens_corr <- pairs(bens_corr_table[,3:6])
#Implies a loose connection between CDMP benefits and HT total + GT benefits - worth exploring with a regression


#age, average premium, claim frequency, benefit size, membership by age pre/post COVID

#Age - pre/post death spiral
pre_ds_mship <- phi_master_wide %>%
  #filter(spiral_indicator=="During death spiral", age_group!="Total") %>%
  filter(QUARTER==spiral_end_date, age_group!="Total") %>%
  select(1,2,4,7,14) %>%
  rename(pre_ht=`HT insured persons`,pre_gt=`GT insured persons`,pre_cdmp=`CDMP insured persons`) %>%
  group_by(age_group) %>%
  summarize(pre_ht=sum(pre_ht),pre_gt=sum(pre_gt),pre_cdmp=sum(pre_cdmp))

post_ds_mship <- phi_master_wide %>%
  #filter(spiral_indicator=="During death spiral", age_group!="Total") %>%
  filter(QUARTER==as.Date("2023-03-31"), age_group!="Total") %>%
  select(1,2,4,7,14) %>%
  rename(post_ht=`HT insured persons`,post_gt=`GT insured persons`,post_cdmp=`CDMP insured persons`) %>%
  group_by(age_group) %>%
  summarize(post_ht=sum(post_ht),post_gt=sum(post_gt),post_cdmp=sum(post_cdmp))

#Code to display the data as a proportion
age_plot_data <- data.frame(
  age_group = unique(pre_ds_mship$age_group),
  pre_ht = prop.table(pre_ds_mship$pre_ht),
  pre_gt = prop.table(pre_ds_mship$pre_gt),
  pre_cdmp = prop.table(pre_ds_mship$pre_cdmp),
  post_ht = prop.table(post_ds_mship$post_ht),
  post_gt = prop.table(post_ds_mship$post_gt),
  post_cdmp = prop.table(post_ds_mship$post_cdmp)
)

#I've done snapshots here - negligible difference if I take the average. Consider if this or a fully stacked column chart is best
ggplot(age_plot_data, aes(x=age_group)) +
  geom_bar(aes(y=post_ht), stat="identity",fill="blue", position="identity", width=0.4) +
  geom_bar(aes(y=-pre_ht), stat="identity",fill="red", position="identity", width=0.4) +
  coord_flip() +
  labs(
    title="Age Pyramid",
    x="Age group",
    y="`HT insured persons",
    fill="Pre/Post death spiral"
  ) +
  scale_y_continuous(labels=abs, breaks = seq(-0.35, 0.35, by=0.05), limits = c(-0.35, 0.35)) +
  geom_text(aes(y=post_ht, label=percent(pre_ht, scale=100, accuracy=0.1)), 
            position=position_dodge(width=0.4),
            vjust=0,
            size=4) +
  geom_text(aes(y=pre_ht, label=percent(post_ht, scale=100, accuracy=0.1)), 
           position=position_dodge(width=0.4),
            vjust=0,
            hjust=6,
            size=4)
#This shows less proportion of 65+ and greater proportion of younger members post DS

phi_master %>% filter(category %in% c("HT insured persons", "GT insured persons", "CDMP insured persons"), !(spiral_indicator %in% c("Pre death spiral")) ) %>%
  ggplot(aes(QUARTER, value, fill=age_group)) +
    geom_bar(stat="identity", position = "fill")+
    facet_grid(category~spiral_indicator, scales="free")
#not showing a material change in age mix for insured persons anywhere


#Claim frequency - pre/post death spiral
phi_master %>% filter(age_group=="Total",  category %in% c("HT episodes per insured person", "GT average services per insured person", "CDMP average programs per insured person")) %>%
  ggplot(aes(QUARTER, value, color=spiral_indicator))+
  geom_line()+
  facet_grid(category~., scales="free")
#Very noisy due to impacts of COVID lockdowns and restrictions on elective surgery - but there is evidence to suggest that claim frequency is stabilising across HT, GT, and CDMP implying decreased death spiral pressures 

#HT ACS - pre/post death spiral
phi_master %>% filter(age_group=="Total", category %in% c("HT average total benefits per insured person", "HT average hospital accommodation benefits per insured person", "HT average medical benefits per insured person", "HT average prostheses benefits per insured person")) %>%
  ggplot(aes(QUARTER, value, color=spiral_indicator))+
  geom_line()+
  facet_grid(category~., scales="free")
#Similarly appears that ACS is stabilising implying death spiral pressures are subsiding - remaining pressures mainly driven by hospital accommodation

#GT/CDMP ACS - pre/post death spiral
phi_master %>% filter(age_group=="Total", category %in% c("GT average benefits per insured person", "CDMP average benefits per insured person", "CDMP average benefits per program")) %>%
  ggplot(aes(QUARTER, value, color=spiral_indicator))+
  geom_line()+
  facet_grid(category~., scales="free")
#CDMP ACS appears to have decreased/on a downwards trajectory
#GT ACS does not appear to be stabilising - still aggressively increasing.


#----------------------------------------------------------------------------------------------------

##########################################################
########## FINAL CHARTS FOR PRESENTATION/REPORT ##########
##########################################################


#Final charts for presentation/executive summary
setwd("C:/Users/CAF4577/OneDrive - CNA/Documents/Data analytics/Results")

#State of the PHI industry
phi_industry_plot <- phi_master %>%
  #filter(age_group == "Total", category %in% c("HT mship as prop of Aus pop", "GT mship as prop of Aus pop")) %>%
  filter(age_group == "Total", category %in% c("HT mship as prop of Aus pop")) %>%
  ggplot(aes(QUARTER, value, color=category)) +
  geom_line(size=1.5, color="chocolate2") +
  labs(title = "Trends in PHI hospital treatment membership as a proportion of the Australian Population", 
       x = "Date", 
       y = "Membership as a proportion of Australian population",
       caption = "Source: APRA - Quarterly Private Health Insurance Benefit Trends March 2023") +
  scale_x_date(
    limits = c(as.Date("1997-09-30"), as.Date("2023-03-31")),
    date_breaks = "1 year",
    date_labels = "%Y",
    date_minor_breaks = "3 months"
  ) +
  geom_vline(
    xintercept = c(as.Date("2000-10-10"), as.Date("2005-06-30"), as.Date("2015-06-30"), as.Date("2020-06-30")),
    color = "lightgrey",
    linetype = "solid",
    size = 0.25
  ) +
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "none") +
  ylim(0.25, 0.55) +
  scale_y_continuous(labels = percent_format(scale = 100))

print(phi_industry_plot)
ggsave("PHI industry plot.pdf", phi_industry_plot)

#PHI death spiral
##Age
###Adjusting data to simplify age grouping to over/under 65
phi_master <- phi_master %>%
  mutate(age_group_simple = case_when(
    age_group=="25-34" ~ "Under 65 years old",
    age_group=="35-49" ~ "Under 65 years old",
    age_group=="50-64" ~ "Under 65 years old",
    age_group=="65+" ~ "Over 65 years old",
    age_group=="Total" ~ "Total"
  ))
phi_master$age_group_simple <- factor(phi_master$age_group_simple, c("Under 65 years old", "Over 65 years old"))


age_trend_plot <- phi_master %>% filter(age_group_simple!="Total", category == "HT mship as prop of Aus pop") %>%  
  ggplot(aes(QUARTER, value, fill=age_group_simple)) +
  geom_col(position="stack") +
  scale_x_date(limits=c(spiral_start_date, as.Date("2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y",
               date_minor_breaks = "3 months")+
  labs(title = "1. Trends in PHI membership by age during the death spiral", 
       x = "Date", 
       y = "Membership as a proportion of Australian population",
       caption = "Source: APRA - Quarterly Private Health Insurance Benefit Trends March 2023")+
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "bottom") +
  scale_fill_discrete(name = "Age group") +
  scale_y_continuous(labels= scales::percent_format())

print(age_trend_plot)



##Claim characteristics
###HT
ht_acs_plot <- phi_master %>% filter(age_group=="Total", category =="HT average total benefits per insured person", spiral_indicator =="During death spiral") %>%
  ggplot(aes(x=QUARTER, y=value, fill=age_group)) +
  geom_col(position="stack", fill="khaki4") +
  labs(title = "2. Trends in average costs during the death spiral", 
       x = "Date", 
       y = "Average cost per insured person",
       caption = "Source: APRA - Quarterly Private Health Insurance Benefit Trends March 2023") +
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "none") +
  scale_fill_discrete(name = "Age group") +
  scale_y_continuous(labels= scales::dollar_format(scale=1), limits=c(0,400)) +
  scale_x_date(limits=c(spiral_start_date, as.Date("2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y",
               date_minor_breaks = "3 months")

print(ht_acs_plot)

ht_freq_plot <- phi_master %>% filter(age_group=="Total", category =="HT episodes per insured person", spiral_indicator =="During death spiral") %>%
  ggplot(aes(x=QUARTER, y=value, fill=age_group)) +
  geom_line(size = 1.5, position="stack", color="royalblue4") +
  labs(title = "3.Trends in claim frequency during the death spiral", 
       x = "Date", 
       y = "Hospital episodes per insured person",
       caption = "Source: APRA - Quarterly Private Health Insurance Benefit Trends March 2023") +
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "none") +
  scale_fill_discrete(name = "Age group") +
  scale_y_continuous(labels= scales::number_format(scale=100), limits=c(0.075,0.11)) +
  scale_x_date(limits=c(spiral_start_date, as.Date("2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y",
               date_minor_breaks = "3 months")

print(ht_freq_plot)

blank <- ggplot(NULL)+
  theme_classic()+
  theme(axis.line=element_blank())

age_trend <- grid.arrange(blank, age_trend_plot, nrow=2)
ht_sev_freq <- grid.arrange(ht_acs_plot, ht_freq_plot, nrow=2)
death_spiral_chars <- grid.arrange(age_trend, ht_sev_freq, ncol=2)
ggsave("death spiral trends.pdf",death_spiral_chars)


#Chronic disease management programs
##claim characteristics
cdmp_mship_plot <- phi_master %>% filter(age_group=="Total", category %in% c("CDMP insured persons")) %>%
  ggplot(aes(x=QUARTER, y=value)) +
  geom_line(size = 1.5, color="chocolate2")+
  labs(title = "Trends in CDMP membership", 
       x = "Date", 
       y = "Insured persons",
       caption = "Source: APRA - Quarterly Private Health Insurance Benefit Trends March 2023") +
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "none")+
  scale_y_continuous(labels=scales::comma_format())+
  scale_x_date(limits=c(as.Date("2007-01-31"), as.Date("2022-12-31")),
               date_breaks = "1 year",
               date_labels = "%Y",
               date_minor_breaks = "3 months")

  

print(cdmp_mship_plot)

cdmp_age_plot <- phi_master %>% filter(age_group!="Total", category %in% c("CDMP insured persons")) %>%
  ggplot(aes(x=QUARTER,y=value, fill=age_group))+
  geom_col(position="fill")+
  labs(title = "Trends in CDMP age distribution", 
       x = "Date", 
       y = "Proportion of insured persons",
       fill = "Age group",
       caption = "Source: APRA - Quarterly Private Health Insurance Benefit Trends March 2023") +
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "bottom")+
  scale_y_continuous(label=percent_format(scale=100))+
  scale_x_date(limits=c(as.Date("2007-01-31"), as.Date("2022-12-31")),
               date_breaks = "1 year",
               date_labels = "%Y",
               date_minor_breaks = "3 months")


print(cdmp_age_plot)

cdmp_chars_plot <- grid.arrange(cdmp_mship_plot, cdmp_age_plot, nrow=2)

ggsave("CDMP characteristics.pdf", cdmp_chars_plot)
ggsave("CDMP age distribution.pdf", cdmp_age_plot)



#Chronic diseases
##Disease burden
disease_burden_plot <- aihw_master %>% filter(category=="Years lived with disability - age standardised rate", disease_group %in% c("Cardiovascular diseases","Mental and substance use disorders", "Endocrine disorders", "Respiratory diseases")) %>%
  ggplot(aes(year, value, color=disease_group)) +
  geom_line(size=1.5)+
  geom_point(size=5, shape = "x")+
  scale_colour_brewer(palette="RdYlBu")+
  labs(title = "Disease burden for living Australians", 
       x = "Year", 
       y = "Years lived with disability",
       color = "Chronic disease",
       caption = "Source: AIHW Australian burden of disease study 2022") +
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "left")+
  scale_x_continuous(breaks= c(2003, 2011, 2015, 2018, 2022))

print(disease_burden_plot)


##Proportion of hospitalisations
#exclude diseases wtih 0% hospitalisation
hosps_plot <- aihw_master %>% filter(category =="Proportion of total hospitalisations", disease_group %in% c("Cardiovascular","Endocrinology","Mental health","Metabolic","Pulmonary","Rheumatology")) %>%
  ggplot(aes(x=year, y=value, fill=disease_group)) +
  geom_col()+
  scale_y_continuous(label=percent_format(scale=1))+
  scale_x_continuous(breaks=breaks_width(1))+
  geom_text(aes(label=percent(value, scale=1, accuracy=0.1)), position=position_stack(vjust=0.5)) +
  scale_fill_brewer(palette="RdYlBu")+
  labs(title = "Chronic disease hospitalisation as a proportion of total hospitalisations in Australia", 
       x = "Year", 
       y = "Proportion of total hospitalisations in Australia",
       fill = "Chronic disease",
       caption = "Source: AIHW National Hospital Morbidity Database") +
  theme_classic() +
  theme(plot.title = element_text(
    color = "black",
    size = 14,
    face = "bold",
    hjust = 0.5),
    axis.title.x = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    axis.title.y = element_text(
      color="black",
      size=10,
      hjust = 0.5),
    legend.position = "left")

print(hosps_plot)

chronic_diseases <- grid.arrange(disease_burden_plot, hosps_plot, nrow=2)

ggsave("Chronic diseases.pdf",chronic_diseases)


