
#===============================================================================================================
# Clean SJ permit data downloaded from City website
#
# Permit data San Jose downloaded from https://sjpermits.org/permits/general/reportdata.asp
# and read into R using "0.100 read SJ building permits v1.R"
#
# Revised 9/20/2024 WAS for SCUIRL, permits updated to August 2024
#===============================================================================================================

# clear environment
  rm(list = ls())
  
# Load packages 
  if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
  pacman::p_load(broom, dtplyr,  cowplot,estimatr, knitr) 
  p_load(ggpubr, gridExtra, ggrepel, ggmap, googleAnalyticsR) 
  p_load(leaflet, sf, tigris, tmap, tmaptools, tm) 
  p_load(data.table, tidyverse, tidyfast,gridExtra,ggpubr, grid, tidycensus) 
  
# Set working directory
  main_folder="xxxxx/Econ 192 students"
  #setwd(main_folder)
  getwd()
  
# commands to merge with tracking: full_join_track, left_join_track, right_join_track
  merge_url <- "https://github.com/mkevane/econ192/raw/scripts/merge%20full%20join%20with%20tracking.R"
  source(merge_url)
  
# turn off scientific notation except for big numbers
  options(scipen = 9)
  options(digits=3) 

#===============================================================================
#   Load and clean permit data
#===============================================================================

# Read raw data on permits
  perm <- "https://github.com/mkevane/econ192/raw/data/building_permits_sj_city.Rdata"
  load(url(perm))
  
# The remainder of the script cleans various variables but needs to be checked
  
# clean vars
  df1 <- bp_sj %>%  
    mutate(DWELLUNITS=as.numeric(DWELLUNITS),
           DWELLUNITS= ifelse(new_cons==TRUE, DWELLUNITS, NA))
  df1$APN <- gsub("-", "", df1$APN)
  
# remove duplicates in same year (need to be more sure why there are duplicates - about 3,000)
  df1<- distinct(df1,APN, JOBLOCATION, year, .keep_all= TRUE)
# remove duplicates across years (need to be more sure why there are duplicates- about 2,000)
  df1<- distinct(df1,APN, JOBLOCATION, .keep_all= TRUE)
  
# create ADU permit indicator variable
  # Using Grepl to identify ADUs
  # https://stackoverflow.com/questions/21311386/using-grep-to-help-subset-a-data-frame-in-r

  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW ADU", JOBLOCATION), 1, 0))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("DETACHED ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("DETACHED SECONDARY UNIT ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("CONVERT \\(E\\) SPACE TO ATTACHED ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("\\(BEPM 100%\\) ADU$", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW A 2ND UNIT \\(ADU\\)", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GREWAL NEW 2ND UNIT", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("2ND UNIT ADDITION", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("CONVERT \\(E\\) ATTACHED GARAGE IN TO ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ATTACHED GARAGE TO ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW ATTACHED 2ND UNIT \\(ADU\\)", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GARAGE TO ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADD\\/ALT  TO \\(E\\) ADU AND \\(E\\) SFR", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW 2ND UNIT \\(ADU\\)", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GARAGE CONVERSION TO ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GARAGE\\/STORAGE CONVERSION TO AN ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW 2ND UNIT \\(ADU\\)", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW ATTACHED 2ND UNIT \\(ADU\\)", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ATTACHED 2-STORY ADU SECONDARY UNIT", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU 2nd LEVEL", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GARAGE CONVERSION JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("CONVERT GARAGE TO ADU & ADD UTIL RM", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GARAGE CONV. JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("CODE CASE JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW 2ND LIVING UNIT \\(ADU\\)", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU \\/ GARAGE", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GARAGE CONV. JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("FAM RM CONVERTED INTO JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("GARAGE CONVERSION TO A JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ATTACHED JADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl(" ADU$", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl(" JADU$", JOBLOCATION), 1, ADU_permit))

  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU CONVERSION$", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ATTACHED ADU", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("CONVERT TO ADU UNITS", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU CONV.", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("2ND UNIT \\(ADU\\)", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU W\\/ GARAGE", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU ABOVE GARAGE", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ATTACHED ADU CONVERSION\\/EXTENSION", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU\\/ALTERATIONS", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("NEW DET ADU & DEMO DET GARAGE", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("ADU, DEMO EXIST CAR PORT", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(grepl("SPECIFIC PREAPPROVED ADU EPR", JOBLOCATION), 1, ADU_permit))
  df1 <- df1 %>% mutate(ADU_permit=ifelse(SUBDESC == "2nd Unit Added", 1, ADU_permit))
  
  df1 <- df1 %>%  mutate(ADU_permit= ifelse(is.na(ADU_permit), 0,ADU_permit))

# to check
  # ADU <- df1 %>% filter(str_detect(JOBLOCATION, "ADU")) %>% filter(ADU_permit==0) %>% filter(!grepl("ADULT", JOBLOCATION))
  # ADU <- df1 %>% filter(SUBDESC == "2nd Unit Added") %>% filter(ADU_permit==0)

# cleanup address: split JOBLOCATION by parenthesis, drop everything after end of street address
  df1$address <- iconv(df1$JOBLOCATION, to = "ASCII//TRANSLIT")
  df1 <- df1 %>% separate_wider_delim(address, delim = "(", names = c("address", "address2"),
    too_few = "align_start",
    too_many = "merge")
  #df1$address <- gsub("\\s*\\([^\\)]+\\)","",df1$address)  
  df1$address <- gsub("\\ ST .*$"," ST",df1$address) 
  df1$address <- gsub("\\ CT .*$"," CT",df1$address) 
  df1$address <- gsub("\\ AV .*$"," AV",df1$address) 
  df1$address <- gsub("\\ DR .*$"," DR",df1$address) 
  
  df1$address <- gsub("\\ WY .*$"," WY",df1$address) 
  df1$address <- gsub("\\ TR .*$"," TR",df1$address) 
  df1$address <- gsub("\\ CL .*$"," CL",df1$address) 
  df1$address <- gsub("\\ LANE .*$"," LANE",df1$address) 
  df1$address <- gsub("\\ LN .*$"," LN",df1$address) 
  df1$address <- gsub("\\ PL .*$"," PL",df1$address) 
  
  df1$address <- gsub("\\ RD .*$"," RD",df1$address) 
  df1$address <- gsub("\\ PY .*$"," PY",df1$address) 
  df1$address <- gsub("\\ BL .*$"," BL",df1$address)
  
  df1$address <- gsub("\\ LP .*$"," LP",df1$address) 
  df1$address <- gsub("\\ EX .*$"," EX",df1$address) 
  df1$address <- gsub("\\ CREEK .*$"," CREEK",df1$address)
  
  df1$address <- gsub("\\ CM .*$"," CM",df1$address) 
  df1$address <- gsub("\\ HEIGHTS .*$"," HEIGHTS",df1$address) 
  df1$address <- gsub("\\ THE ALAMEDA .*$"," THE ALAMEDA",df1$address)
  df1$address <- gsub("\\ ROW .*$"," ROW",df1$address) 
  df1$address <- gsub("\\ VI .*$"," VI",df1$address) 
  df1$address <- gsub("\\ ROBLES EAST .*$"," ROBLES EAST",df1$address) 
   
  df1$address <- gsub("\\ AVENIDA LAS BRISAS .*$"," AVENIDA LAS BRISAS",df1$address)
  df1$address <- gsub("\\ HY .*$"," HY",df1$address)
  df1$address <- gsub("\\ AVENIDA BENITO .*$"," AVENIDA BENITO",df1$address)
  df1$address <- gsub("\\ PLAZA SOL .*$"," PLAZA SOL",df1$address)
  df1$address <- gsub("\\ AVENIDA ELISA .*$"," AVENIDA ELISA",df1$address)
  df1$address <- gsub("\\ CAMINO LEONOR .*$"," CAMINO LEONOR",df1$address)
  df1$address <- gsub("\\ CAMINO RICARDO .*$"," CAMINO RICARDO",df1$address)
  df1$address <- gsub("\\ STNEW .*$"," ST",df1$address)
  df1$address <- gsub("\\ CAMINO PABLO .*$"," CAMINO PABLO",df1$address)
  df1$address <- gsub("\\ LPNEW .*$"," LP",df1$address)
  df1$address <- gsub("\\ AL .*$"," AL",df1$address)
  df1$address <- gsub("\\ AVNEW .*$"," AV",df1$address)
  df1$address <- gsub("\\  EXNEW .*$"," EX",df1$address)
  df1$address <- gsub("\\  RDTHE .*$"," RD",df1$address)
  df1$address <- gsub("\\  VIA VENEZIA .*$"," VIA VENEZIA",df1$address)
  df1$address <- gsub("\\  STRETAIL .*$"," ST",df1$address)
  df1$address <- gsub("\\  ST2ND .*$"," ST",df1$address)
  df1$address <- gsub("\\  RDNEW .*$"," RD",df1$address) 
  df1$address <- gsub("\\  PASEO TRANQUILLO .*$"," PASEO TRANQUILLO",df1$address) 
  df1$address <- gsub("\\  WYNEW .*$"," WY",df1$address)
  df1$address <- gsub("\\  AVNEW .*$"," AV",df1$address)
  df1$address <- gsub("\\  PLNEW .*$"," PL",df1$address)
  df1$address <- gsub("\\  STRADA CIRCOLARE .*$"," STRADA CIRCOLARE",df1$address)
  df1$address <- gsub("\\  CHEMIN DE RIVIERE .*$"," CHEMIN DE RIVIERE",df1$address)
  df1$address <- gsub("\\  CTNEW .*$"," CT",df1$address) 
  df1$address <- gsub("\\  CTFOUNDATION .*$"," CT",df1$address) 
  df1$address <- gsub("\\  COUR DE CHARLES .*$"," COUR DE CHARLES",df1$address)
  df1$address <- gsub("\\  COUR DU VIN .*$"," COUR DU VIN",df1$address)
  df1$address <- gsub("\\  PLACE DU JARDIN .*$"," PLACE DU JARDIN",df1$address)
  df1$address <- gsub("\\  PLACE DE LOUIS .*$"," PLACE DE LOUIS",df1$address)
  df1$address <- gsub("\\  DRNEW .*$"," DR",df1$address) 
  
  df1$address <- gsub("\\  LNNEW.*$"," LN",df1$address) 
  df1$address <- gsub("\\  SQ .*$"," SQ",df1$address)  
  df1$address <- gsub("\\  RUE MIRASSOU .*$"," RUE MIRASSOU",df1$address)
  df1$address <- gsub("\\  WYNEW .*$"," WY",df1$address) 
  df1$address <- gsub("\\  WYPLAN .*$"," WY",df1$address)
  df1$address <- gsub("\\  PLBF .*$"," PL",df1$address) 
  df1$address <- gsub("\\  BLNEW .*$"," BL",df1$address) 
  df1$address <- gsub("\\  VIA CARMELA .*$"," VIA CARMELA",df1$address)
  df1$address <- gsub("\\  LN2 NEW .*$"," LN",df1$address)
  df1$address <- gsub("\\  CT2 NEW .*$"," CT",df1$address)
  df1$address <- gsub("\\  CTNEW .*$"," CT",df1$address) 
  df1$address <- gsub("\\  STRADA CIRCOLARE .*$"," XXX",df1$address)
  
  df1$address <- gsub("\\  VIA CRISTOBAL   CASTILLA .*$"," VIA CRISTOBAL CASTILLA",df1$address)
  df1$address <- gsub("\\  PASEO OLIVOS .*$"," PASEO OLIVOS",df1$address)
  df1$address <- gsub("\\  DR2-STY .*$"," DR",df1$address)
  df1$address <- gsub("\\  ST386 .*$"," ST",df1$address)
  df1$address <- gsub("\\  WW  Bldg.*$"," WW",df1$address)
  df1$address <- gsub("\\  DR10- .*$"," DR",df1$address)
  df1$address <- gsub("\\  DR10 .*$"," DR",df1$address)
  df1$address <- gsub("\\  VISTA MONTANA.*$"," VISTA MONTANA",df1$address)
  df1$address <- gsub("\\  PL369.*$"," PL",df1$address)
  df1$address <- gsub("\\  VISTA MONTANA .*$"," VISTA MONTANA",df1$address)
  
  df1$address <- gsub("\\  VIA DEL ORO.*$"," VIA DEL ORO",df1$address)
  df1$address <- gsub("\\  PLAZA BANDERAS.*$"," PLAZA BANDERAS",df1$address)
  df1$address <- gsub("\\  AVPARRA.*$"," AV",df1$address)
  df1$address <- gsub("\\  AVARRIOLA.*$"," AV",df1$address)
  df1$address <- gsub("\\  CORTE DE LA REINA.*$"," CORTE DE LA REINA",df1$address)
  
  df1$APN <- gsub("00000000","",df1$APN)
  df1$APN <- gsub("???","",df1$APN)

# Cleanup - need to look at this carefully- why Rollyear not working
  # something about dates messed up perhaps?  Also APN not always numeric?
  permits <- df1 %>%  
    mutate(Rollyear1=ifelse(month(mdy(ISSUEDATE))<=6, 
          year(mdy(ISSUEDATE)), year(mdy(ISSUEDATE))+1 ),
          Rollyear2=ifelse(month(dmy(ISSUEDATE))<=6, 
          year(dmy(ISSUEDATE)), year(dmy(ISSUEDATE))+1  ),
          new_cons = ifelse(new_cons=="TRUE",1,0),
          any_permit = as.numeric(any_permit)) %>% 
    mutate(Rollyear=ifelse(!is.na(Rollyear1),Rollyear1, ifelse(!is.na(Rollyear2), Rollyear2, NA))) %>% 
    mutate(Rollyear=ifelse(Rollyear>2025, Rollyear-100, Rollyear)) %>% 
    mutate(APN=as.numeric(APN),
       new_cons = replace_na(new_cons,0),
       any_permit = replace_na(any_permit,0),
       DWELLUNITS = replace_na(DWELLUNITS,0),
       # Add San Jose, CA to address
       address = paste0(df1$address, ", San Jose, CA")
    ) %>% 
    select(-address2)
  
  save(permits, file="Data/permits_sj_city_clean.Rdata")
  
#### Following from 0.033 cleaning addresses v1.R
  
  # Clean up addresses (Does this have to be done?)
  #s_u <- s_u %>% 
  #mutate(address = ifelse(APN=="48444055", "2574 VISTA DEL SOL DR", address),
  #address = ifelse(APN=="47208071", "775 LOTUS ST", address), 
  #address = ifelse(APN=="68917068", "5733 HILLBRIGHT CIR", address), 
  #address = ifelse(APN=="43917022", "999 Franquette Ave, San Jose, CA 95125", address), 
  #address = ifelse(APN=="37829010", "1486 English Dr, San Jose, CA 95129", address), 
  #address = ifelse(APN=="46257018", "4387 Pitch Pine Ct, San Jose, CA 95136", address),
  #address = ifelse(APN=="43941006", "2712 Plummer Ave, San Jose, CA 95125", address), 
  #address = ifelse(APN=="68414054", "683 River View Dr, San Jose, CA 95111", address), 
  #address = ifelse(APN=="41221045", "691 Elden Dr, Campbell, CA 95008", address), 
  #address = ifelse(APN=="37705036", "1043 Harlan Dr, San Jose, CA 95129", address), 
  #address = ifelse(APN=="47231005", "555 S 16th St, San Jose, CA 95112", address), 
  #address = ifelse(APN=="47222029", "670 S 11th St, San Jose, CA 95112", address), 
  #address = ifelse(APN=="42902033", "1318 Cristina Ave, San Jose, CA 95125", address), 
  #address = ifelse(APN=="30721007", "4333 Colombo Dr, San Jose, CA 95130", address),
  #address = ifelse(APN=="67628004", "2690 PEARTREE LN, San Jose", address)) 
  
  # Check this address
  #df1$address <- gsub("683 RIVER VIEW DR..." , "san jose, ca, usa", df1$address)
  
  # (MK adapted this from warning messages from google maps geocode)
  # This is what geocode automatically recoded. But we should do by hand after verification
  # Have to check why these need to be corrected- ie does permit really not indicate if street is S or N?
  # df1$address <- gsub("860 8TH ST, San Jose" , "860 s 8th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("263 HEDDING ST, S..." , "263 w hedding st, san jose, ca 95110, usa", df1$address)
  # df1$address <- gsub("1032 11TH ST, San..." , "1032 s 11th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("340 14TH ST, San Jose" , "340 s 14th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("625 15TH ST, San Jose" , "625 n 15th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("645 15TH ST, San Jose" , "645 n 15th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("330 16TH ST, San Jose" , "330 s 16th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("436 13TH ST, San Jose" , "436 n 13th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("392 16TH ST, San Jose" , "392 n 16th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("619 12TH ST, San Jose" , "619 n 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("555 16TH ST, San Jose" , "555 n 16th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("145 17TH ST, San Jose" , "145 n 17th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("670 11TH ST, San Jose" , "670 n 11th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("522 13TH ST, San Jose" , "522 n 13th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("339 7TH ST, San Jose" , "339 n 7th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("432 16TH ST, San Jose" , "432 n 16th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("461 6TH ST, San Jose" , "461 n 6th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("829 MONROE ST, San Jose" , "829 s monroe st, san jose, ca 95128, usa", df1$address)
  # df1$address <- gsub("110 33RD ST, San Jose" , "110 n 33rd st, san jose, ca 95116, usa", df1$address)
  # df1$address <- gsub("1747 SILVERWOOD D..." , "1747 silvertree dr, san jose, ca 95131, usa", df1$address)
  # df1$address <- gsub("518 15TH ST, San Jose" , "518 n 15th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("425 13TH ST, San Jose" , "425 n 13th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("421 15TH ST, San Jose" , "421 n 15th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("454 21ST ST, San Jose" , "454 n 21st st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("185 19TH ST, San Jose" , "185 n 19th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("662 11TH ST, San Jose" , "662 n 11th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("78 12TH ST, San Jose" , "78 n 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("365 11TH ST, San Jose" , "365 n 11th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("817 8TH ST, San Jose" , "817 s 8th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("151 MORRISON AVE,..." , "151 s morrison ave, san jose, ca 95126, usa", df1$address)
  # df1$address <- gsub("606 12TH ST, San Jose" , "606 s 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("86 33RD ST, San Jose" , "86 n 33rd st, san jose, ca 95116, usa", df1$address)
  # df1$address <- gsub("453 3RD ST, San Jose" , "3rd st, san francisco, ca, usa", df1$address)
  # df1$address <- gsub("378 16TH ST, San Jose" , "378 s 16th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("482 12TH ST, San Jose" , "482 n 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("711 TAYLOR ST, San Jose" , "711 e taylor st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("680 9TH ST, San Jose" , "680 s 9th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("320 7TH ST, San Jose" , "320 n 7th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("342 20TH ST, San Jose" , "342 n 20th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("332 13TH ST, San Jose" , "332 n 13th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("646 15TH ST, San Jose" , "646 n 15th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("539 8TH ST, San Jose" , "539 n 8th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("201 20TH ST, San Jose" , "201 n 20th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("446 14TH ST, San Jose" , "446 n 14th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("90 34TH ST, San Jose" , "90 s 34th st, san jose, ca 95116, usa", df1$address)
  # df1$address <- gsub("366 10TH ST, San Jose" , "366 s 10th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("55 14TH ST, San Jose" , "55 s 14th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("563 12TH ST, San Jose" , "563 n 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("665 10TH ST, San Jose" , "665 s 10th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("134 MISSION ST, S..." , "134 e mission st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("467 13TH ST, San Jose" , "467 n 13th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("734 12TH ST, San Jose" , "734 n 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("946 5TH ST, San Jose" , "946 n 5th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("648 12TH ST, San Jose" , "648 s 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("387 12TH ST, San Jose" , "387 n 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("846 MONROE ST, San Jose" , "846 n monroe st, san jose, ca 95128, usa", df1$address)
  # df1$address <- gsub("49 20TH ST, San Jose" , "49 s 20th st, san jose, ca 95116, usa", df1$address)
  # df1$address <- gsub("275 MISSION ST, S..." , "275 e mission st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("748 11TH ST, San Jose" , "748 s 11th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("683 GENEVIEVE LN,..." , "683 n genevieve ln, san jose, ca 95128, usa", df1$address)
  # df1$address <- gsub("609 14TH ST, San Jose" , "609 n 14th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("538 MONROE ST, San Jose" , "538 n monroe st, san jose, ca 95128, usa", df1$address)
  # df1$address <- gsub("508 MONROE ST, San Jose" , "508 s monroe st, san jose, ca 95128, usa", df1$address)
  # df1$address <- gsub("782 12TH ST, San Jose" , "782 n 12th st, san jose, ca 95112, usa", df1$address)
  # df1$address <- gsub("815 TAYLOR ST, San Jose" , "815 w taylor st, san jose, ca 95126, usa", df1$address)
  
