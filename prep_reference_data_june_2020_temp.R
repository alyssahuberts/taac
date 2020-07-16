library(foreign)
library(readstata13)
library(stringdist)
library(dplyr)
library(stringr)
library(haven)
library(janitor)
setwd("/Users/alyssahuberts/Dropbox/TAAC Scorecard/5 Merge election data/1_data/")
#Date created: 6/29/2020
#Date last edited: 6/29/2020

# What we need
# For every survey respondent in TAAC
  # An incumbent candidate
  # An incumbent party 
# This means we need a walkthrough for the political data to align with the survey data 
# The challenge is that a) the survey, b) the incumbency data, and c) the 2016
# candidacy data all use different references to subcounties and districts AND
# districts change between 2011 and 2016

# Walkthrough should be focused on the survey
  # Create a baseline of all the subcounties that we need id's for

  taac_survey <- read_dta("/Users/alyssahuberts/Dropbox/TAAC Scorecard/4 Data Analysis/2_data/1_rawdata/TAAC_final.dta") %>% filter(DataSource_Endline ==1)
  # This data has some id's/names that shouldn't be used. Get rid of them
  taac_survey$scid <- NULL
  taac_survey$scounty_name <-NULL
  taac_survey$district_name <-NULL
  taac_survey$districtid <-NULL
  walkthrough_taac_survey <- taac_survey[,c("subcounty", "district")]
  walkthrough_taac_survey <- unique(walkthrough_taac_survey) %>% rename(taac_survey_district = district, taac_survey_subcounty = subcounty)
  
  # now we want the 2011 incumbents
  winners_2011_LC3 <- read.csv("election/2011_SC_Chairperson_winners.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
    clean_names %>% 
   rename(district_id=x,  ea = x_1, scounty_id = x_2)
  winners_2011_LC3$district_id <- str_pad(winners_2011_LC3$district_id,3, side = "left",pad = "0")
  winners_2011_LC3$ea_id <- str_pad(winners_2011_LC3$ea,3, side = "left",pad = "0")
  winners_2011_LC3$scounty_id <- str_pad(winners_2011_LC3$scounty_id,3, side = "left",pad = "0")
  winners_2011_LC3$scid <- paste(winners_2011_LC3$district_id,winners_2011_LC3$ea_id, winners_2011_LC3$scounty_id, sep = "-")
  walkthrough_incumbents <- winners_2011_LC3[,c("scid", "scounty", "district_id", "district")]
  walkthrough_incumbents <- unique(walkthrough_incumbents) %>% 
  rename(incumbents_scid = scid, incumbents_scounty = scounty, incumbents_district_id = district_id, incumbents_district = district)
  # walkthrough needs a column which mimics the survey's subcounty 
  # first step is just changing case
  walkthrough_incumbents$taac_survey_subcounty <- str_to_title(walkthrough_incumbents$incumbents_scounty)
  # now correct discrepencies. Note that we only need to correct the
  # discrepencies for subcounties in the survey (the others don't matter)
  walkthrough_incumbents$taac_survey_subcounty <- str_replace(walkthrough_incumbents$taac_survey_subcounty, "Town", "TC")
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="AII-VU/AJIVU"] <- "Aiivu"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="LABONGO-AMIDA"] <- "Amida"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="MUYEMBE TOWN"] <- "Bulambuli TC/ Muyembe"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="BUMASOBO"] <- "Bumasobo/ Buluganya"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="BULUGANYA"] <- "Buluganya"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="BUNYAFWA"] <- "Bunyafa"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="CAWENTE"] <- "Chawente"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="TORORO"] <- "Eastern Division (Tororo)"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="KAPCHORWA"] <- "Kapchorwa TC"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="KARENGA(NAPOR"] <- "Karenga"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="KOCH-GOMA"] <- "Kochgoma"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="LOPEI"] <- "Lopeei"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="LOTUKEI"] <- "Lotuke"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="LWASSO"] <- "Lwaaso"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="MADI-OPEI"] <- "Madi Opei"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="NAKAPIRIPIRIT"] <- "Nakapiripirit TC"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="NAKAPELIMORU"] <- "Nakipelimoru"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="NALWANZA"] <- "Nalwaza"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="BULEGENI"] <- "Namisuni/ Bulegeni"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="NAMISUNI"] <- "Namisuni/ Bulegeni"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="NORTHERN"& walkthrough_incumbents$incumbents_district=="SOROTI"] <- "Northern Division (Soroti)"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="SERERE/OLIO"] <- "Olio"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="OMIYA-ANYIMA"] <- "Omiya Anyima"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="PALABEK-KAL"] <- "Palabek Kal"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="OGILI"] <- "Palabek Ogili"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="OLI RIVER"] <- "River Oli"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="SIDOK (KOPOTH)"] <- "Sidok"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="SIMU"] <- "Simu/Sisiyi"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="SISIYI"] <- "Simu/Sisiyi"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="EASTERN" &  walkthrough_incumbents$incumbents_district=="SOROTI"] <- "Eastern Division (Soroti)"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="SOUTH DIVISION" & walkthrough_incumbents$incumbents_district =="MOROTO"] <- "South Division (Moroto)"
  walkthrough_incumbents$taac_survey_subcounty[walkthrough_incumbents$incumbents_scounty=="WESTERN" & walkthrough_incumbents$incumbents_district=="SOROTI"] <- "Western Division (Soroti)"
  # can match everything except Palabek Ogili and Midigo
  
  
  
  
  # now we want the 2016 candidates
  cands_16 <- read.dta13("election/2016_LGCands.dta", nonint.factors = TRUE)
  cands_16$cands_16_scid <- cands_16$scid
  walkthrough_cands16 <- cands_16[,c("scid", "scounty_name", "district_name","districtid")] 
  walkthrough_cands16 <- unique(walkthrough_cands16) %>% 
    rename(cands_16_scid = scid, cands_16_scounty_name = scounty_name, cands_16_district_name = district_name, cands_16_district_name = district_name, cands_16_district_id = districtid)
  
  # walkthrough needs a column which mimics the survey's subcounty 
  # first step is just changing case
  walkthrough_cands16$taac_survey_subcounty <- str_to_title(walkthrough_cands16$cands_16_scounty_name)
  # now correct discrepencies. Note that we only need to correct the
  # discrepencies for subcounties in the survey (the others don't matter)
  walkthrough_cands16$taac_survey_subcounty <- str_replace(walkthrough_cands16$taac_survey_subcounty, "Town Council", "TC")
  
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="ABIM TOWN COUNCIL"] <- "Abim TC"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="ABIM"] <- "Abim"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="AII-VU/AJIVU"] <- "Aiivu"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="LABONGO-AMIDA"] <- "Amida"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="BULAMBULI TOWN COUNCIL"] <- "Bulambuli TC/ Muyembe"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="BUMASOBO"] <- "Bumasobo/ Buluganya"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="BULUGANYA"] <- "Buluganya"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="BUNYAFWA"] <- "Bunyafa"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="CAWENTE"] <- "Chawente"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="EASTERN" &  walkthrough_cands16$cands_16_district_name=="SOROTI"] <- "Eastern Division (Soroti)"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="TORORO EASTERN" ] <- "Eastern Division (Tororo)"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="KARENGA(NAPORE)" ] <- "Karenga"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="KARUJUBU DIVISION" ] <- "Karujubu"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="KITGUM MATIDI" ] <- "Kitgum TC"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="KOCH-GOMA" ] <- "Kochgoma"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="LOPEI" ] <- "Lopeei"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="LOTUKEI" ] <- "Lotuke"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="LWASSO" ] <- "Lwaaso"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="MADI-OPEI" ] <- "Madi Opei"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="NAKAPELIMORU" ] <- "Nakipelimoru"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="NALWANZA" ] <- "Nalwaza"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="NAMISUNI" ] <- "Namisuni/ Bulegeni"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="BULEGENI" ] <- "Namisuni/ Bulegeni"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="NORTHERN" &  walkthrough_cands16$cands_16_district_name=="SOROTI"] <- "Northern Division (Soroti)"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="SERERE/OLIO" ] <- "Olio"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="OMIYA-ANYIMA" ] <- "Omiya Anyima"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="PALABEK-KAL" ] <- "Palabek Kal"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="OGILI" ] <- "Palabek Ogili"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="OLI RIVER" ] <- "River Oli"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="SIDOK (KOPOTH)" ] <- "Sidok"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="SIMU" ] <- "Simu/Sisiyi"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="SISIYI" ] <- "Simu/Sisiyi"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="SOUTH DIVISION" & walkthrough_cands16$cands_16_district_name =="MOROTO"] <- "South Division (Moroto)"
  walkthrough_cands16$taac_survey_subcounty[walkthrough_cands16$cands_16_scounty_name=="WESTERN" &  walkthrough_cands16$cands_16_district_name=="SOROTI"] <- "Western Division (Soroti)"
  

  x <- anti_join(walkthrough_taac_survey, walkthrough_incumbents, by = "taac_survey_subcounty")
  # only missing midigo
  y <- anti_join(walkthrough_taac_survey, walkthrough_cands16, by = "taac_survey_subcounty")
  # only missing kapchorwa TC, Koboko TC, Tegeres
  
  
  walkthrough <- left_join(walkthrough_taac_survey, walkthrough_cands16, by = "taac_survey_subcounty")
  walkthrough <- left_join(walkthrough, walkthrough_incumbents, by = "taac_survey_subcounty")
  save(walkthrough,file = "/Users/alyssahuberts/Dropbox/TAAC Scorecard/5 Merge election data/1_data/walkthrough.Rdata")
  