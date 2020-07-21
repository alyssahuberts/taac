  library(foreign)
  library(readstata13)
  library(stringdist)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(haven)
  library(tidyverse)
  setwd("/Users/alyssahuberts/Dropbox/TAAC Scorecard/5 Merge election data/1_data/")
  
  # Date created: 6/29/2020
  # Date last edited: 6/29/2020 (AJH)
  # new stab at fuzzy matching
  
  
  # What we have 
    # Database of 2016 candidates
    # Database of 2011 candidates, with winners identified (this is our incumbents)
    # Survey responses 
  
  # What we want to do 
    # Create a database that has the full set of scid/district ids we will need, with the incumbent and incumbent party
    # For every survey respondent, delineate:
      # if they voted in the election (polit_lc3_vote, polit_lc5_vote)
      # if they gave us a name (polit_lc3_gave_name, polit_lc5_gave_name)
      # if we could match the name 
    # for each name, come up with the best match 
    # for each match, identify whether that candidate: 
      # is incumbent
      # is incumbent party 
############################################################      
    # read in taac survey and filter to match stata data
######################################################
    taac_survey <- read_dta("/Users/alyssahuberts/Dropbox/TAAC Scorecard/4 Data Analysis/2_data/1_rawdata/TAAC_final.dta") %>% 
      select(resp_id, DataSource_Endline, district, subcounty, polit_lc3_vote, polit_lc3_vote_conf, polit_lc3_vote_name, polit_lc5_vote, polit_lc5_vote_conf, polit_lc5_vote_name,polit_lc5_vote_name_u, polit_lc5_vote_full_name, sc_treat, region4, nusf_role_13,subp_id) %>% 
    filter(DataSource_Endline ==1)
    taac_survey <-  taac_survey %>% filter(!is.na(sc_treat))
    taac_survey$region4 <-ifelse(is.na(taac_survey$region4), 0, taac_survey$region4)
    taac_survey <- taac_survey[taac_survey$region4!=1,]
    taac_survey <- taac_survey[!(taac_survey$district == "Napak"),]
    taac_survey <- taac_survey[!(taac_survey$district == "Nakapiripirit"),]
    taac_survey <- taac_survey[!(taac_survey$district == "Moroto"),]
    taac_survey <- taac_survey[!(taac_survey$district == "Kotido"),]
    taac_survey <- taac_survey[!(taac_survey$district == "Kaabong"),]
  
    taac_survey <- taac_survey %>% filter(nusf_role_13 == 0)
    
    ##### MISNAMED SUBCOUNTIES - For a certain subset of respondents, we can't tell what subcounty they're in to match the data:
    # Biiso (Bullisa) could be Biiso or Butiaba
    # Aber (Oyam) could be Aber or Kamdini
    # Rigbo (Arua) could be Rigbo or Ewanga 
    # Namisuni/Bulegeni  could be either
    # Simu/Sisiyi could be either 
    # However, based on which candidate a majority of respondents in the subproject voted for, we can identify them by subproject 
    
    taac_survey[(taac_survey$subp_id == 160| taac_survey$subp_id == 166|taac_survey$subp_id == 167| 
                taac_survey$subp_id == 168), "subcounty"] <- "Rigbo"
    taac_survey[(taac_survey$subp_id == 162|taac_survey$subp_id ==163|taac_survey$subp_id ==164|taac_survey$subp_id ==165|taac_survey$subp_id == 169|taac_survey$subp_id == 170| taac_survey$subp_id == 171), "subcounty"] <- "Ewanga"
    taac_survey[(taac_survey$subp_id == 262|taac_survey$subp_id == 263| taac_survey$subp_id == 264|taac_survey$subp_id == 265), "subcounty"] <- "Namisuni"
    taac_survey[(taac_survey$subp_id == 266|taac_survey$subp_id == 267| taac_survey$subp_id == 268|taac_survey$subp_id == 269), "subcounty"] <- "Simu"
    taac_survey[(taac_survey$subp_id == 270|taac_survey$subp_id == 271| taac_survey$subp_id == 272), "subcounty"] <- "Biiso"
    taac_survey[(taac_survey$subp_id == 273|taac_survey$subp_id == 274| taac_survey$subp_id == 275|taac_survey$subp_id == 276|
                   taac_survey$subp_id == 277|taac_survey$subp_id == 278| taac_survey$subp_id == 279|
                   taac_survey$subp_id == 280|taac_survey$subp_id == 281| taac_survey$subp_id == 282|
                   taac_survey$subp_id == 283|taac_survey$subp_id == 284| taac_survey$subp_id == 285|
                   taac_survey$subp_id == 286|taac_survey$subp_id == 287| 
                   taac_survey$subp_id == 290|taac_survey$subp_id == 291| taac_survey$subp_id == 292|
                   taac_survey$subp_id == 293|taac_survey$subp_id == 294), "subcounty"] <- "Butiaba"
    taac_survey[(taac_survey$subp_id == 933|taac_survey$subp_id == 934| taac_survey$subp_id == 935|
                   taac_survey$subp_id == 936|taac_survey$subp_id == 948| taac_survey$subp_id == 949|taac_survey$subp_id ==951), "subcounty"] <- "Aber"
    taac_survey[(taac_survey$subp_id == 937|taac_survey$subp_id == 938| taac_survey$subp_id == 940|
                   taac_survey$subp_id == 941|taac_survey$subp_id == 942| taac_survey$subp_id == 944|taac_survey$subp_id ==945|
                   taac_survey$subp_id == 946| taac_survey$subp_id == 947), "subcounty"] <- "Kamdini"
    taac_survey[(taac_survey$subp_id == 296|taac_survey$subp_id == 297| taac_survey$subp_id == 298), "subcounty"] <- "Kihungya"
    
  
    # load in the crosswalk so that we know we have the identifier for each subcounty across the three relevant datasets
    load("/Users/alyssahuberts/Dropbox/TAAC Scorecard/5 Merge election data/1_data/walkthrough.Rdata")
    
    taac_survey$taac_survey_subcounty = as.character(taac_survey$subcounty)
    taac_survey <- left_join(taac_survey, walkthrough, by = "taac_survey_subcounty")
    taac_survey$polit_lc3_gave_name <- ifelse((taac_survey$polit_lc3_vote_name != ""& !is.na(taac_survey$polit_lc3_vote_name)),1,0)
    taac_survey$polit_lc5_gave_name <- ifelse((taac_survey$polit_lc5_vote_name != ""& !is.na(taac_survey$polit_lc5_vote_name)),1,0)
    
    # This allows us to identify the subcounty for all except the following subprojects:
      # 165 - Sam Adyebo is Rigbo but Kommaa is Ewanga
      # 288, 290 - malitabu mugenyi is butiaba, we can't identify this mulimba seremosi, but there are several seremoth's in biiso
      # 943- can't identify DILA or james ogwal
      # 950 - olweny bosco is aber because obua is kamdini
    # for now, change those to "unidentified"
    #taac_survey[(taac_survey$subp_id == 165| taac_survey$subp_id == 288| taac_survey$subp_id == 290| taac_survey$subp_id == 943| taac_survey$subp_id == 943|taac_survey$subp_id == 950), "subcounty"] <-"unidentified"
    
    taac_survey$polit_lc3_vote_name <- tolower(taac_survey$polit_lc3_vote_name)
    taac_survey$polit_lc5_vote_name <- tolower(taac_survey$polit_lc5_vote_name)
    
    
    # Load in 2016 candidates 
    cands_16 <- read.dta13("election/2016_LGCands.dta", nonint.factors = TRUE) %>%
      clean_names() %>%
      mutate(candidate_full_name = tolower(paste(surname,firstname, sep = " "))) %>% 
      select(scounty_name,scid, party, candidate_full_name, districtid,district_name,pos) %>% 
      rename(cands_16_scounty_name = scounty_name, cands_16_scid = scid, cands_16_district_id = districtid, cands_16_district_name =district_name, position = pos) %>% 
      unique()
    # write a  function to, for each observation in the survey, either pick the
    # best match within the subcounty/district, or say we can't identify the candidate
    # named
    all_matches <- tibble(resp_id = numeric(), polit_lc3_vote_name = character(), m_jw_3 = character(), d_jw_3 = numeric(), 
                          m_dl_3 = character(), d_dl_3 = numeric(), m_lcs_3 = character(), d_lcs_3 = numeric(), m_jc_3 = character(), d_jc_3 = numeric(),
                          all_options_3 = character(), polit_lc5_vote_name = character(),
                          m_jw_5 = character(), d_jw_5 = numeric(), 
                          m_dl_5 = character(), d_dl_5 = numeric(), m_lcs_5 = character(), d_lcs_5 = numeric(), m_jc_5 = character(), d_jc_5 = numeric(),
                          all_options_5 = character())
    
  
    check_match <- function(resp_id= 67707){
      # lc3
      targets_lc3 <- cands_16[(cands_16$cands_16_scid == as.character(taac_survey[taac_survey$resp_id == resp_id, "cands_16_scid"])&(cands_16$position == "LC3 Chairperson")),]
      if(length(targets_lc3$candidate_full_name) ==0){
        targets_lc3 <- tibble(candidate_full_name = NA)
        }
      matches <- tibble(resp_id = resp_id)
      matches$polit_lc3_vote_name <- taac_survey[taac_survey$resp_id == resp_id, "polit_lc3_vote_name"] %>%  pull()
      # jw 
        matches$m_jw_3 <- targets_lc3[amatch(matches$polit_lc3_vote_name, targets_lc3$candidate_full_name, method = "jw", maxDist = 1),"candidate_full_name"]
        matches$d_jw_3 <- stringdist(matches$polit_lc3_vote_name, matches$m_jw_3, method = "jw")
      # dl 
        matches$m_dl_3 <- targets_lc3[amatch(matches$polit_lc3_vote_name, targets_lc3$candidate_full_name, method = "dl",maxDist = 20),"candidate_full_name"]
        matches$d_dl_3 <- stringdist(matches$polit_lc3_vote_name, matches$m_dl_3, method = "dl")
      # lcs 
        matches$m_lcs_3 <- targets_lc3[amatch(matches$polit_lc3_vote_name, targets_lc3$candidate_full_name, method = "lcs",maxDist = 20),"candidate_full_name"]
        matches$d_lcs_3 <- stringdist(matches$polit_lc3_vote_name, matches$m_lcs_3, method = "lcs")
      # jaccard
        matches$m_jc_3 <- targets_lc3[amatch(matches$polit_lc3_vote_name, targets_lc3$candidate_full_name, method = "jaccard",maxDist = 1),"candidate_full_name"]
        matches$d_jc_3 <- stringdist(matches$polit_lc3_vote_name, matches$m_jc_3, method = "jaccard")
        
        matches$all_options_3 <- paste(targets_lc3$candidate_full_name, collapse = ", ")
      
      # lc5
        
        targets_lc5 <- cands_16[(cands_16$cands_16_district_id == as.character(taac_survey[taac_survey$resp_id == resp_id, "cands_16_district_id"])&(cands_16$position == "LC5 Chairperson")),]
        if(length(targets_lc5$candidate_full_name) ==0){
          targets_lc5 <- tibble(candidate_full_name = NA)
        }
        matches$polit_lc5_vote_name <- taac_survey[taac_survey$resp_id == resp_id, "polit_lc5_vote_name"] %>% pull()
        # jw 
        jw <- amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "jw", maxDist = 1)
        matches$m_jw_5 <- ifelse(!is.na(jw), targets_lc5[amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "jw", maxDist = 1),"candidate_full_name"], NA)
        matches$d_jw_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_jw_5, method = "jw")
        # dl 
        dl <- amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "dl",maxDist = 20)
        matches$m_dl_5 <- ifelse(!is.na(dl), targets_lc5[,"candidate_full_name"],NA)
        matches$d_dl_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_dl_5, method = "dl")
        # lcs 
        lcs <- amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "lcs",maxDist = 20)
        matches$m_lcs_5 <- ifelse(!is.na(lcs),targets_lc5[,"candidate_full_name"], NA)
        matches$d_lcs_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_lcs_5, method = "lcs")
        # jaccard
        jc <- amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "jaccard",maxDist = 1)
        matches$m_jc_5 <- ifelse(!is.na(jc),targets_lc5[jc,"candidate_full_name"],NA)
        matches$d_jc_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_lcs_5, method = "jaccard")
        matches$all_options_5 <- paste(targets_lc5$candidate_full_name, collapse = ", ")
        all_matches <<- bind_rows(all_matches,matches)
        }
    lapply(taac_survey$resp_id,check_match)
    
    # From here, we have matches, but we don't have a summary measure of how
    # good they are. How to pick the best match? 

    all_matches$match_id_3 <- ifelse(all_matches$polit_lc3_vote_name != ""& all_matches$d_jw_3 <.3, all_matches$m_jw_3,NA) 
    all_matches$match_id_3 <- ifelse(is.na(all_matches$match_id_3) & all_matches$polit_lc3_vote_name != ""& all_matches$d_lcs_3 <11, all_matches$m_dl_3,all_matches$match_id_3) 
    all_matches$match_id_3 <- ifelse(all_matches$polit_lc3_vote_name != ""& is.na(all_matches$match_id_3) & all_matches$d_dl_3 <6, all_matches$m_dl_3,all_matches$match_id_3) 
    all_matches$match_id_3 <- ifelse(all_matches$polit_lc3_vote_name != ""& is.na(all_matches$match_id_3) & all_matches$d_jw_3 <.3, all_matches$m_jw_3,all_matches$match_id_3) 
    all_matches$match_id_3 <- ifelse(all_matches$polit_lc3_vote_name != ""& is.na(all_matches$match_id_3) & all_matches$d_jc_3 <.3, all_matches$m_jc_3,all_matches$match_id_3) 
    
  
    # Manually correct the obvious failures of the algorithm 
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "yikii levi"|all_matches$polit_lc3_vote_name =="yiki levi"| all_matches$polit_lc3_vote_name == "yikii ben"), "dema levi yiki", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "sizza moses"|all_matches$polit_lc3_vote_name =="sizza"|all_matches$polit_lc3_vote_name =="ceaser moses"|all_matches$polit_lc3_vote_name =="ceaser"), "siisa moses nasilu", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "silver"), "ukadha muhammad silver", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "oyuru charles"), "oyuru geoffrey girang", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "otim peter"|all_matches$polit_lc3_vote_name =="otim bitek"|all_matches$polit_lc3_vote_name =="otim"|all_matches$polit_lc3_vote_name =="bitek otim"|all_matches$polit_lc3_vote_name =="betek"), "okot p. bitek ben", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "omairo micheal"|all_matches$polit_lc3_vote_name == "omairo"), "olupot omairo john michael", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "okello cirlo"), "maja cirlo okelu wange", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "lutinga"|all_matches$polit_lc3_vote_name =="lutinang."| all_matches$polit_lc3_vote_name =="ben okot."), "okot john ben lutinga", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "labeja dyel"), "labeja john bosco", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "kwigķiriza"), "kwikiriza geofrey", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "kilegule"), "kiragule isihaka", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "kilama ogun"), "wodacholi kilama fearless", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "kefa asia"), "ekaforu kefa", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "james wafwana"), "wafana james kadooli", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "iriso okoyo"| all_matches$polit_lc3_vote_name == "iriso"), "okoyo yosamu", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "ibrahim wamandi"), "wamandi pesajange ibrahim", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "horras awizia"), "awuzia horace", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "guttenberg"), "ogwang john gutenberg", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "febiano"|all_matches$polit_lc3_vote_name =="febian"|all_matches$polit_lc3_vote_name =="fabian kigenyi"| all_matches$polit_lc3_vote_name =="fabian"), "higenyi febiano hestete", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "febiano"|all_matches$polit_lc3_vote_name =="febian"|all_matches$polit_lc3_vote_name =="fabian kigenyi"), "higenyi febiano hestete", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "doctor ojara"), "ojara atkinson", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "denis auong."|all_matches$polit_lc3_vote_name =="denis achan"), "awon denis", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "cony omwong"|all_matches$polit_lc3_vote_name =="con omwong"), "omwony constantine", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "cleophas"), "bigiirwa cleophus", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "bosco odama"|all_matches$polit_lc3_vote_name == "bosco"), "odama john", all_matches$match_id_3)
    all_matches$match_id_3 <- ifelse(all_matches$polit_lc3_vote_name == "bony odongo", "odongo boniface", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "asia akodo"|all_matches$polit_lc3_vote_name=="asea geofrey"|all_matches$polit_lc3_vote_name == "akoro"|all_matches$polit_lc3_vote_name == "akolo"|all_matches$polit_lc3_vote_name == "akodo" ), "asiason godfrey akodo", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "ali okello"), "okello richard alii", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "abubakari peter"), "baiga abubakar", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "samuel ndaa"), "ndaa sam", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "okello cirillo"), "maja cirlo okelu wange", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "david ocaka"), "ocaka patrick", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "john ogwang"), "ogwang john gutenberg", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "mudoba james"), "hasahya mudoba", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "abubakari peteya"), "baiga abubakar", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "asea akoro"), "asiason godfrey akodo", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "ben okot"), "okot john ben lutinga", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "christopher ikaforo"), "ekaforu kefa", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "james ofana"), "wafana james kadooli", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "jerry apejua"), "candia jerry abejoa", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "kepher ekaforo"), "ekaforu kefa", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "lalobo otto"), "otto charles lalobo", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "mia bola"), "boola miya juma", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "mr ocan"), "ochan stephen", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "mr odongtoo"), "odong-too charles", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "mr olung"|all_matches$polit_lc3_vote_name =="mr.olung"), "olung julius peter", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "mr olung"), "olung julius peter", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "muhammed"), "ukadha muhammad silver", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "munilu"), "gudoi muniru ali mugamba", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "obong."), "obong charles okwera", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "okello wang"|all_matches$polit_lc3_vote_name =="okello wang amaza"|all_matches$polit_lc3_vote_name =="okeluwang"), "maja cirlo okelu wange", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "okumu kony."), "lakony francis okumu", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "paskali"), "mboineki pascal", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "peter mudoba"), "hasahya mudoba", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "silva"), "ukadha muhammad silver", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "tiki levi"), "dema levi yiki", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "umony kon"), "omwony constantine", all_matches$match_id_3)
     all_matches$match_id_3 <- ifelse((all_matches$polit_lc3_vote_name == "adebo samuel"| all_matches$polit_lc3_vote_name == "samuel adyebo"), "adyebo samuel", all_matches$match_id_3)
     
    # Same thing for LC5
     all_matches$match_id_5 <- ifelse(all_matches$polit_lc5_vote_name != ""& all_matches$d_jw_5 <.3, all_matches$m_jw_5,NA) 
     all_matches$match_id_5 <- ifelse(is.na(all_matches$match_id_5) & all_matches$polit_lc5_vote_name != ""& all_matches$d_lcs_5 <11, all_matches$m_dl_5,all_matches$match_id_5) 
     all_matches$match_id_5 <- ifelse(all_matches$polit_lc5_vote_name != ""& is.na(all_matches$match_id_5) & all_matches$d_dl_5 <6, all_matches$m_dl_5,all_matches$match_id_5) 
     all_matches$match_id_5 <- ifelse(all_matches$polit_lc5_vote_name != ""& is.na(all_matches$match_id_5) & all_matches$d_jw_5 <.3, all_matches$m_jw_5,all_matches$match_id_5) 
     all_matches$match_id_5 <- ifelse(all_matches$polit_lc5_vote_name != ""& is.na(all_matches$match_id_5) & all_matches$d_jc_5 <.3, all_matches$m_jc_5,all_matches$match_id_5) 
     
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "johnson okello"|all_matches$polit_lc5_vote_name == "johnson okello."|all_matches$polit_lc5_vote_name == "okello"), "okello denish johnson", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "okurut"), "okurut john michael", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "charles ntirehoki"), "ntairehoki charles amooti", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "apollo jaramogi"|all_matches$polit_lc5_vote_name == "apollo jaramongi"| all_matches$polit_lc5_vote_name =="jaramongi"| all_matches$polit_lc5_vote_name == "apollo jaramogi"|all_matches$polit_lc5_vote_name == "jaramogi" |all_matches$polit_lc5_vote_name == "apollo jaramogi"), "jaramogi apollo ollo", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "hassan nynya"), "nginya hassan said", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "david ebong"| all_matches$polit_lc5_vote_name == "david ebony bong"), "ebong david", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "mboizi"|all_matches$polit_lc5_vote_name == "athar mboizi"|all_matches$polit_lc5_vote_name == "mboizi kezekia"|all_matches$polit_lc5_vote_name == "bweaisi wako"|all_matches$polit_lc5_vote_name == "mbwezi"| all_matches$polit_lc5_vote_name =="wako mboizi"), "mboizi arthur waako", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "mugimba wilson"), "isingoma wilson mugimba kamanyire", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "mujasi"), "mujasi", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "adeya"|all_matches$polit_lc5_vote_name == "adea"), "adea nelson akar", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "egunyu"), "egunyu george michael", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "john ogwok"|all_matches$polit_lc5_vote_name == "mr.ogwok"), "ogwok john komakech", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "akia"|all_matches$polit_lc5_vote_name == "akia."| all_matches$polit_lc5_vote_name == "mr akiya"|all_matches$polit_lc5_vote_name == "akia mathew" ), "ochen mathew akiya", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "mulekhwa"), "mulekwa herbert padie", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "ogwok"|all_matches$polit_lc5_vote_name == "ogwok." ), "ogwok john komakech", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "waya"|all_matches$polit_lc5_vote_name == "wire richard"), "waya richard wanjala", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "wonazufu"| all_matches$polit_lc5_vote_name =="monzofu"|all_matches$polit_lc5_vote_name == "simon wananzovu"|all_matches$polit_lc5_vote_name == "simon wanasofu"|all_matches$polit_lc5_vote_name == "wananzofu"|all_matches$polit_lc5_vote_name == "wonasofu"), "wonanzofu simon peter", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "bernard mujjasi"|all_matches$polit_lc5_vote_name == "benard n mujasi"|all_matches$polit_lc5_vote_name == "benard mujasi"|all_matches$polit_lc5_vote_name == "bernard mujjasi"|all_matches$polit_lc5_vote_name == "bernard mujassi"), "mujasi masaba bernard elly", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "cosmas"|all_matches$polit_lc5_vote_name == "cosmos"), "byaruhanga cosmas", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "largo"), "oringa largo godfrey", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "largo"), "oringa largo godfrey", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "nabendde amlam"|all_matches$polit_lc5_vote_name == "nabende"|all_matches$polit_lc5_vote_name == "james nabendde"|all_matches$polit_lc5_vote_name =="james ndabenda" ), "nabende amuramu james", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "oola." ), "oola david dla marck", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "otobi godfrey" ), "orach godfrey otobi", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "richard wire" ), "waya richard wanjala", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "songor chelimo"|all_matches$polit_lc5_vote_name == "christopher chepkurui sonko" |all_matches$polit_lc5_vote_name == "christopher sonko" | all_matches$polit_lc5_vote_name =="cristopher sonko"|all_matches$polit_lc5_vote_name == "chebkuri songor" |all_matches$polit_lc5_vote_name ==  "chelimo songor cherotwo"|all_matches$polit_lc5_vote_name ==  "cristopher sonko"|all_matches$polit_lc5_vote_name == "songoi chemonges"|all_matches$polit_lc5_vote_name == "songor cherotwo"|all_matches$polit_lc5_vote_name == "sonko chepkurui"), "chekurui songhor christopher", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "akia mathew" ), "ochen mathew akiya", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "akia mathew"|all_matches$polit_lc5_vote_name == "akia methew"|all_matches$polit_lc5_vote_name == "akiya"|all_matches$polit_lc5_vote_name == "akiya martin"), "ochen mathew akiya", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "alenyo"|all_matches$polit_lc5_vote_name == "alenyo oo" ), "alenyo esrom william", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "atwom" ), "atwom denis opio", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "bob okaiye." ), "okae bob", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "geoffrey otopi" ), "orach godfrey otobi", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "hanssan nynya"|all_matches$polit_lc5_vote_name== "husna niya"|all_matches$polit_lc5_vote_name== "hussain niya"|all_matches$polit_lc5_vote_name== "hussain niya" |all_matches$polit_lc5_vote_name== "hussain niya."|all_matches$polit_lc5_vote_name== "hussain"|all_matches$polit_lc5_vote_name== "hussain niya"|all_matches$polit_lc5_vote_name=="hussain nyinya"), "nginya hassan said", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "harberton mulekwa" |all_matches$polit_lc5_vote_name == "herbat mulekwa"), "mulekwa herbert padie", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "innocent komakech" ), "kumakech f. innocent", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "john owok" ), "ogwok john komakech", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "kamwada" ), "kamwada william harvey", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "larogo"|all_matches$polit_lc5_vote_name == "larago" ), "oringa largo godfrey", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "leopard"|all_matches$polit_lc5_vote_name == "lonard opio." ), "oringa largo godfrey", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "mr ogwok"|all_matches$polit_lc5_vote_name == "mr. ogwok" ), "ogwok john komakech", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "mulekwa"), "mulekwa herbert padie", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "nyakua sam wadri"|all_matches$polit_lc5_vote_name == "nyangua"|all_matches$polit_lc5_vote_name == "wadri"), "wadri sam nyakua", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "odok"|all_matches$polit_lc5_vote_name == "odok peter"|all_matches$polit_lc5_vote_name == "simon peter odoch"), "ojok simon odoch", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "atobi"), "orach godfrey otobi", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "atube omach"), "orach godfrey otobi", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "isingoma mugimba"), "isingoma wilson mugimba kamanyirei", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "majinjachi paul"), "kapchemeiko paul machinjach", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "okojo opit"), "opit joseph okojo", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "okuruti"), "okurut john michael", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "otobi godfrey orac"|all_matches$polit_lc5_vote_name == "otopi"|all_matches$polit_lc5_vote_name =="otopi geoffrey"), " orach godfrey otobi", all_matches$match_id_5)
     all_matches$match_id_5 <- ifelse((all_matches$polit_lc5_vote_name == "paul kapchemyeko"), "kapchemeiko paul machinjach", all_matches$match_id_5)

    # Merge responses back onto TAAC 
    taac_survey <- left_join(taac_survey, all_matches[c("resp_id", "match_id_3", "match_id_5")], by ="resp_id")
    taac_survey[taac_survey$polit_lc3_gave_name==0,]$match_id_3<-NA
    taac_survey[taac_survey$polit_lc5_gave_name==0,]$match_id_5<-NA
    taac_survey$polit_lc3_matched <-ifelse(!is.na(taac_survey$match_id_3),1,0)
    taac_survey$polit_lc5_matched <-ifelse(!is.na(taac_survey$match_id_5),1,0)
    
    taac_survey <- taac_survey %>% rename(polit_lc3_matched_name = match_id_3,
                                          polit_lc5_matched_name = match_id_5)

############################   
# Link candidates to incumbency data
############################     
    
    # read in lc3
    winners_2011_LC3 <- read.csv("election/2011_SC_Chairperson_winners.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
    clean_names %>% 
    rename(district_id=x,  ea = x_1, scounty_id = x_2)
    winners_2011_LC3$district_id <- str_pad(winners_2011_LC3$district_id,3, side = "left",pad = "0")
    winners_2011_LC3$ea_id <- str_pad(winners_2011_LC3$ea,3, side = "left",pad = "0")
    winners_2011_LC3$scounty_id <- str_pad(winners_2011_LC3$scounty_id,3, side = "left",pad = "0")
    winners_2011_LC3$scid <- paste(winners_2011_LC3$district_id,winners_2011_LC3$ea_id, winners_2011_LC3$scounty_id, sep = "-")
    winners_2011_LC3 <-winners_2011_LC3 %>% 
      rename(incumbents_scid = scid, incumbents_scounty = scounty, incumbents_district_id = district_id, incumbents_district = district, incumbent_lc3_party = party)
    winners_2011_LC3$incumbent_lc3_name = tolower(paste(winners_2011_LC3$surname,  winners_2011_LC3$other_name, sep = " "))
    winners_2011_LC3$incumbent_lc3_name <- ifelse(winners_2011_LC3$incumbent_lc3_name == "asiason godfrey", "asiason godfrey akodo", winners_2011_LC3$incumbent_lc3_name)
    
    winners_2011_LC3 <- winners_2011_LC3 %>% select(incumbents_district, incumbents_scid,incumbents_scounty, incumbents_district,
                                                    incumbents_district_id, incumbent_lc3_name, incumbent_lc3_party)
    
    # read in lc5
    winners_2011_LC5 <- read.csv("election/2011_District_Chairperson_winners.csv", stringsAsFactors = FALSE) %>% clean_names()
    winners_2011_LC5$district_id <- str_pad(as.character(winners_2011_LC5$district_id),3, side = "left", pad = "0")
    winners_2011_LC5 <- winners_2011_LC5 %>% rename(incumbents_district_id = district_id, 
                                                    incumbents_district = district, 
                                                    incumbent_lc5_name = name,
                                                    incumbent_lc5_party = party )
    winners_2011_LC5$incumbent_lc5_name <- tolower(winners_2011_LC5$incumbent_lc5_name)
    incumbents <- left_join(winners_2011_LC3, winners_2011_LC5, by = c("incumbents_district_id", "incumbents_district"))
    
    # Match this incumbency data to candidacy data to create the variables is_incumbent, is_incumbent_party,  incumbent_ran,and incumbent_party_ran
    incumbents <- left_join(incumbents, walkthrough, by = c("incumbents_scid","incumbents_scounty", "incumbents_district_id", "incumbents_district" ))
    cands_16_lc3 <- left_join(cands_16[cands_16$position == "LC3 Chairperson",], incumbents, by = c("cands_16_scounty_name", "cands_16_scid", "cands_16_district_id", "cands_16_district_name"))
    # lc3 is incumbent
    cands_16_lc3$lc3_is_incumbent <- ifelse(  (stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "dl") <6|
                                           stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "lcs") <6|
                                           stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "jaccard") <.3|
                                           stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "jw") <.3),1,0)
    # manually correct the four districts where multiple candidates meet this criteria
    cands_16_lc3[(cands_16_lc3$candidate_full_name == "onyum patrick okwang"|
               cands_16_lc3$candidate_full_name == "gimei martin"|
               cands_16_lc3$candidate_full_name == "nangoyan peter lolimo"|
                cands_16_lc3$candidate_full_name == "odengeria albert okoboi" ),]$lc3_is_incumbent <-0
    #lc3  incumbent ran
    cands_16_lc3 <- cands_16_lc3 %>% group_by(cands_16_scid) %>% mutate(lc3_incumbent_ran = max(lc3_is_incumbent)) %>% ungroup()
    cands_16_lc3$lc3_is_incumbent <- ifelse(  (stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "dl") <6|
                                                 stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "lcs") <6|
                                                 stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "jaccard") <.3|
                                                 stringdist(cands_16_lc3$candidate_full_name, cands_16_lc3$incumbent_lc3_name, method = "jw") <.3),1,0)
    # lc3 is incumbent party 
    cands_16_lc3$lc3_is_incumbent_party <- ifelse(cands_16_lc3$party == cands_16_lc3$incumbent_lc3_party,1,0)
    cands_16_lc3 <- cands_16_lc3 %>% select("cands_16_scid","cands_16_scounty_name", "candidate_full_name", "party", "lc3_is_incumbent","lc3_is_incumbent_party" ,"lc3_incumbent_ran") %>% 
    rename(polit_lc3_matched_name = candidate_full_name, polit_lc3_party = party)
    
    # lc 5
    cands_16_lc5 <- cands_16[cands_16$position == "LC5 Chairperson",]
    incumbents_lc5 <- incumbents %>% group_by(cands_16_district_id, incumbent_lc5_name, incumbent_lc5_party) %>% tally()
    cands_16_lc5 <- left_join(cands_16_lc5, incumbents_lc5[,c("cands_16_district_id", "incumbent_lc5_name", "incumbent_lc5_party")], by = "cands_16_district_id")
    # lc5 is incumbent
    cands_16_lc5$lc5_is_incumbent <- ifelse(stringdist(cands_16_lc5$candidate_full_name, cands_16_lc5$incumbent_lc5_name, method = "dl") <6|
                                                                                     stringdist(cands_16_lc5$candidate_full_name, cands_16_lc5$incumbent_lc5_name, method = "lcs") <6|
                                                                                     stringdist(cands_16_lc5$candidate_full_name, cands_16_lc5$incumbent_lc5_name, method = "jaccard") <.3|
                                                                                     stringdist(cands_16_lc5$candidate_full_name, cands_16_lc5$incumbent_lc5_name, method = "jw") <.3,1,0)
    # lc5 incumbent ran
    cands_16_lc5<- cands_16_lc5 %>% group_by(cands_16_scid) %>% mutate(lc5_incumbent_ran = max(lc5_is_incumbent)) %>% ungroup()
    # lc5 is incumbent party 
    cands_16_lc5$lc5_is_incumbent_party <- ifelse(cands_16_lc5$party == cands_16_lc5$incumbent_lc5_party,1,0)
    cands_16_lc5<- cands_16_lc5 %>% select("candidate_full_name", "party", "lc5_is_incumbent","lc5_is_incumbent_party" ,"lc5_incumbent_ran") %>% 
      rename(polit_lc5_matched_name = candidate_full_name, polit_lc5 =party)
    
# Now merge the candidacy and incumbency data back to the TAAC survey 
  taac_survey <- left_join(taac_survey, cands_16_lc3, by= c("polit_lc3_matched_name", "cands_16_scid", "cands_16_scounty_name"))
  taac_survey <- left_join(taac_survey, cands_16_lc5, by= "polit_lc5_matched_name") 
  
taac_survey$missing_incumbent_data = ifelse(is.na(taac_survey$lc5_is_incumbent),1,0)
View(taac_survey[taac_survey$polit_lc3_matched ==1 & taac_survey$missing_incumbent_data ==1,])