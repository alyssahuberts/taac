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
  
  # work off the survey responses, which are the relevant subcounties for our purposes 
    # read in taac survey
    taac_survey <- read_dta("/Users/alyssahuberts/Dropbox/TAAC Scorecard/4 Data Analysis/2_data/1_rawdata/TAAC_final.dta") %>% 
      select(resp_id, DataSource_Endline, district, subcounty, polit_lc3_vote, polit_lc3_vote_conf, polit_lc3_vote_name, polit_lc5_vote, polit_lc5_vote_conf, polit_lc5_vote_name,polit_lc5_vote_name_u, polit_lc5_vote_full_name, sc_treat, region4) %>% 
    filter(DataSource_Endline ==1)
   taac_survey <-  taac_survey %>% filter(is.na(region4)| region4==0)
   taac_survey <-  taac_survey %>% filter(!is.na(sc_treat))
   taac_survey <- taac_survey %>% filter(district!= "Nakapiripirit"& 
                                           district!= "Napak"&
                                           district!= "Moroto" & 
                                           district != "Kotido" & 
                                           district!= "Kaabong")
    
    # load in the walkthrough so that we know we have the identifier for each subcounty across the three relevant datasets
    load("/Users/alyssahuberts/Dropbox/TAAC Scorecard/5 Merge election data/1_data/walkthrough.Rdata")
    
    # note that we report two subcounties (Namisuni/ Bulegeni, Simu/Sisiyi that
    # seem like they are combinations of two different subcounties in our
    # dataset. They each have multiple incumbents:
 
    # Namisuni Bulugeni: timothy nabendi (namisuni), lemji wolimba (bulegeni)
    #  Michael chuna buyi (Simu) JAMES WOFUBA NAMUZEKYE (sisiyi) 
    
    # We'll manually do those at the end: for now, drop them. 
    taac_survey <- taac_survey[(taac_survey$subcounty != "Namisuni/ Bulegeni" & taac_survey$subcounty != "Simu/Sisiyi"),]
  
    taac_survey$taac_survey_subcounty = as.character(taac_survey$subcounty)
    taac_survey <- left_join(taac_survey, walkthrough, by = "taac_survey_subcounty")
    taac_survey$polit_lc3_gave_name <- ifelse((taac_survey$polit_lc3_vote_name != ""& !is.na(taac_survey$polit_lc3_vote_name)),1,0)
    taac_survey$polit_lc5_gave_name <- ifelse((taac_survey$polit_lc5_vote_name != ""& !is.na(taac_survey$polit_lc5_vote_name)),1,0)
    
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
      matches$polit_lc3_vote_name <- taac_survey[taac_survey$resp_id == resp_id, "polit_lc3_vote_name"] %>% pull()
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
        matches$m_jw_5 <- targets_lc5[amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "jw", maxDist = 1),"candidate_full_name"]
        matches$d_jw_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_jw_3, method = "jw")
        # dl 
        matches$m_dl_5 <- targets_lc5[amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "dl",maxDist = 20),"candidate_full_name"]
        matches$d_dl_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_dl_5, method = "dl")
        # lcs 
        matches$m_lcs_5 <- targets_lc5[amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "lcs",maxDist = 20),"candidate_full_name"]
        matches$d_lcs_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_lcs_5, method = "lcs")
        # jaccard
        matches$m_jc_5 <- targets_lc5[amatch(matches$polit_lc5_vote_name, targets_lc5$candidate_full_name, method = "jaccard",maxDist = 1),"candidate_full_name"]
        matches$d_jc_5 <- stringdist(matches$polit_lc5_vote_name, matches$m_lcs_5, method = "jaccard")
        matches$all_options_5 <- paste(targets_lc5$candidate_full_name, collapse = ", ")
        all_matches <<- rbind(all_matches,matches)
        }
    lapply(taac_survey$resp_id,check_match)
    
    # From here, we have matches, but we don't have a summary measure of how
    # good they are. How to pick the best match? We could go back and do it
    # manually, but for now I'm going to impose the following rules:
      # If jw < .3, use that match. if not, 
        # if dl < 7, use that match. If not, 
          # if jc < .3, use that match
    all_matches$match_id <- ifelse(all_matches$d_jw_3 <.3, all_matches$m_jw_3,NA) 
    all_matches$match_id <- ifelse(is.na(all_matches$match_id) & all_matches$d_dl_3 <6, all_matches$m_dl_3,all_matches$match_id) 
    all_matches$match_id <- ifelse(is.na(all_matches$match_id) & all_matches$d_jc_3 <.3, all_matches$m_jc_3,all_matches$match_id) 
    
    # Merge responses back onto TAAC 
    taac_survey <- left_join(taac_survey, all_matches[c("resp_id", "match_lc3_bound", "match_lc5_bound")], by ="resp_id")
    taac_survey$polit_lc3_matched_name[taac_survey$polit_lc3_gave_name==1]<-NA
    taac_survey$polit_lc5_matched_name[taac_survey$polit_lc5_gave_name==1]<-NA
    
    taac_survey$polit_lc3_matched_name <-ifelse(!is.na(taac_survey$match_lc3_bound),1,0)
    taac_survey$polit_lc5_matched_name <-ifelse(!is.na(taac_survey$match_lc5_bound),1,0)
    

    ##################
    # Misreports: Check if the respondent reversed 
    # their LC3 and LC5 votes
    #################
    all_mismatches <- tibble(resp_id = numeric(), polit_lc3_vote_name = character(), match_lc3_bound = character(), match_lc3_best = character(), match_lc3_best_dist = numeric(),all_options_lc3 = character(),
                          polit_lc5_vote_name = character(), match_lc5_bound = character(), match_lc5_best = character(), match_lc5_best_dist = numeric(), all_options_lc5 = character())
    check_match <- function(resp_id= 67707){
      # lc5: did they name an lc3 from their subcounty as their lc5 choice?
      if(taac_survey[taac_survey$resp_id == resp_id, "cands_16_scid"] %in% cands_16$cands_16_scid){
        targets_lc5 <- cands_16[cands_16$cands_16_district_id == as.character(taac_survey[taac_survey$resp_id == resp_id, "cands_16_district_id"]),]
        mismatches <- tibble(resp_id = resp_id)
        mismatches$polit_lc5_vote_name <- taac_survey[taac_survey$resp_id == resp_id, "polit_lc5_vote_name"] %>% pull()
        mismatches$match_lc5_bound <- targets_lc5[amatch(taac_survey[taac_survey$resp_id == resp_id, "polit_lc5_vote_name"], targets_lc5$candidate_full_name, method = "jw", maxDist = .3),"candidate_full_name"]
        mismatches$match_lc5_best <- targets_lc3[amatch(taac_survey[taac_survey$resp_id == resp_id, "polit_lc3_vote_name"], targets_lc3$candidate_full_name, method = "jw", maxDist = 1),"candidate_full_name"]
        mismatches$match_lc5_best_dist <- min(stringdist(taac_survey[taac_survey$resp_id == resp_id, "polit_lc3_vote_name"], targets_lc3$candidate_full_name, method = "jw"),na.rm=TRUE)
        mismatches$all_options_lc3 <- paste(targets_lc3$candidate_full_name, collapse = ", ")
      }else{
        matches <- tibble(resp_id = resp_id,  polit_lc3_vote_name =NA,
                          match_lc3_bound = NA, match_lc3_best = NA, match_lc3_best_dist = NA, all_options_lc3= NA)
      }
      # lc3: did they name an lc5 from their district as their lc3 choice?
      if(taac_survey[taac_survey$resp_id == resp_id, "cands_16_district_id"]  %in% cands_16$cands_16_district_id){
        targets_lc5 <- cands_16[cands_16$cands_16_district_id == as.character(taac_survey[taac_survey$resp_id == resp_id, "cands_16_district_id"]),]
        matches$polit_lc5_vote_name <- taac_survey[taac_survey$resp_id == resp_id, "polit_lc5_vote_name"] %>% pull()
        matches$match_lc5_bound <- targets_lc5[amatch(taac_survey[taac_survey$resp_id == resp_id, "polit_lc5_vote_name"], targets_lc5$candidate_full_name, method = "jw", maxDist = .3),"candidate_full_name"]
        matches$match_lc5_best <- targets_lc5[amatch(taac_survey[taac_survey$resp_id == resp_id, "polit_lc5_vote_name"], targets_lc5$candidate_full_name, method = "jw", maxDist = 1),"candidate_full_name"]
        matches$match_lc5_best_dist <- min(stringdist(taac_survey[taac_survey$resp_id == resp_id, "polit_lc5_vote_name"], targets_lc5$candidate_full_name, method = "jw"),na.rm=TRUE)
        matches$all_options_lc5 <- paste(targets_lc5$candidate_full_name, collapse = ", ")
      }else{
        matches$polit_lc5_vote_name <-NA
        matches$match_lc5_bound <- NA
        matches$match_lc5_best <- NA
        matches$match_lc5_best_dist <- NA
        matches$all_options_lc5 <- NA
      }
      all_matches <<- bind_rows(all_matches, matches)
    }
    lapply( taac_survey$resp_id,check_match)
    
   
    
    # load in the incumbent's name 
    winners_2011_LC3 <- read.csv("election/2011_SC_Chairperson_winners.csv", stringsAsFactors = FALSE, header = TRUE) %>%
      clean_names %>%
      filter(status == "WINNER")
    # match walkthrough's formatting
    winners_2011_LC3 <- read.csv("election/2011_SC_Chairperson_winners.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
      clean_names %>% 
      rename(district_id=x,  ea = x_1, scounty_id = x_2)
    winners_2011_LC3$district_id <- str_pad(winners_2011_LC3$district_id,3, side = "left",pad = "0")
    winners_2011_LC3$ea_id <- str_pad(winners_2011_LC3$ea,3, side = "left",pad = "0")
    winners_2011_LC3$scounty_id <- str_pad(winners_2011_LC3$scounty_id,3, side = "left",pad = "0")
    winners_2011_LC3$scid <- paste(winners_2011_LC3$district_id,winners_2011_LC3$ea_id, winners_2011_LC3$scounty_id, sep = "-")
    winners_2011_LC3$incumbent <- paste(winners_2011_LC3$surname, winners_2011_LC3$other_name, sep = " ")
    incumbents_lc3 <- winners_2011_LC3 %>% 
      select(scid, party,incumbent) %>%
      rename(incumbents_scid = scid, incumbent_lc3_party = party, incumbent_lc3 = incumbent)
    
    # join the incumbent for each subcounty to the survey data using walkthrough
    taac_survey <-left_join(taac_survey, incumbents_lc3, by = "incumbents_scid")
  
    
    

  