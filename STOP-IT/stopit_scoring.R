library(dplyr)
library(rio)

########################################
##### STEP 1 - LOAD & PREPROCESS   #####
########################################
# Load data
rawdat <- read.csv(paste0("C:/Users/coren/Documents/PEPR/NEUROPSY-SCORES/STOP-IT/",
                          "input/001_stopit_trials_9d057487-ecf1-43c0-9edd-283ae3abb1d7.csv"), 
                   header = TRUE, na.strings = c("null"), stringsAsFactors = FALSE)

# Preprocess
dat <- rawdat %>%
  mutate(
    block_i = as.numeric(block_i),
    SSD = as.numeric(SSD),
    rt = as.numeric(rt)
  ) %>%
  # Remove practice
  filter(block_i > 0) %>%
  # We mimic what the authors recommend in the paper, not sure how the focus is identified
  # it is hardcoded in the JS ?
  filter(focus == 'focus', Fullscreen == 'yes')


###########################################################
##### STEP 2 - TRIAL CLASSIFICATION                   #####
###########################################################
dat <- dat %>%
  mutate(
    is_stop = ifelse(signal == "yes", 1, 0),
    is_go = ifelse(signal == "no", 1, 0),
    
    has_response = ifelse(response != "undefined", 1, 0),
    
    is_correct_go = ifelse(is_go == 1 & correct %in% c("true", "TRUE"), 1, 0),
    
    is_premature = ifelse(is_go == 1 & rt < 0, 1, 0)
  )


###########################################################
##### STEP 3 - FUNCTION TO CALCULATE INDICES          #####
###########################################################

calculate_stopit_indices <- function(data) {
  
  indices <- data %>%
    group_by(participantID) %>%
    do({
      df_sub <- .
      
      # --- 1. STOP SIGNAL TRIALS ---
      stop_trials <- df_sub %>% filter(is_stop == 1)
      Nstop <- nrow(stop_trials)
      
      # presp: probability of responding on a stop trial
      presp <- mean(stop_trials$has_response, na.rm = TRUE)
      
      # ssd: average stop-signal delay
      ssd <- round(mean(stop_trials$SSD))
      
      # usRT: RT on unsuccessful stop trials
      # Logic: subset response != undefined. Replace NA RTs with -250.
      us_trials <- stop_trials %>% filter(has_response == 1)
      usRT_vals <- us_trials$rt
      usRT_vals[is.na(usRT_vals)] <- -250
      usRT <- round(mean(usRT_vals))
      
      # --- 2. GO TRIALS ---
      go_trials <- df_sub %>% filter(is_go == 1)
      Ngo <- nrow(go_trials)
      
      # Go Response Trials (for RT stats)
      go_resp_trials <- go_trials %>% filter(has_response == 1)
      goRT_vals <- go_resp_trials$rt
      goRT_vals[is.na(goRT_vals)] <- -250
      
      # goRT_all: Mean RT on Go trials with response
      goRT_all <- round(mean(goRT_vals))
      
      # goRT_sd: SD of Go RTs
      goRT_sd <- round(sd(goRT_vals))
      
      # goRT_correct: Mean RT on CORRECT Go trials
      go_correct_trials <- go_trials %>% filter(is_correct_go == 1)
      goRT_correct <- round(mean(go_correct_trials$rt))
      
      # go_omission: Proportion of Go trials without a response
      go_omission <- 1 - (nrow(go_resp_trials) / Ngo)
      
      # go_error: Proportion of incorrect responses given a response occurred
      go_error <- 1 - (nrow(go_correct_trials) / nrow(go_resp_trials))
      
      # go_premature: Proportion of premature responses
      premature_trials <- go_trials %>% filter(rt < 0)
      go_premature <- nrow(premature_trials) / Ngo
      
      # --- 3. SSRT CALCULATION ---
      # Max Go RT (from trials with a response)
      max_go_rt <- max(go_resp_trials$rt)
      
      # Adjusted distribution: If no response, replace with max_go_rt. Else use rt.
      go_adj <- ifelse(go_trials$response == "undefined", max_go_rt, go_trials$rt)
      
      # Nth RT: Quantile at prob = presp (Type 6)
      nth <- as.vector(round(quantile(go_adj, probs = presp, type = 6)))
      
      # Final SSRT
      ssrt <- nth - ssd
      
      # --- 4. ADDITIONAL INDICES ---
      go_correct_rt <- go_correct_trials$rt
      
      goRT_median <- round(median(go_correct_rt, na.rm = TRUE))
      
      goRT_cv <- round((sd(go_correct_rt, na.rm = TRUE) / mean(go_correct_rt, na.rm = TRUE)) * 100, 2)
      
      go_lapses <- round((sum(go_correct_rt > 1000, na.rm = TRUE) / length(go_correct_rt)) * 100, 2)
      
      go_perseverations <- sum(go_resp_trials$rt < 200, na.rm = TRUE)
      
      m_rt <- mean(go_correct_rt, na.rm = TRUE)
      s_rt <- sd(go_correct_rt, na.rm = TRUE)
      go_outliers <- sum(go_correct_rt > (m_rt + 3 * s_rt) | go_correct_rt < (m_rt - 3 * s_rt), na.rm = TRUE)
      
      # Block slopes
      block_stats <- go_correct_trials %>%
        group_by(block_i) %>%
        summarise(
          mean_rt = mean(rt, na.rm = TRUE),
          se_rt = sd(rt, na.rm = TRUE) / sqrt(n()),
          .groups = 'drop'
        )
      
      mod_rt <- lm(mean_rt ~ block_i, data = block_stats)
      Slope_RT <- round(coef(mod_rt)[["block_i"]], 2)
      
      mod_se <- lm(se_rt ~ block_i, data = block_stats)
      Slope_SE <- round(coef(mod_se)[["block_i"]], 2)
      
      data.frame(
        presp = presp,
        ssd = ssd,
        ssrt = ssrt,
        usRT = usRT,
        goRT_all = goRT_all,
        goRT_correct = goRT_correct,
        goRT_sd = goRT_sd,
        go_omission = go_omission,
        go_error = go_error,
        go_premature = go_premature,
        Nstop = Nstop,
        Ngo = Ngo,
        subject = unique(df_sub$participantID),
        goRT_cv = goRT_cv,          
        go_lapses = go_lapses,      
        Slope_RT = Slope_RT,        
        Slope_SE = Slope_SE,        
        goRT_median = goRT_median,  
        go_perseverations = go_perseverations, 
        go_outliers = go_outliers,  
        stringsAsFactors = FALSE
      )
    }) %>%
    ungroup() %>%
    select(-participantID)
  
  return(indices)
}

# Calculate indices
indices_full <- calculate_stopit_indices(dat)

# Export to Excel
export(indices_full, paste0("C:/Users/coren/Documents/PEPR/NEUROPSY-SCORES/STOP-IT/",
                            "output/001_manual.xlsx"), overwrite = TRUE)

