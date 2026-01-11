library(tidyr)
library(dplyr)
library(readxl)
library(rio)

########################################
##### STEP 1 - LOAD & PREPROCESS   #####
########################################
rawdat <- read_excel("input-ANT.xlsx") 
dat <- rawdat %>%
  mutate(SlideTarget.RT = as.numeric(as.character(rawdat$SlideTarget.RT)),
         cond = paste0(FlankerType, "_", WarningType),
         # if slidetargetRT == 0 -> no résponse
         RT = ifelse(SlideTarget.RT == 0, NA, SlideTarget.RT)) %>%
  # Block 1 = training
  filter(Block != 1) 

# min & max
TRmin = 200 
TRmax = 1700


###########################################################
##### STEP 2 - OUTLIER DETECTION & HIERARCHY          #####
###########################################################
# Calculating conditional Means/SDs for 3SD pruning
dat <- dat %>%
  group_by(Subject, cond) %>%
  mutate(
    MEAN_RT_COND = mean(RT[SlideTarget.ACC == 1], na.rm=TRUE),
    SD_RT_COND = sd(RT[SlideTarget.ACC == 1], na.rm=TRUE)
  ) %>%
  ungroup()

dat <- dat %>%
  mutate(
    # Hierarchy of errors/outliers
    is_omission = ifelse(SlideTarget.RT == 0, 1, 0),
    is_perseveration = ifelse(SlideTarget.RT > 0 & SlideTarget.RT < TRmin, 1, 0),
    is_too_slow = ifelse(SlideTarget.RT > TRmax, 1, 0),
    
    # 3SD outlier (only applied if not already an error/timeout)
    is_outlier = case_when(
      is_omission == 1 | is_perseveration == 1 | is_too_slow == 1 ~ 0,
      RT >= (MEAN_RT_COND + 3 * SD_RT_COND) ~ 1,
      RT <= (MEAN_RT_COND - 3 * SD_RT_COND) ~ 1,
      TRUE ~ 0
    ),
    
    is_valid_timing = ifelse(RT >= TRmin & RT <= TRmax, 1, 0),
    
    # Commission error
    is_wrong_response = case_when(
      SlideTarget.ACC == 0 & is_valid_timing == 1 & is_outlier == 0 ~ 1,
      TRUE ~ 0
    ),
    
    # Valid hit
    is_hit = case_when(
      SlideTarget.ACC == 1 & is_valid_timing == 1 & is_outlier == 0 ~ 1,
      TRUE ~ 0
    )
  )


###########################################################
##### STEP 3 - FUNCTION TO CALCULATE INDICES          #####
###########################################################

calculate_all_ant_indices <- function(data, prefix = "ANT_") {
  
  # 1. Global performance (speed & stability)
  global_stats <- data %>%
    filter(is_hit == 1) %>%
    group_by(Subject) %>%
    summarise(
      RT_Mean = mean(RT, na.rm=TRUE),
      RT_Median = median(RT, na.rm=TRUE),
      RT_SD = sd(RT, na.rm=TRUE), 
      RT_CV = (sd(RT, na.rm=TRUE) / mean(RT, na.rm=TRUE)) * 100, 
      .groups = 'drop'
    )
  
  # 2. LAPSE ANALYSIS 
  # Arbritrary, impossible to find a solid ref for the threshold
  # We define a lapse as a correct response > 1000ms
  # Umar -> possible to change later?
  lapse_stats <- data %>%
    filter(is_hit == 1) %>%
    group_by(Subject) %>%
    summarise(
      # Percentage of hits that are lapses 
      Lapses = (sum(RT > 1000, na.rm=TRUE) / n()) * 100,
      .groups = 'drop'
    )
  
  # 3. Network Components (RT & Acc)
  rt_means <- data %>%
    filter(is_hit == 1) %>%  # RT only from correct responses
    group_by(Subject) %>%
    summarise(
      RT_No = mean(RT[WarningType == "no"], na.rm=TRUE),
      RT_Double = mean(RT[WarningType == "double"], na.rm=TRUE),
      RT_Center = mean(RT[WarningType == "center"], na.rm=TRUE),
      RT_Spatial = mean(RT[WarningType == "tloc"], na.rm=TRUE),
      RT_Cong = mean(RT[FlankerType == "congruent"], na.rm=TRUE),
      RT_Incong = mean(RT[FlankerType == "incongruent"], na.rm=TRUE),
      .groups = 'drop'
    )
  
  acc_means <- data %>%
    filter(is_valid_timing == 1 & is_outlier == 0) %>%  # All valid trials
    group_by(Subject) %>%
    summarise(
      Acc_No = mean(SlideTarget.ACC[WarningType == "no"], na.rm=TRUE),
      Acc_Double = mean(SlideTarget.ACC[WarningType == "double"], na.rm=TRUE),
      Acc_Center = mean(SlideTarget.ACC[WarningType == "center"], na.rm=TRUE),
      Acc_Spatial = mean(SlideTarget.ACC[WarningType == "tloc"], na.rm=TRUE),
      Acc_Cong = mean(SlideTarget.ACC[FlankerType == "congruent"], na.rm=TRUE),
      Acc_Incong = mean(SlideTarget.ACC[FlankerType == "incongruent"], na.rm=TRUE),
      .groups = 'drop'
    )
  
  # Combine and calculate network scores
  cond_means <- rt_means %>%
    left_join(acc_means, by = "Subject") %>%
    mutate(
      # Network scores (RT)
      Alerting_RT = RT_No - RT_Double,
      Orienting_RT = RT_Center - RT_Spatial,
      Conflict_RT = RT_Incong - RT_Cong,
      
      # Network scores (accuracy)
      Alerting_Acc = Acc_Double - Acc_No,
      Orienting_Acc = Acc_Spatial - Acc_Center,
      Conflict_Acc = Acc_Cong - Acc_Incong
    )
  
  # 4. vigilance / block effects
  block_stats <- data %>%
    filter(is_hit == 1) %>%
    group_by(Subject, Block) %>%
    summarise(
      Mean_Block = mean(RT, na.rm=TRUE),
      SE_Block = sd(RT, na.rm=TRUE) / sqrt(n()), 
      .groups = "drop"
    ) 
  
  vigilance <- block_stats %>%
    group_by(Subject) %>%
    do({
      mod_rt <- lm(Mean_Block ~ Block, data = .) 
      mod_se <- lm(SE_Block ~ Block, data = .) 
      data.frame(
        Slope_RT = coef(mod_rt)[["Block"]],
        Slope_SE = coef(mod_se)[["Block"]],
        Var_SE = sd(.$SE_Block, na.rm=TRUE)
      )
    }) %>%
    ungroup()
  
  # 5. error counts
  errors <- data %>%
    group_by(Subject) %>%
    summarise(
      n_trials = n(),
      N_Hits = sum(is_hit),
      N_Omission = sum(is_omission),
      N_Commission = sum(is_wrong_response),
      N_Perseveration = sum(is_perseveration),
      N_Outliers = sum(is_outlier),
      Acc_Overall = sum(is_hit, na.rm=TRUE) / n(),
      .groups = 'drop'
    )
  
  # 6. merge All
  all_indices <- global_stats %>%
    left_join(lapse_stats, by = "Subject") %>%   
    left_join(cond_means %>% select(Subject, contains("Alerting"), contains("Orienting"), contains("Conflict")), by = "Subject") %>%
    left_join(vigilance, by = "Subject") %>%
    left_join(errors, by = "Subject")
  
  # add prefix
  all_indices <- all_indices %>%
    rename_with(~ paste0(prefix, .), -Subject)
  
  return(all_indices)
}
############################################
##### STEP 4 - CALCULATE INDICES       #####
############################################

# full dataset
indices_full <- calculate_all_ant_indices(dat, prefix = "ANT_")


# export
export(indices_full,
       "output-ANT.xlsx",
       overwrite = TRUE)

