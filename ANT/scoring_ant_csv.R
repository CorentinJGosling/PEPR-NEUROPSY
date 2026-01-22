library(dplyr)
library(tidyr)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Safe wrappers for statistics (handle edge cases)
safe_mean <- function(x) if (length(x) == 0) 0 else mean(x, na.rm = TRUE)
safe_median <- function(x) if (length(x) == 0) 0 else median(x, na.rm = TRUE)
safe_sd <- function(x) if (length(x) < 2) 0 else sd(x, na.rm = TRUE)
safe_se <- function(x) if (length(x) == 0) 0 else safe_sd(x) / sqrt(length(x))
safe_min <- function(x) if (length(x) == 0) 0 else min(x, na.rm = TRUE)
safe_max <- function(x) if (length(x) == 0) 0 else max(x, na.rm = TRUE)

# Linear regression slope
linear_slope <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(0)
  if (length(x) < 2) return(0)
  coef(lm(y ~ x))[2]
}

# Round to 2 decimal places (pepr format)
round2 <- function(x) round(x, 2)

# ============================================================================
# RT THRESHOLDS
# ============================================================================

RT_MIN <- 200    # Perseveration threshold
RT_MAX <- 1700   # Too-slow threshold
RT_LAPSE <- 1000 # Lapse threshold
OUTLIER_SD <- 3  # Standard deviations for outlier detection

# ============================================================================
# STEP 1: LOAD & PREPROCESS
# ============================================================================

# Get all CSV files in input folder
input_folder <- "input"
csv_files <- list.files(input_folder, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files found in input/ folder")
}

# Read and combine all files
rawdat <- lapply(csv_files, function(file) {
  # Extract subject ID from filename (e.g., "001" from "001 - 2026-01-16 - Data.csv")
  filename <- basename(file)
  subject_id <- sub("^(\\d+).*", "\\1", filename)
  
  # Read CSV
  d <- read.csv(file, stringsAsFactors = FALSE)
  
  # Add subject ID (keep as character to preserve leading zeros)
  d$Subject <- as.character(subject_id)
  
  return(d)
}) %>% bind_rows()

# Preprocess
dat <- rawdat %>%
  mutate(
    # Map CueType numeric to labels (Standard Fan et al., 2002 mapping)
    cue_type_label = case_when(
      CueType == 1 ~ "no",
      CueType == 2 ~ "center",
      CueType == 3 ~ "double",   
      CueType == 4 ~ "spatial", 
      TRUE ~ as.character(CueType)
    ),
    # Rename for consistency
    target_type = Congruency,
    # Practice flag
    is_practice = (block == 0),
    # Create condition label
    condition = paste0(target_type, "_", cue_type_label),
    # Normalize RT: 0 or "None" response -> NA
    RT = ifelse(RT == 0 | Response == "None", NA, as.numeric(RT))
  ) %>%
  # Remove practice trials
  filter(!is_practice)

# ============================================================================
# STEP 2: TRIAL CLASSIFICATION
# ============================================================================

# Calculate per-condition stats for 3SD outlier detection
# Uses ONLY correct trials with valid timing (200-1700ms)
condition_stats <- dat %>%
  filter(Correct == 1, !is.na(RT), RT >= RT_MIN, RT <= RT_MAX) %>%
  group_by(Subject, condition) %>%
  summarise(
    cond_mean = safe_mean(RT),
    cond_sd = safe_sd(RT),
    .groups = 'drop'
  )

# Join condition stats and apply hierarchical classification
dat <- dat %>%
  left_join(condition_stats, by = c("Subject", "condition")) %>%
  mutate(
    # Hierarchy of errors (checked in order)
    is_omission = (is.na(RT) | RT == 0),
    is_perseveration = (!is.na(RT) & RT > 0 & RT < RT_MIN),
    is_too_slow = (!is.na(RT) & RT > RT_MAX),
    is_valid_timing = (!is.na(RT) & RT >= RT_MIN & RT <= RT_MAX),
    
    # 3SD outlier detection (only for correct + valid timing)
    is_outlier = case_when(
      Correct == 0 | !is_valid_timing | is.na(cond_mean) ~ FALSE,
      RT >= (cond_mean + OUTLIER_SD * cond_sd) ~ TRUE,
      RT <= (cond_mean - OUTLIER_SD * cond_sd) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # 2SD outlier for reporting (only for correct + valid timing)
    is_outlier_2sd = case_when(
      Correct == 0 | !is_valid_timing | is.na(cond_mean) ~ FALSE,
      RT >= (cond_mean + 2 * cond_sd) ~ TRUE,
      RT <= (cond_mean - 2 * cond_sd) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Final classifications
    is_wrong_response = (Correct == 0 & is_valid_timing & !is_outlier),
    is_hit = (Correct == 1 & is_valid_timing & !is_outlier)
  )

# ============================================================================
# STEP 3: FUNCTION TO CALCULATE INDICES
# ============================================================================

calculate_ant_indices <- function(data) {

  # Create filtered datasets for analysis
  # Note: 'data' here is already filtered for practice trials in Step 1

  # Process each subject
  results <- data %>%
    group_by(Subject) %>%
    group_split() %>%
    lapply(function(subj_data) {

      subject_id <- unique(subj_data$Subject)
      subj_hits <- subj_data %>% filter(is_hit)
      subj_valid <- subj_data %>% filter(is_valid_timing, !is_outlier)

      # =======================================================================
      # A. OVERALL STATISTICS (from hits only)
      # =======================================================================

      overall_stats <- subj_hits %>%
        summarise(
          mean.all = round2(safe_mean(RT)),
          med.all = round2(safe_median(RT)),
          sd.all = round2(safe_sd(RT)),
          min.all = round2(safe_min(RT)),
          max.all = round2(safe_max(RT)),
          mean.RT.correct = round2(safe_mean(RT)),
          sd.RT.correct = round2(safe_sd(RT))
        )

      # Overall accuracy (hits / all test trials)
      n_test_trials <- nrow(subj_data)
      n_hits <- nrow(subj_hits)
      pc.all <- round2(ifelse(n_test_trials > 0, (n_hits / n_test_trials) * 100, 0))
      e.all <- round2(ifelse(n_test_trials > 0, ((n_test_trials - n_hits) / n_test_trials) * 100, 0))

      # =======================================================================
      # B. NETWORK EFFECTS - RT-based (from hits only)
      # =======================================================================

      rt_by_cue <- subj_hits %>%
        group_by(cue_type_label) %>%
        summarise(mean_rt = safe_mean(RT), .groups = 'drop')

      rt_by_flanker <- subj_hits %>%
        group_by(target_type) %>%
        summarise(mean_rt = safe_mean(RT), .groups = 'drop')

      # Extract condition means
      nocue <- round2(rt_by_cue$mean_rt[rt_by_cue$cue_type_label == "no"])
      double <- round2(rt_by_cue$mean_rt[rt_by_cue$cue_type_label == "double"])
      centre <- round2(rt_by_cue$mean_rt[rt_by_cue$cue_type_label == "center"])
      spatial <- round2(rt_by_cue$mean_rt[rt_by_cue$cue_type_label == "spatial"])

      cong <- round2(rt_by_flanker$mean_rt[rt_by_flanker$target_type == "congruent"])
      incong <- round2(rt_by_flanker$mean_rt[rt_by_flanker$target_type == "incongruent"])

      # Handle missing values
      # if (length(nocue) == 0) nocue <- 0
      # if (length(double) == 0) double <- 0
      # if (length(centre) == 0) centre <- 0
      # if (length(spatial) == 0) spatial <- 0
      # if (length(cong) == 0) cong <- 0
      # if (length(incong) == 0) incong <- 0

      # Calculate network effects (RT)
      alert <- round2(nocue - double)
      orient <- round2(centre - spatial)
      conflict <- round2(incong - cong)

      # =======================================================================
      # C. NETWORK EFFECTS - Accuracy-based (from valid trials)
      # =======================================================================

      acc_by_cue <- subj_valid %>%
        group_by(cue_type_label) %>%
        summarise(acc = safe_mean(Correct), .groups = 'drop')

      acc_by_flanker <- subj_valid %>%
        group_by(target_type) %>%
        summarise(acc = safe_mean(Correct), .groups = 'drop')

      # Extract accuracies
      acc_no <- acc_by_cue$acc[acc_by_cue$cue_type_label == "no"]
      acc_double <- acc_by_cue$acc[acc_by_cue$cue_type_label == "double"]
      acc_center <- acc_by_cue$acc[acc_by_cue$cue_type_label == "center"]
      acc_spatial <- acc_by_cue$acc[acc_by_cue$cue_type_label == "spatial"]

      acc_cong <- acc_by_flanker$acc[acc_by_flanker$target_type == "congruent"]
      acc_incong <- acc_by_flanker$acc[acc_by_flanker$target_type == "incongruent"]

      # Handle missing values
      # if (length(acc_no) == 0) acc_no <- 0
      # if (length(acc_double) == 0) acc_double <- 0
      # if (length(acc_center) == 0) acc_center <- 0
      # if (length(acc_spatial) == 0) acc_spatial <- 0
      # if (length(acc_cong) == 0) acc_cong <- 0
      # if (length(acc_incong) == 0) acc_incong <- 0

      # Calculate network effects (Accuracy) - not used in output but calculated
      alerting_acc <- round2((acc_double - acc_no) * 100)
      orienting_acc <- round2((acc_spatial - acc_center) * 100)
      executive_control_acc <- round2((acc_cong - acc_incong) * 100)

      # =======================================================================
      # D. ERROR RATES BY CONDITION
      # =======================================================================

      # Error rates by cue type
      err_by_cue <- subj_valid %>%
        group_by(cue_type_label) %>%
        summarise(err_rate = (1 - safe_mean(Correct)) * 100, .groups = 'drop')

      e.nocue <- round2(err_by_cue$err_rate[err_by_cue$cue_type_label == "no"])
      e.double <- round2(err_by_cue$err_rate[err_by_cue$cue_type_label == "double"])
      e.centre <- round2(err_by_cue$err_rate[err_by_cue$cue_type_label == "center"])
      e.spatial <- round2(err_by_cue$err_rate[err_by_cue$cue_type_label == "spatial"])

      # if (length(e.nocue) == 0) e.nocue <- 0
      # if (length(e.double) == 0) e.double <- 0
      # if (length(e.centre) == 0) e.centre <- 0
      # if (length(e.spatial) == 0) e.spatial <- 0

      # Error rates by flanker type
      err_by_flanker <- subj_valid %>%
        group_by(target_type) %>%
        summarise(err_rate = (1 - safe_mean(Correct)) * 100, .groups = 'drop')

      e.cong <- round2(err_by_flanker$err_rate[err_by_flanker$target_type == "congruent"])
      e.incong <- round2(err_by_flanker$err_rate[err_by_flanker$target_type == "incongruent"])

      if (length(e.cong) == 0) e.cong <- 0
      if (length(e.incong) == 0) e.incong <- 0

      # =======================================================================
      # E. CONDITION-SPECIFIC STATISTICS (8 conditions)
      # =======================================================================

      # Create condition labels: C1T1 = CueType 1 × Congruent, C1T2 = CueType 1 × Incongruent
      cond_stats_hits <- subj_hits %>%
        mutate(
          target_code = ifelse(target_type == "congruent", "1", "2"),
          cond_label = paste0("C", CueType, "T", target_code)
        ) %>%
        group_by(cond_label) %>%
        summarise(
          mean_rt = round2(safe_mean(RT)),
          median_rt = round2(safe_median(RT)),
          n_hits = n(),
          .groups = 'drop'
        )

      cond_stats_valid <- subj_valid %>%
        mutate(
          target_code = ifelse(target_type == "congruent", "1", "2"),
          cond_label = paste0("C", CueType, "T", target_code)
        ) %>%
        group_by(cond_label) %>%
        summarise(
          n_valid = n(),
          .groups = 'drop'
        )

      # Merge and calculate accuracy
      cond_stats <- expand.grid(
        CueType = 1:4,
        target_code = c("1", "2"),
        stringsAsFactors = FALSE
      ) %>%
        mutate(cond_label = paste0("C", CueType, "T", target_code)) %>%
        left_join(cond_stats_hits, by = "cond_label") %>%
        left_join(cond_stats_valid, by = "cond_label") %>%
        mutate(
          n_hits = ifelse(is.na(n_hits), 0, n_hits),
          n_valid = ifelse(is.na(n_valid), 0, n_valid),
          mean_rt = ifelse(is.na(mean_rt), 0, mean_rt),
          median_rt = ifelse(is.na(median_rt), 0, median_rt),
          accuracy = ifelse(n_valid > 0, round2((n_hits / n_valid) * 100), 0)
        )

      # Extract as named vectors
      med_cond <- setNames(cond_stats$median_rt, paste0("med.", cond_stats$cond_label))
      mean_cond <- setNames(cond_stats$mean_rt, paste0("mean.", cond_stats$cond_label))
      pc_cond <- setNames(cond_stats$accuracy, paste0("pc.", cond_stats$cond_label))

      # =======================================================================
      # F. VIGILANCE / BLOCK EFFECTS (from hits, excluding practice block 0)
      # =======================================================================

      block_stats <- subj_hits %>%
        group_by(block) %>%
        summarise(
          mean_rt = safe_mean(RT),
          se = safe_se(RT),
          .groups = 'drop'
        )

      if (nrow(block_stats) > 0) {
        hits_rt_block_change <- round2(linear_slope(block_stats$block, block_stats$mean_rt))
        hits_se_block_change <- round2(linear_slope(block_stats$block, block_stats$se))
        hits_rt_se_variability <- round2(safe_sd(block_stats$se))
      } else {
        hits_rt_block_change <- 0
        hits_se_block_change <- 0
        hits_rt_se_variability <- 0
      }

      # Overall standard error of the mean RT (across all hits)
      hits_rt_se <- round2(safe_se(subj_hits$RT))

      # =======================================================================
      # G. ERROR CLASSIFICATION COUNTS
      # =======================================================================

      omitted_responses_count <- sum(subj_data$is_omission)
      impulsive_responses_count <- sum(subj_data$is_perseveration)
      wrong_responses_count <- sum(subj_data$is_wrong_response)
      outliers_3sd <- sum(subj_data$is_outlier)
      outliers_2sd <- sum(subj_data$is_outlier_2sd)

      # =======================================================================
      # H. DEMOGRAPHICS & METADATA
      # =======================================================================

      # Extract from first row 
      first_row <- subj_data[1, ]
      uniqueID <- ifelse(is.null(first_row$uniqueID) || first_row$uniqueID == "undefined", "", first_row$uniqueID)
      Age <- ifelse(is.null(first_row$age) || first_row$age == "", "", first_row$age)
      Sex <- ifelse(is.null(first_row$sex) || first_row$sex == "", "", first_row$sex)
      Group <- ifelse(is.null(first_row$group) || first_row$group == "", "", first_row$group)
      ANTdate <- ifelse(is.null(first_row$Date) || first_row$Date == "", "", first_row$Date)
      targFile <- ifelse(is.null(first_row$targFile) || first_row$targFile == "", "", first_row$targFile)

      # =======================================================================
      # I. ASSEMBLE RESULTS FOR THIS SUBJECT
      # =======================================================================

      result <- data.frame(
        # Demographics
        uniqueID = uniqueID,
        studyID = "",
        ANTversion = "0.1",
        targFile = targFile,
        ANTdate = ANTdate,
        ANTtime = "",
        SessionDur = "",
        Session = "",
        Age = Age,
        Sex = Sex,
        Group = Group,

        # Trial counts
        ANT.N = nrow(subj_hits),

        # Overall stats
        med.all = overall_stats$med.all,
        mean.all = overall_stats$mean.all,
        sd.all = overall_stats$sd.all,
        min.all = overall_stats$min.all,
        max.all = overall_stats$max.all,

        # Network effects
        alert = alert,
        orient = orient,
        conflict = conflict,

        # Overall accuracy
        pc.all = pc.all,
        e.all = e.all,

        # RT by cue type
        nocue = nocue,
        double = double,
        centre = centre,
        spatial = spatial,

        # RT by flanker type
        cong = cong,
        incong = incong,

        # Condition medians
        med.C1T1 = med_cond["med.C1T1"],
        med.C1T2 = med_cond["med.C1T2"],
        med.C2T1 = med_cond["med.C2T1"],
        med.C2T2 = med_cond["med.C2T2"],
        med.C3T1 = med_cond["med.C3T1"],
        med.C3T2 = med_cond["med.C3T2"],
        med.C4T1 = med_cond["med.C4T1"],
        med.C4T2 = med_cond["med.C4T2"],

        # Condition means
        mean.C1T1 = mean_cond["mean.C1T1"],
        mean.C1T2 = mean_cond["mean.C1T2"],
        mean.C2T1 = mean_cond["mean.C2T1"],
        mean.C2T2 = mean_cond["mean.C2T2"],
        mean.C3T1 = mean_cond["mean.C3T1"],
        mean.C3T2 = mean_cond["mean.C3T2"],
        mean.C4T1 = mean_cond["mean.C4T1"],
        mean.C4T2 = mean_cond["mean.C4T2"],

        # Error rates by cue
        e.nocue = e.nocue,
        e.double = e.double,
        e.centre = e.centre,
        e.spatial = e.spatial,

        # Error rates by flanker
        e.incong = e.incong,
        e.cong = e.cong,

        # Accuracy by condition
        pc.C1T1 = pc_cond["pc.C1T1"],
        pc.C1T2 = pc_cond["pc.C1T2"],
        pc.C2T1 = pc_cond["pc.C2T1"],
        pc.C2T2 = pc_cond["pc.C2T2"],
        pc.C3T1 = pc_cond["pc.C3T1"],
        pc.C3T2 = pc_cond["pc.C3T2"],
        pc.C4T1 = pc_cond["pc.C4T1"],
        pc.C4T2 = pc_cond["pc.C4T2"],

        # Correct RT stats
        mean.RT.correct = overall_stats$mean.RT.correct,
        sd.RT.correct = overall_stats$sd.RT.correct,

        # Error counts
        omitted_responses_count = omitted_responses_count,
        impulsive_responses_count = impulsive_responses_count,
        outliers_3sd = outliers_3sd,
        outliers_2sd = outliers_2sd,

        # Vigilance
        hits_rt_se_variability = hits_rt_se_variability,
        hits_rt_se = hits_rt_se,
        wrong_responses_count = wrong_responses_count,
        hits_rt_block_change = hits_rt_block_change,
        hits_se_block_change = hits_se_block_change,

        stringsAsFactors = FALSE
      )

      # Add subject ID (keep as character to preserve leading zeros)
      result$Subject <- as.character(subject_id)

      return(result)
    })

  # Combine all subject results
  final_results <- bind_rows(results)

  return(final_results)
}

# ============================================================================
# EXECUTION
# ============================================================================

# Calculate indices
results <- calculate_ant_indices(dat)

# Export results
output_file <- "output-ANT-csv.csv"
results$Subject <- as.character(results$Subject)
write.csv(results, output_file, row.names = FALSE, na = "")
