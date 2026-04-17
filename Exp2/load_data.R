# ==============================================================================
# load_data.R
# Purpose: Import raw experimental data, clean variables, and assign groups.
# Author: Gemini Assistant (modified for user's specific experimental structure)
# ==============================================================================

# --- Load required libraries ---
library(tidyverse)
library(readxl)

# --- 1. Data Import ---
# Note: Ensure Data_exp2.xlsx is in the same working directory
raw_df <- read_excel("Data_exp2.xlsx")

# --- 2. Data Cleaning and Preprocessing ---
df <- raw_df %>%
  # Rename the prediction category column for clarity
  rename(Category = PredHominess) %>%
  
  # Ensure Participant ID is numeric (handles leading zeros like '0016')
  # Using as.character first prevents issues with factor underlying values
  mutate(Participant = as.numeric(as.character(Participant))) %>%
  
  # Assign Group based on Participant ID (ASD: 1-23, Control: 24+)
  mutate(Group = if_else(Participant <= 23, "ASD", "Control")) %>%
  
  # Convert grouping variables to factors for statistical modeling (LMM/t-test)
  mutate(across(c(Category, Group), as.factor)) %>%
  
  # Calculate overall Mean Rating across all 5 aesthetic dimensions for each trial
  mutate(mean_rating = rowMeans(
    select(., Hominess, Naturalness, Personalness, Beauty, Approachability), 
    na.rm = TRUE
  ))

# --- 3. Sanity Check (English console output) ---
cat("\n--- Data successfully loaded and cleaned ---\n")
cat(sprintf("Total observations: %d\n", nrow(df)))
cat(sprintf("Number of Participants: %d\n", n_distinct(df$Participant)))
cat("Group distribution:\n")
print(table(distinct(df, Participant, Group)$Group))

# --- 4. Final Object Cleanup ---
# We keep only the 'df' object to keep the environment clean
rm(raw_df)