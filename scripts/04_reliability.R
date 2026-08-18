# Authors: Olivia B. Newton and Tyler R. Talbot
# Purpose: Compute Cronbach's alpha for all survey scales.

# Read individual-level data ----------------------------------------------

survey_processed <- read_csv(here("data", "processed", "survey_individual.csv"))

# Reliability: Demonstrability --------------------------------------------

psych::alpha(select(survey_processed, starts_with("demonstrability")),
             check.keys = TRUE)

# Reliability: Big Five subscales -----------------------------------------

psych::alpha(select(survey_processed, c("big5_1", "big5_6")),
             check.keys = TRUE)  # extraversion

psych::alpha(select(survey_processed, c("big5_2", "big5_7")),
             check.keys = TRUE)  # agreeableness
# low reliability may be due to wrong word used in item (trusting vs trustworthy)

psych::alpha(select(survey_processed, c("big5_3", "big5_8")),
             check.keys = TRUE)  # conscientiousness

psych::alpha(select(survey_processed, c("big5_4", "big5_9")),
             check.keys = TRUE)  # neuroticism

psych::alpha(select(survey_processed, c("big5_5", "big5_10")),
             check.keys = TRUE)  # openness

# Reliability: Intent to remain -------------------------------------------

psych::alpha(select(survey_processed, starts_with("intent_remain")),
             check.keys = TRUE)

# Reliability: Team viability ---------------------------------------------

psych::alpha(select(survey_processed, c("satis1", "satis2", "satis3", "satis4")),
             check.keys = TRUE)
