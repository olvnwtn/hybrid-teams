# Author: Olivia B. Newton
# Purpose: Run the full analysis pipeline in order.
#          Exploratory supplement scripts are run separately.

source(here::here("scripts", "00_load_packages.R"))
source(here("scripts", "01_import_data.R"))
source(here("scripts", "02_clean_data.R"))
source(here("scripts", "03_merge_data.R"))
source(here("scripts", "04_summarize_data.R"))
source(here("scripts", "05_assess_reliability.R"))
source(here("scripts", "06_justify_aggregation.R"))
source(here("scripts", "07_test_hypotheses.R"))
source(here("scripts", "08_generate_figures.R"))