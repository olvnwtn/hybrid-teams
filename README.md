# Hybrid Teams Project

Replication package for "Redacted" by 

## Overview

## Data Availability and Provenance Statements

### Summary of Availability

No data can be made publicly available. The raw and processed data used
in this study are confidential and are not included in this package.

### Statement about Rights

### Details on each Data Source

## Computational Requirements

### Software Requirements

### Controlled Randomness

## Description of Programs/Code

Scripts are in `scripts/`. Run `run_all.R` to execute the full pipeline, or run the numbered scripts in order:

* `00_load_packages.R` - Load packages (versions recorded by renv)
* `01_import_data.R` - Import raw data
* `02_clean_data.R` - Clean all raw data sources and save processed outputs
* `03_merge_data.R` - Merge processed sources into the analysis dataset
* `04_summarize_data.R` - Descriptive statistics and correlations
* `05_assess_reliability.R` - Scale reliability (Cronbach's alpha)
* `06_justify_aggregation.R` - Within-group agreement (rwg) and ICCs
* `07_test_hypotheses.R` - Hypothesis tests (H1-H7)
* `08_generate_figures.R` - Generate manuscript figures
* `run_all.R` - Source the numbered scripts in order

Exploratory supplement analyses are in a separate script, run independently
of the pipeline.

## Instructions

## List of Figures and Programs

## References

