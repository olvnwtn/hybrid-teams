# Author: Olivia B. Newton
# Purpose: Load packages for analysis pipeline.
#.         Versions are recorded by renv.

# Packages ----------------------------------------------------------------

library(corrplot)
library(DescTools)
library(distributions3)
library(fastDummies)
library(gganimate)
library(ggdist)
library(ggpubr)
library(ggsci)
library(here)
library(Hmisc)
library(hrbrthemes)
library(janitor)
library(likert)
library(lsr)
library(mediation)
library(openxlsx)
library(pracma)
library(psych)
library(RColorBrewer)
library(readxl)
library(reshape)
library(reshape2)
library(rstatix)
library(skimr)
library(tidyverse)
library(tidyquant)
library(TOSTER)
library(viridis)

library(conflicted)

conflict_prefer("recode", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("count", "dplyr")
conflict_prefer("select", "dplyr")
