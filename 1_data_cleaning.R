# this script cleans data exported from google docs (where we  coded studies)
# and creates two .csv files with all the varioables we will need for analysis.  
# fsv_data.csv is for the food sovereignty review
# r2f_data.csv is for the right to food review

# load required packages
library(tidyverse)

# clean  food sovereignty data
fsv_data <- read_csv("input_data/HLPE Food Sov Keying - FSv.csv", skip = 1 ) %>%
  select(1:27) 
names(fsv_data) <- c(
  "ID",
  "key_access",
  "key_complete",
  "key_exclude",
  "title",
  "authors",
  "year",
  "pub",
  "type_peer_grey",
  "type_quant_qual",
  "type_interv_observ",
  "type_cross_case_long",
  "study_date",
  "study_location",
  "study_dev",
  "study_region",
  "study_sample_size",
  "measure_fsv",
  "measure_fsn",
  "summary_findings",
  "notes",
  "desc_intervention",
  "impact_fsn",
  "case_study",
  "causality",
  "conditionality",
  "conditionality_desc" )

# do some cleaning
fsv_data$key_exclude[fsv_data$key_exclude == "--"] <- NA
fsv_data$conditionality[fsv_data$conditionality == "middle-range"] <- "Middle-range"
fsv_data$causality[fsv_data$causality == "causal Mechanism"] <- "Causal Mechanism"
fsv_data$causality[fsv_data$causality == "Desriptive"] <- "Desriptive"
fsv_data$causality[fsv_data$causality == "Desriptive"] <- "Desriptive"
fsv_data$causality[fsv_data$causality == "Desriptive"] <- "Descriptive"
fsv_data$type_quant_qual[fsv_data$type_quant_qual == "Qualitative"] <- "qualitative"
fsv_data$type_quant_qual[fsv_data$type_quant_qual == "qualitative"] <- "Qualitative"
fsv_data$type_quant_qual[fsv_data$type_quant_qual == "quantitative"] <- "Quantitative"
fsv_data$type_quant_qual[fsv_data$type_quant_qual == "quantitative"] <- "Quantitative"
fsv_data$type_quant_qual[fsv_data$type_quant_qual == "quantitative and qualitative"] <- "Quantitative and qualitative"
fsv_data$type_cross_case_long[fsv_data$type_cross_case_long == "case control"] <- "other"

# calculate some aditional variables
fsv_data <- mutate(fsv_data, 
                   excluded = !is.na(key_exclude),
                   graded = !is.na(impact_fsn))
fsv_data <- separate(data = fsv_data, col = measure_fsv, into = c("gen_id", "gen_def"), sep = ": ", remove = FALSE) ###Edited MC
fsv_data$impact_fsn <- factor(fsv_data$impact_fsn, c("-", "0", "+", "=+")) ###Edited MC

write_csv(fsv_data, "input_data/fsv_data.csv")

# right to food review

r2f_data <- read_csv("input_data/HLPE Right to Food Keying - R2F Keying.csv", skip = 1 ) %>%
  select(1:27) 
names(r2f_data) <- c(
  "ID",
  "key_access",
  "key_complete",
  "key_exclude",
  "title",
  "authors",
  "year",
  "pub",
  "type_peer_grey",
  "type_quant_qual",
  "type_interv_observ",
  "type_cross_case_long",
  "study_date",
  "study_location",
  "study_dev",
  "study_region",
  "study_sample_size",
  "measure_r2f",
  "measure_fsn",
  "summary_findings",
  "notes",
  "desc_intervention",
  "impact_fsn",
  "case_study",
  "causality",
  "conditionality",
  "conditionality_desc" )

# do some cleaning
r2f_data$key_exclude[r2f_data$key_exclude == "--"] <- NA
r2f_data$conditionality[r2f_data$conditionality == "middle-range"] <- "Middle-range"
r2f_data$causality[r2f_data$causality == "causal Mechanism"] <- "Causal Mechanism"
r2f_data$causality[r2f_data$causality == "Desriptive"] <- "Desriptive"
r2f_data$causality[r2f_data$causality == "Desriptive"] <- "Desriptive"
r2f_data$causality[r2f_data$causality == "Desriptive"] <- "Descriptive"
r2f_data$type_quant_qual[r2f_data$type_quant_qual == "Qualitative"] <- "qualitative"
r2f_data$type_quant_qual[r2f_data$type_quant_qual == "qualitative"] <- "Qualitative"
r2f_data$type_quant_qual[r2f_data$type_quant_qual == "quantitative"] <- "Quantitative"
r2f_data$type_quant_qual[r2f_data$type_quant_qual == "quantitative"] <- "Quantitative"
r2f_data$type_quant_qual[r2f_data$type_quant_qual == "quantitative and qualitative"] <- "Quantitative and qualitative"
r2f_data$type_cross_case_long[r2f_data$type_cross_case_long == "case control"] <- "other"
r2f_data$study_dev[r2f_data$study_dev == "Develped"] <- "Developed"
r2f_data$study_dev[r2f_data$study_dev == "Gloobla"] <- "Developed"
r2f_data$study_dev[r2f_data$study_dev == "Develping"] <- "Developing"
r2f_data$study_dev[r2f_data$study_dev == "Develping"] <- "Developing"
r2f_data$impact_fsn[r2f_data$impact_fsn == "reverse+"] <- "=+" ###Edited MC

r2f_data <- separate(data = r2f_data, col = measure_r2f, into = c("gen_id", "gen_def"), sep = ". ", remove = FALSE) ###Edited MC
r2f_data$impact_fsn <- factor(r2f_data$impact_fsn, c("-", "0", "+", "=+")) ###Edited MC

# calculate some aditional variables related to keying/exclusion
r2f_data <- mutate(r2f_data, 
                   excluded = !is.na(key_exclude),
                   graded = !is.na(impact_fsn))

write_csv(r2f_data, "input_data/r2f_data.csv")


