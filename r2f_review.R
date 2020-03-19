# a system for sumarizing and graphing the rapid review of 
# the impacts of the right to food on FSN
# for the HLPE working group on agroecology
# by Devon Dampson, devonds@gmail.com

# load required packages
library(tidyverse)

# load spreadsheet of keyed literature exported to working directory as .csv from google docs, exclude bias assesment
setwd('~/Documents/FSRF FAO/HLPE/hlpe_reviews_fsv_r2f')

r2f_data <- read_csv("HLPE Right to Food Keying - R2F Keying.csv", skip = 1 ) %>%
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

head(r2f_data )
str(r2f_data )

# calculate some aditional variables related to keying/exclusion
r2f_data <- mutate(r2f_data, 
                   excluded = !is.na(key_exclude),
                   graded = !is.na(impact_fsn))
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
 
str(r2f_data )


# calculate some summaries of the keying process
# how many accessed and evaluted after screening? 
nrow(r2f_data)
# how many excluded? 
nrow(r2f_data)-sum(r2f_data$graded)

# remove excluded and ungraded studies
r2f_data <- filter(r2f_data, graded == TRUE & excluded == FALSE)

# how many keyed?
sum(r2f_data$graded)

# how many quantitative/qualitative/both? 
s_type_q <-  count(r2f_data, type_quant_qual)
s_type_q
# how many intervention / observation?
s_type_int <-  count(r2f_data, type_interv_observ)
s_type_int
# how many crossectional/longitudinal?
s_type_lat <- count(r2f_data, type_cross_case_long)
s_type_lat

# how many peer reviewed / grey 
r2f_data %>%
  group_by(type_peer_grey) %>%
  summarise(type = n())

# pub date range
min(r2f_data$year)
max(r2f_data$year)

# graphs
#set color themes
my.theme=theme(axis.title.y=element_text(size=rel(1.5),margin = margin(t = 10, r = 10, b = 20, l = 20)),
               axis.title.x=element_text(size=rel(1.5),margin = margin(t = 10, r = 10, b = 10, l = 10)),
               axis.text= element_text(size = rel(1.0)),
               legend.key.size = unit(0.5, "cm"),
               legend.text = element_text(size = rel(0.7)),
               legend.background = element_rect(fill=NA, color="white"),
             legend.key = element_rect(fill = "white", color = NA),
             panel.border=element_rect(fill=NA,size=1,color="black"),
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white"),
               strip.text = element_text(size = rel(1.2)),
               strip.background =  element_rect(fill = NA, colour = NA),
               plot.margin = unit(c(1,1,1,1), "cm"))

pr_color <- "deepskyblue4"



# (pie ) charts of study types?? 
ggplot(data = r2f_data) +
  geom_bar(mapping = aes(x= type_quant_qual, color = type_quant_qual, fill = type_quant_qual)) +
  labs(title = "Study Types: Quantitative and Qualitative",
       x = "study type",
       y = "number of studies") +
  theme(legend.position = "none")+my.theme
  
  ggsave("Study Types Quantitative and Qualitative_r2f.png", width = 17, height = 17, pointsize=1.5, units = "cm") 
## and so on


# graphs that charectarize the sample: studies by pub year, region

# keyed studies by year --- 
ggplot(data = r2f_data, mapping = aes(x = year)) +
  geom_bar(color = pr_color, fill = pr_color) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Publication year",
    y = "Number of screened studies")+my.theme
    
    ggsave("Year and Number of screened studies_r2f.png", width = 17, height = 17, pointsize=1.5, units = "cm") 




# keyed studies by region
ggplot(data = r2f_data, mapping = aes(x = study_region, color = study_region, fill = study_region, na.rm = TRUE)) +
  geom_bar(color = pr_color, fill = pr_color) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(
    x = "Region",
    y = "Number of studies")+my.theme
    
ggsave("Region and Number of screened studies_r2f.png", width = 17, height = 17, pointsize=1.5, units = "cm") 


# report developed / developing
r2f_data %>%
  group_by(study_dev) %>%
  summarise(dev = n())

#calculate and chart several variables by impact on FSN 
r2f_impact <- r2f_data %>%
  count(impact_fsn) %>%
  drop_na(impact_fsn) 
colnames(r2f_impact) <- c("impact", "freq")
r2f_impact$impact <- factor(r2f_impact$impact, c("-", "0", "+", "=+"))
r2f_impact

# plot impact on FSN
ggplot(data = fsv_impact) + 
  geom_bar(mapping = aes(x = impact, y = freq), stat="identity", color=pr_color, fill=pr_color) + labs(x = "Impact on FSN",
      y = "Number of studies") + scale_x_discrete(labels = c("+"="+","-"="-", "0"="0", "=+"= "Reverse +")) +my.theme
      ggsave("Impact on FSNand Number of screened studies_r2f.png", width = 17, height = 17, pointsize=1.5, units = "cm") 



# report impact on FSN
r2f_data %>%
  group_by(impact_fsn) %>%
  summarize(impact = n())

#calculate and chart several variables by generalization logic
str(r2f_data)
r2f_gen <- r2f_data %>%
  count(measure_r2f) %>%
  drop_na(measure_r2f) 
r2f_gen
colnames(r2f_gen) <- c("gen_logic", "freq") 
r2f_gen
r2f_gen <- separate(data = r2f_gen, col = gen_logic, into = c("gen_id", "gen_def"), sep = ". ")
r2f_gen <- drop_na(r2f_gen)
r2f_gen 


ggplot(data = r2f_gen) + 
  geom_bar(mapping = aes(x = gen_id, y = freq, color=gen_id, fill=gen_id), stat="identity") +
  theme(legend.position = "none") +
  labs( x = "Right to food action",
        y = "Number of studies")+ my.theme



r2f_gen <- mutate(r2f_gen, 
                  action = str_c(gen_id, gen_def, sep = ". "))
# abreviate descriptions
r2f_gen$action[r2f_gen$gen_id == "B"] <- "B. Addressing inequities in land access and confronting land concentration"
r2f_gen$action[r2f_gen$gen_id == "C"] <- "C. Recognizing, valuing, and supporting local and traditional knowledge"
r2f_gen$action[r2f_gen$gen_id == "D"] <- "D. Increasing autonomy over production process through agroecological practices"
r2f_gen$action[r2f_gen$gen_id == "E"] <- "E. Asserting/expanding social and economic rights of producer/consumer communites"
r2f_gen$action[r2f_gen$gen_id == "E"] <- "F. Promoting gender equity" ###Edited MC


ggplot(data = r2f_gen) + 
  geom_bar(mapping = aes(x = gen_id, y = freq, color=action, fill=action), stat="identity") +
  labs( x = "Food sovereignty action",
        y = "Number of studies") + my.theme



ggplot(data = r2f_gen) + 
  geom_bar(mapping = aes(x = action, y = freq, color=action, fill=action), stat="identity") +
  labs( x = "food sovereignty action",
        y = "number of studies") + my.theme +   coord_flip() +  theme(legend.position = "none") 

# plots of impacts grouped by gen logic ###Edited MC
r2f_data$impact_fsn
str(r2f_data)
r2f_data %>%
  drop_na(gen_id) %>%
  drop_na(impact_fsn) %>%
  ggplot() +
  geom_bar(mapping = aes(x = impact_fsn, color = impact_fsn, fill = impact_fsn)) +
  facet_wrap(~gen_id, nrow = 2) +  
  scale_x_discrete(labels = c("+"="+","-"="-", "0"="0", "=+"= "Reverse +"))+
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  labs(x = "Impact of right to food on FSN",
       y = "Number of studies") + my.theme
ggsave("Impact of right to food on FSN by impact.png", width = 17, height = 17, pointsize=1.5, units = "cm") 
       

# proportion of + and reverse+ results
sum_impact <- r2f_data %>%
  drop_na(impact_fsn) %>%
  group_by(gen_id, impact_fsn) %>%
  summarize(impact = n())
sum_impact

impact_by_action <- sum_impact %>%
  group_by(gen_id) %>%
  summarize(total = sum(impact),
            positive = sum(impact[impact_fsn == "+"]),
            reverse_pos = sum(impact[impact_fsn == "reverse+"]))
impact_by_action <- impact_by_action %>%
  mutate(portion_pos = positive / total,
         portion_pos_or_rev = (positive + reverse_pos) / total)
impact_by_action





###Action type by region
str(r2f_data )
r2f_genSR <- r2f_data %>%
  count(measure_r2f, study_region) %>%
  drop_na(measure_r2f)   
colnames(r2f_genSR) <- c("gen_logic", "Study_region","freq") 
r2f_genSR <- separate(data = r2f_genSR, col = gen_logic, into = c("gen_id", "gen_def"), sep = ". ")
r2f_genSR <- drop_na(r2f_genSR)
r2f_genSR



cbp1 <- c("#addd8e", "olivedrab4", "black", "#2ca25f", "darkslategrey", "#2c7fb8", "grey20", "grey50")
ggplot(data = r2f_genSR) + 
  geom_bar(mapping = aes(x = gen_id, y = freq, fill=Study_region), stat="identity") +scale_fill_manual(values=cbp1, name = "Study region")+
  labs( x = "Right to food action",
        y = "Number of studies") +my.theme
  theme(legend.position = "none")
ggsave("Impact of right to food on FSN by regionr2f.png", width = 17, height = 17, pointsize=1.5, units = "cm") 




###Extra with lit- Marcela
r2f_gen2 <- r2f_data %>%
  count(measure_r2f, type_peer_grey) %>%
  drop_na(measure_r2f)   
colnames(r2f_gen2) <- c("gen_logic", "Lit_type","freq") 
r2f_gen2 <- separate(data = r2f_gen2, col = gen_logic, into = c("gen_id", "gen_def"), sep = ". ")
r2f_gen2 <- drop_na(r2f_gen2)
r2f_gen2


ggplot(data = r2f_gen2) + 
  geom_bar(mapping = aes(x = gen_id, y = freq,  fill=Lit_type), stat="identity") +scale_fill_manual(values=cbp1, name = "Type of literature")+
  labs( x = "Right to food action",
        y = "Number of studies") +my.theme
  theme(legend.position = "none")
  ggsave("Impact of right to food on FSN by lit typer2f.png", width = 17, height = 17, pointsize=1.5, units = "cm") 



#Including type of literature - Marcela

ggplot(data = r2f_gen2) + 
  geom_bar(mapping = aes(x = gen_id, y = freq,  fill=Lit_type), stat="identity") +
  labs( x = "Right to food action",
        y = "Number of studies") +my.theme+
  coord_flip() +scale_fill_manual(values=cbp1, name = "Type of literature")+  theme(legend.position = c(0.85, 0.9))
ggsave("ActiontypebyTypeLitr2f.png", width = 27, height = 13, pointsize=1.5, units = "cm") 
 


# view negitive resutls
neg <- r2f_data %>%
  filter(impact_fsn == "-") %>%
  arrange(measure_r2f)
view(neg)
# view 0 outcomes
neutral <- r2f_data %>%
  filter(impact_fsn == "0") %>%
  arrange(measure_r2f)
view(neutral)

sum_impact %>%
  group_by(gen_id) %>%
  summarize(
    portion_positive = impact[impact_fsn == "+"] / sum(impact))



# plot causaltiy and conditionality
r2f_data %>%
  drop_na(causality) %>%
  ggplot() +
  geom_bar(mapping = aes(x = causality)) +
  theme(axis.text.x = element_text(angle = 90))

r2f_data %>%
  drop_na(conditionality) %>%
  ggplot() +
  geom_bar(mapping = aes(x = conditionality)) +
  theme(axis.text.x = element_text(angle = 90))




