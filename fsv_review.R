# a system for sumarizing and graphing the rapid review of 
# the impacts of food sovereingty on FSN
# for the HLPE working group on agroecology
# by Devon Dampson, devonds@gmail.com

# load required packages
library(tidyverse)
library(RColorBrewer)

## this WD command is just for Marcela's computer.  Modify it to set the working directory where YOU keep your r scripts and data for this project. 
# setwd('~/Documents/FSRF FAO/HLPE/hlpe_reviews_fsv_r2f')

# load spreadsheet of keyed literature exported to working directory as .csv from google docs, exclude bias assesment
## (marcela, I changed the file name below because that is how it is downlading from google drive for me --DS)
fsv_data <- read_csv("input_data/fsv_data.csv")

# calculate some summaries of the keying process
# how many accessed and evaluted after screening? 
nrow(fsv_data)
# how many excluded? 
nrow(fsv_data)-sum(fsv_data$graded)

# remove excluded and ungraded studies
fsv_data <- filter(fsv_data, graded == TRUE & excluded == FALSE)

# how many keyed?
sum(fsv_data$graded)

# how many quantitative/qualitative/both? 
s_type_q <-  count(fsv_data, type_quant_qual)
s_type_q
# how many intervention / observation?
s_type_int <-  count(fsv_data, type_interv_observ)
s_type_int
# how many crossectional/longitudinal?
s_type_lat <- count(fsv_data, type_cross_case_long)
s_type_lat

# how many peer reviewed / grey 
fsv_data %>%
  group_by(type_peer_grey) %>%
  summarise(type = n())

fsv_data %>%
  group_by(measure_fsv,type_peer_grey) %>%
  summarise(type = n())
  
##  How many keyed per region
fsv_data %>%
  group_by(measure_fsv,study_region ) %>%
  summarise(type = n())

# pub date range
min(fsv_data$year)
max(fsv_data$year)

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
colors_lit_type <- c("seagreen3", "slategray4", "deepskyblue3")

# graphs that charectarize the sample: studies by pub year, region

# keyed studies by year ---
pubyears <- 1992:2018
ggplot(data = fsv_data, mapping = aes(x = year, fill = type_peer_grey)) +
  geom_bar(color = "black", width = 1) +
  scale_fill_manual(values = colors_lit_type, name = "Literature type") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Publication year",
    y = "Food sovereignty studies") +
  scale_x_continuous("Publication year", labels = pubyears, breaks = pubyears) +
  my.theme
ggsave("Year and number of screened studies.png", width = 17, height = 17, pointsize=1.5, units = "cm") 


# keyed studies by region 
## [took out color spec in geom_bar() to add colors for regions, because qualitatively different. dissagree? -- DS]
ggplot(data = fsv_data, mapping = aes(x = study_region, fill = study_region, na.rm = TRUE)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(
    x = "Region",
    y = "Number of studies") +
  my.theme
    
ggsave("Region and Number of screened studies.png", width = 17, height = 17, pointsize=1.5, units = "cm") 

    

# report developed / developing
fsv_data %>%
  group_by(study_dev) %>%
  summarise(dev = n())

#calculate and chart several variables by impact on FSN 
fsv_impact <- fsv_data %>%
  count(impact_fsn) %>%
  drop_na(impact_fsn) 
colnames(fsv_impact) <- c("impact", "freq")
fsv_impact$impact <- factor(fsv_impact$impact, c("-", "0", "+", "reverse+"))

fsv_impact


# plot impact on FSN
ggplot(data = fsv_impact) + 
  geom_bar(mapping = aes(x = impact, y = freq), stat="identity", color=pr_color, fill=pr_color) +
  labs(x = "Impact on FSN",
      y = "Number of studies")+my.theme
ggsave("Impact on FSN and Number of screened studies.png", width = 17, height = 17, pointsize=1.5, units = "cm") 

# plot impact on FSN facited by region
ggplot(data = fsv_data, mapping = aes(x = impact_fsn, fill = study_region)) + 
  geom_bar() +
  scale_fill_discrete(name = "Study Region") +
  labs(x = "Impact on FSN",
       y = "Number of studies") +
    my.theme
ggsave("Impact on FSN by Region.png", width = 17, height = 17, pointsize=1.5, units = "cm") 
 
# plot impact on FSN by Type of literature -Marcela
fsv_impactTypeLit <- fsv_data %>%
  count(impact_fsn, type_peer_grey) %>%
  drop_na(impact_fsn) 
colnames(fsv_impactTypeLit) <- c("impact", "Literature_type", "freq")
fsv_impactTypeLit$impact <- factor(fsv_impactTypeLit$impact, c("-", "0", "+", "reverse+"))

ggplot(data = fsv_impactTypeLit) + 
  geom_bar(mapping = aes(x = impact, y = freq, fill=Literature_type), stat="identity") + 
  scale_fill_manual(values=colors_lit_type, name = "Type of literature")+
  labs(x = "Impact on FSN",
      y = "Number of studies")+my.theme
png(filename="Type of literature",res=300,pointsize=1.5)
ggsave("barImpactbyTypeLit.png", width = 15, height = 10, pointsize=1.5, units = "cm") 

# report impact on FSN
fsv_data %>%
  group_by(impact_fsn) %>%
  summarize(impact = n())

#calculate and chart several variables by generalization logic

# seperates the "measure_fsv" variable into a letter and a description,
# creating a new object called fsv_gen
fsv_gen <- fsv_data %>%
  count(measure_fsv, type_peer_grey) %>%
  drop_na(measure_fsv)   
colnames(fsv_gen) <- c("gen_logic", "lit_type", "freq") 
fsv_gen <- separate(data = fsv_gen, col = gen_logic, into = c("gen_id", "gen_def"), sep = ": ")
fsv_gen <- drop_na(fsv_gen)

# abreviate action type descriptions so that we can plot them later -DS
fsv_gen <- mutate(fsv_gen, 
                  action = str_c(gen_id, gen_def, sep = ". "))
fsv_gen <- mutate(fsv_gen, 
                  action_short = gen_id)
# abreviate descriptions
fsv_gen$action[fsv_gen$gen_id == "B"] <- "B. Addressing inequities in land access and confronting land concentration"
fsv_gen$action[fsv_gen$gen_id == "C"] <- "C. Recognizing, valuing, and supporting local and traditional knowledge"
fsv_gen$action[fsv_gen$gen_id == "D"] <- "D. Increasing autonomy over production process through agroecological practices"
fsv_gen$action[fsv_gen$gen_id == "E"] <- "E. Asserting/expanding social and economic rights of producer/consumer communites"

fsv_gen$action_short[fsv_gen$gen_id == "A"] <- "A. Local markets"
fsv_gen$action_short[fsv_gen$gen_id == "B"] <- "B. Land access"
fsv_gen$action_short[fsv_gen$gen_id == "C"] <- "C. Traditional knowledge"
fsv_gen$action_short[fsv_gen$gen_id == "D"] <- "D. Production autonomy"
fsv_gen$action_short[fsv_gen$gen_id == "E"] <- "E. Rights of communites"
fsv_gen$action_short[fsv_gen$gen_id == "F"] <- "F. Gender equity"


# ***horizontal w abreviated action type names**** 
ggplot(data = fsv_gen) + 
  geom_bar(mapping = aes(x = reorder(action_short, desc(action_short)), y = freq, fill=lit_type), stat="identity") +
  labs( x = "Food sovereignty action type",
        y = "Number of studies") +
  coord_flip() +
  scale_fill_manual(values=colors_lit_type, name = "Type of literature") +
  my.theme

# OR plot number of studies by fsv action type, with legend to right
ggplot(data = fsv_gen) + 
  geom_bar(mapping = aes(x = gen_id, y = freq, fill=action), stat="identity") +
  labs( x = "Food sovereignty action",
        y = "Number of studies") +
  scale_fill_brewer(palette = "Dark2", name = "Action Type") +
  my.theme
ggsave("fsv bar plot n studies by action type with legend.png", width= 16.5, height = 8.5, units = "cm", pointsize = 1.5)

ggplot(data = fsv_gen) + 
  geom_bar(mapping = aes(x = gen_id, y = freq,  fill=lit_type), stat="identity") +
  scale_fill_manual(values=colors_lit_type, name = "Type of literature") +
  labs( x = "Food sovereignty action",
       y = "Number of studies") + 
  my.theme

# EXPORT DATA SET
write_csv(fsv_data, path = "supplemental_data/fsv_data.csv")


# plot impact by region (devon)
ggplot(data = fsv_data) + 
  geom_bar(mapping = aes(x = study_region, fill=impact_fsn)) +
  labs( x = "Region",
        y = "Number of studies") + 
  scale_fill_brewer(palette = "Set1", name = "Impact on FSN") +
  my.theme +
  coord_flip()
ggsave("bar plot N studies by region and impact on fsn.png", width= 16.5, height = 8.5, units = "cm", pointsize = 1.5)


# plot impacts grouped by gen logic
fsv_data <- separate(data = fsv_data, col = measure_fsv, into = c("gen_id", "gen_def"), sep = ": ", remove = FALSE)
fsv_data$impact_fsn <- factor(fsv_data$impact_fsn, c("-", "0", "+", "reverse+"))

# abreviate action type descriptions so that we can plot them later -DS
fsv_data <- mutate(fsv_data, 
                  action_short = gen_id)
# short descriptions
fsv_data$action_short[fsv_data$gen_id == "A"] <- "A. Local markets"
fsv_data$action_short[fsv_data$gen_id == "B"] <- "B. Land access"
fsv_data$action_short[fsv_data$gen_id == "C"] <- "C. Traditional knowledge"
fsv_data$action_short[fsv_data$gen_id == "D"] <- "D. Production autonomy"
fsv_data$action_short[fsv_data$gen_id == "E"] <- "E. Rights of communites"
fsv_data$action_short[fsv_data$gen_id == "F"] <- "F. Gender equity"

fsv_data %>%
  drop_na(action_short) %>%
  ggplot() +
  geom_bar(mapping = aes(x = impact_fsn, fill = impact_fsn)) +
  scale_fill_brewer(palette = "RdYlBu") +
  facet_wrap(~ action_short, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
  labs(x = "Impact of food sovereignty on FSN",
       y = "Number of studies") +
  my.theme
ggsave("fsv impact facited by action type.png", height = 13, width = 27, pointsize = 1.5, units = "cm")

       
# plot conditionality by region (devon)
ggplot(data = fsv_data) + 
  geom_bar(mapping = aes(x = study_region, fill=conditionality)) +
  labs( x = "Region",
        y = "Number of studies") + 
  my.theme +
  coord_flip()
       

# proportion of + and reverse+ results
sum_impact <- fsv_data %>%
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


# view negitive resutls
neg <- fsv_data %>%
  filter(impact_fsn == "-") %>%
  arrange(measure_fsv)
view(neg)
# view 0 outcomes
neutral <- fsv_data %>%
  filter(impact_fsn == "0") %>%
  arrange(measure_fsv)
view(neutral)

sum_impact %>%
  group_by(gen_id) %>%
  summarize(
    portion_positive = impact[impact_fsn == "+"] / sum(impact))
 


# plot causaltiy and conditionality
fsv_data %>%
  drop_na(causality) %>%
  ggplot() +
  geom_bar(mapping = aes(x = causality)) +
 theme(axis.text.x = element_text(angle = 90))

fsv_data %>%
  drop_na(conditionality) %>%
  ggplot() +
  geom_bar(mapping = aes(x = conditionality)) +
  theme(axis.text.x = element_text(angle = 90))



###Action type by region
str(fsv_data )
fsv_genSR <- fsv_data %>%
  count(measure_fsv, study_region) %>%
  drop_na(measure_fsv)   
colnames(fsv_genSR) <- c("gen_logic", "Study_region","freq") 
fsv_genSR <- separate(data = fsv_genSR, col = gen_logic, into = c("gen_id", "gen_def"), sep = ": ")
fsv_genSR <- drop_na(fsv_genSR)
fsv_genSR
cbp1 <- c("#addd8e", "olivedrab4", "black", "#2ca25f", "darkslategrey", "#2c7fb8", "grey20", "grey50")
ggplot(data = fsv_genSR) + 
  geom_bar(mapping = aes(x = gen_id, y = freq, fill=Study_region), stat="identity") +scale_fill_manual(values=cbp1, name = "Study region")+
  labs( x = "Food sovereignty action",
        y = "Number of studies") +my.theme
theme(legend.position = "none")

# plot impact by conditionality (devon)
ggplot(data = fsv_data) + 
  geom_bar(mapping = aes(x = conditionality, fill=impact_fsn)) +
  labs( x = "Conditionality",
        y = "Number of studies") + 
  my.theme +
  coord_flip()

