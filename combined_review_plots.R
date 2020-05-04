# load required libaries

library(tidyverse)
library(RColorBrewer)
library(gridExtra)

# load data

fsv_data <- read_csv("input_data/fsv_data.csv")
r2f_data <- read_csv("input_data/r2f_data.csv")

fsv_data <- filter(fsv_data, graded == TRUE & excluded == FALSE)
r2f_data <- filter(r2f_data, graded == TRUE & excluded == FALSE)

# set factor levels where needed

fsv_data <- fsv_data %>% 
  mutate(impact_fsn = as.factor(impact_fsn)) %>%
  mutate(impact_fsn = fct_relevel(impact_fsn, c("-", "0", "+", "reverse+"))) %>%
  mutate(causality = as.factor(causality)) %>%
  mutate(causality = fct_relevel(causality, c("Descriptive", "Causal Effect", "Causal Mechanism"))) %>%
  mutate(conditionality = as.factor(conditionality)) %>%
  mutate(conditionality = fct_relevel(conditionality, c("Narrow", "Middle-range", "Universal"))) 

#  set color themes

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
sec_color <- "seagreen3"
colors_lit_type <- c("seagreen3", "slategray4", "deepskyblue3")

'''
Figure 1. Publication year of studies on the impact of food sovereignty and the right to food on FSN. 
Included studies were published between January 1992 and September 2018.
2018 is a partial year.
'''

pubyears <- 1992:2018

f1_fsv_byyear <- ggplot(data = fsv_data, mapping = aes(x = year, fill = type_peer_grey)) +
  geom_bar(color = "black", width = 1) +
  scale_fill_manual(values = colors_lit_type, name = "Literature type") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Publication year",
    y = "Food sovereignty studies") +
  scale_x_continuous("Publication year", labels = pubyears, breaks = pubyears) +
  my.theme +
  theme(legend.position = "bottom")


f1_r2f_byyear <- ggplot(data = r2f_data, mapping = aes(x = year, fill = type_peer_grey)) +
  geom_bar(color = "black", width = 1) +
  scale_fill_manual(values = colors_lit_type, name = "Literature type") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(
    x = "Publication year",
    y = "Right to Food studies") +
  scale_x_continuous("Publication year", labels = pubyears, breaks = pubyears) +
  my.theme
theme(legend.position = "none")

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
extract_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- extract_legend(f1_fsv_byyear)

grid.arrange(arrangeGrob(f1_fsv_byyear + theme(legend.position="none"), f1_r2f_byyear + theme(legend.position="none"), nrow=1),
                         mylegend, nrow=2,
                         heights=c(10, 1))

'''
Figure 2. Geographic regions of the studies
'''
# make a dataframe with the relivant data from both reviews

fsv_byregion <- fsv_data %>%
  group_by(study_region) %>%
  summarize(n = n())
r2f_byregion <- r2f_data %>%
  group_by(study_region) %>%
  summarize(n = n())

byregion <- bind_rows(fsv_byregion, r2f_byregion, .id = "review")
region_dummies <- tibble(review = as.character(c(2, 2)), study_region = c("Australia/New Zealand", "Western Asia/North Africa"), n = c(0, 0))
byregion <- bind_rows(byregion, region_dummies)
region_order <- c("Asia and Pacific", "Australia/New Zealand", "North America", "South America and Caribbean", "SubSaharan Africa", "Western Asia/North Africa", "Western Europe", "Multiple Regions")

ggplot(data = byregion, mapping = aes(x = factor(study_region, level = rev(region_order)), y = n)) +
  geom_bar(
    aes(fill = review), 
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
    ) +
  coord_flip() +
  scale_fill_manual(values = c(pr_color, sec_color),
                    name = "", 
                    labels = c("Food Sovereignty", "Right to Food")) +
  labs(x = "Study Region", y = "Number of studies", fill = "") +
  theme(legend.position = "bottom") +
  my.theme

'''
Figure 3. Studies by action type
'''

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

# Number of Studies by Food Sov Action Type, by Lit Type
# ***horizontal w abreviated action type names***
ggplot(data = fsv_gen) + 
  geom_bar(mapping = aes(x = reorder(action_short, desc(action_short)), y = freq, fill=lit_type), stat="identity") +
  labs( x = "Food sovereignty action type",
        y = "Number of studies") +
  coord_flip() +
  scale_fill_manual(values=colors_lit_type, name = "Type of literature") +
  my.theme


# report number of studies by action type and impact

fsv_data %>% 
  group_by(gen_id, impact_fsn) %>% 
  summarise(n())


'''
Figure 4. Reported impact on FSN
'''
fsv_data$impact_fsn <- factor(fsv_data$impact_fsn, c("-", "0", "+", "reverse+")) 
r2f_data$impact_fsn <- factor(r2f_data$impact_fsn, c("-", "0", "+", "reverse+")) 

f4a <- ggplot(data = fsv_data, mapping = aes(x = impact_fsn, fill = type_peer_grey)) +
  geom_bar() +
  scale_fill_manual( values = colors_lit_type, name = "Type of literature")+
  labs(x = "Impact",
       y = "Food sovereignty studies") +
  theme(legend.position = "bottom") +
  my.theme

f4b <- ggplot(data = r2f_data, mapping = aes(x = impact_fsn, fill = type_peer_grey)) +
  geom_bar() +
  scale_fill_manual( values = colors_lit_type, name = "Type of literature")+
  labs(x = "Impact",
       y = "Right to food studies") +
  my.theme

f4_legend <- extract_legend(f4a)

grid.arrange(arrangeGrob(f4a + theme(legend.position="none"), f4b + theme(legend.position="none"), nrow=1),
             f4_legend, nrow=2,
             heights=c(10, 1))

'''
Other -- 
table of results by action type
'''
# Food sovereignty
> fsv_data %>% 
  +   group_by(gen_id, impact_fsn) %>% 
  +   summarise(n())
# Right to Food
