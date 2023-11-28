# Meta-analysis on the effects of increased CO2 on pollen and reproduction
# Study authors: Dan Katz and Allison Kozak
# Summer 2023

#Set up work environment
library(googlesheets4)
library(metafor)
library(ggplot2)
library(dplyr)
library(tidyr)

#Read in data from google sheet: 
data_url <- "https://docs.google.com/spreadsheets/d/1Xlvh1YfJ3H5yebCseq1KC_i0rE4HW86keOMqpLNKZrs/edit?usp=sharing"
p_raw <- googlesheets4::read_sheet(data_url, sheet ="data", .name_repair = "universal")
p <- p_raw %>% 
  #filter(agricultural != "yes"|is.na(agricultural)) %>% 
 # filter(!is.na(eCO2.SD)) %>% 
  filter(!is.na(eCO2.mean)) %>% 
  mutate(lnR = round(log(eCO2.mean) - log(aCO2.mean), 3),
         lnR = case_when(measurement.type == "start of reproduction" ~ lnR * -1,  #ES should be flipped for this one since an earlier start date 
                          measurement.type != "start of reproduction" ~ lnR), #means more pollen exposure
         vlnR = eCO2.SD^2 / (eCO2.study.n * eCO2.mean^2) +
                aCO2.SD^2 / (aCO2.study.n * aCO2.mean^2),
         sdlnR = sqrt(vlnR),
         ES_ratio = exp(lnR),
         ES_sd = exp(sdlnR)
           ) 

unique(p_raw$measurement.type)
#some stats
length(unique(p$paper.index))
nrow(p)
length(unique(p$species))

mean(p$lnR[!is.infinite(p$lnR)], na.rm = T)

#some data visualization

#most basic results
ggplot(p, aes(x = lnR)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 0, lty = 2, color = "red", lwd = 1.3) +
  geom_vline(xintercept = mean(p$lnR[!is.infinite(p$lnR)], na.rm = T), lty = 2, color = "blue", lwd = 1.3)
  
exp(mean(p$lnR[!is.infinite(p$lnR)], na.rm = T))

p %>% 
  filter(Experiment.Type != "gradient study") %>% 
  filter(Experiment.Type != "rural/urban locations") %>% 
ggplot(aes(x = lnR)) + geom_histogram() + theme_bw() + facet_wrap(~Experiment.Type)

p %>% group_by(Experiment.Type) %>% 
  filter(!is.infinite(lnR)) %>% 
  summarize(mean_lnR = mean(lnR, na.rm = TRUE),
            mean_r = exp(mean_lnR))

?geom_vline

p %>% 
  #filter(Experiment.Type == "FACE") %>%  
  mutate(study_obs = as.numeric(as.factor(paper.index))) %>% 
  ggplot(aes(y = study_obs, xmin = lnR - sdlnR, x = lnR, xmax = lnR + sdlnR,
             col = Growth.Form)) +  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
  geom_pointrange(alpha = 0.5)  + facet_wrap(~measurement.type, scales = "free_x") + 
  geom_vline(xintercept = 0, lty = 2) + theme_bw()

length(unique(p$study.name))

names(p)


#### response types #######################################################
unique(p$measurement.type)
p %>% 
  filter(measurement.type == "allergenicity") #-> test

## allergenicity
p %>% 
  filter(measurement.type == "allergenicity") %>% 
  mutate(study_obs = as.numeric(as.factor(paper.index)),
         row_n = row_number(),
         study_obs_n = paste(study.name, row_n)) %>% 
  ggplot(aes(y = study_obs_n, xmin = lnR - sdlnR, x = lnR, xmax = lnR + sdlnR,
             col = species)) +  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
  geom_pointrange(alpha = 0.5)  + facet_wrap(~measurement.type, scales = "free") + 
  geom_vline(xintercept = 0, lty = 2) + ggthemes::theme_few() 

#pollen size
#test <- 
p %>% 
  filter(measurement.type == "pollen size") %>% 
  mutate(study_obs = as.numeric(as.factor(paper.index)),
         row_n = row_number(),
         study_obs_n = paste(study.name, row_n)) %>% 
  ggplot(aes(y = study_obs_n, xmin = lnR - sdlnR, x = lnR, xmax = lnR + sdlnR, col = species)) +  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
  geom_pointrange(alpha = 0.5)  + facet_wrap(~measurement.type, scales = "free") + 
  geom_vline(xintercept = 0, lty = 2) + ggthemes::theme_few() 

#reproductive tissue production
p %>% 
  #filter(measurement.type == "amount of reproductive tissue") %>% 
  mutate(study_obs = as.numeric(as.factor(paper.index)),
         row_n = row_number(),
         study_obs_n = paste(study.name, row_n)) %>% 
  ggplot(aes(y = study_obs_n, xmin = lnR - sdlnR, x = lnR, xmax = lnR + sdlnR, color = wind.pollinated)) +  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
  geom_pointrange(alpha = 0.5)  + facet_wrap(~measurement.type, scales = "free") + 
  geom_vline(xintercept = 0, lty = 2) + ggthemes::theme_few() 

#phenology: reproductive duration and start of reproduction
p %>% 
  filter(measurement.type == "reproductive duration" | measurement.type == "start of reproduction") %>% 
  mutate(study_obs = as.numeric(as.factor(paper.index)),
         row_n = row_number(),
         study_obs_n = paste(study.name, row_n)) %>% 
  ggplot(aes(y = study_obs_n, xmin = lnR - sdlnR, x = lnR, xmax = lnR + sdlnR)) +  
  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
  geom_pointrange(alpha = 0.5)  + facet_wrap(~measurement.type, scales = "free") + 
  geom_vline(xintercept = 0, lty = 2) + ggthemes::theme_few() 





p %>% 
#  group_by(species) %>% 
  summarize(n = n())
