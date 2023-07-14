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
  filter(agricultural != "yes"|is.na(agricultural)) %>% 
 # filter(!is.na(eCO2.SD)) %>% 
  mutate(lnR = round(log(eCO2.mean) - log(aCO2.mean), 3),
         vlnR = eCO2.SD^2 / (eCO2.study.n * eCO2.mean^2) +
                aCO2.SD^2 / (aCO2.study.n * aCO2.mean^2) 
           ) %>% 
  mutate(lnR = case_when(measurement.type == "start of reproduction" ~ lnR * -1,  #ES should be flipped for this one since an earlier start date 
                          measurement.type != "start of reproduction" ~ lnR)) #means more pollen exposure


ggplot(p, aes(x = lnR)) + geom_histogram() + theme_bw() + facet_wrap(~Experiment.Type)


p %>% 
  #filter(Experiment.Type == "FACE") %>% 
  mutate(study_obs = as.numeric(as.factor(paper.index))) %>% 
  ggplot(aes(y = study_obs, xmin = lnR - vlnR, x = lnR, xmax = lnR + vlnR,
             col = wind.pollinated)) +  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
  geom_pointrange(alpha = 0.5)  + facet_wrap(~measurement.type) + 
  geom_vline(xintercept = 0, lty = 2) + theme_bw()

length(unique(p$study.name))
