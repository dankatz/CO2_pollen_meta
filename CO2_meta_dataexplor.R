# Meta-analysis on the effects of increased CO2 on pollen and reproduction
# Study authors: Dan Katz and Allison Kozak
# Summer 2023 - Spring 2024

#Set up work environment
library(googlesheets4)
library(metafor)
library(ggplot2)
library(dplyr)
library(tidyr)


devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans,
               ape, phytools, flextable)

#Read in data from google sheet: 
data_url <- "https://docs.google.com/spreadsheets/d/1Xlvh1YfJ3H5yebCseq1KC_i0rE4HW86keOMqpLNKZrs/edit?usp=sharing"
p_raw <- googlesheets4::read_sheet(data_url, sheet ="data", .name_repair = "universal")
p <- p_raw %>% 
  #filter(agricultural != "yes"|is.na(agricultural)) %>% 
  filter(!is.na(eCO2.SD)) %>% 
  filter(!is.na(eCO2.mean)) %>% 
  mutate(lnR = round(log(eCO2.mean) - log(aCO2.mean), 3),
         lnR = case_when(measurement.type == "start of reproduction" ~ lnR * -1,  #ES should be flipped for this one since an earlier start date 
                          measurement.type != "start of reproduction" ~ lnR), #means more pollen exposure
         vlnR = eCO2.SD^2 / (eCO2.n * eCO2.mean^2) +
                aCO2.SD^2 / (aCO2.n * aCO2.mean^2),
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
ggplot(aes(x = lnR)) + geom_histogram() + theme_bw() + facet_wrap(~Experiment.Type)

p %>% #group_by(Experiment.Type) %>% 
  #filter(measurement.type == "amount of reproductive tissue") %>% 
  filter(measurement.type == "allergenicity") %>% 
  
  filter(!is.infinite(lnR)) %>% 
  summarize(mean_lnR = mean(lnR, na.rm = TRUE),
            mean_r = exp(mean_lnR))

p %>% #group_by(Experiment.Type) %>% 
  filter(measurement.type == "amount of reproductive tissue") %>% 
 # filter(measurement.type == "allergenicity") %>% 
  ggplot(aes(x = lnR)) + geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 0, lty = 2, color = "red", lwd = 1.3) +
  geom_vline(xintercept = mean(p$lnR[!is.infinite(p$lnR) & p$measurement.type == "amount of reproductive tissue"], na.rm = T), lty = 2, color = "blue", lwd = 1.3)



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
  filter(measurement.type == "amount of reproductive tissue") %>% 
  mutate(study_obs = as.numeric(as.factor(paper.index)),
         row_n = row_number(),
         study_obs_n = paste(study.name, row_n)) %>% 
  ggplot(aes(y = study_obs_n, xmin = lnR - sdlnR, x = lnR, xmax = lnR + sdlnR, color = Experiment.Type)) +  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
  geom_pointrange(alpha = 0.5)  + facet_wrap(~measurement.type, scales = "free") + 
  geom_vline(xintercept = 0, lty = 2) + ggthemes::theme_few() 



p %>% 
  filter(measurement.type == "amount of reproductive tissue") %>% 
  mutate(study_obs = as.numeric(as.factor(paper.index)),
         row_n = row_number(),
         study_obs_n = paste(study.name, row_n)) %>% 
  ggplot(aes(x = lnR)) +  #Experiment.Type wind.pollinated Growth.Form photosynthesis.type Country
 
 geom_histogram() + theme_bw() + 
  geom_vline(xintercept = 0, lty = 2, color = "red", lwd = 1.3) +
  geom_vline(xintercept = mean(p$lnR[!is.infinite(p$lnR)], na.rm = T), lty = 2, color = "blue", lwd = 1.3)


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


length(p$Experiment.Type[p$Experiment.Type == "FACE" & !is.na(p$aCO2.n.indv.plants)])
length(p$Experiment.Type[p$Experiment.Type != "FACE"])

p_indiv <- p %>% filter(!is.na(aCO2.n.indv.plants))
p_area <- p %>% filter(is.na(aCO2.n.indv.plants))
#p_area <- p %>%  filter(Experiment.Type == "FACE")

p_area %>% group_by(study.name) %>% 
  summarize(n = n())



### meta-analysis of studies using metafor ##################

p_subset <- p_raw %>% 
 # filter(measurement.type == "amount of reproductive tissue" | measurement.type == "allergenicity" | measurement.type == "start of reproduction") %>% 
 # filter( measurement.type == "allergenicity") %>% 
  filter(!is.na(eCO2.SD)) %>% 
  filter(!is.na(eCO2.mean)) %>% 
  filter(eCO2.mean != 0) %>%
  filter(aCO2.mean != 0) %>% 
  filter(eCO2.SD != 0) %>% 
  filter(aCO2.SD != 0) %>% 
  filter(!is.na(eCO2.n)) %>% 

  mutate(lnR = round(log(eCO2.mean) - log(aCO2.mean), 3),
         lnR = case_when(measurement.type == "start of reproduction" ~ lnR * -1,  #ES should be flipped for this one since an earlier start date 
                         measurement.type != "start of reproduction" ~ lnR), #means more pollen exposure
         vlnR = eCO2.SD^2 / (eCO2.n * eCO2.mean^2) +
           aCO2.SD^2 / (aCO2.n * aCO2.mean^2),
         sdlnR = sqrt(vlnR),
         ES_ratio = exp(lnR),
         ES_sd = exp(sdlnR),
         obs = 1:n(),
         dif_co2 = eCO2 - aCO2,
         study_n = as.numeric(as.factor(study.name))
  ) 

dat <- escalc(measure="ROM", m1i = eCO2.mean, m2i = aCO2.mean, sd1i = eCO2.SD, sd2i = aCO2.SD, 
              n1i = eCO2.n, n2i = aCO2.n,
              data = p_subset)  #names(dat)


res <- rma.mv(yi, vi, mods = ~ measurement.type - 1,#factor(Growth.Form) ,  #+ factor(N2.Fixing)
                  random = ~ 1| factor(study_n),
           data=dat)
res #exp(0.19)
forest(res)

# a figure for how response varies according to response type
data.frame(row.names(res$b), res$b[1:7], res$ci.lb, res$ci.ub) %>% 
  mutate( response_type = gsub(x = row.names.res.b., pattern = "measurement.type", replacement = "")) %>% 
  # filter(response_type != "percent flowering") %>% 
  # filter(response_type != "start of reproduction") %>% 
  # filter(response_type != "pollen size") %>% 
  # filter(response_type != "reproductive allocation") %>% 
  ggplot(aes(y = response_type, x = res.b.1.7., xmin = res.ci.lb, xmax = res.ci.ub)) + geom_errorbarh(height = 0.2) + geom_point() +
  ggthemes::theme_few(base_size = 16) +
  xlab("effect size (logRR)") + geom_vline(xintercept = 0, lty = 2) + ylab("response to CO2 enrichment")
          
      
#with back transformation to response ratio
data.frame(row.names(res$b), res$b[1:7], res$ci.lb, res$ci.ub) %>% 
  mutate( response_type = gsub(x = row.names.res.b., pattern = "measurement.type", replacement = "")) %>% 
  mutate(
  res.b.1.7. = case_when(response_type == "start of reproduction" ~ res.b.1.7. * -1,  #ES should be flipped for this one since an earlier start date
                         response_type != "start of reproduction" ~ res.b.1.7.),
  res.ci.lb = case_when(response_type == "start of reproduction" ~ res.ci.lb * -1,  #ES should be flipped for this one since an earlier start date
                        response_type != "start of reproduction" ~ res.ci.lb),
  res.ci.ub = case_when(response_type == "start of reproduction" ~ res.ci.ub * -1,  #ES should be flipped for this one since an earlier start date
                        response_type != "start of reproduction" ~ res.ci.ub)) %>%
  ggplot(aes(y = response_type, x = exp(res.b.1.7.), xmin = exp(res.ci.lb), xmax = exp(res.ci.ub))) + geom_errorbarh(height = 0.2) + geom_point() +
  ggthemes::theme_few(base_size = 16) +
  xlab("effect size (response ratio)") + geom_vline(xintercept = 1, lty = 2) + ylab(expression(response~to~CO[2]~enrichment)) 


orchaRd::orchard_plot(res, mod = "measurement.type", xlab = "lnRR", group = "study_n", 
                      transfm = "none", twig.size = 0.5, trunk.size = .8, branch.size = 2, angle = 0, N = res$aCO2.n)

test <- filter(dat, measurement.type == "percent flowering")

forest(dat$yi, dat$vi,
       xlim=c(-2.5,3.5),        ### adjust horizontal plot region limits
       order="obs",             ### order by size of yi
       slab=NA, annotate=FALSE, ### remove study labels and annotations
       efac=0,                  ### remove vertical bars at end of CIs
       pch=19,                  ### changing point symbol to filled circle
       col="gray40",            ### change color of points/CIs
       psize=2,                 ### increase point size
       cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
       lty=c("solid","blank"))  ### remove horizontal line at top of plot
 #addpoly(res, mlab="", cex=1)

forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-5,6),
       #ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.75, header="Author(s) and Year", mlab="", shade=TRUE)
funnel(res, main="Standard Error")




### meta-analysis of studies measuring amount of reproductive tissue using metafor ##################

p_subset <- p_raw %>% 
  filter(measurement.type == "amount of reproductive tissue") %>% 
  # filter( measurement.type == "allergenicity") %>% 
  filter(!is.na(eCO2.SD)) %>% 
  filter(!is.na(eCO2.mean)) %>% 
  filter(eCO2.mean != 0) %>%
  filter(aCO2.mean != 0) %>% 
  filter(eCO2.SD != 0) %>% 
  filter(aCO2.SD != 0) %>% 
  filter(!is.na(eCO2.n)) %>% 
  
  mutate(lnR = round(log(eCO2.mean) - log(aCO2.mean), 3),
         # lnR = case_when(measurement.type == "start of reproduction" ~ lnR * -1,  #ES should be flipped for this one since an earlier start date 
         #                 measurement.type != "start of reproduction" ~ lnR), #means more pollen exposure
         vlnR = eCO2.SD^2 / (eCO2.n * eCO2.mean^2) +
           aCO2.SD^2 / (aCO2.n * aCO2.mean^2),
         sdlnR = sqrt(vlnR),
         ES_ratio = exp(lnR),
         ES_sd = exp(sdlnR),
         obs = 1:n(),
         dif_co2 = eCO2 - aCO2,
         study_n = as.numeric(as.factor(study.name))
  ) 

dat <- escalc(measure="ROM", m1i = eCO2.mean, m2i = aCO2.mean, sd1i = eCO2.SD, sd2i = aCO2.SD, 
              n1i = eCO2.n, n2i = aCO2.n,
              data = p_subset) 


res <- rma.mv(yi, vi, mods = ~  factor(Growth.Form) + factor(N2.Fixing) + factor(wind.pollinated) +
                factor(neighbors) + 
                factor(Experiment.Type),
              random = ~ 1| factor(study_n),
              data = dat)
res #exp(0.19)

orchaRd::orchard_plot(res, mod = "Growth.Form", xlab = "lnRR", group = "study_n", 
                      transfm = "none", twig.size = 0.5, trunk.size = .8, branch.size = 2, angle = 0)

orchaRd::orchard_plot(res, mod = "N2.Fixing", xlab = "lnRR", group = "study_n", 
                      transfm = "none", twig.size = 0.5, trunk.size = .8, branch.size = 2, angle = 0)

orchaRd::orchard_plot(res, mod = "wind.pollinated", xlab = "lnRR", group = "study_n", 
                      transfm = "none", twig.size = 0.5, trunk.size = .8, branch.size = 2, angle = 0)

orchaRd::orchard_plot(res, mod = "Experiment.Type", xlab = "lnRR", group = "study_n", 
                      transfm = "none", twig.size = 0.5, trunk.size = .8, branch.size = 2, angle = 0)


orchaRd::orchard_plot(res, mod = "neighbors", xlab = "lnRR", group = "study_n", 
                      transfm = "none", twig.size = 0.5, trunk.size = .8, branch.size = 2, angle = 0)




forest(res)


# a figure for how response varies according to plant type
data.frame(row.names(res$b), res$b, res$ci.lb, res$ci.ub) %>% 
  mutate( response_type = gsub(x = row.names.res.b., pattern = "measurement.type", replacement = "")) %>% 
  mutate( response_type = gsub(x = response_type, pattern = "factor(", replacement = "", fixed = TRUE)) %>% 
  mutate( response_type = gsub(x = response_type, pattern = "Growth.Form)", replacement = "", fixed = TRUE)) %>% 
  mutate( response_type = gsub(x = response_type, pattern = ")yes", replacement = "", fixed = TRUE)) %>% 
  ggplot(aes(y = response_type, x = exp(res.b), xmin = exp(res.ci.lb), xmax = exp(res.ci.ub))) + geom_errorbarh(height = 0.2) + geom_point() +
  ggthemes::theme_few(base_size = 16) +
  xlab("effect size (RR)") + geom_vline(xintercept = 1, lty = 2) + ylab(expression(plant~type)) 





forest(dat$yi, dat$vi,
       xlim=c(-2.5,3.5),        ### adjust horizontal plot region limits
       order="obs",             ### order by size of yi
       slab=NA, annotate=FALSE, ### remove study labels and annotations
       efac=0,                  ### remove vertical bars at end of CIs
       pch=19,                  ### changing point symbol to filled circle
       col="gray40",            ### change color of points/CIs
       psize=2,                 ### increase point size
       cex.lab=1, cex.axis=1,   ### increase size of x-axis title/labels
       lty=c("solid","blank"))  ### remove horizontal line at top of plot
#addpoly(res, mlab="", cex=1)

forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-5,6),
       #ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.75, header="Author(s) and Year", mlab="", shade=TRUE)
funnel(res, main="Standard Error")





