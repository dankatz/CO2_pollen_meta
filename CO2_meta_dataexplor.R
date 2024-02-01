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
ggplot(aes(x = lnR)) + geom_histogram() + theme_bw() + facet_wrap(~Experiment.Type)

p %>% group_by(Experiment.Type) %>% 
  filter(!is.infinite(lnR)) %>% 
  summarize(mean_lnR = mean(lnR, na.rm = TRUE),
            mean_r = exp(mean_lnR))

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


length(p$Experiment.Type[p$Experiment.Type == "FACE" & !is.na(p$aCO2.n.indv.plants)])
length(p$Experiment.Type[p$Experiment.Type != "FACE"])

p_indiv <- p %>% filter(!is.na(aCO2.n.indv.plants))
p_area <- p %>% filter(is.na(aCO2.n.indv.plants))
#p_area <- p %>%  filter(Experiment.Type == "FACE")

p_area %>% group_by(study.name) %>% 
  summarize(n = n())



### meta-analysis of studies that include individual plant sample sizes using metafor ##################

p_subset <- p_raw %>% 
  filter(measurement.type == "amount of reproductive tissue" | measurement.type == "percent flowering") %>% 
  filter(!is.na(eCO2.SD)) %>% 
  filter(!is.na(eCO2.n.indv.plants)) %>% 
  filter(!is.na(eCO2.mean)) %>% 
  filter(eCO2.mean != 0) %>%
  filter(aCO2.mean != 0) %>% 
  mutate(lnR = round(log(eCO2.mean) - log(aCO2.mean), 3),
         lnR = case_when(measurement.type == "start of reproduction" ~ lnR * -1,  #ES should be flipped for this one since an earlier start date 
                         measurement.type != "start of reproduction" ~ lnR), #means more pollen exposure
         vlnR = eCO2.SD^2 / (eCO2.n.indv.plants * eCO2.mean^2) +
           aCO2.SD^2 / (aCO2.n.indv.plants * aCO2.mean^2),
         sdlnR = sqrt(vlnR),
         ES_ratio = exp(lnR),
         ES_sd = exp(sdlnR),
         obs = 1:n(),
         dif_co2 = eCO2 - aCO2,
         study_n = as.numeric(as.factor(study.name))
  ) 

dat <- escalc(measure="ROM", m1i = eCO2.mean, m2i = aCO2.mean, sd1i =eCO2.SD, sd2i = aCO2.SD, 
              n1i = eCO2.n.indv.plants, n2i = aCO2.n.indv.plants,
              data = p_subset)

res <- rma.mv(yi, vi, mods = ~ factor(Growth.Form) + factor(N2.Fixing), 
                  random = ~ 1| factor(study_n),
           data=dat)
res
exp(0.19)
forest(res)

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


op <- par(cex=0.75, font=2)
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"))
par(op)

 

### custom meta-analysis practice ###########################################################
library(rjags)
library(metafor)


#starting with replicating an existing meta-analysis
# https://wviechtb.github.io/meta_analysis_books/borenstein2009.html
dat <- read.table(header=TRUE, text = "
study   mean1 sd1  n1 mean2 sd2  n2
Carroll    94  22  60    92  20  60
Grant      98  21  65    92  22  65
Peck       98  28  40    88  26  40
Donat      94  19 200    82  17 200
Stewart    98  21  50    88  22  45
Young      96  21  85    92  22  85")

dat <- escalc("ROM", m1i=mean1, sd1i=sd1, n1i=n1, m2i=mean2, sd2i=sd2, n2i=n2,
              slab=study, data=dat)
dat
res <- rma(yi, vi, data=dat, digits=2)
res
forest(res)

# res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)


sink("model_toy.txt")
cat("  
model{
  
##Likelihood
for(i in 1:N){

P[i] <- 1/vi[i]

yi[i] ~ dnorm(d, P[i])
}

## define the priors
d ~ dnorm(0, 0.0001)

##transform the ln(OR) to OR
OR <- exp(d)

}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model_toy.txt', 
                   data = list(
                    vi = dat$vi,
                    yi = dat$yi,
                    N = nrow(dat)),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter = 4000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("d", "OR"),  n.iter = 1000, thin = 3) #variables to monitor #"b", "c" "b_snap"
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2





### custom hierarchical meta-analysis ###########################################################
library(rjags)
library(metafor)

#prepare data
p_subset <- p_raw %>% 
  filter(measurement.type == "amount of reproductive tissue" | measurement.type == "percent flowering") %>% 
  filter(!is.na(eCO2.SD)) %>% 
  #filter(!is.na(eCO2.n.indv.plants)) %>% 
  filter(!is.na(eCO2.mean)) %>% 
  filter(eCO2.mean != 0) %>%
  filter(aCO2.mean != 0) %>% 
  mutate(lnR = round(log(eCO2.mean) - log(aCO2.mean), 3),
         lnR = case_when(measurement.type == "start of reproduction" ~ lnR * -1,  #ES should be flipped for this one since an earlier start date 
                         measurement.type != "start of reproduction" ~ lnR), #means more pollen exposure
         vlnR = eCO2.SD^2 / (eCO2.n.indv.plants* eCO2.mean^2) +
           aCO2.SD^2 / (aCO2.n.indv.plants * aCO2.mean^2),
         sdlnR = sqrt(vlnR),
         ES_ratio = exp(lnR),
         ES_sd = exp(sdlnR),
         obs = 1:n(),
         dif_co2 = eCO2 - aCO2,
         study_n = as.numeric(as.factor(study.name))  ) 

#analysis
sink("model.txt")
cat("  
model{
  
##Likelihood
for(i in 1:N){ #observation loop

P[i] <- 1/sdlnR[i]

lnR[i] ~ dnorm(obs[i] , P[i])
obs[i] <- d[study[i]] + beta_growthform[growth_form[i]] + beta_nfixing[nfixing[i]]
}

#### define the priors at the study level
for(j in 1:study_N){d[j] ~ dnorm(d_overall, d_sd)

#transform the ln(OR) to OR
OR[j] <- exp(d[j])
}

#### define the priors at the observation level
for(b in 1:3){beta_growthform[b] ~ dnorm(0, 0.0001)}
for(b in 1:2){beta_nfixing[b] ~ dnorm(0, 0.0001)}



d_overall~ dnorm(0, 0.0001)
d_sd ~ dgamma(0.01, 0.01)

or_overall <- exp(d_overall)

}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model.txt', 
                   data = list(
                     sdlnR = p_subset$sdlnR, #vi = sdlnR
                     lnR = p_subset$lnR, #yi = lnR
                     N = nrow(p_subset),
                     study = p_subset$study_n,
                     study_N = max(p_subset$study_n),
                     growth_form = as.numeric(as.factor(p_subset$Growth.Form)),
                     nfixing = as.numeric(as.factor(p_subset$N2.Fixing))
                     ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter = 400000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("d", "OR"),  n.iter = 1000, thin = 3) #variables to monitor #"b", "c" "b_snap"
#plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2

mcmc_samples_params <- coda.samples(jags, variable.names=c("d_overall", "or_overall"),  n.iter = 10000, thin = 3) #variables to monitor #"b", "c" "b_snap"
plot(mcmc_samples_params)


mcmc_samples_params <- coda.samples(jags, variable.names=c("beta_growthform", "beta_nfixing"),  n.iter = 10000, thin = 3) #variables to monitor #"b", "c" "b_snap"
plot(mcmc_samples_params)

















###################################################################
#scraps


sink("model_c.txt")
cat("  
model{
  
##Likelihood

#core trees loop
  for(tree in 1:n_trees_core){ 
    for(i in 1:nobs_per_tree_core[tree]){
      Y_hat[tree, i] <- 0.99  * exp( -exp(-c[tree] * (t[tree, i] - b[tree])))
      Y[tree, i] ~ dnorm(Y_hat[tree, i], LAMBDA1[tree])
    } #end obs loop
  } #end tree loop

#snapshot trees loop
  for(tree_snap in 1:n_trees_snap){
      Y_hat_snap[tree_snap] <- 0.99  * exp( -exp(-rate_global_mean_snap * (t_snap[tree_snap] -
                                          b_snap[tree_snap])))
      Y_snap[tree_snap] ~ dnorm(Y_hat_snap[tree_snap], LAMBDA1_snap[tree_snap])
  } #end tree loop

  
#Priors
for(tree in 1:n_trees_core){
  LAMBDA1[tree] <- 250 #~ dgamma(0.01,0.01) #uninformative gamma prior
  
  #a[tree] ~ dunif(0.99, 1) #the asympotote #assuming that asymptote is at 1
  b[tree] ~ dnorm(site_halfway_point_core[site_vector_core[tree]], LAMBDA3_core) #shifting left and right-  #
                                                                  #LAMBDA3_core[site_vector_core[tree]]
                                                                      #When b = log(2), f(0) = a/2, also called the halfway point
  c[tree] ~ dnorm(rate_global_mean, rate_global_sigma) #the steepness of the curve
} #end priors tree loop: core

for(tree in 1:n_trees_snap){
  LAMBDA1_snap[tree] <- 250 #~dgamma(0.01,0.01) #uninformative gamma prior
  #b_snap[tree] <- max(b_snap_orig[tree], 0) #keeping the halfway point for each tree above Dec 10
  b_snap[tree] ~ dnorm(site_halfway_point_snap[site_n_snap[tree]], LAMBDA3_snap) #[site_n_snap[tree]]
} #end priors tree loop: snap


for(site in 1:n_sites_core){
   site_halfway_point_core[site] ~ dnorm(0, 0.001)
} #end priors site loop

for(site in 1:n_sites_snap){
   site_halfway_point_snap[site] ~ dnorm(0, 0.001)
} #end priors site loop

rate_global_mean <- 1.02 #dnorm(0, 0.001)
rate_global_mean_snap <- 1.02 #max(rate_global_mean, 0)#preventing backflow of information #prevent it from wandering negative

rate_global_sigma <- 27.2 #~ dgamma(0.01,0.01)
rate_global_sigma_snap <- rate_global_sigma #preventing backflow of information

LAMBDA3_core <- 0.031 #~ dgamma(0.01,0.01)
LAMBDA3_snap <- 0.031 #LAMBDA3_core
# LAMBDA3_snap ~ dgamma(0.01,0.01) #uninformative gamma prior 
# c_sim ~ dnorm(rate_global_mean, rate_global_sigma)
# c_sim_snap ~ dnorm(rate_global_mean_snap, rate_global_sigma_snap)

#simulation for each tree core
  for(tree in 1:n_trees_core){
    for(i in 1:max_t){
      Y_hat_sim[tree, i] <- 0.99  * exp( -exp(-c[tree] * (t_sim[i] - b[tree])))
    }
  }

#simulation for each tree snap
  for(tree in 1:n_trees_snap){
    for(i in 1:max_t){
      Y_hat_sim_snap[tree, i] <- 0.99  * exp( -exp(-rate_global_mean_snap * (t_sim[i] - b_snap[tree])))
    }
  }


#simulation for each site mean core
for(site in 1:n_sites_core){
    for(i in 1:max_t){
      Y_hat_sim_site[site, i] <- 0.99 * exp( -exp(-rate_global_mean * (t_sim[i] - b_site_sim_core[site])))
    }
      b_site_sim_core[site] ~ dnorm(site_halfway_point_core[site], LAMBDA3_core) #LAMBDA3_core[site]) 
} #end site sim loop

#simulation for each site mean snap
for(site in 1:n_sites_snap){
    for(i in 1:max_t){
      Y_hat_sim_site_snap[site, i] <- 0.99 * exp( -exp(-rate_global_mean_snap * (t_sim[i] -
                                                   b_site_sim_snap[site])))
    }
      b_site_sim_snap[site] ~ dnorm(site_halfway_point_snap[site], LAMBDA3_snap)
} #end site sim loop
    
}#end model
    ",fill=TRUE)
sink() 

jags <- jags.model('model_c.txt', 
                   data = list(
                     #core sites
                     Y = as.matrix(data_for_model_core_prop_open_ragged),  #data_for_model_prop_open[3,1]
                     t = as.matrix(data_for_model_core_day_experiment_ragged), 
                     nobs_per_tree_core = data_for_model_core_nobs_per_tree$nobs_per_tree,
                     n_trees_core = nrow(data_for_model_core_nobs_per_tree),
                     n_sites_core = max(data_for_model_core_nobs_per_tree$site_n_core),
                     site_vector_core = data_for_model_core_nobs_per_tree$site_n_core,
                     t_sim = 1:max(data_for_model_core_day_experiment, na.rm = TRUE),
                     max_t = max(p_core_sites$day_experiment, na.rm = TRUE),
                     
                     #snap sites
                     Y_snap = data_for_model_snap$prop_open, #Y_snap_df_ass
                     t_snap = data_for_model_snap$day_experiment,
                     #tree_n_snap = data_for_model_snap$tree_n_snap,
                     site_n_snap = data_for_model_snap$site_n_snap,
                     #site_vector_snap = unique(data_for_model_snap$site_n_snap),
                     n_trees_snap = max(data_for_model_snap$tree_n_snap),
                     n_sites_snap = max(data_for_model_snap$site_n_snap)
                   ),
                   n.chains = 3,
                   n.adapt = 100)  # diffuse priors

#dic <- dic.samples(jags, n.iter = 1000, type = "pD"); print(dic) #model DIC
#Sys.time()
update(jags,n.iter = 4000) 
mcmc_samples_params <- coda.samples(jags, variable.names=c("site_halfway_point_core"),  n.iter = 1000, thin = 3) #variables to monitor #"b", "c" "b_snap"
plot(mcmc_samples_params)
results_param <- summary(mcmc_samples_params)
results_params2 <- data.frame(results_param$statistics, results_param$quantiles) #multi-var model
results_params2$parameter<-row.names(results_params2)
results_params2$tree <- as.numeric(gsub("[^0-9.-]", "", results_params2$parameter))
hist(results_params2$Mean)

