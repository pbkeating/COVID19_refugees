
print(getwd())


start_time <- Sys.time()

# Get arguments from bash script ------------------------------------------

# Pull in Arguments from bash script
args <- commandArgs(TRUE) # provides runs, cores
eval(parse(text=args)) 
print(args)

# # Get Array Task ID -- use this for the country index
# slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
# print(slurm_arrayid)
 


# R0 Values  ----------------------------------------------


if (!exists("n.Rs")){
    n.Rs <- 20
}

print(n.Rs)

# Simulation specifics   ----------------------------------------------
#n.sims <- 50 # simulations per R
run_time <- 550
start_date_="2020-03-15"
step.size <- .25
seeds <- 1


if (R0_val == "low"){
# Controlled transmission 
    R0 <- runif(n.Rs, 1.5,2)
} else if (R0_val=="mid"){
# Moderate transmission
    R0 <- runif(n.Rs, 2, 3)
} else if (R0_val=="high"){
    # Inflated transmission (inflated by the diphtheria inflation factor)
    R0 <- runif(n.Rs, 2*1.65, 3*1.65)
}

print(R0)





# Transmission Characteristics ---------------------------------
# we are going to assume recovery has mean of 7 days from infection
# then we will use a beta consistent with our estimates from above
# https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus

# incubation period 5.2 days based on an estimate from Lauer et al. 2020
sigma <- 1/(5.2) # subtract 1 for latent period adjustment
# time from symptom onset to recovery  
n_Icomp <- 3 # number of I compartments (using 3 to capture Erlang distribution)
gamma <-  1/6 * n_Icomp 



# sample point estimates of our betas from above
betas <- R0 * gamma / n_Icomp






# SETUP -------------------------------------------------------------------

require(tidyverse)
#require(gridExtra)
# require(rstan)
# library(RColorBrewer)
# library(RgoogleMaps)

options(scipen = 999)


#source("R/DataLoadUtils.R")
source("R/BasicEpiAnalyses.R")
source("R/DataUtils.R")
source("R/model_functions.R")
source("R/StochasticSEIR.R")
source("R/CalcHospDeaths.R")





# SETUP MODEL -------------------------------------------------------------

load(file="results/p_severe_kutupalong.rda") # Loads




# Population Size - Kutupalong camp
N <- 600000




# Hospitalization   ----------------------------------------------


# time from hospitalization to discharge mean= 11.5, sd=3.625
# time from hospitalization to death mean= 11.2, sd= 1.88
# Time to hospitalization among symptom-based, mean 4.65 days (95% CI 0.93-12.42) (Bi et al. 2020)
# Time from hospitalization to discharge,  11.5 days (95% CI 8.0-17.3) (Sanche et al. 2020)
# Time from hospitalization to death, 11.2 days (95% CI 8.7-14.9) (Sanche et al. 2020)

#p_hosp  <- c(p_severe_kutupalong, p_hosp_kutupalong_high)
p_death <- .1
p_vent  <- .15
p_ICU   <- .264

time_hosp_pars = c(1.23, 0.79)
time_death_pars = c(log(11.25), log(1.15))
time_disch_pars = c(log(11.5), log(1.22)) 
time_ICU_pars = c(log(10.5), log((10.5-7)/1.35))
time_ICUdur_pars = c(log(17.46), log(4.044))
time_vent_pars = c(log(10.5), log((10.5-8)/1.35))




# RUN MODEL ---------------------------------------------------------------


# Load SEIR model sim results (before hospitalization stuff)
sim_res <- read_csv(paste0("results/sim_res_nohosp_", R0_val,".csv"))

# Get Hospitalizations
sim_hospdeath <- build_hospdeath_par(cores=cores, sim_res, p_hosp=p_severe_kutupalong, 
                                     p_death=p_death, p_hosp_type = "gamma",
                                     time_hosp_pars, time_death_pars, time_disch_pars)
# add ventilation
sim_hospdeath <- sim_hospdeath %>% mutate(incidVent = rbinom(incidH, incidH, rep(p_vent, nrow(sim_hospdeath))))

# Get simulation info to fill in where needed
sim_info <- sim_res %>% select(sim, beta_ind, sim_beta, sim_id, beta, seeds) %>% 
    filter(!duplicated(paste0(sim, beta_ind)))

# Merge with Sim results
sim_res_full <- full_join(sim_res %>% select(-c(sim, beta_ind, sim_id, beta, seeds)), sim_hospdeath, by=c("date"="t", "sim_beta"="sim_beta"))
sim_res_full <- full_join(sim_res_full, sim_info, by="sim_beta") %>%
    select(sim_beta, sim, beta_ind, sim_id, beta, seeds, date, day, everything())
sim_res_full <- sim_res_full %>% replace_na(list(incidH=0, incidD=0, incidVent=0, incid=0, exposed=0, hosp_curr=0)) %>% 
    mutate(scenario = "Mid") %>% rename(t=date) %>%
    filter(!is.na(day))

sim_res_full <- sim_res_full %>% 
    arrange(sim_beta, t) %>%
    group_by(sim_beta) %>%
    mutate(cum_hosp = cumsum(incidH),
           cum_vent = cumsum(incidVent),
           cum_death = cumsum(incidD)) %>% ungroup()

sim_res_full <- sim_res_full %>% filter(!is.na(t))  




# Save it
dir.create("results", recursive = TRUE, showWarnings = FALSE)
write_csv(sim_res_full, paste0("results/sim_res_", R0_val,".csv"))


print(Sys.time() - start_time)
print(memory.size()/1000)

