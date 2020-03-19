##' Import, Merge, and Format incidence data
##' - Data are used in plotting curves and predicting importations



# SETUP -------------------------------------------------------------------

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('lubridate')) install.packages('lubridate'); library(lubridate)
if(!require('gsheet')) install.packages('gsheet'); library(gsheet)

ncov_timeseries_url <- "https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?usp=sharing&sle=true"




# LAUREN'S DATA -----------------------------------------------------------

library(tidyverse)

# Read and use data from Lauren Gardners website
conf_cases <- gsheet::gsheet2tbl(ncov_timeseries_url)
case_loc_data <- conf_cases[,1:5]
conf_cases <- conf_cases %>% gather(key="t", value="cum_cases",-`Province/State`,-`Country/Region`,-Lat,-Long, -`First confirmed date in country (Est.)`)
conf_cases <- conf_cases %>% mutate(cum_cases = ifelse(is.na(cum_cases), 0, cum_cases))

# Sum days with multiple
conf_cases <- conf_cases %>% mutate(date = as.Date(lubridate::mdy_hm(t))) %>% 
    rename(prov_state=`Province/State`, country=`Country/Region`) %>%
    arrange(country, prov_state, date) %>%
    group_by(prov_state, country, date) %>%
    summarize(cum_cases = max(cum_cases))
conf_cases_first <- conf_cases %>% group_by(prov_state, country) %>% 
    summarize(date = min(date)-1) %>% mutate(cum_cases = 0)
conf_cases <- conf_cases %>% bind_rows(conf_cases_first) %>% arrange(prov_state, country, date)

conf_cases <- conf_cases %>% mutate(cases_incid=0) %>% group_by(prov_state, country) %>% 
    mutate(cases_incid = c(0,diff(cum_cases)))
conf_cases <- conf_cases %>% mutate(epiweek = lubridate::epiweek(date)) 
conf_cases <- conf_cases %>% mutate(China_source=(grepl("china", tolower(country)) | grepl("hong kong", tolower(country)) | 
                                                      grepl("macau", tolower(country))) | grepl("taiwan", tolower(country)))
#conf_cases <- conf_cases %>% complete(date, epiweek, prov_state, country)

write_csv(conf_cases, "data/china_incid_data_report.csv")





# Get totals data

conf_cases_total <- conf_cases %>% filter(China_source==TRUE) %>% group_by(date) %>%
    summarize(cases_incid = sum(cases_incid, na.rm = TRUE)) %>% 
    mutate(cases_cum = cumsum(cases_incid))

write_csv(conf_cases_total, "data/china_incid_total.csv")

plot_current_totals <- function(){
    library(ggplot2)
    conf_cases_total <- readr::read_csv("data/china_incid_total.csv", col_types = cols())
    ggplot(conf_cases_total, aes(date, cases_incid)) + geom_bar(stat="identity") + 
        ylab("Incident Cases (no.)")
}
plot_current_totals()







# LINELIST DATA -----------------------------------------------------------
# ** this is important to use for validation of importations

linelist_url <- "https://docs.google.com/spreadsheets/d/1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/htmlview#"
ll_data <- gsheet::gsheet2tbl(linelist_url); rm(linelist_url)
colnames(ll_data)
if (!("linelist_current.csv" %in% list.files("data"))){ # save first backup
    write_csv(ll_data, "data/linelist_backup.csv"); rm(ll_data_backup)
} else {
    ll_data_backup <- read_csv("data/linelist_current.csv") # load and resave a backup. Just in case the googlesheet gets screwed up
    write_csv(ll_data_backup, "data/linelist_backup.csv")
    rm(ll_data_backup)
}


# Format Dates
date_origs <- ll_data %>% as_tibble() %>% dplyr::select_at(vars(matches("date"))) %>% rename_all(funs(paste0(., "_orig")))
ll_data <- ll_data %>% mutate_at(vars(contains("date")), dmy)
ll_data <- ll_data %>% bind_cols(date_origs)
#mutate(date_onset_symptoms = dmy(date_onset_symptoms))

write_csv(ll_data, "data/linelist_current.csv") # save current linelist


# # Filter to just those when any mention of shenzhen
# shen_rows <- apply(ll_data, 1, FUN=function(x) sum(grepl("shenzhen", x, ignore.case = TRUE)))>0
# ll_data_shenzhen <- ll_data[shen_rows, ]
# 
# # get aggragated data
# shen_data_aggr <- ll_data_shenzhen %>% count(date_confirmation)
#
# Plot the epi curve of these
# ggplot(shen_data_aggr, aes(x=date_confirmation, y=n)) +
#     geom_bar(stat="identity")





# REMOVE EVERYTHING FROM THE CURRENT WORKSPACE
#rm(list = ls()); gc()
