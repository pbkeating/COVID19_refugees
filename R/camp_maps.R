

## Plot map and other details of Kutupalong camp



# SETUP -------------------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(grid)
library(ggmap)

# install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#                    "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library(sf)

source("R/DataUtils.r")



#Shapefiles dir
dir_path <- "data/shapefiles"
#Adm1 boundaries
adm1_file <- "190310_Outline_Rohingya_Refugee_Camp_A1.shp"
#Adm2 boundaries
adm2_file <- "190310_Outline_Rohingya_Refugee_CampBlock_A2.shp"
#Health sites
healthsites_file <- "data/reach_bgd_data_whohealthservices_12122017.csv"



# DATA --------------------------------------------------------------------

# Shapefiles
crs_ <- 4326 #28992
# https://data.humdata.org/dataset/outline-of-camps-sites-of-rohingya-refugees-in-cox-s-bazar-bangladesh
shp1 <- read_sf(dsn = file.path(dir_path, adm1_file), stringsAsFactors = F) %>% 
    st_transform(crs = crs_)

shp2 <- read_sf(dsn = file.path(dir_path, adm2_file), stringsAsFactors = F) %>% 
    st_transform(crs = crs_)

# Camp names
shp1$New_Camp_N


# Health sites
# https://data.humdata.org/dataset/reach-bangladesh-who-health-services
health_sites <- read_csv(healthsites_file) %>% as.data.frame()
# Get rid of NA cols 
health_sites <- health_sites[,colSums(is.na(health_sites))!=nrow(health_sites)]
health_sites <- health_sites %>% rename(facility_type = `Facility type`) %>%
    mutate(hospital = as.factor(ifelse(facility_type=="Hospital", 1,0))) %>%
    mutate(facility_type = as.factor(facility_type)) %>%
    mutate(facility_type = relevel(facility_type, "Hospital"))
health_sites_points <- st_as_sf(health_sites, coords = c("Longitude", "Latitude"), crs = crs_)#, agr = "constant")


    
# CAMP MAP ----------------------------------------------------------------

map1 <- ggplot() + 
            geom_sf(data=shp2, colour = "lightgrey", fill = NA) +
            geom_sf(data=shp1, colour = "black", fill = NA) +
            geom_sf(data=health_sites_points, color="red", size=1) +
            #geom_point(data = health_sites, mapping = aes(x = Longitude, y = Latitude), colour = "red") + 
            theme_classic() + 
            scale_shape("Health Sites") +
            coord_sf(xlim = c(92.12, 92.19), ylim = c(21.12, 21.23), expand = FALSE)
map1



# Make hospitals bigger, diff color

table(health_sites$`Facility type`)

library(grid)


map2 <- ggplot() + 
    geom_sf(data=shp1, colour = NA, fill="grey97") +
    geom_sf(data=shp2, colour = "grey85", fill=NA) +
    geom_sf(data=shp1, colour = "black", fill=NA) +
    #geom_sf(data=health_sites_points, color="red", size=1) +
    geom_point(data = health_sites, 
               mapping = aes(x=Longitude, y=Latitude, color=facility_type, size=facility_type, shape=facility_type)) + 
    scale_size_manual("Facility type", values=c(3, rep(1,8))) + 
    scale_shape_manual("Facility type", values = c(17,rep(16,8))) +
    scale_color_discrete("Facility type") +
    #theme_classic() +
    coord_sf(xlim = c(92.12, 92.19), ylim = c(21.12, 21.23), expand = FALSE) +
    theme_bw() +
    theme(line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())


map2


pdf("figures/map.pdf", width=8, height=8)
map2
dev.off()
# I like this one enough. next




# Population by Age -------------------------------------------------------

pop_age <- read_csv("data/age_kutupalong.csv")
age_smooth <- smooth.spline(pop_age$mid, pop_age$tot_per_year)
pr_age <- predict(age_smooth, data.frame(mid=0:89)) %>% as.data.frame()
pr_age <- pr_age %>% rename(age=mid, prop=mid.1) %>% mutate(prop = prop / sum(pr_age$mid.1))
sum(pr_age$prop)

plot(pr_age$age, pr_age$prop)
points(pop_age$mid, pop_age$tot_per_year, col="blue", pch=20)

# Get age by 10yr groups
pr_age <- pr_age %>% mutate(age10 = floor(age/10)*10)
pr_age10 <- pr_age %>% group_by(age10) %>% summarise(pr10 = sum(prop)) %>%
    mutate(age = paste0(age10, "-", age10+9))


# Get age for China
pr_ageCh <- get_age_pop(country="China")
pr_ageCh <- data.frame(age=names(pr_ageCh), n=as.integer(pr_ageCh))
pr_ageCh <- pr_ageCh %>% mutate(pr10 = n / sum(pr_ageCh$n))

age10_ <- seq(0,100, 10)
age_dat <- bind_rows(pr_age10 %>% select(age, pr10) %>% mutate(loc="Kutupalong"),
                     pr_ageCh %>% select(age, pr10) %>% mutate(loc="China")) %>%
    mutate(age = factor(age, levels = paste0(age10_, "-", age10_+9), 
                             labels = paste0(age10_, "-", age10_+9)))


p_age <- ggplot(age_dat, aes(age, pr10, group=loc, color=loc)) + 
    geom_line(size=1.5) +
    scale_color_discrete("Location") +
    xlab("Age") + ylab("Proportion") +
    theme_classic() +
    theme(legend.position = c(0.8, 0.7)) 
p_age


#
pdf("figures/age.pdf", width=6, height=4)
p_age
dev.off()

#












# 
# 
# # Try Google Maps ---------------------------------------------------------
# ##--> dont really like how it looks
# 
# 
# library(sp)
# register_google(key="AIzaSyCTrf56nQ_BD4Nt6DA-cHxmeDCcjX9KNik")
# 
# 
# p <- ggmap(get_googlemap(center = c(lon = 92.1615, lat = 21.203),
#                          zoom = 15, scale = 2,
#                          maptype ='hybrid',
#                          color = 'color'))
# p + geom_polygon(data=shp1, colour = "black", fill = NA)
# 
# p + geom_point(data = health_sites, mapping = aes(x = Longitude, y = Latitude), colour = "red")
# 
# 
# 
# 
# 
# 
# kutupalong_map <- get_map(
#     bbox = unname(st_bbox(shp1_)),
#     zoom = 10, maptype = 'toner-lite', source = 'stamen'
# ) %>% ggmap()







