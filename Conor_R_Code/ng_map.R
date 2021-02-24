rm(list=ls())

link="https://github.com/adam-porton/PubPol543/blob/main/Data/r1_sect_7.csv?raw=true"
nigeria_df <- as.data.frame(read.csv(file = url(link)))
link="https://github.com/adam-porton/PubPol543/blob/main/Data/r1_sect_a_3_4_5_6_8_9_12.csv?raw=true"
nigeria_weights <- as.data.frame(read.csv(file = url(link)))
nigeria_weights <- nigeria_weights[,c("hhid","wt_baseline")]
nigeria_df <- merge(nigeria_df, nigeria_weights, by="hhid")

nigeria_df$state[(substring(nigeria_df$state,2,2) == ".")] <- substr(nigeria_df$state[(substring(nigeria_df$state,2,2) == ".")] , 4, length(nigeria_df$state))
nigeria_df$state[(substring(nigeria_df$state,3,3) == ".")] <- substr(nigeria_df$state[(substring(nigeria_df$state,3,3) == ".")] , 5, length(nigeria_df$state))
nigeria_df$state[(nigeria_df$state == "FCT")] <- "Fct, Abuja"
nigeria_df$pct_lost_income <-  as.integer(nigeria_df$s7q2 == "3. Reduced")
temp <- nigeria_df[,c("hhid", "state")]
nigeria_df <- aggregate(pct_lost_income ~ hhid, nigeria_df, max)
nigeria_df <- merge(nigeria_df, temp, by="hhid")
nigeria_df <- aggregate(pct_lost_income ~ state, nigeria_df, mean)
nigeria_df$pct_lost_income <- nigeria_df$pct_lost_incom * 100

linkMap="https://github.com/cfhenn/nigeria_covid_survey_r_visualizations/raw/main/Data/nigeria_geojson.geojson" # link desde github
library(sf)

map_ng=read_sf(linkMap)


map_ng_vars=merge(map_ng, #map first
                   nigeria_df, 
                   by='state') 

library(ggplot2)
# plot original map
base=ggplot(data=map_ng) + geom_sf(fill='grey90',
                                     color=NA) + theme_classic()

colMap= base + geom_sf(data=map_ng_vars,
                       aes(fill=pct_lost_income),
                       color=NA)

colMap=colMap + scale_fill_gradient(low = 'white',
                             high= 'black',
                             name = "% of state population")


colMap=colMap + ggtitle("Economic impact of COVID-19 in Nigeria by state:", 
                        subtitle = "Percentage of each state's population who have lost income since March 2020")

colMap <- colMap + labs(title=NULL, x =NULL, y = NULL, caption = "Source: LSMS-supported high-frequency phone surveys on COVID-19")

colMap
