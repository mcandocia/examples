#setwd('C:\\Users\\mscan\\Downloads\\workout_gpx\\strava_gpx')
setwd('/ntfsl/data/workouts')
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggmap)

center = c('longitude'=-87.63500,'latitude'=41.963480)
center2 = c('longitude'=-87.645154, 'latitude'=41.969371)

cframe = data.frame(lon=c(center[1], center2[1]), lat=c(center[2], center2[2]),
                    zone=c('zone 1 (100m)','zone 2 (80m)'), radius=c(100,80))

map = get_googlemap(center = center, zoom = 15)

#this was run with 'location':False in CENSOR_PARAMS
run = read.csv('FLAG_EXAMPLE/workout_gpx/garmin_fit/running_2017-09-25_23-39-16.csv')
#this was run with 'location':True in CENSOR_PARAMS
crun = read.csv('CENSOR_EXAMPLE/workout_gpx/garmin_fit/running_2017-09-25_23-39-16.csv')

ggmap(map) + geom_path(data=run, aes(x=position_long, y=position_lat, group=1), size=2) + 
  ggtitle('Original Map of Montrose Beach Run') + xlab('Longitude') + ylab('Latitude') + 
  theme(axis.title = element_text(size=rel(1.5)), plot.title=element_text(size=rel(2)))

zones = c('regular', 'zone 1 (100m)','zone 2 (80m)')

ggmap(map) + geom_path(data=run, aes(x=position_long, y=position_lat, group=1,
                                     color=zones[1+FLAG_0 + 2*FLAG_1]),
                       size=2) +
  scale_color_manual(values=c('regular'='#000000','zone 1 (100m)'='#EE5511','zone 2 (80m)'='magenta')) +
  guides(color=guide_legend('Path Region')) +
  geom_point(data=cframe, aes(x=lon, y=lat, color=zone)) +
  geom_point(data=cframe, aes(x=lon, y=lat, color=zone, size=radius/4), alpha=0.2) + 
  scale_size_identity() + 
  ggtitle('Zone-Highlighted Map of Montrose Beach Run',
          subtitle='the paths in zones 1 and 2 will be removed when data is censored') + 
  xlab('Longitude') + ylab('Latitude') + 
  theme(axis.title = element_text(size=rel(1.5)), plot.title=element_text(size=rel(2)),
        legend.title=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.5)),
        plot.subtitle=element_text(size=rel(1.3))) 
  

#shorthand to convert factor to number
anc <- function(x) as.numeric(as.character(x))

crun$group = 1
groupcnt = 1
cflag = FALSE
crun2=crun
for (i in 1:nrow(crun2)){
  if (crun2[i,'position_lat']=='[CENSORED]'){
    if (cflag){
      next
    }
    else{
      cflag=TRUE
      groupcnt = groupcnt+1
    }
  }
  else{
    cflag=FALSE
  }
  crun2[i,'group']=groupcnt
}
crun2 = crun2 %>% filter(position_lat != '[CENSORED]') %>%
  mutate(position_lat=anc(position_lat),
         position_long=anc(position_long))

ggmap(map) + geom_path(data=crun2, aes(x=position_long, y=position_lat, group=group,
                                       color=factor(group)), size=2) + 
  guides(color=guide_legend('Segment #')) + 
  ggtitle('Censored Map of Montrose Beach Run') + xlab('Longitude') + ylab('Latitude') + 
  theme(axis.title = element_text(size=rel(1.5)), plot.title=element_text(size=rel(2)),
        legend.title=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.5)))
