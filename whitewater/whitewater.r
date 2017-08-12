setwd('/hddl/workspace/whitewater')
library(plyr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

accidents = read.csv('accidents.csv')
accidents_causes = read.csv('accidents_causes.csv')
accidents_factors = read.csv('accidents_factors.csv')
accidents_injuries = read.csv('accidents_injuries.csv')
causes = read.csv('causes.csv')
factors = read.csv('factors.csv')
injuries = read.csv('injuries.csv')

print(injuries)
print(causes)

accident.short = accidents %>% select(-c(description, contactname, contactphone, contactemail, cause))

#experience
# I = inexperienced
# S = some experience
# E = experienced paddler
# X = extensive experience

#boattype 
# R = raft
# K = kayak
# O = open canoe
# I = inflatable kayak
# O = other
# T = (factor to Other) <- TODO

#GOAL:

# look at matrix of experience vs. difficulty, and do this by outcome (injury vs. death)
# also look at heights

#look at injury types among 
#  a) heights
#  b) difficulty
#  c) level of experience
#  d) cause of incident

#look at cause of incident among
#  a) heights
#  b) difficulty
#  c) level of experience
#  d) possibly a combination of these

accident.short$year = factor(substr(accident.short$accidentdate,1,4))
names(accident.short)[1] = 'accident_id'
names(factors)[1] = 'factor_id'
names(causes)[1] = 'cause_id'
names(injuries)[1] = 'injury_id'

#refactor experience level
accident.short$experience = mapvalues(accident.short$experience,
                                from=c('', ' ','E','I','S','X'),
                                to=c('N/A','N/A','Experienced','Inexperienced',
                                     'Some Experience','Extensive Experience'))

accident.short$experience = factor(as.character(accident.short$experience),
                             levels=c('N/A','Inexperienced','Some Experience',
                                      'Experienced','Extensive Experience'))

accident.short$year_numeric = as.numeric(as.character(accident.short$year))

accident.short = accident.short %>% mutate(difficulty=mapvalues(difficulty, from=c('','N/A',
                                                                                   'II+','III+','IV-','IV+','V+'),
                                                                to=c('Unknown','Not Applicable',
                                                                     'II','III','IV','IV','V')),
                                           difficulty=factor(as.character(difficulty), levels = c('I','II','III',
                                                                                                  'IV','V',
                                                                                                  'VI','Not Applicable','Unknown')))

rdc = remove_duplicate_colnames <- function(data){
  cnames = colnames(data)
  dupcnames = duplicated(cnames)
  return(data[,!dupcnames])
}

#expanded
#because I forgot dplyr is not the same as plyr, define these constants...
accident_id = 'accident_id'
factor_id = 'factor_id'
injury_id = 'injury_id'
cause_id = 'cause_id'
#now make joins for bar plots
a.factors = plyr::join(accident.short, accidents_factors, by=accident_id) %>% join(factors, by=factor_id)
a.injuries = plyr::join(accident.short, accidents_injuries, by=accident_id) %>% join(injuries, by=injury_id)
a.causes = plyr::join(accident.short, accidents_causes, by=accident_id) %>% join(causes, by=cause_id)

a.causes.factors = plyr::join(a.factors,a.causes,by=accident_id) %>% rdc
a.causes.injuries = plyr::join(a.injuries,a.causes,by=accident_id) %>% rdc
a.factors.injuries = plyr::join(a.injuries, a.factors, by=accident_id) %>% rdc
a.all = plyr::join(a.factors.injuries, a.causes, by=accident_id) %>% rdc

#plot experience vs. year
ggplot(accident.short %>%filter(year_numeric > 1969, type=='F'), aes(x=year_numeric, fill=experience,
                                                           alpha=ifelse(experience=='N/A',0.25,1))) + 
  geom_bar() + scale_alpha_identity()+ scale_fill_brewer('Experience\nLevel of Victim',palette="Dark2") + 
  theme_bw() + 
  theme(axis.text = element_text(size=12),
          axis.title=element_text(size=24),
          plot.title =element_text(hjust=0.5, size=24),
          plot.subtitle = element_text(hjust=0.5, size=12)) +
  xlab("Year") + ylab("Count") + ggtitle("Number of Whitewater Accidents Per Year",
                                         subtitle="since 1970; as recorded by American Whitewater") +
  scale_x_continuous(breaks=seq(1970,2020,5))

rename3 <- function(data){
  names(data)[3] = 'cnt'
  return(data)
}

line_breaker <- function(x){
  gsub(' ','\n',x)
}
cause_frequency_order = a.causes %>% group_by(cause) %>% summarise(cnt=n()) %>% 
  mutate(cause = factor(as.character(cause), levels = levels(cause)[order(-cnt)]))
cause_frequency_order = levels(cause_frequency_order$cause)

factor_frequency_order = a.factors %>% group_by(factor) %>% summarise(cnt=n()) %>% 
  mutate(factor = factor(as.character(factor), levels = levels(factor)[order(-cnt)]))
factor_frequency_order = levels(factor_frequency_order$factor)

# causes
ggplot(a.causes %>% group_by(cause) %>% summarise(cnt=n()) %>% 
         mutate(cause = factor(as.character(cause), levels = levels(cause)[order(-cnt)])) %>%
         filter(),
       aes(x=cause,y=cnt)) + geom_bar(stat='identity') + theme_bw() + 
  xlab('Cause') + ylab('Count') + scale_x_discrete(label=line_breaker) + 
  ggtitle('Frequency of Causes of Whitewwater Accidents Resulting in Death/Injury')

#injury vs. cause not a meaningful graph due to large number of NA values  for injuyy...omit injuries from this point forth
ggplot(a.causes.injuries %>% dcast(cause+injury~., drop=FALSE) %>% rename3(), 
       aes(x=cause,y=injury, fill=cnt)) + geom_tile()

#filter out NA values
#causes of 
ggplot(a.causes.factors %>% filter(FALSE| (!is.na(cause) & (!is.na(factor) | TRUE)) ) %>% 
         dcast(cause+factor~., drop=FALSE) %>% rename3() %>% 
         mutate(cause=factor(as.character(cause), levels=cause_frequency_order),
                factor=factor(as.character(factor), levels=factor_frequency_order)),
       aes(x=cause,y=factor, fill=cnt))  + geom_tile() + 
  scale_fill_gradient('Count', low='white',high='red') + theme_bw() + 
  scale_x_discrete(label=line_breaker) + xlab('Cause of Accident') +
  ylab('Contributing Factor to Accident') + 
  geom_text(aes(x=cause,y=factor,label=cnt), alpha=0.5,size=8) + 
  ggtitle('Cause of Accident vs. Contributing Factors in Whitewater Accidents',
          subtitle='from recorded accidents since 1970') + theme(axis.text = element_text(size=12),
                                                                        axis.title=element_text(size=24),
                                                                        plot.title =element_text(hjust=0.5, size=24),
                                                                        plot.subtitle = element_text(hjust=0.5, size=12))


#experience vs. level of rapids

ggplot(accident.short %>% dcast(experience + difficulty ~ .,drop=FALSE) %>% rename3() %>% 
         filter(difficulty != "Unknown") %>%
         group_by(experience) %>% mutate(pct = cnt/sum(cnt)), aes(x=experience,y=difficulty,fill=pct)) +
  geom_tile() + scale_fill_gradient('Percentage of\nExperience Level', label=percent,low='white',high='red') +
  geom_text(aes(label=percent(pct)), alpha=0.8, size=8) + xlab("Experience of Victim") + ylab("Difficulty Ranking of River") +
  theme_bw() + theme(axis.text = element_text(size=12),
                     axis.title=element_text(size=24),
                     plot.title =element_text(hjust=0.5, size=24),
                     plot.subtitle = element_text(hjust=0.5, size=12)) + 
  ggtitle('Victim Experience vs. River Difficulty in Whitewater Accidents',
          subtitle='percentages are a proportion of each experience category; data from recorded accidents since 1970')


#note in https://www.americanwhitewater.org/content/Accident/detail/accidentid/4079 that victim had history of 
#poor health issues as contributing factor, died in hospital (considered 'I')