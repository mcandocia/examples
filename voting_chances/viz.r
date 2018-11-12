library(ggplot2)
library(scales)
library(dplyr)

notate_effect <- function(x){
  sprintf('P(lost)=%.3g%%\nP(win and made a difference)=%.3g%%\nP(win and didn\'t make a difference)=%.3g%%',100*x[1,2], 100*x[2,2],100*x[3,2])
}

x = 0:10000
color_options=c('Lost and didn\'t make a difference','Won and made a difference','Won but didn\'t make a difference')

df = data.frame(
  vals=x,
  probs=dbinom(x, 10000, 0.5),
  cumulative=pbinom(x, 10000, 0.5),
  color=color_options[(x>=5000)+1 + (x>=5001)]
)

color_values = c('blue','red','black')
names(color_values) = color_options

change1 = df %>% group_by(color) %>% summarize(sum(probs))

ggplot(df %>% filter(vals >= 4900 & vals <= 5100)) + geom_bar(aes(x=vals,y=probs, fill=color, color=color), stat='identity') + 
  scale_color_manual('Effect on Election', values=color_values) + scale_fill_manual('Effect on Election', values=color_values) + scale_y_continuous(label=percent) + 
  xlab('Everyone else\'s votes') + ylab('Probability of outcome') + ggtitle('Probability of casting a tie-breaking vote \nin a 10,000-voter election',
                                                                            subtitle=paste('assuming 2 candidates with equal support', notate_effect(change1), sep='\n')
                                                                            )



df2  = data.frame(
  vals=x,
  probs=dbinom(x, 10000, 0.5),
  cumulative=dbinom(x, 10000, 0.5),
  color=color_options[(x>=4995)+1 + (x>=5001)]
)

change2 = df2 %>% group_by(color) %>% summarize(sum(probs))

ggplot(df2 %>% filter(vals >= 4900 & vals <= 5100)) + geom_bar(aes(x=vals,y=probs, fill=color, color=color), stat='identity') + 
  scale_color_manual('Effect on Election', values=color_values) + scale_fill_manual('Effect on Election', values=color_values) + scale_y_continuous(label=percent) + 
  xlab('Everyone else\'s votes') + ylab('Probability of outcome') + ggtitle('Probability of a group of 6 friends casting a tie-breaking vote \nin a 10,000-voter election',
                                                                            subtitle=paste('assuming 2 candidates with equal support and friends vote the same way', notate_effect(change2), sep='\n')
  )

df3 = data.frame(
  vals=x,
  probs=dbinom(x, 10000, 0.49),
  cumulative=pbinom(x, 10000, 0.49),
  color=color_options[(x>=4995)+1 + (x>=5001)]
)

change3 = df3 %>% group_by(color) %>% summarize(sum(probs))

ggplot(df3 %>% filter(vals >= 4900 & vals <= 5100)) + geom_bar(aes(x=vals,y=probs, fill=color, color=color), stat='identity') + 
  scale_color_manual('Effect on Election', values=color_values) + scale_fill_manual('Effect on Election', values=color_values) + scale_y_continuous(label=percent) + 
  xlab('Everyone else\'s votes') + ylab('Probability of outcome') + ggtitle('Probability of a group of 6 friends casting a tie-breaking vote \nin a 10,000-voter election',
                                                                            subtitle=paste('assuming 2 candidates with 49% and 51% support and all friends vote the same way', notate_effect(change3), sep='\n')
  ) 


wide_x = 0:10000000
df4 = data.frame(
  vals=wide_x,
  probs=dbinom(wide_x, 10000*100, 0.5),
  cumulative=pbinom(wide_x, 10000*100, 0.5),
  color=color_options[(wide_x>=499995)+1 + (wide_x>=500001)]
)

change4 = df4 %>% group_by(color) %>% summarize(sum(probs))

ggplot(df4 %>% filter(vals <= 500100 & vals >= 499900)) + geom_bar(aes(x=vals,y=probs, fill=color, color=color), stat='identity') + 
  scale_color_manual('Effect on Election', values=color_values) + scale_fill_manual('Effect on Election', values=color_values) + scale_y_continuous(label=percent) + 
  xlab('Everyone else\'s votes') + ylab('Probability of outcome') + ggtitle('Probability of a group of 6 friends casting a tie-breaking vote \nin a 1,000,000-voter election',
                                                                            subtitle=paste('assuming 2 candidates with equal support and all friends vote the same way', notate_effect(change4), sep='\n')
  ) 

wide_x = 0:1000000
df5 = data.frame(
  vals=wide_x,
  probs=dbinom(wide_x, 10000*100, 0.499),
  cumulative=pbinom(wide_x, 10000*100, 0.499),
  color=color_options[(wide_x>=499995)+1 + (wide_x>=500001)]
)

change5 = df5 %>% group_by(color) %>% summarize(sum(probs))

ggplot(df5 %>% filter(vals <= 500100 & vals >= 498900)) + geom_bar(aes(x=vals,y=probs, fill=color, color=color), stat='identity') + 
  scale_color_manual('Effect on Election', values=color_values) + scale_fill_manual('Effect on Election', values=color_values) + scale_y_continuous(label=percent) + 
  xlab('Everyone else\'s votes') + ylab('Probability of outcome') + ggtitle('Probability of a group of 6 friends casting a tie-breaking vote \nin a 1,000,000-voter election',
                                                                            subtitle=paste('assuming 2 candidates with 49.9% and 50.1% support and all friends vote the same way', notate_effect(change5), sep='\n')
  ) 


