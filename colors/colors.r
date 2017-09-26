library(ggplot2)
library(plyr)
library(dplyr)
library(ucidata)
library(cetcolor)
library(reshape2)
library(RColorBrewer)
library(ggthemes)

setwd('/hddl/workspace/colors/')

lpng <- function(x, height=640, width=780){
  png(x, height=height, width=width)
}

bigger_text = theme(plot.title=element_text(size=rel(1.8)), 
                    legend.title=element_text(size=rel(2)),
                    legend.text=element_text(size=rel(1.3)),
                    axis.title = element_text(size=rel(2)))

#iris - example where regular scales can work
lpng('iris_default.png')
print(
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + 
  geom_point() + ggtitle("Iris Sepal Length vs. Petal Length by Species, using Default ggplot Palette") +
  bigger_text
)
dev.off()
#from RColorBrewer
dark_palette = brewer.pal(3, 'Dark2')

#from ggthemes
cbbPalette <- colorblind_pal()(3)
lpng('iris_dark2.png')
print(
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + 
  geom_point() + ggtitle("Iris Sepal Length vs. Petal Length by Species, using Dark2 Palette") +
  scale_color_manual(values = dark_palette) +
  bigger_text
)
dev.off()

lpng('iris_cb.png')
print(
ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, color=Species)) + 
  geom_point() + ggtitle("Iris Sepal Length vs. Petal Length by Species, using Colorblind-Friendly Palette") +
  scale_color_manual(values = cbbPalette) +
  bigger_text
)
dev.off()
#mpg - example where there are a few too many colors to use for classes
lpng('mpg_default.png')
print(
ggplot(mpg, aes(x=cty, y=hwy, color=class)) + 
         geom_jitter(alpha=0.8, size=3.2) + 
  ggtitle('Gas Mileage of Various Car Types, using Default ggplot Palette') + 
  xlab('City MPG') + ylab('Highway MPG') +
  bigger_text
)
dev.off()

lpng('mpg_dark2.png')
print(
ggplot(mpg, aes(x=cty, y=hwy, color=class)) + 
  geom_jitter(alpha=0.8, size=3.2) + 
  ggtitle('Gas Mileage of Various Car Types, using Dark2 Palette') + 
  xlab('City MPG') + ylab('Highway MPG') + 
  scale_color_manual(values=brewer.pal(7, 'Dark2')) +
  bigger_text
)
dev.off()

#minivan and subcompact might be difficult to distinguish for tritan colorblindness
lpng('mpg_cb.png')
print(
ggplot(mpg, aes(x=cty, y=hwy, color=class)) + 
  geom_jitter(alpha=0.8, size=3.2) + 
  ggtitle('Gas Mileage of Various Car Types, using Colorblind-Friendly Palette') + 
  xlab('City MPG') + ylab('Highway MPG')+ 
  theme_dark() + theme(panel.background = element_rect(fill='#BBBBBB')) +
  bigger_text

)
dev.off()

lpng('mpg_cb_shp.png')
print(
ggplot(mpg, aes(x=cty, y=hwy, color=class, shape=class)) + 
  geom_jitter(alpha=0.8, size=3.2) + 
  ggtitle('Gas Mileage of Various Car Types, using Colorblind-Friendly Palette w/Shapes') + 
  xlab('City MPG') + ylab('Highway MPG') + 
  scale_color_manual(values=colorblind_pal()(7)) + 
  theme_dark() + theme(panel.background = element_rect(fill='#BBBBBB')) +
  scale_shape_manual(values=c(20,20,20,15,19,19,17)) +
  bigger_text
)
dev.off()

mmpg = melt(mpg, measure.vars = c('cty','hwy')) 

lpng('mpg_boxplot.png')
print(
ggplot(mmpg, aes(x=variable, y=value, fill=class)) + geom_boxplot() + 
  facet_grid(.~class) + theme_bw() + 
  ylab('gas mileage') + xlab('') + 
  scale_fill_manual(values=brewer.pal(7,'Dark2')) + 
  ggtitle("Gas Mileage of Various Car Types via Boxplots") +
  bigger_text
)
dev.off()
#co2 - try using for grid plot to show contrasting colors on a scale

co2df = data.frame(CO2=as.numeric(co2), year = rep(1959:1997, each=12), month=rep(month.name, 39)) %>% 
  mutate(month=factor(as.character(month), levels=month.name))

lpng('co2_default.png')
print(
ggplot(co2df, aes(x=month, y=year, fill=CO2)) + geom_tile() + 
  scale_x_discrete(position='top') + 
  scale_y_reverse(breaks = 1959:1997, expand = c(0, 0.1)) + 
  ggtitle(bquote(paste('CO'[2],' Levels in Mauna Loa, using Default Color Scale')))+ 
  guides(fill=guide_legend(bquote(paste("CO"[2], " (ppm)")) ))+
  xlab('') + ylab('') + 
  bigger_text
)
dev.off()

lpng('co2_cet.png')
print(
ggplot(co2df, aes(x=month, y=year, fill=CO2)) + geom_tile() + 
  scale_x_discrete(position='top') + 
  scale_y_reverse(breaks = 1959:1997, expand = c(0, 0.1)) + 
  ggtitle(bquote(paste('CO'[2],' Levels in Mauna Loa, using Perceptually Uniform Color Scale'))) + 
  scale_fill_gradientn(colors=cet_pal(5, 'inferno')) + 
  guides(fill=guide_legend(bquote(paste("CO"[2], " (ppm)")) ))+
  xlab('') + ylab('') + 
  bigger_text
)
dev.off()

#have white be a "middle option"


#ucidata - wine: use to demonstrate that color can be used to represent an object itself
lpng('wine_plot.png')
print(
  ggplot(wine, aes(x=fixed_acidity, y=volatile_acidity, color=tolower(color) )) + 
    geom_jitter( alpha=0.6, size=0.64, height=0.005, width=0.035) + 
    scale_color_manual(values=c('red'='red','white'='white')) +
    ggtitle("Wine Fixed and Volatile Acidity by Color") + 
    theme_dark() + guides(color=guide_legend("Wine Color")) + 
    xlab("Fixed Acidity") + ylab('Volatile Acidity') +
    bigger_text
)
dev.off()











#stacked bar plots color grayscale example (need to convert duplicate image to grayscale afterwards)
lpng('diamond_plot.png', height=480)
print(
ggplot(diamonds, aes(x=cut, fill=color)) + geom_bar() + 
  ggtitle('Count and Color of Diamonds by Cut, Default (rainbow) Palette') + 
  bigger_text
)
dev.off()

lpng('diamond_plot_gs.png', height=480)
print(
  ggplot(diamonds, aes(x=cut, fill=color)) + geom_bar() + 
    ggtitle('Count and Color of Diamonds by Cut, Default (rainbow) Palette in Grayscale') + 
    bigger_text
)
dev.off()

lpng('diamond_plot_ylorrd.png', height=480)
print(
  ggplot(diamonds, aes(x=cut, fill=color)) + geom_bar() + 
    ggtitle('Count and Color of Diamonds by Cut, Yellow-Orange-Red Palette') + 
    scale_fill_manual(values=brewer.pal(7, 'YlOrRd')) + 
    bigger_text
)
dev.off()


lpng('diamond_plot_ylorrd_gs.png', height=480)
print(
  ggplot(diamonds, aes(x=cut, fill=color)) + geom_bar() + 
    ggtitle('Count and Color of Diamonds by Cut, Yellow-Orange-Red Palette in Grayscale') + 
    scale_fill_manual(values=brewer.pal(7, 'YlOrRd')) + 
    bigger_text
)
dev.off()