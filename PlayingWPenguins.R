# This script is for the purpose of showing I can visualise data to the standard
# that I currently can

pacman::p_load(palmerpenguins, ggplot2, tidyverse, wesanderson)

data(package = 'palmerpenguins')

peng <- penguins_raw

peng$`Clutch Completion`<-as.factor(peng$`Clutch Completion`)
peng$Sex<-as.factor(peng$Sex)
peng$Island<-as.factor(peng$Island)
#peng$Species<-as.factor(peng$Species)

# first try
dplyr::filter(peng, Sex!='NA') %>%
ggplot(aes(Sex, `Body Mass (g)`))+
  geom_boxplot()+
  #geom_point(alpha=0.4)+
  ggbeeswarm::geom_beeswarm(aes(alpha = `Clutch Completion`, colour=Species))+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = 'Body mass (g)')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Body Masses of Penguins by Sex", # Update your titles
       subtitle="from the palmerpenguins package (R)",
       caption="Source: palmerpenguins",
       x="Sex")+
  labs(colour = 'Clutch Completion')

# Further developed, practiced a bit with visualising several variables clearly
# (This isn't to do statistics on)
dplyr::filter(peng, Sex!='NA') %>%
  ggplot(aes(Sex, `Body Mass (g)`, fill=Species))+
  geom_boxplot(show.legend = T)+
  scale_fill_manual(values = wes_palette("IsleofDogs1"))+
  #geom_point(alpha=0.4)+
  ggbeeswarm::geom_beeswarm(aes(alpha = `Clutch Completion`,color = Island, shape = Species))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = 'Body mass (g)')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Body Masses of Penguins by Sex", # Update your titles
       subtitle="further differentiated by their island, their species\nand whether their clutch is complete",
       caption="Source: palmerpenguins",
       x="Sex",
       #shape="Island",
       )#+
 # labs(colour = 'Clutch Completion')
