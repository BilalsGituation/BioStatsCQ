# This script is for the purpose of showing I can visualise data to the standard
# that I currently can, keep records of notes in the versioning

pacman::p_load(palmerpenguins, ggplot2, tidyverse, wesanderson)
#data(package = 'palmerpenguins')

peng <- penguins_raw

peng <-dplyr::filter(peng, Sex!='NA')
peng <-dplyr::filter(peng, `Clutch Completion`!='NA')
peng <-dplyr::filter(peng, Species!='NA')

peng$`Clutch Completion`<-as.factor(peng$`Clutch Completion`)
peng$Sex<-as.factor(peng$Sex)
peng$Island <- factor(peng$Island, labels = c("Biscoe\nIsland", "Dream\nIsland", "Torgersen\nIsland"))
peng$Species <- factor(peng$Species, labels = c("Adelie (P. adeliae)", "Chinstrap (P. antarctica)", "Gentoo (P. papua)"))

# first try, the next one went through a more rigorous editing process
dplyr::filter(peng, Sex!='NA') %>%
ggplot(aes(Sex, `Body Mass (g)`))+
  geom_boxplot()+
  ggbeeswarm::geom_beeswarm(aes(alpha = `Clutch Completion`, colour=Species))+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = 'Body mass (g)')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Body Masses of Penguin Species Members, grouped by Sex",
       subtitle="from the palmerpenguins package (R)",
       caption="Source: palmerpenguins",
       x="Sex")+
  labs(colour = 'Clutch Completion')

names(peng) # Just so you don't have to do this yourself
# 1] "studyName"           "Sample Number"       "Species"             "Region"
# [5] "Island"              "Stage"               "Individual ID"       "Clutch Completion"
# [9] "Date Egg"            "Culmen Length (mm)"  "Culmen Depth (mm)"   "Flipper Length (mm)"
# [13] "Body Mass (g)"       "Sex"                 "Delta 15 N (o/oo)"   "Delta 13 C (o/oo)"
# [17] "Comments"

# Now to try and visualise the mass and dimension data of the penguins after first
# representing some categorical data relationships

# I want one of species stacked onto island

peng %>%
  ggplot(aes(x = Sex, fill = Species))+
  geom_bar(position="stack", stat="count")+
  facet_grid(~ Island)

# Bonus: we can now easily compare how many male and female penguins were
# sampled from each island

# Let's make that a bit nicer and add some counts though




peng %>%
  ggplot(aes(x = Sex, fill = Species)) +
  geom_bar(aes(fill = Species), colour = 'Black', size = 0.125, position="stack", stat = 'count')+
  geom_text(aes(label=..count..),stat = 'count',position='stack', vjust= 2)+
  facet_grid(~ Island)+
  labs(y = "Penguins sampled (n)", colour = "Penguin Species")+
  #scale_fill_manual(name = "Penguin Species")+
  scale_fill_brewer(name = "Penguin Species", palette = "YlOrBr")+
  scale_x_discrete(labels=c("FEMALE" = "F", "MALE" = "M"))+
  labs(title="Counts of Penguin Subgroups in R's palmerpenguins Package", # Update your titles
       subtitle="Differentiated by their island, their species and sex",
       caption="Source: palmerpenguins",
       )+
  theme(plot.background = element_rect(fill = "cornsilk3"),
        panel.background = element_rect(fill = "cornsilk2",
                                        colour = 'black'),
        panel.grid.major = element_line(size = 0.25,
                                        linetype = 'dashed',
                                        colour = "grey19"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(size = 0.125,
                                        linetype = 'dotted',
                                        colour = "grey10"),
        strip.text.x = element_text(size = 12,
                                    face = "bold"),
        strip.background.x = element_rect(colour = 'black',
                                          fill = 'wheat3'),
        legend.background = element_rect(fill = "wheat3", color = "black", size = 0.5),
        axis.title = element_text(face = 'bold'),
        axis.text = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'))

# Ok, we've made that look like a nicotine-stained newspaper in our playing around.
# Newspapers are published ;)

# Next goal is to work on the dimensions of this, because I just exported the graph
# immediately above to PDF and it wasn't readable


# Practiced a bit with clearly visualising several variables including an IV that's not count
# (This isn't to do statistics on yet)

peng %>%
  ggplot(aes(Sex, `Body Mass (g)`))+
  geom_boxplot(aes(fill=Species), show.legend = T)+
  scale_fill_manual(values = wes_palette("GrandBudapest2"))+
  geom_point(
    aes(#x = Species,
        alpha = `Clutch Completion`,
        colour = Island,
        shape = Species,
        group = Species
        ), position=position_jitterdodge()
    )+
  scale_alpha_discrete(range=c(0.4, 1))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  #stat_summary(colour = "magenta", fun.data=mean_cl_normal)+
  scale_y_continuous(name = 'Body mass (g)')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Body Masses of Penguins by Sex", # Update your titles
       subtitle="further differentiated by their island, their species\nand whether their clutch is complete",
       caption="Source: palmerpenguins",
       x="Sex",
       #shape="Island",
       )+
   theme(panel.background = element_rect(fill = "white" ,colour = 'black'),
         panel.grid.major = element_line(size = 0.25, linetype = 'dashed',
                                         colour = "grey19"),
         panel.grid.minor = element_line(size = 0.125, linetype = 'dotted',
                                         colour = "grey19"))

# Let's leave that for now and set ourselves another challenge. Let's try grouping
# our data by the category "Island" and plotting scatters with lines of each, then
# choose an appropriate statistical test to see whether there is a trend between or
# among variables

# let's start with the scatter
peng %>%
ggplot(aes(x=`Culmen Length (mm)`,
           y=`Culmen Depth (mm)`,
           colour = Species #Island,
           #shape = Species
  )) +
  geom_point(size=2)+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  geom_smooth(method = 'lm', size = 0.5)+
  facet_grid(~Sex)
  # would just be playing around at this point. this one would probably be good to explore the
# method legend(). going to try and apply what i learned about backgrounds to facets in stacked bars above


  #legend(x='right', legend = Island#legend(x='right', legend = c("Island","Clutch Completion","Species"), pt.cex = 1.2)

 # labs(colour = 'Clutch Completion')


# This is a class exercise but I'm gonna do it here:
# Do a GLM on the penguin data to see if you can use other variables to predict the sex of a penguin based on other factors

# We know that we can't predict sex based on any geographical factors.
# We know that the Gentoo penguins have higher body mass

# Let's visualise (left alone for now)

dplyr::filter(peng, Sex!='NA') %>%
  ggplot(aes(Sex, `Culmen Length (mm)`, fill=Species))+
  stat_boxplot(geom = "errorbar", width=0.5, position = position_dodge(1)) +
  geom_boxplot(position = position_dodge(1))+
  scale_fill_manual(values = wes_palette("IsleofDogs1"))+
  #geom_dotplot(binaxis='y', dotsize=1, position = 'dodge')+
  #geom_point(alpha=0.4)+
  ggbeeswarm::geom_beeswarm(aes(shape = `Clutch Completion`,color = Island, fill=Species, group = Species))+
  scale_color_manual(values = wes_palette("Darjeeling1"))+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = 'Culmen Length (mm)')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Body Masses of Penguins by Sex", # Update your titles
       subtitle="further differentiated by their island, their species\nand whether their clutch is complete",
       caption="Source: palmerpenguins",
       x="Sex",
       shape="Species",
  )

# Visually, it looks like flipper length is more of a predictor of island or species than sex

