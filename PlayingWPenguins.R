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

names(peng)
# 1] "studyName"           "Sample Number"       "Species"             "Region"
# [5] "Island"              "Stage"               "Individual ID"       "Clutch Completion"
# [9] "Date Egg"            "Culmen Length (mm)"  "Culmen Depth (mm)"   "Flipper Length (mm)"
# [13] "Body Mass (g)"       "Sex"                 "Delta 15 N (o/oo)"   "Delta 13 C (o/oo)"
# [17] "Comments"

# gonna try and visualise the mass and dimension data of the penguins after first
# representing some categorical data relationships

# So first I want a stacked bar plot of region stacked on island, because I have no idea
# whether the species are indeed region-specific and I want to know how the geography works
# to do that thoroughly (and practice)

peng %>%
  ggplot(aes(x = Region, fill = Island))+
  geom_bar()

# Now I know that all of the islands are in the Anvers region and have git-pushed my
# first stacked bar plot!

# Next I want one of species stacked onto island

dplyr::filter(peng, Sex!='NA') %>%
  ggplot(aes(x = Sex, fill = Species))+
  geom_bar(position="stack", stat="count")+
  facet_grid(~ Island)

# Bonus: we can now easily compare how many male and female penguins were
# sampled from each island, which won't be as easy to see in any plot involving
# points, and certainly not before we make it look nicer, which we'll have a go
# at doing to the graph made by the previous block

# gone back to make this so we can know that the facets are Island names
peng$Island <- factor(peng$Island, labels = c("Biscoe\nIsland", "Dream\nIsland", "Torgersen\nIsland"))
# I would find this more informative since the facets as a set aren't named, like you see on the bottom
# x-axis of the next graph

# Now let's do the same to sort that overly-wide legend out
peng$Species <- factor(peng$Species, labels = c("Adelie (P. adeliae)", "Chinstrap (P. antarctica)", "Gentoo (P. papua)"))

Numbers <- peng %>%
  count(Island, Sex, Species)

peng <-dplyr::filter(peng, Sex!='NA')
peng <-dplyr::filter(peng, `Clutch Completion`!='NA')
peng <-dplyr::filter(peng, Species!='NA')

# Logging off because holiday, time away from screen.
# we are on the geom_rect line to try and make our facets nicer

peng %>%
  ggplot(aes(x = Sex, fill = Species)) +
  #geom_rect(peng, mapping = aes(fill = Island), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+
  geom_bar(aes(fill = Species), position="stack", stat = 'count')+
  geom_text(aes(label=..count..),stat = 'count',position='stack', vjust= 2)+ # https://stackoverflow.com/questions/63653351/how-to-use-stat-count-to-label-a-bar-chart-with-counts-or-percentages-in-ggplo this is the one! thank you for my almost-nice graph!
  facet_grid(~ Island)+
  labs(y = "Penguins sampled (n)", colour = "Penguin Species")+ # fixed this, see prev. commit for
  scale_fill_manual(name = "Penguin Species",values = wes_palette("IsleofDogs1"))+ # this palette nicely differentiated 3 colours before
  scale_x_discrete(labels=c("FEMALE" = "F", "MALE" = "M"))# + # shortened the facet x axis labels
# Next thing I want to tackle is that the facets themselves could look nicer. We'll add a background after to contrast with what we have
#theme(strip.background=element_rect(fill=Island))




# need to call it. tried but should revisit:
# https://stackoverflow.com/questions/62522673/adding-counts-and-percentages
# https://blog.albertkuo.me/post/2022-01-04-reordering-geom-col-and-geom-bar-by-count-or-value/
# https://ggplot2.tidyverse.org/reference/geom_text.html
# https://plotnine.readthedocs.io/en/stable/tutorials/miscellaneous-show-counts-on-a-stacked-bar-plot.html
# https://statisticsglobe.com/add-count-labels-on-top-of-ggplot2-barchart-in-r


# Further developed, practiced a bit with visualising several variables clearly
# (This isn't to do statistics on)

# 27/05/2022 going to try to separate the points and stat summmaries, so that they
# are on top of the island boxplot that they came from

peng %>%
  ggplot(aes(Sex, `Body Mass (g)`))+
  geom_boxplot(aes(fill=Species), show.legend = T)+
  scale_fill_manual(values = wes_palette("GrandBudapest2"))+
  #geom_point(alpha=0.4)+
  geom_point(
    aes(#x = Species,
        alpha = `Clutch Completion`,
        colour = Island,
        shape = Species,
        group = Species
        ), position=position_jitterdodge()
    )+
  scale_alpha_discrete(range=c(0.4, 1))+ # This is cool, lets you make a scalar representation motif(?)
  # into more like "These are the n categories of transparency as shown in legend"

  # What I learned about geom_beeswarm:
  # While it is neat, there is no position argument because geom_beeswarm is itself a usage of geom_point
  # with the argument position="beeswarm"

  # Now this has become an ok job of representing an independent variable against 4 factors.
  # We can see what we did in our stacked bars, that Adelle penguins were studied on all 3 islands,
  # Chinstraps were only found on Dream Island, and Gentoos were only found on Biscoe Island.

  # I have to commit these notes then delete them


  #geom_point(aes(alpha = `Clutch Completion`,color = Island, shape = Species, group = Species))+
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

  #legend(x='right', legend = c("Island","Clutch Completion","Species"), pt.cex = 1.2)

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

