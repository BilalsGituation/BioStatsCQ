pacman::p_load(tidyverse, wrappedtools, ggplot2, Rmisc, sjmisc, stringr)

n_group <- 10
rawdata <- tibble(group=sample(x = c('c','t'),
                               size = n_group*2,
                               replace = T),
                  measure=rnorm(n = n_group*2))
t1 <- t.test(rawdata$measure~rawdata$group,
       var.equal=T)

t2 <- t.test(x=rawdata |> filter(group=='c') |> pull(measure),
             y=rawdata |> filter(group=='t') |> pull(measure),
             var.equal=T)

set.seed(2022)
x = rnorm(n=10, mean=170, sd=15)
y = rnorm(n=10, mean=175, sd=15)
(t3_out <- t.test(x=x,y=y,
            var.equal=T))
set.seed(2022)
(t4_out <- t.test(x=x,y=y,
                  var.equal=F))

xy <- tibble(group=c(rep('group_x', n_group), rep('group_y', n_group)),
             measure=c(x,y))

# We represent the data stored in xy one-dimensionally, side-by-side, with confidence intervals

ggplot(xy, aes(group,measure))+
  ggbeeswarm::geom_beeswarm(alpha=.3)+
  stat_summary(fun.data=mean_cl_normal)+
  ylab('mean\u00b1CI')

# We learn some Ronseal functions (Ronseal does what it says on the tin)

meansd(x)
meansd(y)
diff(t3$estimate)

# solo work just to demonstrate that I can do multivariate statistics
set.seed(8066) # Randomly generated
TestResults <- tibble(group=sample(x = c('control','group1','group2'),
                               size = n_group*3,
                               replace = T),
                  measure=rnorm(n = n_group*3))

# Our categorical independent variable has 3 categories,
# so we are going to do a one-way ANOVA of TestResults
# and find out whether our data say that
# measure changes significantly when group changes

MVTest1 <- aov(measure ~ group, data = TestResults)

summary(MVTest1) # The p-value from the seed-generated data was 0.354, suggesting that there was no significant effect of group on measure

#ggplot(TestResults, aes(group, measure))+
#       geom_boxplot(fill = 'yellow')+
#       geom_dotplot(binaxis = 'y',
#                    stackdir = 'center',
#                    dotsize = .5,
#                    fill = 'magenta',
#                    binwidth = 0.16)+
#       stat_summary(fun.data=mean_cl_normal)+
#       ylab('mean\u00b1CI')

#I am leaving the above in, but beeswarm is clearly better
ggplot(TestResults, aes(group,measure))+
  ggbeeswarm::geom_beeswarm(alpha=.3)+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = "Test Result", breaks = seq(-2.2, 2.2, 0.25))+
  ylab('mean\u00b1CI')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Box plot + Dot plot",
       subtitle="The effect of the Independent Variable 'Experimental Group'\non the dependent variable 'Test Result'",
       caption="Source: set.seed(8066)",
       x="Test Group") # Please open or export this graph. It is much nicer than it looks!


#t.test(rnorm(20,120,15), mu = 100, alternative = 'greater')

# Next thing we want to do is create some data with several factors and one dependent variable

# So, let's say our factors are mouse (random), treatment (fixed, 3 categories), treatment side (fixed, 2 categories)
# and we have calculated our independent variable c("Density", expression("Synapses/"*mu~"m"^3))

# Let's assume that we already know that the factor of whether the mouse is female or male is insignificant

set.seed(5555)
n_group=50
TestResults <- tibble(side=sample(x = c('left','right'),
                                       size = n_group*3,
                                       replace = T),
                      mouse=sample(x = c('f1','f2','m3','f4','m5','m9','f7','f8','m11', 'f12','f13','m14','m16','m17','f18'), # Only 15 of 21 mice survived :(
                                   size = n_group*3,
                                   replace = T),
                      SynDns=runif(n_group*3,0,20))


pattern <- c('f1','f4','m14','m17','f8')
pattern2 <- c('f12','m5','m9','m3','f18')

# Thank you Andreas Busjahn (my teacher) for providing me with this super-succinct
# value-matching block to fill my treatment variable based on which of these fictional
# mice the density value is from

TestResults <- mutate(TestResults,
                      treatment = case_when(mouse %in% pattern~"overexpression",
                             mouse %in% pattern2~"knockout",
                             TRUE~"control"))

# My to-do list for this script:
# -Two-way ANOVA testing synaptic density between and across factors

# -Graph: plot 2bar clusters (left/right), dots (density), facets (treatment), maybe mice on a feature or we explore and find something better than this