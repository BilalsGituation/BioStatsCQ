pacman::p_load(tidyverse, wrappedtools, ggplot2, Rmisc, sjmisc, stringr, lme4, broom, AICcmodavg, dplyr, WRS2)

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
#set.seed(8066) # Randomly generated
#TestResults <- tibble(group=sample(x = c('control','group1','group2'),
#                               size = n_group*3,
#                               replace = T),
#                  measure=rnorm(n = n_group*3))

# Our categorical independent variable has 3 categories,
# so we are going to do a one-way ANOVA of TestResults
# and find out whether our data say that
# measure changes significantly when group changes

#MVTest1 <- aov(measure ~ group, data = TestResults)

#summary(MVTest1) # The p-value from the seed-generated data was 0.354, suggesting that there was no significant effect of group on measure

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

# So, let's say our factors are mouse (random), treatment (fixed, 3 categories), sample side (fixed, 2 categories)
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

# Thank you Andreas Busjahn for providing me with this super-succinct
# value-matching block to fill my treatment variable based on which of these fictional
# mice the density value is from

TestResults <- mutate(TestResults,
                      treatment = case_when(mouse %in% pattern~"overexpression",
                             mouse %in% pattern2~"knockout",
                             TRUE~"control"))

# http://www.sthda.com/english/wiki/normality-test-in-r

shapiro.test(TestResults$SynDns)

# Shapiro-Wilk normality test
#
# data:  TestResults$SynDns
# W = 0.93455, p-value = 2.088e-06
#
# The probability that the distribution of Synaptic Density is not normal due to
# chance is lower than 0.0001%
# So we scrap our parametric analysis below and replace it with something that shows I know what I'm doing!
# https://cran.r-project.org/web/packages/WRS2/vignettes/WRS2.pdf

# We will then use the t2way method in WRS2, after a read of the article (p18)
# and mcp2atm method will be used for posthoc

# Let's learn incrementally instead of attempting a mixed multivariate study, since the aov method seems much more straighforward. I still have to learn the R here.
# Factors: treatment and side, both fixed.
# Using the trimmed mean to do small-sample non-parametric analysis (20%), which appears to be standard for robust non-parametric multivariate statistics

TestResults$side <- factor(TestResults$side) # Convert independent variables to factors
TestResults$treatment <- factor(TestResults$treatment) # so we don't get the "attempt to select less than one element" Error # possibly should be an ordered variable but we can commit that out later
TestResults$mouse <- factor(TestResults$mouse) # rerunning the next line caused no error with this line + prev 2
GeneTestFix <- t2way(formula = SynDns ~ side*treatment, tr=0.2, data = TestResults)

GeneTestFix

# If you set the same seed before creating the TestResults set, you should find no significant difference in Synaptic density with respect to treatment or sample side (or their interaction:
# > GeneTestFix
# Call:
#   t2way(formula = SynDns ~ side * treatment, data = TestResults,
#         tr = 0.2)
#
#                 value p.value
# side           0.5574   0.460
# treatment      2.4609   0.311
# side:treatment 3.8292   0.168

# the effect of the interaction is strongest, but the independent variables and
# their interaction all do not have a significant effect on synaptic density

# Let's show the public that we can interpret posthoc outputs
PostGene2way <- mcp2atm(SynDns ~ side * treatment, data = TestResults)
PostGene2way

# > PostGene2way
# Call:
#   mcp2atm(formula = SynDns ~ side * treatment, data = TestResults)
#
#                     psihat  ci.lower ci.upper p-value
# side1            -3.25530 -12.05703  5.54644 0.45949
# treatment1        1.94302  -7.46309 11.34913 0.60246
# treatment2        5.61832  -4.20955 15.44619 0.15852
# treatment3        3.67530  -3.88519 11.23579 0.23697
# side1:treatment1  5.85406  -3.55205 15.26017 0.12462
# side1:treatment2  7.41115  -2.41672 17.23902 0.06638
# side1:treatment3  1.55709  -6.00340  9.11758 0.61443

# It's to be expected that when we produce the random distributions for treatments
# using the exact same random number generation process, then plug them into observations
# indiscriminately, that we would be returned insignificant p values for the difference
# both BETWEEN and AMONG factors. We can tell that from the results, as well as the simple plot below



# My to-do list for this script:
# -Three-way ANOVA testing synaptic density between and across factors
# -Graph: plot 2bar clusters (left/right), dots (density), facets (treatment), maybe mice on a feature or we explore and find something better than this.

# Attempt at the Three-way mixed

# Factor nesting check
# Side (fixed) is nested within mouse (random)
# Treatment is fixed and separate

# lmer is a method for mixed linear effects modelling
# let's check out the density against just treatment

ggplot(TestResults, aes(treatment,SynDns))+
  ggbeeswarm::geom_beeswarm(alpha=.3)+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = expression(paste("Synaptic Density(boutons/",mu,"m/",m^3,")", sep=""))#,
                     #breaks = seq(-2.2, 2.2, 0.25)
                     )+
  ylab('mean\u00b1CI')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Box plot + Dot plot",
       subtitle="The effect of the Independent Variable 'Experimental Group'\non the dependent variable 'Test Result'",
       caption="Source: set.seed(5555)",
       x="Test Group")
