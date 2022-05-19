# This document was created in order to explore the surface of factor
# stats. I will move onto some plotting afterwards

# So, let's use a couple of random generation strategies, rather than one,
# to throw more uncertainty into our TestResults table

# IMPORTANT: I am clearing my RStudio environment of all objects before I begin
# writing

set.seed(1000)
n_group=100
TestResults <- tibble(side=sample(x = c('left','right'),
                                  size = n_group*3,
                                  replace = T),
                      mouse=sample(x = c('f1','f2','m3','f4','m5','m9','f7','f8','m11', 'f12','f13','m14','m16','m17','f18'), # Only 15 of 21 mice survived :(
                                   size = n_group*3,
                                   replace = T),
                      SynDns=c(runif(n_group,0,20), runif(n_group/2,0,25), runif(n_group*1.5,0,34)))


pattern <- c('f1','f4','m14','m17','f8')
pattern2 <- c('f12','m5','m9','m3','f18')

TestResults <- mutate(TestResults,
                      treatment = case_when(mouse %in% pattern~"overexpression",
                                            mouse %in% pattern2~"knockout",
                                            TRUE~"control"))

# Tibble created successfully. a scroll suggests we created some variation with this approach

TestResults$side <- factor(TestResults$side) # Convert independent variables to factors
TestResults$treatment <- factor(TestResults$treatment) # so we don't get the "attempt to select less than one element" Error # possibly should be an ordered variable but we can commit that out later
TestResults$mouse <- factor(TestResults$mouse) # Still don't know how to define this as random for the mixed model, but we're not doing the proper analysis in this script

ggplot(TestResults, aes(treatment,SynDns))+
  ggbeeswarm::geom_beeswarm(alpha=.3)+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = expression(paste("Synaptic Density(boutons/",mu,"m/",m^3,")", sep=""))#,
                     #breaks = seq(-2.2, 2.2, 0.25)
  )+
  ylab('mean\u00b1CI')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Box plot + Dot plot", # Update your titles
       subtitle="The effect of the Independent Variable 'Experimental Group'\non the dependent variable 'Test Result'",
       caption="Source: set.seed(1000)",
       x="Test Group")


# The distributions look the same with seed 5975. I am going to play around with the value randomiser after this push
# Edit: I would prefer to advance with this seed. 1000 chosen after.

# Normality test not needed as each treatment group has n > 30

GeneTestFix <- t2way(formula = SynDns ~ side*treatment, tr=0.2, data = TestResults)
GeneTestFix

# No significant differences between groups at seed 9975
# Ok, by changing the proportions of the random column entries entered and the seed to 1000, we have
# got some significance variation in our tests. Each factor has no effect on the DV on its own, but
# they interact to cause an effect. There is a 2%, 0.02, 1 in 50 chance that this effect was due to chance
# (overlooking that we intentionally created the dataset for interpreting ANOVA outputs)

# Call:
#   t2way(formula = SynDns ~ side * treatment, data = TestResults,
#         tr = 0.2)
#
#                 value  p.value
# side           1.3702   0.244
# treatment      1.5588   0.464
# side:treatment 8.2376   0.020


PostGene2way <- mcp2atm(SynDns ~ side * treatment, data = TestResults)
PostGene2way

# Do we get the same results when we switch the IV names? Yes, only the names
# get reversed
PostGene2wayREV <- mcp2atm(SynDns ~ treatment*side, data = TestResults)
PostGene2wayREV

# No significant differences between groups at seed 9975,
# But at seed 1000 we can see (in side*treatment) that there is a significant
# difference in side 1 with

# Call:
#   mcp2atm(formula = SynDns ~ side * treatment, data = TestResults)
#
#                    psihat  ci.lower ci.upper p-value
# side1            -4.10778 -11.03716  2.82160 0.24349
# treatment1        3.10576  -3.33374  9.54526 0.24821
# treatment2        0.24108  -6.80116  7.28333 0.93429
# treatment3       -2.86468 -10.08194  4.35258 0.34056
# side1:treatment1 -7.15954 -13.59904 -0.72004 0.00849 # Heyoo
# side1:treatment2 -0.61348  -7.65572  6.42877 0.83384
# side1:treatment3  6.54606  -0.67120 13.76332 0.03086 # Over here

ggplot(TestResults, aes(side,SynDns))+
  ggbeeswarm::geom_beeswarm(alpha=.3)+
  stat_summary(fun.data=mean_cl_normal)+
  scale_y_continuous(name = expression(paste("Synaptic Density(boutons/",mu,"m/",m^3,")", sep=""))#,
                     #breaks = seq(-2.2, 2.2, 0.25)
  )+
  ylab('mean\u00b1CI')+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Box plot + Dot plot", # Update your titles
       subtitle="The effect of the Independent Variable 'Experimental Group'\non the dependent variable 'Test Result'",
       caption="Source: set.seed(1000)",
       x="Hemisphere")+
  facet_grid(# graph was faceted by sampling side after posthoc stats
    rows = TestResults$treatment,
    cols = NULL,
    scales = "fixed",
    space = "fixed",
    shrink = TRUE,
    labeller = "label_value",
    as.table = TRUE,
    switch = NULL,
    drop = TRUE,
    margins = FALSE,
    facets = NULL
  )


# So, within the treatments "control" and "overexpression", the var "side" has
# an effect making the synaptic density different

# can I say this alternatively?

# Advice: this is really messy to interpret. Separate analyses should be done on each factor if you get this, ideally!