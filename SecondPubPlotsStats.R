# This document was created in order to explore the surface of multifactor
# stats. I will move onto some plotting afterwards

# So, let's use a couple of random generation strategies, rather than one,
# to throw more uncertainty into our TestResults table

# IMPORTANT: I am clearing my RStudio environment of all objects before I begin
# writing

set.seed(9975)
n_group=100
TestResults <- tibble(side=sample(x = c('left','right'),
                                  size = n_group*3,
                                  replace = T),
                      mouse=sample(x = c('f1','f2','m3','f4','m5','m9','f7','f8','m11', 'f12','f13','m14','m16','m17','f18'), # Only 15 of 21 mice survived :(
                                   size = n_group*3,
                                   replace = T),
                      SynDns=c(runif(n_group*1.5,0,20), runif(n_group*1,5,25), runif(n_group/2,0,30)))


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
  labs(title="Box plot + Dot plot",
       subtitle="The effect of the Independent Variable 'Experimental Group'\non the dependent variable 'Test Result'",
       caption="Source: set.seed(9975)",
       x="Test Group")

# The distributions look the same with seed 5975. I am going to play around with the value randomiser after this push
# Edit: I would prefer to advance with this seed

