# https://www.rstudio.com/resources/cheatsheets/

#tibble
pacman::p_load(tidyverse)
tibble1 <- tibble(PatCode=paste('Pat', 1:20),
                  'Age (years)' = runif (n = 20, min =18, max = 70),
                  Treatment=sample (x=c('placebo','verum'),
                                    size=20,
                                    replace=TRUE))#,
# check.name =FALSE)

str(tibble1)
tibble1[1:3,2]
tibble1$'Age (years)'[1:3]
tibble1[1:3,'Age (years)']
tibble1[1:3,c('PatCode','Age (years)')]
interesting_vars <- c('PatCode','Age (years)')
tibble1[1:3, interesting_vars]
tibble1[1:3, ]
tibble1|>select(PatCode,Treatment)
tibble1|> pull(Treatment)
tibble1[["Treatment"]]
tibble1 |> slice(1:3)

#lists
ListName <- list(column1=c('entry1', 'entry2',
                           'entry3', 'entry4'),
                 column2=c('entry5','entry6'),
                 column3=c('FirstThing', 'SecondThing'),
                 amounts=1:10)

#t-tests
var <- t.test(1:100,5:105) # returns list and eg P can be derived via var$p.value
str(var) # shows the complete output, from which you can get the vector namess like $p.value and $method
var$method


# for loops ####

VarCount <- 10
for(my_i in 1:VarCount){
  print(paste('run:',my_i))
}

VarCount <- 0
for(my_i in 1:VarCount){
  print(paste('run:',my_i))
}

for(my_i in seq_len(10)){
  print(paste('run:',my_i))
}

VarCount <- 10
for(my_i in seq_len(VarCount)){
  print(paste('run:',my_i))
}

for(my_i in colnames(tibble1)){
  print(paste('column:',my_i))
}

#ep<-list(1,2,3,4,5,6,7,8,9)
# Prints 6 series of 5 episodes
ep <- NULL
SeriesCount<- 6
Episodes <- 5
for(series in seq_len(SeriesCount)){
  print(paste('Series: ',series))
  for(ep in seq_len(Episodes)){
    print(paste('Episode: ', ep))
  }
}

# while loop ####
total <- 0
while(total<2){
  total <- rnorm(n = 1, mean = 0, sd = 1)
  print(total)
}

# conditional statement ####

if(rnorm(1)<1){
  print('below 1')
} else {
  print('above or equal to 1')
}


tiny2 <- ifelse(test = rnorm(1)<1,
                yes = 'below',
                no = 'larger or equal')

p <- .033
paste0('this is', # paste0 pastes things with no separator argument
      ifelse(p>0.05,yes=' not ',no=' '),
      'significant')

answer <- 'This is'
if(p>0.05) {
  answer <- paste(answer,'not')
} else {
  answer <- paste(answer,'extremely')
}
answer <- paste(answer,'significant')
answer

# Data filtering


tibble1 |> filter(`Age (years)`>36,
                  Treatment=='verum')

tibble1[which(tibble1$Treatment=='placebo' &
                tibble1$`Age (years)`<30),]

# This is where we had reached at the end of 05.05.2022
# They should probably have better commenting on them
#