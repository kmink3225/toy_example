library(tidyverse)
library(epitools)
library(DescTools)


set.seed(00)

temp <- lapply(c(paste0('scenario0', 0:9), 'scenario99'), function(x) {
  data.frame(
    template = sample(as.character(LETTERS[1:8]), 1200, replace = TRUE),
    scenario = rep(x, 1200),
    adjusted_label = ifelse(runif(1200) <= 0.8, 'positive', 'negative'),
    dsp_label = ifelse(runif(1200) <= 0.8, 'positive', 'negative'))%>%
    mutate(concentration = case_when(
        runif(1200) <= 0.16 ~ 'gray',
        runif(1200) <= 0.16 ~ 'mild positive',
        runif(1200) <= 0.16 ~ 'positive',
        runif(1200) <= 0.16 ~ 'strong positive',
        runif(1200) <= 0.16 ~ 'extreme positive',
        TRUE ~ 'negative'
      )
  )
})

data1<-do.call('rbind',temp)

summary<-left_join(
data1%>%
    group_by(template,concentration,scenario,dsp_label)%>%
    summarise(dsp_count=n())%>%ungroup%>%
    rename(binary_label=dsp_label),
data1%>%filter(scenario=='scenario00')%>%
    group_by(template,concentration,adjusted_label)%>%
    summarise(adjusted_count=n())%>%ungroup%>%
    rename(binary_label=adjusted_label), 
    by=c('template','concentration','binary_label'))



#BreslowDayTest {DescTools}
#https://search.r-project.org/CRAN/refmans/DescTools/html/BreslowDayTest.html

