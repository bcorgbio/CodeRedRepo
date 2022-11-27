library(tidyverse)
library(knitr)
library(vroom)
library(MuMIn)

dat.f <- list.files(path="./Project8_data")

dat.l <- list()

for(i in dat.f){
  metadata <- unlist(strsplit(i,'_'))
  subj <- metadata[2]
  ang <- metadata[3]
  exp <- gsub('.csv','',metadata[4])
  trial.force <- read_csv(paste0('Project8_data/',i),col_names=FALSE)
  trial.force$X1 <- gsub("[a-zA-Z :]","",trial.force$X1)
  mvcf <- trial.force %>% 
    pull(X1) %>% 
    as.numeric() %>% 
    mean()
  dat.l[[i]] <- tibble(
    subject=subj,
    ang=as.numeric(ang),
    exp=exp,
    force=mvcf
  )
}
dat <- do.call(rbind,dat.l)
dat %>% 
  group_by(exp,subject) %>% 
  mutate(force=force/max(force)) %>% 
  ggplot(aes(ang,force,col=exp))+geom_point()
