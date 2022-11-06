install.packages("ape")
install.packages("phytools")
install.packages("vroom")

library(tidyverse)
library(Momocs)
library(ape)
library(phytools)
library(vroom)

f <- list.files("class_out_data",pattern=".txt|.csv",full.names = TRUE)

out <- read_delim(f[1],delim="\t") %>% 
  as.matrix()

out %>% 
  list() %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()
