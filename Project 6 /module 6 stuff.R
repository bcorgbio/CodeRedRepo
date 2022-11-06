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

#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename")

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

outs.l %>% 
  Out() %>% 
  coo_flipx() %>% 
  stack()
  # visualizing all fore and hindwings together. also doesn't separate size comparatively

#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename")

#add wing info
out.df <- out.df %>% 
  mutate(wing=gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(filename))) %>% 
  na.omit()

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

#extract wing info
wings <- gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(names(outs.l)))

# separate forewings and hindwings
outs <-  outs.l %>% 
  Out(fac=list(wing=wings)) %>% 
  coo_flipx()

forewings <- outs %>% 
  filter(wing=="forewing")

hindwings <- outs %>% 
  filter(wing=="hindwing")

forewings %>% 
  stack()
  # just looking at forewings
hindwings %>% 
  stack()
  # just looking at hindwings

# finding minimum number of points and basing our analysis on this
fore.min <- forewings %>% 
  coo_nb() %>% 
  min()

forewings %>%
  coo_interpolate(fore.min) %>% 
  fgProcrustes() %>% 
  stack()

hind.min <- hindwings %>% 
  coo_nb() %>% 
  min()

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_slide(id=1) %>% 
  coo_align()  %>%
  fgProcrustes() %>%
  stack()

# efourier
forewings %>%
  coo_interpolate(fore.min) %>% 
  coo_align()  %>%
  fgProcrustes() %>% 
  efourier(norm=FALSE) 

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  fgProcrustes() %>% 
  efourier(norm=FALSE) 

# PCA
forewing.pca <- forewings %>%
  coo_interpolate(fore.min) %>%
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()

hindwing.pca <-hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  efourier(norm=FALSE) %>% 
  PCA()

hindwings %>% 
  coo_interpolate(hind.min) %>% 
  coo_align()  %>%
  coo_slide(id=1) %>% 
  fgProcrustes() %>% 
  stack

forewing.pca %>% 
  plot_PCA(title = "forewings")
  # produces a biplot of the first two PCs, the amount of variance explained 
  # by each, and a representation of the morphoshape described by the PCs 
  # (the outlines in the background)
hindwing.pca %>% 
  plot_PCA(title = "hindwings")

# comparative analysis - loading tree

lep.tree <- ape::read.tree("lep_tree2.tre")

plot(lep.tree,cex=0.1)

