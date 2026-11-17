#CA1

#Install the packages:

#tidyverse: dplyr (data mmanipulation) + ggplot2 (plots) + readr
#gridExtra:  (plots side by side)
#reshape2: Change table's shape
#fastDummies:  0/1
#factoextra: PCA plots
#FactoMineR: PCA package used with factoextra

install.packages(c("tidyverse", "gridExtra", "reshape2", "fastDummies", "factoextra", "FactoMineR"))

#load packages
library(tidyverse)
library(gridExtra)
library(reshape2)
library(fastDummies)
library(factoextra)
library(FactoMineR)
