# QS shiny - test data input
library(dplyr)
library(tidyr)

# Extract raw data
path <- "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/"

path <- "/Users/michaelniemantsverdriet/Desktop/"

# Load functions from global.r
source("/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/global.R")

# Select txt files
txt.names <- c("03-Jun-20_Peregrine_Val_Day1_Plate1_AP.txt", "03-Jun-20_Peregrine_Val_Day1_Plate1_JC.txt")

txt.files.df <- data.frame(datapath = paste0("/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/", c("QS5-01_2020142_P133_Plate3.txt", "QS5-01_2020143_P153_Plate1.txt")),
                           name = txt.names,
                           `Exp name` = unname(sapply(txt.names, function(x) substr(x, 0, nchar(x)-4))),
                           stringsAsFactors = F, check.names = F)

txt.files.df <- data.frame(datapath = paste0(path <- "/Users/michaelniemantsverdriet/Desktop/", 
                                             txt.names <- c("03-Jun-20_Peregrine_Val_Day1_Plate1_AP.txt", "03-Jun-20_Peregrine_Val_Day1_Plate1_JC.txt")),
                           name = txt.names,
                           `Exp name` = unname(sapply(txt.names, function(x) substr(x, 0, nchar(x)-4))),
                           stringsAsFactors = F, check.names = F)

# Preprocess both files
txt.df <- preProcessFiles(txt.files.df)

txt.df %>% count(date)

# 14-07-2020 12:34 --> how the date should be
