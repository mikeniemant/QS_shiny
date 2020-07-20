library(dplyr)
library(tidyr)

# Load fucntions from global.r

# Extract raw data
path <- "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/"

xlsx.names <- c("03-Jun-20_Peregrine_Val_Day1_Plate2_JC.xlsx", "04-Jun-20_Peregrine_Val_Day2_Plate1_AP.xlsx", "20190411_PROT-138_Plate1_Dx_FT.xlsx")
xlsx.files.df <- data.frame(datapath = paste0(path, xlsx.names),
                            name = xlsx.names, stringsAsFactors = F)

txt.names <- c("QS5-01_2020142_P133_Plate3.txt", "QS5-01_2020143_P153_Plate1.txt")
txt.files.df <- data.frame(datapath = paste0(path, txt.names),
                           names = txt.names, stringsAsFactors = F)

xlsx.df <- preProcessFiles(xlsx.files.df)
txt.df <- preProcessFiles(txt.files.df)

sapply(xlsx.df, class)
sapply(txt.df, class)

xlsx.df %>% count(date)
txt.df %>% count(date)
