library(dplyr)
library(tidyr)

# Load fucntions from global.r

# Extract raw data
xlsx.files.df <- data.frame(datapath = c("/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/03-Jun-20_Peregrine_Val_Day1_Plate2_JC.xlsx",
                                    "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/04-Jun-20_Peregrine_Val_Day2_Plate1_AP.xlsx",
                                    "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/20190411_PROT-138_Plate1_Dx_FT.xlsx"),
                       name = c("03-Jun-20_Peregrine_Val_Day1_Plate2_JC.xlsx", "04-Jun-20_Peregrine_Val_Day2_Plate1_AP.xlsx", "20190411_PROT-138_Plate1_Dx_FT.xlsx"), stringsAsFactors = F)

txt.files.df <- data.frame(datapath = c("/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/QS5-01_2020142_P133_Plate3.txt"),
                       name = c("QS5-01_2020142_P133_Plate3.txt"), stringsAsFactors = F)

xlsx.df <- preProcessFiles(xlsx.files.df)
txt.df <- preProcessFiles(txt.files.df)

sapply(xlsx.df, class)
sapply(txt.df, class)

xlsx.df %>% count(date)
txt.df %>% count(date)
