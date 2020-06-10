library(dplyr)
library(tidyr)

# Load fucntions from global.r

# Extract raw data
files.df <- data.frame(datapath = c("/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/03-Jun-20_Peregrine_Val_Day1_Plate2_JC.xlsx",
                                    "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/04-Jun-20_Peregrine_Val_Day2_Plate1_AP.xlsx",
                                    "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/QS_shiny/test_data/20190411_PROT-138_Plate1_Dx_FT.xlsx"),
                       name = c("03-Jun-20_Peregrine_Val_Day1_Plate2_JC.xlsx", "04-Jun-20_Peregrine_Val_Day2_Plate1_AP.xlsx", "20190411_PROT-138_Plate1_Dx_FT.xlsx"))
temp.df <- preProcessFiles(files.df)

temp.df %>% count(date)
