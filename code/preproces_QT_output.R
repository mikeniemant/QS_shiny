####
#Title   : Preprocess QT output
#Project : 
#Version : 1.0
#Author  : Michael S.A. Niemantsverdriet
#Date    : 2019-01-13
#Email   : m.niemantsverdriet@skylinedx.com
#Links   : -
#Remarks : General code to preprocess data
####

# Prepare work environment ----
# Libraries
library(nbs)
library(tidyverse)

PATH <- "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/workspace/R/skyline_projects/"

DUMMY.FILE <- paste0(PATH, "plot_ct_shiny/dummy_data/20190411_PROT-138_Plate1_Dx_FT.xlsx")
# DUMMY.FILE <- paste0(PATH, "plot_ct_shiny/dummy_data/20190411_PROT-138_Plate2_Dx_FT.xlsx")
# DUMMY.FILE <- paste0(PATH, "plot_ct_shiny/dummy_data/20190411_PROT-138_Plate3_Dx_FT.xlsx")

df <- suppressMessages(readxl::read_xlsx(DUMMY.FILE, sheet = "Results"))

# Extract variables
# - Experiment Name
# - Experiment Run End Time
# - Instrument Type
which(df$`Block Type` == "Experiment Name")
exp.df <- df[which(df$`Block Type` %in% c("Experiment Name", "Experiment Run End Time", "Instrument Type")), 1:2]
exp.name <- as.character(exp.df[1, 2])
exp.date <- substr(as.character(exp.df[2, 2]), 1, 16)
exp.instr <- as.character(exp.df[3, 2])

# Preprocess results
idx <- which(df$`Block Type` == "Well")
r.df <- df[(idx+1):nrow(df), ]
colnames(r.df) <- unname(unlist(lapply(df[(idx), , drop = T], function(x) as.character(x[[1]]))))
rownames(r.df) <- NULL

# Check if MTP exists
if(!any(colnames(r.df) %in% "MTP")) {
  r.df <- r.df %>% mutate(MTP = "N")
}

# Filter relevant columns
r.df <- r.df[, c("Sample Name", "Target Name", "CT", "Cq Conf", "MTP", "Tm1")]

r.df <- r.df %>% rename(cT = CT)

# Round values
r.df <- r.df %>% mutate_if(as.numeric, function(x) round(x, 3))

# Reclass columns
r.df <- r.df %>% 
  mutate(`Sample Name` = as.character(`Sample Name`),
         `Target Name` = as.character(`Target Name`),
         cT = as.numeric(as.character(cT)),
         `Cq Conf` = as.numeric(as.character(`Cq Conf`)),
         MTP = as.character(MTP),
         Tm1 = as.numeric(as.character(Tm1))) %>% 
  as_tibble()

# Plot cT values
ggplot(r.df,
       aes(x=`Target Name`, y = cT, colour = `Sample Name`)) +
  geom_jitter(width = 0.1) +
  theme_bw()

# Create workbook ----
# Extract data of individual parameters
for(i in c("cT", "Cq Conf", "MTP", "Tm1")) {
  plotType <- sym(i)
  df <- r.df %>% 
    select(`Sample Name`, `Target Name`, !!plotType) %>% 
    mutate(exp_name = exp.name,
           exp_date = exp.date)  
  
  # Check for missing values
  df %>% filter(is.na(!!plotType))
  
  # Long to wide
  df %>% spread(`Target Name`, !!plotType)
}

# CT <- df %>% 
#   select(exp_name, exp_date, `Sample Name`, `Target Name`, CT) %>% 
#   spread(`Target Name`, CT)
# `Cq Conf` <- df %>% 
#   select(exp_name, exp_date, `Sample Name`, `Target Name`, `Cq Conf`) %>% 
#   spread(`Target Name`, `Cq Conf`)
# MTP <- df %>% 
#   select(exp_name, exp_date, `Sample Name`, `Target Name`, MTP) %>% 
#   spread(`Target Name`, MTP)
# Tm1 <- df %>% 
#   select(exp_name, exp_date, `Sample Name`, `Target Name`, Tm1) %>% 
#   spread(`Target Name`, Tm1)

# Create workbook and fill with sheets
# output <- createWorkbook()
#
# output_CT <- createSheet(wb=output, sheetName="CT")
# output_Cq_Conf <- createSheet(wb=output, sheetName="Cq Conf")
# output_MTP <- createSheet(wb=output, sheetName="MTP")
# output_Tm1 <- createSheet(wb=output, sheetName="Tm1")
#
# addDataFrame(x=CT, sheet=output_CT)
# addDataFrame(x=`Cq Conf`, sheet=output_Cq_Conf)
# addDataFrame(x=MTP, sheet=output_MTP)
# addDataFrame(x=Tm1, sheet=output_Tm1)
#
# saveWorkbook(output, "test.xlsx")
