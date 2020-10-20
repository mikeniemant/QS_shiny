# QS shiny - global
# Define global variables
data.df <- NULL
files.df <- NULL
SHEETS <- NULL
NAMES <- NULL

readTxt <- function(file.dir) {
  # Tab file contains three parts
  # 1. run settings and info
  # 2. results
  # 3. melt curve results
  
  # First read file and check the number 
  con = file(file.dir, "r")
  i = 1
  while(TRUE) {
    line = readLines(con, n = 1)
    
    # Extract variables
    # - Experiment Run End Time
    # - Instrument Type
    # - Instrument Serial Number
    
    if(substr(line, 1, 28) == "* Experiment Run End Time = ") {
      exp.date <- substr(line, 29, nchar(line))
      #print(exp.date)
    }
    
    if(substr(line, 1, 26) == "* Instrument Name =       ") {
      exp.instr <- substr(line, 27, nchar(line))
      #print(exp.instr)
    }
    
    if(substr(line, 1, 29) == "* Instrument Serial Number = ") {
      exp.instr.id <- substr(line, 30, nchar(line))
      #print(exp.instr.id)
    }
    
    if(line == "[Results]") {
      break
    }
    i = i + 1
  }
  j = 0
  while(TRUE) {
    line = readLines(con, n = 1)
    if(line == "") {
      break
    }
    j = j + 1
  }
  close(con)
  
  # Read file
  r.df <- suppressMessages(readr::read_tsv(file.dir, skip = i, n_max = j-1))
  
  # Fix date
  print(exp.date)
  if(nchar(as.character(exp.date)) == 24) {
    exp.date <- substr(as.character(exp.date), 1, 16)
  } else {
    exp.date <- format(as.POSIXct(substr(as.character(exp.date), 1, 16)), "%d-%m-%Y %H:%M")
  }
  
  print(exp.date)
  print(class(exp.date))
  # Compile output as list object
  obj <- list(data = r.df,
              date = exp.date,
              instr = exp.instr,
              id = exp.instr.id)
  
  return(obj)
}

processQSResultsTxt <- function(r.df) {
  rownames(r.df) <- NULL
  
  # Check if MTP exists
  if(!any(colnames(r.df) %in% "MTP")) {
    r.df <- r.df %>% mutate(MTP = "N")
  }
  
  r.df$MTP <- factor(r.df$MTP, levels = c("N", "Y"))
  
  # Filter relevant columns
  r.df <- r.df[, c("Sample Name", "Target Name", "CT", "Cq Conf", "MTP", "Tm1")]
  
  # Rename columns
  r.df <- r.df %>% 
    rename(Ct = CT,
           `Sample ID` = `Sample Name`)
  
  # Order gene names
  r.df <- r.df %>% 
    mutate(`Target Name` = factor(r.df$`Target Name`,
                                  levels = unique(r.df$`Target Name`), 
                                  ordered = T))
  
  # Reclass columns
  r.df <- r.df %>% 
    mutate(`Sample ID` = as.character(`Sample ID`),
           Ct = as.numeric(as.character(Ct)),
           `Cq Conf` = as.numeric(as.character(`Cq Conf`)),
           MTP = as.character(MTP),
           Tm1 = as.numeric(as.character(Tm1))) %>% 
    as_tibble()
  
  # For files with the old template, change the sample ID names
  r.df$`Sample ID`[r.df$`Sample ID` == "REF"] <- "Positive control"
  r.df$`Sample ID`[r.df$`Sample ID` == "NTC"] <- "Negative control"
  
  return(r.df)
}

preProcessFiles <- function(if.df) {
  # 4. Validate and preprocess selected files
  df <- data.frame()
  
  for(i in 1:nrow(if.df)) {
    path <- if.df$datapath[i]
    
    pp <- readTxt(path)
    
    pp$data <- processQSResultsTxt(pp$data)
    
    pp <- pp$data %>% 
      mutate(run = i,
             `Exp name` = if.df$`Exp name`[i],
             date = pp$date,
             instrument = pp$instr,
             id = pp$id) %>% 
      select(`Exp name`, instrument, date, everything())
    
    df <- rbind(df, pp)
  }
  
  return(df)
}

prepareOutputDataXlsx <- function(df) {
  Ct <- data.frame()
  `Cq Conf` <- data.frame()
  pc <- data.frame()
  nc <- data.frame()
  MTP <- data.frame()
  Tm1 <- data.frame()
  
  for(i in unique(df$run)) {
    # Filter run and identify order
    temp.df <- df %>% filter(run == i)
    
    temp.df$`Sample ID` <- factor(temp.df$`Sample ID`, levels = unique(temp.df$`Sample ID`), ordered = T)
    
    Ct.raw <- temp.df %>% 
      select(date, `Exp name`, `Sample ID`, `Target Name`, Ct) %>%
      spread(`Target Name`, Ct) %>%
      filter(`Sample ID` != "Positive Control" & `Sample ID` != "Negative Control") 
    `Cq Conf.raw` <- temp.df %>% 
      select(date, `Exp name`, `Sample ID`, `Target Name`, `Cq Conf`) %>%
      spread(`Target Name`, `Cq Conf`) %>% 
      filter(`Sample ID` != "Positive Control" & `Sample ID` != "Negative Control")
    pc.raw <- temp.df %>%
      select(date, `Exp name`, `Sample ID`, `Target Name`, Ct) %>%
      spread(`Target Name`, Ct) %>%
      filter(`Sample ID` == "Positive Control")
    nc.raw <- temp.df %>% 
      select(date, `Exp name`, `Sample ID`, `Target Name`, Ct) %>%
      spread(`Target Name`, Ct) %>%
      filter(`Sample ID` == "Negative Control")
    MTP.raw <- temp.df %>% 
      select(date, `Exp name`, `Sample ID`, `Target Name`, MTP) %>%
      spread(`Target Name`, MTP) %>% 
      filter(`Sample ID` != "Positive Control" & `Sample ID` != "Negative Control")
    Tm1.raw <- temp.df %>% 
      select(date, `Exp name`, `Sample ID`, `Target Name`, Tm1) %>%
      spread(`Target Name`, Tm1) %>% 
      filter(`Sample ID` != "Positive Control" & `Sample ID` != "Negative Control")
    
    Ct <- rbind(Ct, Ct.raw) %>% arrange(date)
    `Cq Conf` <- rbind(`Cq Conf`, `Cq Conf.raw`) %>% arrange(date)
    pc <- rbind(pc, pc.raw) %>% arrange(date)
    nc <- rbind(nc, nc.raw) %>% arrange(date)
    MTP <- rbind(MTP, MTP.raw) %>% arrange(date)
    Tm1 <- rbind(Tm1, Tm1.raw) %>% arrange(date)
  }
  
  # Create workbook and fill with sheets
  output <- xlsx::createWorkbook()
  
  output_Ct <- xlsx::createSheet(wb=output, sheetName="Ct")
  output_Cq_Conf <- xlsx::createSheet(wb=output, sheetName="Cq Conf")
  output_pc <- xlsx::createSheet(wb=output, sheetName="Positive Control")
  output_nc <- xlsx::createSheet(wb=output, sheetName="Negative Control")
  output_MTP <- xlsx::createSheet(wb=output, sheetName="MTP")
  output_Tm1 <- xlsx::createSheet(wb=output, sheetName="Tm1")
  
  xlsx::addDataFrame(x=Ct %>% as.data.frame(), sheet=output_Ct, row.names = F)
  xlsx::addDataFrame(x=`Cq Conf` %>% as.data.frame(), sheet=output_Cq_Conf, row.names = F)
  xlsx::addDataFrame(x=pc %>% as.data.frame(), sheet=output_pc, row.names = F)
  xlsx::addDataFrame(x=nc %>% as.data.frame(), sheet=output_nc, row.names = F)
  xlsx::addDataFrame(x=MTP %>% as.data.frame(), sheet=output_MTP, row.names = F)
  xlsx::addDataFrame(x=Tm1 %>% as.data.frame(), sheet=output_Tm1, row.names = F)
  
  return(output)
}

# Color scheme
x <- c("29 79 110", 
       "74 174 219",
       "198 88 145",
       "170 213 225",
       "137 138 137",
       "131 69 141",
       "13 47 65",
       "42 104 131",
       "130 44 88",
       "82 153 176")
colors <- sapply(strsplit(x, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))

