# QS results dashboard - global

# Define global variables
PATH <- "/Users/michaelniemantsverdriet/Library/Mobile Documents/com~apple~CloudDocs/"

processQSResults <- function(df, sheet) {
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
  
  if(sheet == "Results") {
    # Check if MTP exists
    if(!any(colnames(r.df) %in% "MTP")) {
      r.df <- r.df %>% mutate(MTP = "N")
    }
    
    # Filter relevant columns
    r.df <- r.df[, c("Sample Name", "Target Name", "CT", "Cq Conf", "MTP", "Tm1")]
    
    # Rename columns
    r.df <- r.df %>% 
      rename(cT = CT,
             `Sample ID` = `Sample Name`)
    
    # Order gene names
    r.df <- r.df %>% 
      mutate(`Target Name` = factor(r.df$`Target Name`,
                                    labels = unique(r.df$`Target Name`), 
                                    ordered = T))
    
    # Reclass columns
    r.df <- r.df %>% 
      mutate(`Sample ID` = as.character(`Sample ID`),
             cT = as.numeric(as.character(cT)),
             `Cq Conf` = as.numeric(as.character(`Cq Conf`)),
             MTP = as.character(MTP),
             Tm1 = as.numeric(as.character(Tm1))) %>% 
      as_tibble()
  } else if (sheet == "Melt Curve Result") {
    r.df <- r.df %>% mutate(Well = as.integer(Well),
                            Tm = as.numeric(Tm),
                            `Melt Peak Height` = as.numeric(`Melt Peak Height`))
  }
  
  # Round columns with numeric values
  r.df <- r.df %>% mutate_if(is.numeric, function(x) round(x, 3))
    
  obj <- list(data = r.df,
              name = exp.name,
              date = exp.date,
              instr = exp.instr)
  return(obj)
}
