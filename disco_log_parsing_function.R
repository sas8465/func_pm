library(readr)
library(stringr)


parse_log <- function(file, output_file_name) {
  
  log <- read.csv(file = file, sep = "\n", header = F, fileEncoding = "UTF-16LE", encoding = "UTF-8")
  
  split_log <- str_split_fixed(log$V1, "\\+02:00:", 2)
  
  split_log.df1 <- as.data.frame(split_log)
  
  split_log_car <- paste(split_log.df1$V1, "+02:00:", sep = '')
  
  split_log.df2 <- as.data.frame(split_log_car)
  
  df <- data.frame(split_log.df2$split_log_car, split_log.df1$V2)
  
  names(df)[1] <- "Time"
  names(df)[2] <- "Task"
  
  #Subsetting
  
  dfSubset <- df[grep("Showing", df$Task), ]
  head(dfSubset)
  
  
  #Exporting to CSV:
  
  write.csv(dfSubset, output_file_name, row.names=FALSE, quote=FALSE) 
  
}


parse_log("P2_disco.log", "P2_disco_clean.csv")

