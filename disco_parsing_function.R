#Load Necessary Libraries
library(readr)
library(stringr)
library(tidyverse)
library(lubridate)
library(tm)
library(glue)

parse_disco <- function(input_path, output_path, start_time, video_title, standard_time_lag) {
  
  
  ###This function converts Disco log to MAXQDA coding scheme.###
  
  #input_path <- path to disco log file.
  #output_path <- path to output location. File name will be 
  #start_time <- start time when user uses first widget (units = seconds * 1000)
  #video_title <-title of video being encoded (with .mp4 extension)
  #standard_time_lag <- for disco start and end time are not available. A default duration is used in this case.
  
  ### Load Log: ###
  
  log <- read.csv(file = input_path, sep = "\n", header = F, fileEncoding = "UTF-16LE", encoding = "UTF-8")
  
  #Convert Log to DataFrame & Clean up Formatting 
  
  split_log <- str_split_fixed(log$V1, "\\+02:00:", 2)
  
  split_log.df1 <- as.data.frame(split_log)
  
  split_log_car <- paste(split_log.df1$V1, "+02:00:", sep = '')
  
  split_log.df2 <- as.data.frame(split_log_car)
  
  df <- data.frame(split_log.df2$split_log_car, split_log.df1$V2)
  
  names(df)[1] <- "Time"
  names(df)[2] <- "Task"
  
  #Subsetting log to only get desired codes and time stamps:
  
  dfSubset <- df[grep("Showing", df$Task), ]
  head(dfSubset)
  
  #Cleaning up coded titles by removing parentheses:
 
  x  = dfSubset$Task        
  
  dfSubset$Task <- str_replace_all(string = x,
                                   pattern = "[\\*\\(\\)]",
                                   replacement = "")
  
  #Converting Standard time to unit (seconds * 1000) used by MAXQDA:
  
  dfSubset$Time[1]
  modified_dates <- ymd_hms(dfSubset$Time)
  
  modified_dates[2] - modified_dates[1]
  
  time_df <- data.frame(dfSubset$Time)
  dfSubset$Time2 <- modified_dates
  
  dfSubset = dfSubset[0:-1,] #Delete first view because that is when program is waiting for user to start.
  
  mydiff <- function(data, diff){
    #Difference Function to differentiate times between tasks
    c(rep(0, diff), diff(data, lag = diff))
  }
  
  dfSubset$Time_Differences <- mydiff(dfSubset$Time2, 1)
  
  dfSubset$Standard_Time_Difference <- dfSubset$Time_Differences * 1000
  
  dfSubset$Time_Stamps <- dfSubset$Standard_Time_Difference + start_time
  
  for (i in 2:nrow(dfSubset)) {
    dfSubset$Time_Stamps[i] <-  round(dfSubset$Standard_Time_Difference[i] + dfSubset$Time_Stamps[i-1], digits=0)
    
  }
  
  ### Convert Log to MAXQDA Coding Scheme ###
  
  #List for Coding Section
  
  List1 = list()
  
  searchString <- c(' ')
  replacementString <- ''
  
  for (i in seq(1, nrow(dfSubset),1) ) {
    
    phrase1 <- glue('<Code isCodable="true" name="{dfSubset$Task[i]}" color="#2364a2" guid="{str_replace_all(dfSubset$Task[i], fixed(" "), "")}"/>')
    
    List1[[length(List1)+1]] = phrase1
    
  }
  
  #Removing DUplicates from a List:
  
  List1 = List1[!duplicated(List1)]
  
  #List for Video Selection
  
  List2 = list()
  
  for (i in seq(1, nrow(dfSubset),1) ) {
    
    phrase2 <- glue('<VideoSelection end="{dfSubset$Time_Stamps[i]+standard_time_lag}" name="({dfSubset$Time_Stamps[i]},0),({dfSubset$Time_Stamps[i]+standard_time_lag},0)" creatingUser="2C5846FE-C0B7-4124-9256-794A5742CEBA" begin="{dfSubset$Time_Stamps[i]}" guid="5FB6E502-B2A2-459C-B8F7-C3E17A3A2F87">
    <Coding creatingUser="2C5846FE-C0B7-4124-9256-794A5742CEBA" guid="E9CABA1A-0832-47CE-BD66-A8EC188D37FD">
    <CodeRef targetGUID="{str_replace_all(dfSubset$Task[i], fixed(" "), "")}"/>
    </Coding>
    </VideoSelection>')
    
    List2[[length(List2)+1]] = phrase2
    
  }
  
  #All other lines for MAXQDA formatting. 
  
  line1 <- '<?xml version="1.0" encoding="utf-8"?>'
  line2 <- '<Project origin="MAXQDA 2020 (Release 20.2.1)" xsi:schemaLocation="urn:QDA-XML:project:1.0 http://schema.qdasoftware.org/versions/Project/v1.0/Project.xsd" name="Video-with-2-coded-segments" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" modifiedDateTime="2020-09-29T11:39:59Z" xmlns="urn:QDA-XML:project:1.0">'
  line3 <- '<Users>'
  line4 <- '<User name="Francesca" guid="2C5846FE-C0B7-4124-9256-794A5742CEBA"/>'
  line5 <- '</Users>'  
  line6 <- '<CodeBook>'
  line7 <- '<Codes>'
  line8 <- paste(unlist(List1), collapse='', sep = '\n')
  line9 <- '</Codes>'
  line10 <- '</CodeBook>'
  line11 <- '<Sources>'
  line12 <- glue('<VideoSource name="{video_title}" creatingUser="2C5846FE-C0B7-4124-9256-794A5742CEBA" path="internal://{video_title}" guid="46ACDB89-832F-4998-A542-D579FCBE8F1F">')
  line13 <- paste(unlist(List2), collapse='', sep = '/n')
  line14 <- '</VideoSource>'
  line15 <- '</Sources>'
  line16 <- '</Project>'
  
  #Join lines together:
  
  addr <- paste(line1,
                line2, 
                line3, 
                line4, 
                line5, 
                line6, 
                line7, 
                line8, 
                line9, 
                line10, 
                line11, 
                line12, 
                line13, 
                line14, 
                line15, 
                line16,
                sep="\n"
  )
  
  cat(addr[1])  #test print of all lines to ensure they are correct.
  
  #Output results to file. 
  
  fileConn<-file(output_path)
  writeLines(addr, fileConn)
  close(fileConn)
  
  
}

##Example use case:

#input_path <- 'C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/Parsing Logs/P30_disco.log'
#output_path <- 'C:/Users/leal0/OneDrive/Desktop/Documents/St. Gallen/Process Mining Job/Parsing Logs/output_P30.txt'
#start_time <- 387500
#video_title <- 'P30.mp4'
#standard_time_lag <- 2000

#parse_disco(input_path, output_path, start_time, video_title, standard_time_lag)
