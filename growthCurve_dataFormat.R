#title: "Formatting growth curve data for JMP"
#purpose: importing raw growth curve from plate reader, reorganizing for graphing in JMP, exporting as another xlsx
#author: Andrea Domen
#acknowledgement: James Molyneux

##**REMOVE (C) FROM TEMPERATURE COLUMN LABEL BEFORE USING**##
  
#Load libraries needed
library(tidyverse)
library(readxl)
library(writexl)

###########################
#Function code
format_data <- function(filepath, interval, outputpath) {
  ##Import data, skip first two rows
  ##chop off last 3 rows (extraneous crap)
  ##Remove time column
  df <- read_excel(filepath, skip = 2)
  df <- head(df, -3)
  cleaneddf <- df %>% 
    select(-"Time")
 
  #make a vector for the minutes
  minutes <- head(seq(0, nrow(cleaneddf)*interval, by = interval), -1)
  
  #Add minute and hour column, regroup those to the front
  mutateddf <- cleaneddf %>% 
    mutate(Minutes = minutes) %>% 
    mutate(Hours = floor(Minutes/60)) %>% 
    relocate(Minutes, Hours)
  
  #find out the max column number and assign it to an object
  colnumb <- ncol(mutateddf)
  
  #Finally, switch rows and columns
  finaldf <- mutateddf %>% 
    gather(key = "Well", value = "OD_reading", 4:colnumb)
  
  #write everything to a new excel file
  write_xlsx(finaldf, outputpath)
}
###########################


 # Use the function below
 #     - need 3 things, the filepath where the original excel sheet is, the time interval you ran this at, and the file path where you want the new excel file to go
 #     -at the end of the filepath, put what you want the output file to be called

###FOR EXAMPLE, read time interval of 10 minutes###

#format_data("/Users/andreadomen/Downloads/RAW.xlsx", interval = 10, "/Users/andreadomen/Downloads/OUTPUT_NAME.xlsx")

format_data()


