# SNIPPET 2: EXTRACT DATA FROM A CLEAN EXCEL WORKSHEET AND CREATE A FLAT FILE
#  (all tabs have the same format, cell range is hardwired)

# Run one line of code at a time to see what happens....
# In R studio there is "Run" button on the top right of the script window
# Just highlight a line and click it.

# Note: 
# - This snippet uses the readxl and cellranger packages



#load the packages
library(tidyverse)
library(readxl)
library(cellranger)

file.use <- "DATA/SampleCleanExcelSource.xlsx"

# get all the worksheet names
sheets.list <-  readxl::excel_sheets(file.use )


# use the first worksheet to get the table format

merged.tibble <- readxl::read_excel(file.use, 
                                sheet=sheets.list[1],
                                col_names=TRUE,
                                col_types = "guess", # guess the col format (numeric, text, date); could specify one format per column   
                                .name_repair="universal", # apply the naming fixes as per  default R settings
                                range=cellranger::cell_limits(ul=c(3,1),lr=c(70,4)))

merged.tibble$StkNmXLS <-  sheets.list[1]

# loop through the remaining worksheets and merge the records

for(sheet.read in sheets.list[-1]){  # all sheets except the first one
  print(paste("starting",sheet.read))
  
  
  tmp.tibble <- readxl::read_excel(file.use, 
                                      sheet=sheet.read,
                                      col_names=TRUE,
                                      col_types = "guess", # guess the col format (numeric, text, date); could specify one format per column   
                                      .name_repair="universal", # apply the naming fixes as per  default R settings
                                      range=cellranger::cell_limits(ul=c(3,1),lr=c(70,4)))
  
  tmp.tibble$StkNmXLS <-  sheet.read
  
  merged.tibble <- rbind(merged.tibble,tmp.tibble)
  
  
}


merged.tibble




# write to flat file

# write to file
readr::write_csv(merged.tibble,"DATA/GeneratedFlatFile_FromCleanExcel.csv")







