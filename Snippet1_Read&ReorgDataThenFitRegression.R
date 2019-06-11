# SNIPPET 1: READ & REORGANIZE DATA, THEN FIT AND PLOT A REGRESSION
# Run one line of code at a time to see what happens....
# In R studio there is "Run" button on the top right of the script window
# Just highlight a line and click it.

# Note: 
# - Most of this snippet uses base R
# - There is a 2nd version or expansion of some steps, using"tidyverse" (marked with TIDY)

# TO DO
# - aggregate vs. apply



# load the tidyverse packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


# PART 1: READ IN THE DATA #############################################################################


# read in the data file --------------------------------------------------------------
data.df <- read.csv("DATA/SampleDataFile.csv",stringsAsFactors=FALSE)
# read.csv produces a "data frame", which is basically a table where columns can have a mix of text and numbers
# stringsAsFactors=FALSE means that text columns should not be converted to "factors". 
# Factors are powerful, but can also be messy. Easier to just turn them off by default, unless specifically needed....

# TIDY
data.tibble <- readr::read_csv("DATA/SampleDataFile.csv")
# tibbles are an improved version of data frames (see https://cran.r-project.org/web/packages/tibble/vignettes/tibble.html)
# "readr::" tells R to use the function from the "readr" package.
# it is good practice to put this in, because there may be identical functions names in multiple packages.
# for example
?dplyr::filter
?stats::filter


# look at the data --------------------------------------------------------------

head(data.df) # shows the first few rows

str(data.df) # see the structure 
# In R Studio this is displayed on the right in the Environment window.
# The environment window also has a spreadsheet icon. click it to see the data in spreadsheet format

# TIDY
head(data.tibble)
str(data.tibble)




# PART 2: REORGANIZE THE DATA #############################################################################

# Problem: Need the aggregate catch and spn by year for all stocks, and for stocks 6,7,15


# base R
total.df <- aggregate(cbind(Catch, Spn)  ~ Year ,data = data.df, FUN=sum, na.rm=TRUE )
# aggregate applies a FUN to components of a data frame
head(total.df)

sub.df <- data.df[data.df$StkID %in% c(6,7,15),] 
# subset the df using only the rows that match the list of StkID
subtotal.df <- aggregate(cbind(Catch, Spn)  ~ Year ,data = sub.df , FUN=sum, na.rm=TRUE )



# TIDY
total.tibble <- data.tibble %>% dplyr::group_by(Year) %>% dplyr::summarise(Total_Catch = sum(Catch,na.rm=TRUE),Total_Spn = sum(Spn,na.rm=TRUE))
total.tibble

subtotal.tibble <- data.tibble %>% dplyr::filter(StkID %in% c(6,7,15)) %>% dplyr::group_by(Year) %>% dplyr::summarise(Total_Catch = sum(Catch,na.rm=TRUE),Total_Spn = sum(Spn,na.rm=TRUE))
subtotal.tibble



# PART 3: FIT SIMPLE LINEAR REGRESSION #############################################################################

# same steps in base R and TIDY.

lm.fit <- lm(log(Total_Catch) ~ log(Total_Spn), data=total.tibble) # fit a linear model
names(lm.fit) # print a list of components in the output from lm
lm.fit$residuals # access one of the components
summary(lm.fit) # print a summary of the fit
plot(lm.fit)  # create a series of diagnostic plots



# PART 4: PLOT THE REGRESSION FIT #############################################################################


# base R
plot(x=log(total.df$Spn), y=log(total.df$Catch), xlab = "Spn",ylab="Catch" , type="p", bty="n",pch=21,col="darkblue",bg="lightgrey")
# create a basic plot
# type = "p" means plot points (use "l" for lines, and "o" for overplotting points on top of lines)
# bty = "n" means do not draw a box around the plot area
# pch, col, and bg define the plotting character, color, and background

abline(lm.fit$coefficients,col="red",lwd=2)
# plot a line with intercept a and slope b
# lwd is the line width

title(main="Linear Fit", col.main="darkblue",cex.main=1.5)

# TIDY
# ggplot version to be inserted








