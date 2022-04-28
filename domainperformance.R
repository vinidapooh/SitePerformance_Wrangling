library(dplyr) 
library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(DT)

#reading in the domain performance report from the Report Builder
domain_raw = read_csv("/Users/vvijaypoojari/Documents/Domain Performance.csv")
linenames = unique(domain_raw['Line'])
lineids = unique(domain_raw$`Line Id`)

#Making 2 lists, one for the tab names as the Line IDs, and the other for the dataframes
tabname <- list()
ddf <- list()


#We go by each Line ID on the report, and build out the tables of domain performance for each line
#We also iterate for each of the tab names, by stacking each of the line IDs in consideration

for (i in seq_along(lineids)){
  
  tabname[i] <- assign(paste0("Line_",i),lineids[[i]])
  
  #see if you can get the same result, without mentioning the column names
  ddf[[i]] <-domain_raw%>% select(`Line Id`,Domain,`Advertiser Spending`,Impressions,Clicks,Conversion,Line)%>%
    filter(`Line Id` == lineids[i])%>%
    group_by(Line,Domain)%>%
    summarise(Spend = sum(`Advertiser Spending`), Imp = sum(Impressions), Clicks = sum(Clicks), Conversions = sum(Conversion))%>%
    mutate(CPA = Spend/Conversions)%>%arrange(desc(Spend))

  
}


#the tabs are created by Line IDs, and the data corresponding to them gets it's own tab with the data

write.xlsx(ddf,"domainperf2.xlsx",sheetName = tabname)















