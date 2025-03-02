
library(readxl)
library(writexl)
library(tidyverse)


## Began with copypasted data from 338 site
df<-read_xlsx("canadian-general-polling.xlsx")

# fix the misaligned vals
for (i in seq(nrow(df), 2)) {
  if (is.na(df$Rating[i])) {
    df$`Polling Firm`[i - 1] <- paste0(df$`Polling Firm`[i - 1],
                                      df$`Polling Firm`[i])
  }
}

# And remove the empty rows
cad<-df[!is.na(df$Rating),]

# cleaned df
# write_xlsx(cad,"cad.xlsx")

# Fix single missing value 
# Source: https://innovativeresearch.ca/wp-content/uploads/2025/02/Canadians-and-Trump-1.pdf
if (is.na(cad[6,10])){
  print("Fixed Missing Value")
  cad[6,10] = 3
}

# Again. Source: https://abacusdata.ca/canadian-politics-abacus-data-post-freeland-resignation/
if (is.na(cad[44,10])){
  print("Fixed Missing Value")
  cad[44,10] = 3
}

## Set the simulation date (as current date for now)
CURRENT_DATE = Sys.Date()
sim_date = CURRENT_DATE

#fix a few other things
cad<-cad%>%
  mutate(date = as.Date(`Date (middle)`, origin = "1899-12-30"),
         Sample = as.integer(gsub(",", "", sub(" .*", "", Sample))))

#find the median sample size
med_size<-median(cad$Sample)

## Real Calculations begin now

# Function to get the past averages given sim date and smooth param
calculate_avgs_given_sim_date<-function(sim_date,smooth){
  cad_with_weights<-cad%>%
    filter(date<=sim_date)%>%
    mutate(days_behind=as.numeric(sim_date-date))%>%
    mutate(size_weight = round(sqrt(Sample)/sqrt(med_size),4),
           grade_weight = case_when(Rating == 'A+'~3.0,Rating == 'A−'~2.8,Rating == 'A'~2.6,
                                    Rating == 'B+'~2.4,Rating == 'B'~2.2,Rating == 'B−'~2.0,TRUE~1.5),
           date_weight = round(smooth^days_behind,4))%>%
    mutate(weight = size_weight*grade_weight*date_weight)
  
  return(colSums(cad_with_weights[5:10]*cad_with_weights$weight,na.rm=T)/sum(cad_with_weights$weight))
}



## Begin loop to 'backdate' poll averages
sim_date=CURRENT_DATE
smooth=0.85
cad_ewma_avgs<-data.frame()
while(sim_date>=as.Date("2024-06-01")){
  cad_ewma_avgs<-rbind(cad_ewma_avgs,c(calculate_avgs_given_sim_date(sim_date,smooth),sim_date))
  sim_date=as.Date(sim_date-1)
  
}
colnames(cad_ewma_avgs)=c(colnames(cad_with_weights)[5:10],"date")
cad_ewma_avgs$date<-as.Date(cad_ewma_avgs$date)



# And this is if I ignore everything and take a simple average of the last 4 polls
colMeans(cad_with_weights[1:4,5:10])










