# load required packages
library(RSocrata)
library(dplyr)
library(readr)
library(rgdal)
library(ggplot2)

incidents_hoods <-read_csv("incidents_hoods.csv")

incidents_hoods$Neighborhood <- gsub("/", "-", incidents_hoods$Neighborhood)

for (hood in incidents_hoods$Neighborhood) {
  temp <- incidents_hoods %>%
    filter(Neighborhood==hood & Year < 2015)
  temp_prop <- temp %>%
    filter((Category=="LARCENY/THEFT" |Category=="ARSON"|Category=="BURGLARY"|Category=="VEHICLE THEFT") & Descript!="GRAND THEFT FROM LOCKED AUTO" & Descript!="PETTY THEFT FROM LOCKED AUTO") %>%
    mutate(Type = "Other")
  temp_car <- temp %>%
    filter(Descript=="GRAND THEFT FROM LOCKED AUTO" |Descript=="PETTY THEFT FROM LOCKED AUTO" ) %>%
    mutate(Type="Car break-in")
  temp <- rbind(temp_prop, temp_car) %>%
    group_by(Year, Type) %>%
    summarise(Count = n())
  trend <- nPlot(Count ~ Year, 
                 group = "Type", 
                 data = temp, 
                 type = "lineChart")
  trend$chart(forceY =c(0, 5000))
  trend$save(paste0(hood,".html"), cdn = FALSE)
}


incidents_SOMA <- incidents_hoods %>%
  filter(Neighborhood=="South of Market" & Year <2015) 

property_crimes_SOMA <- incidents_SOMA %>%
  filter(Category=="LARCENY/THEFT" |Category=="ARSON"|Category=="BURGLARY"|Category=="VEHICLE THEFT")

Car_Break_ins_SOMA <- property_crimes_SOMA %>%
  filter(Descript=="GRAND THEFT FROM LOCKED AUTO" |Descript=="PETTY THEFT FROM LOCKED AUTO" )

Other_property_crimes_SOMA <-property_crimes_SOMA %>%
  filter(Descript!="GRAND THEFT FROM LOCKED AUTO") %>% 
  filter(Descript!="PETTY THEFT FROM LOCKED AUTO" )


car_break_in <- Car_Break_ins_SOMA  %>%
  group_by(Year) %>%
  summarize(car_break_in_number=n())%>%
  rename(car_break_in_year=Year)

others <- Other_property_crimes_SOMA  %>%
  group_by(Year) %>%
  summarize(others_number=n())%>%
  rename(others_year=Year)

# join query to identify doctors paid to run Expert-led forums who also received a warning letter
car_break_in_vs_others <- inner_join(car_break_in, others, by=c("car_break_in_year" = "others_year"))%>%
  rename(Year = car_break_in_year)

library(tidyr)

comparison <- gather(car_break_in_vs_others, Type, Count, -Year) 

comparison$Type <- gsub("car_break_in_number", "Car break-ins", comparison$Type)
comparison$Type <- gsub("others_number", "Other", comparison$Type)


# load required packages
library(RSocrata)
library(dplyr)
library(readr)
library(rgdal)
library(ggplot2)

write.csv(car_break_in_vs_others, "car_break_in_vs_others.csv", row.names = FALSE, na="")





trend <- nPlot(Count ~ Year, 
                       group = "Type", 
                       data = comparison, 
                       type = "lineChart")
trend
