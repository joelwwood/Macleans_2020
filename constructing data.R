library(tidyverse)
library(lubridate)

n<-length(c(2015:2022))

dates<-tibble( year = rep(c(2015:2022),12),
               month = c(rep("01",n),
                         rep("02",n),
                         rep("03",n),
                         rep("04",n),
                         rep("05",n),
                         rep("06",n),
                         rep("07",n),
                         rep("08",n),
                         rep("09",n),
                         rep("10",n),
                         rep("11",n),
                         rep("12",n)),
               day="01") %>%
  unite(date,sep="-") %>%
  mutate(date = ymd(date) )



  
  df<-dates %>%
  mutate( BC=30,
          BC=ifelse(date>"2018-03-01",35,BC),
          BC=ifelse(date>"2019-03-01",40,BC),
          
          
          AB=0,
          AB=ifelse((date>="2017-01-01"& date<"2018-01-01"),20,AB),
          AB=ifelse((date>="2018-01-01"& date<"2019-06-01"),30,AB),
          AB=ifelse((date>="2020-01-01"& date<"2020-04-01"),20,AB),
          AB=ifelse((date>="2020-04-01"& date<"2021-04-01"),30,AB),
          AB=ifelse((date>="2021-04-01"& date<"2022-04-01"),40,AB),
          AB=ifelse((date>="2022-04-01"& date<"2023-04-01"),50,AB),
          CA=0,
          CA=ifelse((date>="2019-04-01"& date<"2020-04-01"),20,CA),
          CA=ifelse((date>="2020-04-01"& date<"2021-04-01"),30,CA),
          CA=ifelse((date>="2021-04-01"& date<"2022-04-01"),40,CA),
          CA=ifelse((date>="2022-04-01"& date<"2023-04-01"),50,CA)
            ) 
    

df<-  df %>%
    pivot_longer(BC:CA,names_to="Region",values_to="price") %>%
    mutate(Projection="Actual",
           Projection=ifelse(date>"2021-01-01","Projected",Projection)
           )
df %>%
ggplot(aes(color=Region))+
  geom_line(aes(date,price,linetype=Projection))

saveRDS(df,"canada.RDA")
  
