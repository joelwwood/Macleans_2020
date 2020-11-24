
library(tidyverse)
library(lubridate)

qc_data<-readRDS("qc_data.RDA")
canada_data<-readRDS("canada.RDA")
pbo_data<-readRDS("PBO_data.RDA")

price_data<-bind_rows(qc_data,canada_data,pbo_data)

price_data %>%
  ggplot()+
  geom_line(aes(date,price,color=Region,linetype=Projection))
