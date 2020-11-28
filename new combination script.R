
library(tidyverse)
library(lubridate)
library(cowplot)
library(RColorBrewer)

qc_data<-readRDS("qc_data.RDA") %>%
  mutate(type=ifelse(Projection=="Actual","Actual_QC","Proj_minimum"))

canada_data<-readRDS("canada.RDA") %>%
  mutate(Projection="Actual") %>%
  filter(Region!="AB") %>%
  mutate(type=ifelse(Region=="CA","Actual_CA","Actual_BC"))

pbo_data<-readRDS("PBO_new.RDA") 

price_data<-bind_rows(qc_data,canada_data,pbo_data)


ggplot()+
  geom_line(data=price_data,mapping=aes(date,price,group=type,colour=Region,,linetype=Projection),size=1)+
  geom_point(data=filter(price_data,Projection=="pathway"),mapping=aes(date, price,colour=Region))+
  scale_color_brewer(palette="Dark2")+
  theme_minimal_hgrid()+
  theme(legend.position = "none")+
  labs(x=NULL,
       y="$/tonne CO2e")+
  ggtitle("Carbon price increases in Canada beyond 2022?")+
  annotate("text", x=ymd("2014-06-01"), y = 42,
           label = "BC",color="springgreen4")+
  annotate("text", x=ymd("2014-06-01"), y = 20,
           label = "QC",color="slateblue")+
  annotate("text", x=ymd("2014-06-01"), y = 0,
           label = "Fed",color="darkorange3")+
  annotate("text", x=ymd("2027-01-01"), y = 20,
           label = "projected minimum bid price",color="slateblue")+
  annotate("text", x=ymd("2026-01-01"), y = 280,
                      label = "Fuel Charge & $50 OBP",color="darkorange3")+
  annotate("text", x=ymd("2029-01-01"), y = 144,
           label = "Fuel Charge = OBP",color="darkorange3")+
  annotate("text", x=ymd("2029-05-01"), y = 75,
           label = "Uniform carbon\nprice",color="darkorange3")

