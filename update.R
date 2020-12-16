

#This code updates my Macleans' chart to reflect the federal government's carbon pricing announcement

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

n<-length(c(2023:2030))
canada_new<-tibble( year = c(rep(c(2023:2030),2),2030),
                    month = c(
                      rep("03",n),
                      rep("04",n),"12"),
                    day="01") %>%
  unite(date,sep="-") %>%
  mutate(date = ymd(date) ) %>%
  filter(date>="2023-04-01") %>%
  arrange(date) %>%
  mutate(Region="CA",
         price=c(65,65,80,80,95,95,110,110,125,125,140,140,155,155,170,170),
         Projection="Actual",
         type="Actual_CA")


price_data<-bind_rows(qc_data,canada_data,canada_new,pbo_data)


ggplot()+
  geom_line(data=price_data,mapping=aes(date,price,group=type,colour=Region,,linetype=Projection),size=1)+
  geom_point(data=filter(price_data,Projection=="pathway"),mapping=aes(date, price,colour=Region))+
  scale_color_brewer(palette="Dark2")+
  theme_minimal_hgrid()+
  theme(legend.position = "none")+
  xlim(ymd("2014-01-01"),ymd("2035-01-01"))+
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
           label = "projected minimum bid price",color="slateblue",size=3)+
  annotate("text", x=ymd("2026-01-01"), y = 280,
           label = "Fuel Charge & $50 OBP",color="darkorange3",size=3)+
  annotate("text", x=ymd("2033-01-01"), y = 140,
           label = "Fuel Charge = OBP",color="darkorange3",size=3)+
  annotate("text", x=ymd("2031-05-01"), y = 85,
           label = "Uniform carbon\nprice",color="darkorange3",size=3)+
  annotate("text", x=ymd("2031-05-01"), y = 180,
           label = "New Federal Plan",color="darkorange3")



