library(tidyverse)

##WCI auction settlement prices in USD copy and pasted from http://www.environnement.gouv.qc.ca/changements/carbone/ventes-encheres/historique-prix-encheres-WCI-en.pdf
#these cover Nov 2014 to May 2020

prices<-c("$ 12,10
$ 12,21
$ 12,29
$ 12,52
$ 12,73 
$ 12,73 
$ 12,73 
$ 12,73 
$ 12,73
$ 13,57
$ 13,80
$ 14,75
$ 15,06
$ 14,61 
$ 14,65
$ 15,05
$ 15,31
$ 15,73
$ 17,45
$ 17,16 
$ 17,00
$ 17,87
$ 16,68")

x<-prices %>%
  str_replace_all(",",".") %>%
  str_replace_all("\\$ ","") %>%
  str_split("\n") %>%
  .[[1]]


##Data collected as CAD settlement prices from Summary Report REsults at http://www.environnement.gouv.qc.ca/changements/carbone/ventes-encheres/avis-resultats-en.htm
##minimum price projections in USD from http://www.environnement.gouv.qc.ca/changements/carbone/Ventes-encheres-en.htm
#convert to CAD using 1.3238

prices<-c("15.01
15.01
16.39
17
17.64
16.64
16.45
17.29
17.84
18.82
18.74
19.1
18.44
18.72
19.77
20.27
20.82
23.48
22.82
22.46
23.69
23.17
22.03
NA")

prices<-prices %>%
  str_split("\n") %>%
  .[[1]]

days<-c("18
21
18
17
17
18
16
15
22
16
15
14
21
15
14
14
20
14
20
19
19
20
18
17") %>%
  str_split("\n") %>%
  .[[1]]


qc_df<-tibble( year = rep(c(2015:2020),4)) %>%
  arrange(year) %>%
  mutate(month = rep(c("02","05","08","11"),6)) %>%
  bind_cols(days) %>%
  unite(date,sep="-") %>%
  mutate(date = ymd(date)) 
         
qc_df$prices<-as.double(prices)
  
ggplot(qc_df, aes(date,prices))+
  geom_line()+
  geom_point()


#load projected price floor data
proj_prices<-c("$17.88 	$19.13 	$20.47 	$21.90 	$23.43 	$25.07 	$26.82 	$28.70 	$30.71 	$32.86") %>%
  str_split(" \t") %>%
  .[[1]]
  
proj_prices<-proj_prices %>%
  str_replace_all("\\$","")

year_prices<-tibble(as.double(proj_prices),
         year=c(2021:2030)) 

qc_proj<-tibble(year=rep(c(2021:2030),12)) %>%
  arrange(year) %>%
  left_join(year_prices) %>%
  rename(price=`as.double(proj_prices)`) %>%
  mutate( month = rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),10),
          day = "01",
          price=(price*1.3238)) %>%
  select(year, month, day, price) %>%
  unite(date,c(year,month,day),sep="-") %>%
  mutate( date=ymd(date),
          Region= "QC",
          Projection= "Projected")


#bind the two data frames

qc_df<-qc_df %>%
  mutate(Region = "QC",
         Projection = "Actual") %>%
  rename(price=prices)


qc_full<- bind_rows(qc_df,qc_proj)
 

ggplot()+
  geom_line(data=qc_full,aes(date,price,linetype=Projection))+
  geom_point(data=filter(qc_full,Projection =="Actual"),aes(date,price))
  
saveRDS(qc_full,"qc_data.RDA")
dir()
