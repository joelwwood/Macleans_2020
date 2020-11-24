
##carbon price projections from PBO modelling: 2023- $59 to 2030- $131

year_price<-tibble(year=rep(c(2023:2030),12),
       price=rep(c(59,69,79,89,99,109,120,131),12)) %>%
  arrange(year) %>%
  mutate(day="01",
         month = rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),8)
  ) %>%
  unite(date,c(year,month,day),sep="-") %>%
  mutate(date=ymd(date),
         Region="PBO",
         Projection="Projected") %>%
  filter(date>"2023-03-01")

saveRDS(year_price,"PBO_data.RDA")

