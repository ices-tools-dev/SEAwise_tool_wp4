## code to prepare `data/D6_11_catch.RData` dataset goes here

load("data-raw/D6_11_catch.RData")
load("data-raw/D6_11_stock.RData")


total_landings_stock <- catch %>% group_by(region, case_study, fleet_dynamics, HCR, period, stock) %>%  
  summarize_at(c('landings', 'value_of_landings'), sum)  %>% group_by(region, case_study, fleet_dynamics, HCR, stock) %>% 
  mutate(ratio_landings       = landings/landings[period == '2021'],
         ratio_value          = value_of_landings/value_of_landings[period == '2021'],
         ratio_landings_trunc = ifelse(abs(ratio_landings) > 100, 100*sign(ratio_landings), ratio_landings),
         ratio_value_trunc    = ifelse(abs(ratio_value) > 100, 100*sign(ratio_value), ratio_value)) %>% 
  arrange(region, fleet_dynamics, HCR, stock, period) %>% 
  mutate(position = length(stock), stock = reorder(as.factor(stock), position))


total_landings_fleet <- catch %>% group_by(region, case_study, fleet_dynamics, HCR, period, fleet) %>%  
  summarize_at(c('landings', 'value_of_landings'), sum) %>%  
  group_by(region, case_study, fleet_dynamics, HCR, fleet) %>%  
  mutate(ratio_landings       = landings/landings[period == '2021'],
         ratio_value          = value_of_landings/value_of_landings[period == '2021'],
         ratio_landings_trunc = ifelse(abs(ratio_landings) > 100, 100*sign(ratio_landings), ratio_landings),
         ratio_value_trunc     = ifelse(abs(ratio_value) > 100, 100*sign(ratio_value), ratio_value)) %>% 
  arrange(region, fleet_dynamics, HCR, fleet, period) %>% 
  mutate(position = length(fleet), fleet = reorder(as.factor(fleet), position))


tab_stock <- stock %>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
  summarize_at(c('ratio_f_fmsy', 'ratio_average_age', 'ratio_ssb'), mean, na.rm=T) %>% 
  arrange(region, period, fleet_dynamics, HCR)


tab_stock_land <- total_landings_stock %>% filter(!(region == 'Bay of Biscay' & ratio_landings == Inf))%>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
  summarize_at(c('ratio_landings'), mean, na.rm=T) %>% 
  arrange(region, period, fleet_dynamics, HCR) 


tab_stock <- tab_stock %>% bind_cols(tab_stock_land[,-(1:4)]) %>% tidyr::pivot_longer(cols = 5:8, names_to = 'indicator') %>% 
  mutate(value_trunc = ifelse(value > 100,100, value))


usethis::use_data(stock, overwrite = T)
usethis::use_data(catch, overwrite = T)
usethis::use_data(tab_stock, overwrite = T)
usethis::use_data(tab_stock_land, overwrite = T)
usethis::use_data(total_landings_fleet, overwrite = T)
usethis::use_data(total_landings_stock, overwrite = T)
