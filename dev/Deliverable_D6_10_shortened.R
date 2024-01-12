#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#         Graphs to be included in Deliverable D6.11 (Task 6.6)
#
#   "Synthesis report on the impacts of fisheries on stocks and landings under 
#                     existing management plans"
#
#
#     Summarize the information in all the case studies
#
#
#   * Input: The 'stock' and 'catch' exchange tables used in Task 6.4 for
#             deliverable 6.7. Celtic Sea FLBEIA case study (ILVO) didn't 
#             participate in this task and produced the tables specifically.
#
#
# Dorleta Garcia
# 2023-09-05
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OSMOSE is not included because is not able to simulate current management. 


#### SSB ----

ggplot(stock %>% filter(period != '2021', !(stock_name %in% constant_cpue)), 
       aes(x = period, y = stock,  fill = ratio_ssb_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'darkred',# muted("red"),
    mid = "white",
    high = 'darkblue' , #muted("blue"),
    midpoint = 0, name = "Change\n in SSB",
    limits = c(-100,100), breaks = seq(-100, 100, 50)
  ) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  # geom_text(aes(label = round(ratio_ssb_trunc), color = abs(ratio_ssb_trunc) > 60)) +
  # scale_color_manual(guide = 'none', values = c("black", "white")) +
 xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8)
ggsave('task_6.6/plots/D6_11_SSB.png', scale= 1.8, width = 13, height = 12, units = 'cm')


#### Ratio F to Fmsy ----
ggplot(stock %>% filter(!(stock_name %in% constant_cpue)), 
       aes(x = period, y = stock,  fill = ratio_f_fmsy_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'darkred',# muted("red"),
    mid = "white",
    high = 'darkblue' , #muted("blue"),
    midpoint = 0, name = "F/Fmsy",
    limits = c(-100,100), breaks = seq(-100, 100, 50),
  ) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  # geom_text(aes(label = round(ratio_f_fmsy_trunc), color = abs(ratio_f_fmsy_trunc) > 60)) +
  # scale_color_manual(guide = 'none', values = c("black", "white")) +
  xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8)

ggsave('task_6.6/plots/D6_11_F2Fmsy.png', scale= 1.8, width = 13, height = 12, units = 'cm')


#### Ratio average age ----
ggplot(stock %>% filter(period !=  2021, !(stock_name %in% constant_cpue)),  
       aes(x = period, y = stock,  fill = ratio_average_age_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'darkred',# muted("red"),
    mid = "white",
    high = 'darkblue' , #muted("blue"),
    midpoint = 0, name = "Change in\n average age",
    limits = c(-100,100), breaks = seq(-100, 100, 50),
    guide = guide_colourbar(nbin = 100, draw.ulim = FALSE, draw.llim = FALSE)
  ) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  # geom_text(aes(label = round(ratio_average_age_trunc), color = abs(ratio_average_age_trunc) > 60)) +
  # scale_color_manual(guide = 'none', values = c("black", "white")) +
  xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8) 

ggsave('task_6.6/plots/D6_11_average_age.png', scale= 1.8, width = 13, height = 12, units = 'cm')


#### p(SSB < Blim) ----
ggplot(stock %>% filter(period != '2021', !(stock_name %in% constant_cpue)), 
       aes(x = period, y = stock,  fill = pBlim)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'darkblue',# muted("red"),
    mid = 'white',
    high = 'darkred' , #muted("blue"),
    midpoint = 5, name = "p(SSB < Blim)",
    limits = c(0,100), breaks = seq(0, 100, 25),
  ) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  # geom_text(aes(label = round(pBlim), color = abs(pBlim) > 60)) +
  # scale_color_manual(guide = 'none', values = c("black", "white")) +
  xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(5, 21,29,34,36)+0.5, col = 'red', linewidth = 0.8)

ggsave('task_6.6/plots/D6_11_PSSBBlim.png', scale= 1.8, width = 13, height = 12, units = 'cm')


                                       
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Analyse 'catch' data ---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### Total landings at fleet and stock level ---- 
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

#### Total landings by fleet  ----
ggplot(total_landings_fleet %>% filter(period != '2021'), # fleet_dynamics == 'Landing obligation', HCR == 'PGY'), 
       aes(x = period, y = fleet,  fill = ratio_landings_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'white',# muted("red"),
    mid = "darkblue",
    high = 'darkgreen' , #muted("blue"),
    midpoint = 50, name = "Change\n in Landings",
    limits = c(0,100), breaks = seq(0, 100, 25)
  ) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  # geom_text(aes(label = round(ratio_landings_trunc), color = abs(ratio_landings_trunc) > 60)) +
  # scale_color_manual(guide = 'none', values = c("black", "white")) +
  xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(3, 19,35,45,47)+0.5, col = 'red', linewidth = 0.8)
ggsave('task_6.6/plots/D6_11_landings_by_fleet.png', scale= 1.8, width = 13, height = 15, units = 'cm')


#### Total landings by stock  ----
ggplot(total_landings_stock %>% filter(period != '2021'), # fleet_dynamics == 'Landing obligation', HCR == 'PGY'), 
       aes(x = period, y = stock,  fill = ratio_landings_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'white',# muted("red"),
    mid = "darkblue",
    high = 'darkgreen' , #muted("blue"),
    midpoint = 50, name = "Change\n in Landings",
    limits = c(0,100), breaks = seq(0, 100, 25)) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  # geom_text(aes(label = round(ratio_landings_trunc), color = abs(ratio_landings_trunc) > 60)) +
  # scale_color_manual(guide = 'none', values = c("black", "white")) +
  xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(5, 29,37,42,47)+0.5, col = 'red', linewidth = 0.8)
ggsave('task_6.6/plots/D6_11_landings_by_stock.png', scale= 1.8, width = 13, height = 15, units = 'cm')


#### Total landings value by fleet  ----
ggplot(total_landings_fleet %>% filter(period != '2021'), # fleet_dynamics == 'Landing obligation', HCR == 'PGY'), 
       aes(x = period, y = fleet,  fill = ratio_value_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'white',# muted("red"),
    mid = "darkblue",
    high = 'darkgreen' , #muted("blue"),
    midpoint = 50, name = "Change\n in Landings value",
    limits = c(0,100), breaks = seq(0, 100, 25)) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  xlab('Period') + ylab('Fleet')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(3, 19,35,45,47)+0.5, col = 'red', linewidth = 0.8)
ggsave('task_6.6/plots/D6_11_value_by_fleet.png', scale= 1.8, width = 13, height = 15, units = 'cm')




#### Total landings value by stock  ----
ggplot(total_landings_stock %>% filter(period != '2021'), # fleet_dynamics == 'Landing obligation', HCR == 'PGY'), 
       aes(x = period, y = stock,  fill = ratio_value_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'darkred',# muted("red"),
    mid = "white",
    high = 'darkblue' , #muted("blue"),
    midpoint = 0, name = "Change\n in Landings value",
    limits = c(0,100), breaks = seq(0, 100, 25)) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  xlab('Period') + ylab('Stock')  + facet_wrap(fleet_dynamics~HCR, ncol = 4) +
  geom_hline(yintercept = c(5, 29,37,42,47)+0.5, col = 'red', linewidth = 0.8)
ggsave('task_6.6/plots/D6_11_value_by_stock.png', scale= 1.8, width = 13, height = 15, units = 'cm')


write.csv(total_landings_stock, file = 'task_6.6/new tables/D6_11_landings_stock.csv')
write.csv(total_landings_fleet, file = 'task_6.6/new tables/D6_11_landings_fleet.csv')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Table with overall changes by period at regional level  ---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tab_stock <- stock %>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
                       summarize_at(c('ratio_f_fmsy', 'ratio_average_age', 'ratio_ssb'), mean, na.rm=T) %>% 
                       arrange(region, period, fleet_dynamics, HCR)

       
tab_stock_land <- total_landings_stock %>% filter(!(region == 'Bay of Biscay' & ratio_landings == Inf))%>% group_by(region, period, fleet_dynamics, HCR) %>% filter(period != '2021') %>% 
  summarize_at(c('ratio_landings'), mean, na.rm=T) %>% 
  arrange(region, period, fleet_dynamics, HCR) 

tab_stock <- tab_stock %>% bind_cols(tab_stock_land[,-(1:4)]) %>% pivot_longer(cols = 5:8, names_to = 'indicator') %>% 
                            mutate(value_trunc = ifelse(value > 100,100, value))
                      
tab_stock_wide <- tab_stock[,1:6] %>% pivot_wider(names_from = period, values_from = value)  

write.csv(tab_stock, file = 'task_6.6/output tables/D6_11_summary_region_level.csv')




ggplot(tab_stock, aes(x = period, y = substr(indicator,7, nchar(indicator)),  fill = value_trunc)) +
  geom_tile(color = "white", lwd = 1.5, linetype = 1)  +
  scale_fill_gradient2(
    low = 'darkred',# muted("red"),
    mid = "white",
    high = 'darkblue' , #muted("blue"),
    midpoint = 0, name = "Change\n in indicator",
    limits = c(-100,100), breaks = seq(-100, 100, 50)
  ) +
  theme_hc() +  theme(axis.text.x = element_text(angle=45,  hjust = 1)) + 
  geom_text(aes(label = round(value_trunc), color = abs(value_trunc) > 60)) +
  scale_color_manual(guide = 'none', values = c("black", "white")) +
  xlab('Period') + ylab('Indicator')  + facet_grid(region ~ fleet_dynamics*HCR) 

ggsave('task_6.6/plots/D6_11_Summary.png', scale= 1.8, width = 13, height = 15, units = 'cm')

