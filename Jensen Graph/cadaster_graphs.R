

time_breaks <- c("201401",
                 "201405",
                 "201410",
                 "201502",
                 "201507",
                 "201511",
                 "201604",
                 "201608",
                 "201701",
                 "201705",
                 "201710")

#------------------------------------------------------------------------------#
#### Number of transactions graphs (Full country)####

#lais$sell[is.na(lais$sell)] <- 0



#------------------------------------------------------------------------------#
#### Number of transactions graphs (Around feeders) ####

# Fix sell variable

cad1km$sell[is.na(cad1km$sell)] <- 0


# Date variable
cad1km$aDate <- as.Date(cad1km$aDate)
#cad1km$aDate_year_month <- format(cad1km$aDate,"%Y-%b")
cad1km$aDate_year_month <- cad1km$aDate_year*100 + cad1km$aDate_month



# Aggregate number of transactions
sell_perMonth <- 
  aggregate(sell ~ aDate_year_month + treated,
            FUN = sum,
            data = cad1km)

names(sell_perMonth) <- c("year_month","treated", "sell")

# sell_perMonth_wide <-
#   sell_perMonth %>%
#     spread(treated, sell)

#names(sell_perMonth) <- c("year_month", "sell_control", "sell_treated")
#sell_perMonth$total <- sell_perMonth$sell_control + sell_perMonth$sell_treated

sell_perMonth$year_month <- factor(sell_perMonth$year_month, ordered = T)

sell_perMonth$treated <- factor(sell_perMonth$treated, labels = c("Isolated", "Connected"))

ggplot(data = sell_perMonth,
       aes(x = year_month,
           y = sell,
           color = factor(treated),
           group = treated)) +
  geom_line()+
  theme_minimal() +
  labs(title="Transfer transactions around 1km from feeder roads",
       x ="", 
       y = "Number of transactions",
       color = "") +
  theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1)) + 
  scale_x_discrete(breaks=time_breaks) +

  
  ggsave(file.path(OUT_graphs, "Cadaster/Ntransactions_absolute.png"),
         device = "png")




#------------------------------------------------------------------------------#
#### Number of transactions graphs  per village (Around feeders) ####

cad1km_vill_t <- aggregate(sell ~ vill_code_str + aDate_year_month + treated,
                         FUN = sum,
                         na.rm = T,
                         na.action=NULL,
                         data = cad1km)

sell_perMonth_vill <- 
  aggregate(sell ~ aDate_year_month,
            FUN = mean,
            data = cad1km_vill_t)

sell_perMonth_vill$sell_sd <- 
  aggregate(sell ~ aDate_year_month,
            FUN = sd,
            data = cad1km_vill_t)$sell

#names(sell_perMonth_vill) <- c("year_month","treated", "sell_mean", "sell_sd")
names(sell_perMonth_vill) <- c("year_month","sell", "sell_sd")


sell_perMonth_vill$year_month <- factor(sell_perMonth_vill$year_month, ordered = T)

#sell_perMonth_vill$treated <- factor(sell_perMonth_vill$treated, labels = c("Isolated", "Connected"))

ggplot(data = sell_perMonth_vill,
       aes(x = year_month,
           y = sell)) +
  geom_point(col = "navyblue") +
  geom_errorbar(aes(ymin = sell - sell_sd,
                    ymax = sell + sell_sd),  
                col = "navyblue", width=.2) +
  
  theme_minimal() +
  labs(title="Transfer per village transactions around 1km from feeder roads",
       x ="", 
       y = "Number of transactions",
       color = "") +
  theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1)) + 
  scale_x_discrete(breaks=time_breaks) +
  
  ggsave(file.path(OUT_graphs, "Cadaster/Ntransactions_per_village.png"),
         device = "png")


#------------------------------------------------------------------------------#
#### Average transactions  per village (Around feeders) ####
sM_vill_treat <- 
  aggregate(sell ~ aDate_year_month + treated,
            FUN = mean,
            data = cad1km_vill_t)

sM_vill_treat_wide <-
  sM_vill_treat%>%
     spread(treated, sell)

names(sM_vill_treat_wide) <- c("year_month","control", "treated")


sM_vill_treat_wide$year_month <- factor(sM_vill_treat_wide$year_month, ordered = T)



# Difference between treated and control
sM_vill_treat_wide$diff <- sM_vill_treat_wide$treated - sM_vill_treat_wide$control


ggplot(data = sM_vill_treat_wide,
       aes(x = year_month,
           y = diff,
           group = 1)) +
  geom_line(col = "orangered") +
  geom_hline(yintercept = 0) + 
  annotate(geom="text", x=35, y=0.2, label="On average, \n0.5% superior", size = 3.5) + 
  annotate(geom="text", x=4, y=0.3, label="Connected") + 
  annotate(geom="text", x=3.4, y=-0.22, label="Isolated") + 
  
  theme_minimal() +
  labs(title="Number of transacions - Difference between \nconnected and isolated villages",
       x ="", 
       y = "Absolute difference",
       color = "") +
  theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1)) + 
  scale_x_discrete(breaks=time_breaks) +
  
  ggsave(file.path(OUT_graphs, "Cadaster/Ntransactions_diff_isolatedVconnected.png"),
         device = "png")

