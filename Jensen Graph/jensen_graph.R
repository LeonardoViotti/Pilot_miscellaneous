#------------------------------------------------------------------------------#

#		Rwanda Feeder Roads - Draft Analysis for meeting 10/18/18

# (Not spatial analisys, for that check spatial_analysis_master.R)
#------------------------------------------------------------------------------#


# Load data

# Load clean data
ps		<- read.dta13(file.path(PRICE_SV, "Price_survey_preprocess.dta"), convert.factors = F)

#------------------------------------------------------------------------------#
#### Settings ####

end_date <- "2017-08-05"  #3
#end_date <- "2017-09-28" #3



#start_date <- "2017-06-01" #4
start_date <- "2017-06-05" #4
# start_date <- "2016-06-06" #4
# start_date <- "2016-06-27" #4




#------------------------------------------------------------------------------#
#### Subseting ####

end_markets <-
  unique(master$market_uid[master$feeder_end_dt == end_date])[-1]

sta_markets <-
  unique(master$market_uid[master$feeder_start_dt == start_date])[-1]

 edPs<- ps[ps$market_uid %in%   end_markets ,c("market_uid", "marketname_str", "year_month", "w_std_price_tomato_1", "w_std_price_tomato_2")]
 stPs <- ps[ps$market_uid %in%  sta_markets ,c("market_uid", "marketname_str", "year_month", "w_std_price_tomato_1", "w_std_price_tomato_2")]

 
 #### Average by trader ####
 
 edPs$price_tomato <- rowMeans(edPs[, c("w_std_price_tomato_1", "w_std_price_tomato_2")], na.rm = T)
 
 edPs$year_month <- factor(edPs$year_month, ordered = T)
 
 
 stPs$price_tomato <- rowMeans(stPs[, c("w_std_price_tomato_1", "w_std_price_tomato_2")], na.rm = T)
 
 stPs$year_month <- factor(stPs$year_month, ordered = T)
 
 #### Reshape by trader
 
 edPs_long <- gather(edPs, 
                     key = "trader",
                     value = "price",
                     c("w_std_price_tomato_1", 
                       "w_std_price_tomato_2"))
 edPs_long$trader_market <- paste(edPs_long$market_uid, edPs_long$trader)
 edPs_long$year_month <- factor(edPs_long$year_month, ordered = T)
 
 
 stPs_long <- gather(stPs, 
                     key = "trader",
                     value = "price",
                     c("w_std_price_tomato_1", 
                       "w_std_price_tomato_2"))
 stPs_long$trader_market <- paste(stPs_long$market_uid, stPs_long$trader)
 stPs_long$year_month <- factor(stPs_long$year_month, ordered = T)
 
 
 #### SINGLE PRODUCT GRAPHs ####
 

 ggplot(data = edPs,
        aes(x = year_month,
            y = price_tomato,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(10)) +
   annotate(geom="text",
            x=12.4,
            y=700,
            label= paste("End of construction\n", end_date),
            size = 3) +
   theme_minimal() +
   labs(title="Price of tomato - 3 markets in Rwamagana",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_end.png"),
          device = "png")
 
 
 
 ggplot(data = stPs,
        aes(x = year_month,
            y = price_tomato,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(8)) +
   
   annotate(geom="text",
            x=10.4,
            y=700,
            label= paste("Start of construction\n", start_date),
            size = 3) +
   theme_minimal() +
   labs(title="Price of tomato - 4 markets in Karongi",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_start.png"),
          device = "png")
 
 
# Market: Mukungu 310802048
# District: Karongi
# Product: Tomato
# Construction start: a
