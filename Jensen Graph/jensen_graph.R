#------------------------------------------------------------------------------#

#		Rwanda Feeder Roads - Draft Analysis for meeting 10/18/18

# (Not spatial analisys, for that check spatial_analysis_master.R)
#------------------------------------------------------------------------------#


# Load data

# Load clean data
ps		<- read.dta13(file.path(PRICE_SV, "Price_survey_preprocess.dta"), convert.factors = F)


#-------------------------------------------------------------------------------#
#### MERGE WITH MASTER ####

#### Merge with master and monitoring data ####



#### Any road associated with the market CHANGE THAT
# master.mg <- aggregate(cbind(feeder_end_dt, feeder_start_dt) ~ market_uid, 
#                        function(x) as.integer(any(x >0)), 
#                        data = master[!is.na(master$market_uid), c("market_uid", "feeder_end_dt", "feeder_start_dt")])



master.mg <- master[!is.na(master$market_uid), 
                    c("market_uid", 
                      "feeder_oid", 
                      "feeder_end_dt", 
                      "feeder_start_dt")]


#### Yearmonth construction variables
master.mg$year_month_end <- as.integer(format(master.mg$feeder_end_dt,"%Y"))*100 + as.integer(format(master.mg$feeder_end_dt,"%m"))
master.mg$year_month_st  <- as.integer(format(master.mg$feeder_start_dt,"%Y"))*100 + as.integer(format(master.mg$feeder_start_dt,"%m"))


#### Merge - There are 29 duplicated markets in the sample, hence the merge increases the number of obs
ps <- merge(ps, master.mg, by = "market_uid", all.x = T)


#-------------------------------------------------------------------------------#
#### MONTHS FROM TREATMENT VARIABLE ####

#### Factor levels
levels <- rep(c(2016:2019)*100, each=12) +  c(1:12)

#### Convert date variables to ordered factors
ps$year_month <- factor(ps$year_month, levels =levels,  ordered=TRUE)
ps$year_month_end <- factor(ps$year_month_end, levels =levels, ordered=TRUE)
ps$year_month_st <- factor(ps$year_month_st, levels =levels, ordered=TRUE)

#### Calculate difference
ps$end_diff <- unclass(ps$year_month)-unclass(ps$year_month_end) +1 # Add one to make month one the tratment month
ps$start_diff <- unclass(ps$year_month)-unclass(ps$year_month_st) +1 

#### Replace NAs in end and start diff by 999
ps$end_diff[is.na(ps$end_diff)] <- 999
ps$start_diff[is.na(ps$start_diff)] <- 999 


#View(ps[,c("market_uid", "year_month", "year_month_end", "year_month_st", "end_diff", "start_diff")])



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

var_vec <- c("market_uid", 
             "marketname_str", 
             "year_month", 
             "end_diff",
             "start_diff",
             "w_std_price_tomato_1", 
             "w_std_price_tomato_2",
             "w_std_price_cook_banana_1",
             "w_std_price_cook_banana_2",
             "w_std_price_cook_oil_1",
             "w_std_price_cook_oil_2")


end_markets <-
  unique(master$market_uid[master$feeder_end_dt == end_date])[-1]

sta_markets <-
  unique(master$market_uid[master$feeder_start_dt == start_date])[-1]

 edPs<- ps[ps$market_uid %in%   end_markets ,var_vec]
 stPs <- ps[ps$market_uid %in%  sta_markets ,var_vec]

 
 #### Average by trader ####
 
 edPs$price_tomato <- rowMeans(edPs[, c("w_std_price_tomato_1", "w_std_price_tomato_2")], na.rm = T)
 edPs$price_cook_oil <- rowMeans(edPs[, c("w_std_price_cook_oil_1", "w_std_price_cook_oil_2")], na.rm = T)
 edPs$price_cook_banana <- rowMeans(edPs[, c("w_std_price_cook_banana_1", "w_std_price_cook_banana_2")], na.rm = T)
 
 
 edPs$year_month <- factor(edPs$year_month, ordered = T)
 
 
 stPs$price_tomato <- rowMeans(stPs[, c("w_std_price_tomato_1", "w_std_price_tomato_2")], na.rm = T)
 stPs$price_cook_oil <- rowMeans(stPs[, c("w_std_price_cook_oil_1", "w_std_price_cook_oil_2")], na.rm = T)
 stPs$price_cook_banana <- rowMeans(stPs[, c("w_std_price_cook_banana_1", "w_std_price_cook_banana_2")], na.rm = T)
 
 stPs$year_month <- factor(stPs$year_month, ordered = T)
 
 
 
 edPs$price_tomato_n <- (edPs$price_tomato - mean(edPs$price_tomato, na.rm = T))/sd(edPs$price_tomato, na.rm = T)
 edPs$price_cook_oil_n <- (edPs$price_cook_oil - mean(edPs$price_cook_oil, na.rm = T))/sd(edPs$price_cook_oil, na.rm = T)
 edPs$price_cook_banana_n <- (edPs$price_cook_banana - mean(edPs$price_cook_banana, na.rm = T))/sd(edPs$price_cook_banana, na.rm = T)
 
 stPs$price_tomato_n <- (stPs$price_tomato - mean(stPs$price_tomato, na.rm = T))/sd(stPs$price_tomato, na.rm = T)
 stPs$price_cook_oil_n <- (stPs$price_cook_oil - mean(stPs$price_cook_oil, na.rm = T))/sd(stPs$price_cook_oil, na.rm = T)
 stPs$price_cook_banana_n <- (stPs$price_cook_banana - mean(stPs$price_cook_banana, na.rm = T))/sd(stPs$price_cook_banana, na.rm = T)
 
 
 #### Reshape by trader
 
 # edPs_long <- gather(edPs, 
 #                     key = "trader",
 #                     value = "price",
 #                     c("w_std_price_tomato_1", 
 #                       "w_std_price_tomato_2"))
 # 
 # 
 # 
 # edPs_long$trader_market <- paste(edPs_long$market_uid, edPs_long$trader)
 # edPs_long$year_month <- factor(edPs_long$year_month, ordered = T)
 # 
 # 
 # stPs_long <- gather(stPs, 
 #                     key = "trader",
 #                     value = "price",
 #                     c("w_std_price_tomato_1", 
 #                       "w_std_price_tomato_2"))
 # stPs_long$trader_market <- paste(stPs_long$market_uid, stPs_long$trader)
 # stPs_long$year_month <- factor(stPs_long$year_month, ordered = T)
 
 
 #### SINGLE PRODUCT GRAPHs ####
 

 ggplot(data = edPs,
        aes(x = year_month,
            y = price_tomato_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(10)) +
   annotate(geom="text",
            x=11.5,
            y=2,
            label= paste("End of construction\n", end_date),
            size = 3) +
   theme_minimal() +
   labs(title="Normalized prices of tomato - 3 markets in Rwamagana",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_end_tomato.png"),
          device = "png")
 
 
 
 ggplot(data = stPs,
        aes(x = year_month,
            y = price_tomato_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(8)) +
   
   annotate(geom="text",
            x=9.5,
            y=3,
            label= paste("Start of construction\n", start_date),
            size = 3) +
   theme_minimal() +
   labs(title="Normalized prices of tomato - 4 markets in Karongi",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_start_tomato.png"),
          device = "png")
 
 
 ggplot(data = edPs,
        aes(x = year_month,
            y = price_cook_oil_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(10)) +
   annotate(geom="text",
            x=11.5,
            y=2,
            label= paste("End of construction\n", end_date),
            size = 3) +
   theme_minimal() +
   labs(title="Normalized prices of cooking oil - 3 markets in Rwamagana",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_end_cook_oil.png"),
          device = "png")
 
 
 
 ggplot(data = stPs,
        aes(x = year_month,
            y = price_cook_oil_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(8)) +
   
   annotate(geom="text",
            x=9.5,
            y=2,
            label= paste("Start of construction\n", start_date),
            size = 3) +
   theme_minimal() +
   labs(title="Normalized prices of cooking oil - 4 markets in Karongi",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_start_cook_oil.png"),
          device = "png")
 
 ggplot(data = edPs,
        aes(x = year_month,
            y = price_cook_banana_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(10)) +
   annotate(geom="text",
            x=11.5,
            y=2,
            label= paste("End of construction\n", end_date),
            size = 3) +
   theme_minimal() +
   labs(title="Normalized prices of bananas (cooking) - 3 markets in Rwamagana",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_end_cook_banana.png"),
          device = "png")
 
 
 
 ggplot(data = stPs,
        aes(x = year_month,
            y = price_cook_banana_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(8)) +
   
   annotate(geom="text",
            x=9.5,
            y=2,
            label= paste("Start of construction\n", start_date),
            size = 3) +
   theme_minimal() +
   labs(title="Normalized prices of bananas (cooking) - 4 markets in Karongi",
        x ="", 
        y = "RWFs ",
        color = "") +
   theme(axis.text.x = element_text(angle = 320, hjust=0, vjust = 1),
         legend.position="bottom") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   
   ggsave(file.path(OUT_graphs, "Jensen/event_is_start_cook_banana.png"),
          device = "png")
 
# Market: Mukungu 310802048
# District: Karongi
# Product: Tomato
# Construction start: a
 
 
 #-------------------------------------------------------------------------------#
 #### Event study dewsign
 
 #### Average price per market.
 
 ps_noc <- ps[ps$start_diff != 999,]
 
 
 ps_noc$price_tomato <- rowMeans(ps_noc[, c("w_std_price_tomato_1", "w_std_price_tomato_2")], na.rm = T)
 ps_noc$price_cook_oil <- rowMeans(ps_noc[, c("w_std_price_cook_oil_1", "w_std_price_cook_oil_2")], na.rm = T)
 ps_noc$price_cook_banana <- rowMeans(ps_noc[, c("w_std_price_cook_banana_1", "w_std_price_cook_banana_2")], na.rm = T)
 
 
 
 #### Normalize prices
 
 ps_noc$price_tomato_n <- (ps_noc$price_tomato - mean(ps_noc$price_tomato, na.rm = T))/sd(ps_noc$price_tomato, na.rm = T)
 ps_noc$price_cook_oil_n <- (ps_noc$price_cook_oil - mean(ps_noc$price_cook_oil, na.rm = T))/sd(ps_noc$price_cook_oil, na.rm = T)
 ps_noc$price_cook_banana_n <- (ps_noc$price_cook_banana - mean(ps_noc$price_cook_banana, na.rm = T))/sd(ps_noc$price_cook_banana, na.rm = T)
 
 
 
 #### Crazy grpahs #####
 
 ggplot(data = ps_noc,
       aes(x = start_diff,
           y = price_tomato_n,
           color = marketname_str,
           group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(0)) +
   annotate(geom="text",
            x=3,
            y=2.1,
            label= "Start of construction",
            size = 3) +
   theme_minimal() +
   theme(legend.position="none")+
   labs(title="Normalized prices of tomato",
        x ="Months from start of construction", 
        y = "RWFs ",
        color = "") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   ggsave(file.path(OUT_graphs, "Jensen/ES_start_tomato.png"),
          device = "png")
 
 
 ggplot(data = ps_noc,
        aes(x = start_diff,
            y = price_cook_oil_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(0)) +
   annotate(geom="text",
            x=3,
            y=2.1,
            label= "Start of construction",
            size = 3) +
   theme_minimal() +
   theme(legend.position="none")+
   labs(title="Normalized prices of cooking oil",
        x ="Months from start of construction", 
        y = "RWFs ",
        color = "") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   ggsave(file.path(OUT_graphs, "Jensen/ES_start_cook_oil.png"),
          device = "png")
 
 
 ggplot(data = ps_noc,
        aes(x = start_diff,
            y = price_cook_banana_n,
            color = marketname_str,
            group = market_uid)) + 
   geom_line() +
   geom_vline(xintercept = c(0)) +
   annotate(geom="text",
            x=3,
            y=2.1,
            label= "Start of construction",
            size = 3) +
   theme_minimal() +
   theme(legend.position="none")+
   labs(title="Normalized prices of banana (cooking)",
        x ="Months from start of construction", 
        y = "RWFs ",
        color = "") +
   scale_colour_grey(start = 0.2, end = 0.8) +
   ggsave(file.path(OUT_graphs, "Jensen/ES_start_cook_banana.png"),
          device = "png")
 
 
