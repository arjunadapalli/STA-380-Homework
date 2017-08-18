#### Importing Libraries

    library(tm)

    ## Loading required package: NLP

Problem 1: Flights at ABIA
==========================

#### For this problem, our group created visuals that presented the odds of departure delay based on various features.

##### Couple of things to note:

###### We included all departure delays (any flight that had a positive value under the 'DepDelay' column) in our plots

###### We calculated the "Odds of Delay" by dividing the number of delayed flights (those that met the condition specified above) of a given category by the total number of flights of that category.

### Relative Odds of Delay for various Airlines flying from Austin

![Relative Odds of Delay for various Airlines flying from Austin -
WordCloud](Wordcloud_Depdelay.jpg)

#### Below is the code for the above wordcloud. Creating it involved editing the dataset excel file directly, thus the code will not work without the edited excel file.

    # Code for above wordcloud. Creating it involved editing the dataset excel file directly, thus the code will not work without the edited excel file.
    #library(wordcloud)
    #library(SnowballC)
    #library(RColorBrewer)

    #airlineQ <- read.csv('ABIA.csv', stringsAsFactors = FALSE)

    #airlineQP <- airlineQ[airlineQ$DepDelay>0,]

    #airlineCorpus <- Corpus(VectorSource(airlineQP$UniqueCarrier))

    #dtm <- DocumentTermMatrix(airlineCorpus)

    #m <- as.matrix(dtm)

    #v <- sort(colSums(m),decreasing=TRUE)

    #head(v,16)

    #words <- names(v)

    #d <- data.frame(word=words, freq=v)

    #set.seed(123)

    #wordcloud(words = d$word, freq = d$freq, min.freq = 1,
    #          max.words=200, random.order=FALSE, rot.per=0.35, 
    #          colors=brewer.pal(4, "Dark2"))

    #airlineQQ <- airlineQ[airlineQ$ArrDelay>0,]

    #airlineCorpus2 <- Corpus(VectorSource(airlineQQ$UniqueCarrier))

    #dtm2 <- DocumentTermMatrix(airlineCorpus2)

    #m2 <- as.matrix(dtm2)

    #v2 <- sort(colSums(m2),decreasing=TRUE)

    #head(v2,16)

    #words <- names(v2)

    #d2 <- data.frame(word=words, freq=v2)

    #set.seed(123)

    #wordcloud(words = d2$word,scale=c(4,0.5), freq = d2$freq, min.freq = 1,
    #          max.words=20, random.order=FALSE, rot.per=0.35, 
    #          colors=brewer.pal(4, "Set1"))

### Odds of Delay for various flight destinations

    df<-read.csv("ABIA.csv")

    unique(df$Dest)

    ##  [1] AUS ORD PHX MEM DFW MSP IAH JFK MSY TUS MDW SFO SNA ONT SLC DEN ATL
    ## [18] LAX LAS SAN ABQ BWI MCI CVG DAL HOU CLE IAD RDU EWR ELP HRL MCO BOS
    ## [35] OKC TUL TPA SJC MAF STL LBB BNA JAX PHL ORF DSM SEA FLL LGB IND CLT
    ## [52] OAK DTW
    ## 53 Levels: ABQ ATL AUS BNA BOS BWI CLE CLT CVG DAL DEN DFW DSM DTW ... TUS

    ### Odds by Destination

    # Creating DataFrame of Destination
    odds_df<-data.frame(unique(df$Dest),0,0,0)
    colnames(odds_df)<-c("Destination","Number of Flights","Number of Delays","Odds of Delay")

    for (dest in odds_df$Destination){
      
      odds_df[odds_df$Destination==dest,"Number of Flights"]<-nrow(df[df$Dest==dest,])
      odds_df[odds_df$Destination==dest,"Number of Delays"]<-nrow(df[(df$Dest==dest) & (df$DepDelay>0),])
      odds_df$`Odds of Delay`<-odds_df$`Number of Delays`/odds_df$`Number of Flights`
    }
    ## Filter out destinations with few records:
    odds_df<-odds_df[odds_df[,"Number of Flights"]>20,]
    ## Order
    odds_df<-odds_df[order(odds_df$`Odds of Delay`,decreasing = FALSE),]

    rbpal<-colorRampPalette(c('green','red'))
    odds_df$Col<-rbpal(10)[as.numeric(cut(odds_df$`Odds of Delay`,breaks=10))]

    barplot(odds_df$`Odds of Delay`,names.arg = odds_df$Destination,las=2,ylim = c(0,1),col = odds_df$Col,xlab = "Destination",ylab = "Odds of Delays",main = "Odds of Delay by Destination")

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-3-1.png)

### Odds of Delay by Time of Year (in Months)

    # DataFrame of Time of Year
    odds_df_year<-data.frame(unique(df$Month),0,0,0)
    colnames(odds_df_year)<-c("Time of Year","Number of Flights","Number of Delays","Odds of Delay")

    for (month in odds_df_year$`Time of Year`){
      
      odds_df_year[odds_df_year$`Time of Year`==month,"Number of Flights"]<-nrow(df[df$Month==month,])
      odds_df_year[odds_df_year$`Time of Year`==month,"Number of Delays"]<-nrow(df[(df$Month==month) & (df$DepDelay>0),])
      odds_df_year$`Odds of Delay`<-odds_df_year$`Number of Delays`/odds_df_year$`Number of Flights`
    }


    rbpal<-colorRampPalette(c('green','red'))
    odds_df_year$Col<-rbpal(10)[as.numeric(cut(odds_df_year$`Odds of Delay`,breaks=10))]

    barplot(odds_df_year$`Odds of Delay`,names.arg = odds_df_year$`Time of Year`,las=2,ylim = c(0,1),col = odds_df_year$Col,xlab = "Time of Year",ylab = "Odds of Delays",main = "Odds of Delay by Month")

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    df$date<-paste(df$Month,"/",df$DayofMonth,"/",df$Year,sep="")


    # DataFrame of Destination, Destination Number of Flights, Destination Number of Delays, Odds of Delay
    odds_df_date<-data.frame(unique(df$date),0,0,0)
    colnames(odds_df_date)<-c("Time of Year","Number of Flights","Number of Delays","Odds of Delay")

    for(date in unique(df$date)){
      odds_df_date[odds_df_date$`Time of Year`==date,"Number of Flights"]<-nrow(df[df$date==date,])
      odds_df_date[odds_df_date$`Time of Year`==date,"Number of Delays"]<-nrow(df[(df$date==date) & (df$DepDelay>0),])
      odds_df_date$`Odds of Delay`<-odds_df_date$`Number of Delays`/odds_df_date$`Number of Flights`
    }

####### 

### Odds of Delay in specific holidays

    library(timeDate)
    library(chron)

    hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
               "USNewYearsDay","USThanksgivingDay")        
    myholidays  <- dates(as.character(holiday(2000:2013,hlist)),format="Y-M-D")
    holiday_odds<-odds_df_date[is.holiday(odds_df_date$`Time of Year`,myholidays),]

    barplot(holiday_odds$`Odds of Delay`,names.arg = hlist,las=1,ylim = c(0,1),col = holiday_odds$Col,xlab = "Time of Year",ylab = "Odds of Delays")

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-5-1.png)

Problem 2: Author Attribution
=============================

Problem 3: Practice with Association Rule Mining
================================================

##### ***Revisit the notes on association rule mining, and walk through the R example on music playlists: \[playlists.R\] and \[playlists.csv\]. Then use the data on grocery purchases in \[groceries.txt\] and find some interesting association rules for these shopping baskets. The data file is a list of baskets: one row per basket, with multiple items per row separated by commas -- you'll have to cobble together a few utilities for processing this into the format expected by the "arules" package. Pick your own thresholds for lift and confidence; just be clear what these thresholds are and how you picked them. Do your discovered item sets make sense? Present your discoveries in an interesting and concise way.***

    detach(package:tm, unload=TRUE)
    library(arules)

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

    library(arulesViz)

    ## Loading required package: grid

##### **First, we read in the groceries.txt file and save it in a format which the arules package can read and work on**

    groceries=read.transactions(file="groceries.txt",rm.duplicates=TRUE,format="basket",sep=',')

##### **Next, we plot an item frequency plot which basically tells us which items are the most frequent and these are the ones we would be focussing on, since they will hold quite a lot of interesting relationships with other items which occus less frequently**

    itemFrequencyPlot(groceries,topN=10,type="absolute")

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-8-1.png)

##### **Obtaining the different association rules by setting the support to 0.001 and the confidence to 0.85.**

    groceryrules <- apriori(groceries, parameter = list(support = 0.001, conf = 0.85))

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##        0.85    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##      10  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.01s].
    ## writing ... [199 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

    summary(groceryrules)

    ## set of 199 rules
    ## 
    ## rule length distribution (lhs + rhs):sizes
    ##  3  4  5  6 
    ## 14 98 80  7 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   3.000   4.000   4.000   4.402   5.000   6.000 
    ## 
    ## summary of quality measures:
    ##     support           confidence          lift       
    ##  Min.   :0.001017   Min.   :0.8500   Min.   : 3.327  
    ##  1st Qu.:0.001017   1st Qu.:0.8708   1st Qu.: 3.558  
    ##  Median :0.001220   Median :0.9091   Median : 3.634  
    ##  Mean   :0.001229   Mean   :0.9095   Mean   : 4.134  
    ##  3rd Qu.:0.001322   3rd Qu.:0.9231   3rd Qu.: 4.586  
    ##  Max.   :0.003152   Max.   :1.0000   Max.   :11.235  
    ## 
    ## mining info:
    ##       data ntransactions support confidence
    ##  groceries          9835   0.001       0.85

##### **We can see that 4and 5 itemgroups are the most common with our threshold**

##### **There are 199 association rules for our given threshold**

    plot(groceryrules)

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-10-1.png)

##### **The above plot shows us the relationship between the confidence, support and lift for our 199 itemgroups**

##### **Here are some of those associations**

    inspect(subset(groceryrules,subset=lift>5))

    ##      lhs                        rhs                    support confidence      lift
    ## [1]  {liquor,                                                                      
    ##       red/blush wine}        => {bottled beer}     0.001931876  0.9047619 11.235269
    ## [2]  {citrus fruit,                                                                
    ##       root vegetables,                                                             
    ##       soft cheese}           => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [3]  {pip fruit,                                                                   
    ##       sausage,                                                                     
    ##       sliced cheese}         => {yogurt}           0.001220132  0.8571429  6.144315
    ## [4]  {butter,                                                                      
    ##       cream cheese,                                                                
    ##       root vegetables}       => {yogurt}           0.001016777  0.9090909  6.516698
    ## [5]  {brown bread,                                                                 
    ##       pip fruit,                                                                   
    ##       whipped/sour cream}    => {other vegetables} 0.001118454  1.0000000  5.168156
    ## [6]  {other vegetables,                                                            
    ##       rice,                                                                        
    ##       whole milk,                                                                  
    ##       yogurt}                => {root vegetables}  0.001321810  0.8666667  7.951182
    ## [7]  {grapes,                                                                      
    ##       tropical fruit,                                                              
    ##       whole milk,                                                                  
    ##       yogurt}                => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [8]  {ham,                                                                         
    ##       pip fruit,                                                                   
    ##       tropical fruit,                                                              
    ##       yogurt}                => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [9]  {ham,                                                                         
    ##       pip fruit,                                                                   
    ##       tropical fruit,                                                              
    ##       whole milk}            => {other vegetables} 0.001118454  1.0000000  5.168156
    ## [10] {butter,                                                                      
    ##       sliced cheese,                                                               
    ##       tropical fruit,                                                              
    ##       whole milk}            => {yogurt}           0.001016777  0.9090909  6.516698
    ## [11] {oil,                                                                         
    ##       other vegetables,                                                            
    ##       tropical fruit,                                                              
    ##       whole milk}            => {root vegetables}  0.001321810  0.8666667  7.951182
    ## [12] {cream cheese,                                                                
    ##       curd,                                                                        
    ##       other vegetables,                                                            
    ##       whipped/sour cream}    => {yogurt}           0.001016777  0.9090909  6.516698
    ## [13] {butter,                                                                      
    ##       other vegetables,                                                            
    ##       tropical fruit,                                                              
    ##       white bread}           => {yogurt}           0.001016777  0.9090909  6.516698
    ## [14] {butter,                                                                      
    ##       curd,                                                                        
    ##       tropical fruit,                                                              
    ##       whole milk}            => {yogurt}           0.001220132  0.8571429  6.144315
    ## [15] {butter,                                                                      
    ##       fruit/vegetable juice,                                                       
    ##       tropical fruit,                                                              
    ##       whipped/sour cream}    => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [16] {newspapers,                                                                  
    ##       rolls/buns,                                                                  
    ##       soda,                                                                        
    ##       whole milk}            => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [17] {citrus fruit,                                                                
    ##       fruit/vegetable juice,                                                       
    ##       other vegetables,                                                            
    ##       soda}                  => {root vegetables}  0.001016777  0.9090909  8.340400
    ## [18] {citrus fruit,                                                                
    ##       root vegetables,                                                             
    ##       tropical fruit,                                                              
    ##       whipped/sour cream}    => {other vegetables} 0.001220132  1.0000000  5.168156
    ## [19] {oil,                                                                         
    ##       other vegetables,                                                            
    ##       tropical fruit,                                                              
    ##       whole milk,                                                                  
    ##       yogurt}                => {root vegetables}  0.001016777  0.9090909  8.340400

##### **We see that the rules are not sorted. We usually want the most relevant rules first. We can easily sort by confidence to get the most likely result.**

    groceryrules<-sort(groceryrules, by="confidence", decreasing=TRUE)

    inspect(subset(groceryrules,subset=lift>5))

    ##      lhs                        rhs                    support confidence      lift
    ## [1]  {citrus fruit,                                                                
    ##       root vegetables,                                                             
    ##       soft cheese}           => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [2]  {brown bread,                                                                 
    ##       pip fruit,                                                                   
    ##       whipped/sour cream}    => {other vegetables} 0.001118454  1.0000000  5.168156
    ## [3]  {grapes,                                                                      
    ##       tropical fruit,                                                              
    ##       whole milk,                                                                  
    ##       yogurt}                => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [4]  {ham,                                                                         
    ##       pip fruit,                                                                   
    ##       tropical fruit,                                                              
    ##       yogurt}                => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [5]  {ham,                                                                         
    ##       pip fruit,                                                                   
    ##       tropical fruit,                                                              
    ##       whole milk}            => {other vegetables} 0.001118454  1.0000000  5.168156
    ## [6]  {butter,                                                                      
    ##       fruit/vegetable juice,                                                       
    ##       tropical fruit,                                                              
    ##       whipped/sour cream}    => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [7]  {newspapers,                                                                  
    ##       rolls/buns,                                                                  
    ##       soda,                                                                        
    ##       whole milk}            => {other vegetables} 0.001016777  1.0000000  5.168156
    ## [8]  {citrus fruit,                                                                
    ##       root vegetables,                                                             
    ##       tropical fruit,                                                              
    ##       whipped/sour cream}    => {other vegetables} 0.001220132  1.0000000  5.168156
    ## [9]  {butter,                                                                      
    ##       cream cheese,                                                                
    ##       root vegetables}       => {yogurt}           0.001016777  0.9090909  6.516698
    ## [10] {butter,                                                                      
    ##       sliced cheese,                                                               
    ##       tropical fruit,                                                              
    ##       whole milk}            => {yogurt}           0.001016777  0.9090909  6.516698
    ## [11] {cream cheese,                                                                
    ##       curd,                                                                        
    ##       other vegetables,                                                            
    ##       whipped/sour cream}    => {yogurt}           0.001016777  0.9090909  6.516698
    ## [12] {butter,                                                                      
    ##       other vegetables,                                                            
    ##       tropical fruit,                                                              
    ##       white bread}           => {yogurt}           0.001016777  0.9090909  6.516698
    ## [13] {citrus fruit,                                                                
    ##       fruit/vegetable juice,                                                       
    ##       other vegetables,                                                            
    ##       soda}                  => {root vegetables}  0.001016777  0.9090909  8.340400
    ## [14] {oil,                                                                         
    ##       other vegetables,                                                            
    ##       tropical fruit,                                                              
    ##       whole milk,                                                                  
    ##       yogurt}                => {root vegetables}  0.001016777  0.9090909  8.340400
    ## [15] {liquor,                                                                      
    ##       red/blush wine}        => {bottled beer}     0.001931876  0.9047619 11.235269
    ## [16] {other vegetables,                                                            
    ##       rice,                                                                        
    ##       whole milk,                                                                  
    ##       yogurt}                => {root vegetables}  0.001321810  0.8666667  7.951182
    ## [17] {oil,                                                                         
    ##       other vegetables,                                                            
    ##       tropical fruit,                                                              
    ##       whole milk}            => {root vegetables}  0.001321810  0.8666667  7.951182
    ## [18] {pip fruit,                                                                   
    ##       sausage,                                                                     
    ##       sliced cheese}         => {yogurt}           0.001220132  0.8571429  6.144315
    ## [19] {butter,                                                                      
    ##       curd,                                                                        
    ##       tropical fruit,                                                              
    ##       whole milk}            => {yogurt}           0.001220132  0.8571429  6.144315

##### **It is seen that if you buy liquor and red/blush wine, there is an approximately 11 times higher chance that you would buy bottled beer. This does not come as a surprise and it also shows that you might have a drinking problem**

##### **It is also observed that when oil, other vegetables, tropical fruit, whole milk and yogurt are bought together, the customer has a very high chance of buying root vegetables, indicating that they are going to have a dinner party at home with proper mealsand desserts**

##### **It is also observed that when cream cheese, curd, other vegetables and whipped/sour cream are bought together, yogurt is bought too. This seems interesting that a person picking up so many dairy products doesn't seem to pick up whole milk. He is probably lactose intolerant, and can only have substitutes for milk, which is what the other dairy items might be made out of**

##### **Next we see a group plot of the Items in teh LHS and RHS of the rules**

    plot(groceryrules, method="grouped")

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-13-1.png)

##### **Here, the darker the color, higher is the lift and the bigger the circle is, the more support**

##### **Now that we know how to generate rules, limit the output, we can now target items to generate rules. What this means is we want to set either the Left Hand Side and Right Hand Side.**

    groceryrules<-apriori(data=groceries, parameter=list(support=0.001,confidence = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"),control = list(verbose=F))

    groceryrules<-sort(groceryrules, decreasing=TRUE,by="confidence")

    inspect(groceryrules)

    ##     lhs             rhs                support    confidence lift     
    ## [1] {whole milk} => {other vegetables} 0.07483477 0.2928770  1.5136341
    ## [2] {whole milk} => {rolls/buns}       0.05663447 0.2216474  1.2050318
    ## [3] {whole milk} => {yogurt}           0.05602440 0.2192598  1.5717351
    ## [4] {whole milk} => {root vegetables}  0.04890696 0.1914047  1.7560310
    ## [5] {whole milk} => {tropical fruit}   0.04229792 0.1655392  1.5775950
    ## [6] {whole milk} => {soda}             0.04006101 0.1567847  0.8991124

##### **We can now visualise what is "whole milk" most associated with. The bigger the Circle,the more the support.**

    plot(groceryrules,method="graph",control = list(nodeCol = grey.colors(1), edgeCol = grey(.3),alpha = 1),interactive=FALSE,shading=NA)

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-15-1.png)

##### **Alternatively, we can also visualise the relationships in the following way**

    plot(groceryrules,method="graph",control = list(nodeCol = grey.colors(1), edgeCol = grey(.3),alpha = 1,layout=igraph::in_circle()),interactive=FALSE,shading=NA)

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-16-1.png)

##### **A couple of observations we can make from these results and corresponding visualizations are:**

##### **If you buy whole milk, you are almost 1.5 times more likely (looking at the lift values) to buy other vegetables, yogurt, root vegetables and tropical fruits. Also, you are less likely to buy soda if you have bought whole milk. This shows that you are a health conscious person who likes to stay healthy and eat all types of foods so that you can get all types of nutrients.**

REMOVED R CODE
==============

    #Histogram for Question 1
    xlim<-range(df$DepDelay)
    hist(df$DepDelay, plot=TRUE, xlim = c(min(df$DepDelay,na.rm=T),max(df$DepDelay,na.rm=T)),breaks = 100)

![](STA_380_Homework_2_Adapalli_Fernandez_Malhotra_files/figure-markdown_strict/unnamed-chunk-17-1.png)
