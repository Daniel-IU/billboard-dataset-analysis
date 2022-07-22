#Analysis of tidyr::billboard data set

#Code to initially investigate dataset
str(billboard)
View(billboard)
glimpse(billboard)
skimr::skim_without_charts(billboard)
?billboard
summary(billboard)

#Basic dataset format changes 

#change dataset from wide to long format
billboard2 <- billboard %>% gather(wk1:wk76,key='Week',value ='Position') %>% drop_na(Position) %>% mutate(Year=year(date.entered))

#creating a joined artist and track column
billboard2 <- billboard2 %>% unite(artistTrack, artist, track,sep = ' - ',remove = FALSE)


#Approach 1 :using base::split() and purrr::map()

#split creation
billboard3Split <- billboard2 %>% split(.$artistTrack)

#function for map use 
addActualWeeks <- function(df){
  df %>% mutate(actualWeeks = seq.Date(ymd(date.entered[1]),by='weeks',length.out = nrow(df)))
}

#creating the additional column with actualweeks 
billboardmapped <- map(billboard3Split,addActualWeeks)

#unspliting/combining 
billboardcombined <- do.call(rbind,billboardmapped)

#removing rownames
has_rownames(billboardcombined) #TRUE
billboardcombined <- remove_rownames(billboardcombined)
View(billboardcombined)


#Approach 2 : using for..loop and dplyr::left_join()
#first approach i tried to getting actual weeks (alternative approach)

billboard3 <- billboard2 %>% group_by(artistTrack) %>% summarise(noOfWeeksOnChart=n(),dateEntered=unique(date.entered))
dff <- tibble()
for (i in seq(nrow(billboard3))){
  dff<- rbind(dff,tibble('artistTrack'= billboard3$artistTrack[i],'Week'= paste('wk',seq(billboard3$noOfWeeksOnChart[i]),sep=''),'actualWeeks'= seq.Date(from = billboard3$dateEntered[i],length.out = billboard3$noOfWeeksOnChart[i], by = 'weeks')))
}

billboardjoined <- left_join(dff, billboard2, by=c("artistTrack"='artistTrack','Week'='Week'))

View(billboardjoined)
View(dff)



#Approach 3 : using dplyr::group_split
#approach using group_split didn't work, don't know how to 'unsplit' afterwards

billboard2Split <- billboard2 %>% group_by(artist,track) %>% group_split
View(unlist(billboard2Split))
View(billboard2Split)

#experimenting with spliting and unspliting 
groups<-unique(billboard2$track) # --1/2 
split(billboard2,groups) # -- 2/2 gave a bad result

#unsplitting didn't work
billboard3Split <- billboard2 %>% split(.$artistTrack)
billboard3Split %>% unsplit(names(billboard3Split))
unsplit(billboard3Split,names(billboard3Split))

#unspliting didn't work 
billboard3Split %>% unsplit(billboard2$artistTrack)
billboard3Split[1]


#Code to investigate data (after partial wrangling/cleaning/analysis)
billboard %$% n_distinct(track)  
billboard %>% filter(duplicated(.$track))
View(billboard %>% filter(str_detect(track,'Wher')))

#details of the billboard chart period
chartSpan=billboard3 %$% range(dateEntered)
chartNumOfWeeks <- difftime(chartSpan[2],chartSpan[1],units='weeks')
chartFrom = chartSpan[1]
chartTo = chartSpan[2]



#Visualisations 

billboardgrpdplots <-billboard2 %>% group_by(artistTrack) %>% summarise(Highest_rating=min(Position),Median_rating = median(Position),Lowest_rating=max(Position),Average_rating=mean(Position),noOfWeeksOnChart=n(),dateEntered=unique(date.entered))


#Find Top 10 artists by billboard list feature 
billcounted <- billboard %>% group_by(artist) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head(10)

# Chart showing Top 10 artists featured on billboad top 100 in 1999/2000
ggplot(billcounted)+geom_bar(aes(x=count,y=reorder(artist,+count),fill=artist),stat ='identity')+
  theme_classic()+labs(title ='Top 10 artists featured on billboad top 100 in 1999/2000',subtitle = 'Number of tracks per artist in the billboard top 100',)+
  xlab('Number of tracks')+ylab('Artist')+theme(legend.position ='none')+theme(plot.title = element_text(hjust = '0.5',face='bold'))+
  geom_text(aes(x=count,y=reorder(artist,+count),label = count), data = billcounted,nudge_x = -0.2)

#Chart showing top 20 songs that lasted the longest on the chart
top20reign <- head(arrange(billboardgrpdplots,desc(noOfWeeksOnChart)),20)

ggplot(data=top20reign)+
  geom_bar(aes(x=noOfWeeksOnChart,y=reorder(artistTrack,noOfWeeksOnChart),fill=artistTrack),stat='identity',show.legend = FALSE)+
  scale_x_continuous(breaks = seq(70, by=-10)) + theme_minimal()+
  labs(title ='Top 20 longest reigning songs',subtitle = paste('Within the period of',chartFrom,'-',chartTo,' (',chartNumOfWeeks,')'))+
  xlab('Number of weeks on chart')+ ylab('Artist - Song') +theme(plot.title = element_text(hjust = '0.5',face='bold'))


#Songs that where #1 on the chart 
billboardcombinedNum1 <- billboardcombined %>% filter(Position==1) %>% group_by(artistTrack) %>% summarise(noOfWeeksAsNum1 = n())
nrow(billboardcombinedNum1) # no of songs that topped the chart (17)

ggplot(data=billboardcombinedNum1) +
  geom_bar(aes(x=noOfWeeksAsNum1,y=reorder(artistTrack,noOfWeeksAsNum1),fill=artistTrack),stat='identity',show.legend = FALSE)+
  labs(title ='Chart topping songs',subtitle = paste('Within the period of',chartFrom,'-',chartTo,' (',chartNumOfWeeks,')'))+
  xlab('Number of weeks as #1')+ylab('Artist - Song')+
  scale_x_continuous(breaks = seq(10, by=-1)) +theme_classic()+ +theme(plot.title = element_text(hjust = '0.5',face='bold'))

View(billboardgrpdplots)

# chart showing higher ranking (min) songs stay longer on chart
billboardgrpdplotsmeantime <- billboardgrpdplots %>% group_by(Highest_rating) %>% summarise(meanTimeOnChart = mean(noOfWeeksOnChart))

ggplot(data=billboardgrpdplotsmeantime) +
  geom_smooth(aes(x=Highest_rating,y=meanTimeOnChart))+
  geom_point(aes(x=Highest_rating,y=meanTimeOnChart),color='orange')+
  labs(title ='Higher ranking songs stay longer on chart')+
  xlab('Highest chart position')+ylab('Average number of weeks on chart') +theme_classic()+
  scale_x_continuous(breaks = seq(100, by=-10))+
  scale_y_continuous(breaks = seq(40, by=-5)) ++theme(plot.title = element_text(hjust = '0.5',face='bold'))

# chart showing higher ranking (avg) songs stay longer on chart
ggplot(data=billboardgrpdplots) +
  geom_smooth(aes(x=Average_rating,y=noOfWeeksOnChart))+
  geom_point(aes(x=Average_rating,y=noOfWeeksOnChart),color='red')+
  labs(title ='Higher ranking songs stay longer on chart')+
  xlab('Average rating')+ylab('Number of weeks on chart') +theme_classic()+
  scale_x_continuous(breaks = seq(100, by=-10))+
  scale_y_continuous(breaks = seq(80, by=-5)) +theme(plot.title = element_text(hjust = '0.5',face='bold'))+
  geom_abline(slope=-0.2895,intercept = 34.4046)


#chart showing distribution of average ratings 
ggplot(data=billboardgrpdplots) +geom_boxplot(aes(y=Average_rating),color='red',fill='orange')+
  scale_y_continuous(breaks = seq(100, by=-5))+ theme_classic()+
  ylab('Average rating') + scale_x_continuous(breaks = NULL)+
  labs(title ='Distribution of average ratings') +theme(plot.title = element_text(hjust = '0.5',face='bold'))+
  theme(aspect.ratio = 1.5:1.5)



#songs entry per month
billboardgrpdplotsmonthly <- billboardgrpdplots %>% mutate(yearMonth= my(paste(month(dateEntered,label = TRUE),year(dateEntered))))
billboardgrpdplotsmonthly2 <- billboardgrpdplotsmonthly %>% group_by(yearMonth) %>% summarise(noOfEntrants=n()) %>% mutate(year=year(yearMonth))

ggplot(data=billboardgrpdplotsmonthly2)+ geom_bar(aes(x=yearMonth,y=noOfEntrants,fill=as.character(year)),stat='identity')+
  scale_x_date(date_labels ='%b',date_breaks = '1 month' )+theme_classic()+
  labs(title=('Song entry into the billboard 100 by month'),fill='Year',x='Month',y='Number of songs')+
  theme(plot.title = element_text(hjust = '0.5',face='bold')) + geom_text(aes(x=yearMonth,y=noOfEntrants,label = noOfEntrants), data = billboardgrpdplotsmonthly2, nudge_y = -0.55)
  
ggplot(data=billboardgrpdplots)+ geom_point(aes(x=dateEntered,y=noOfWeeksOnChart))+
  scale_x_date(date_breaks = '1 month',date_labels = '%b')+
  geom_smooth(aes(x=dateEntered,y=noOfWeeksOnChart))


testForChange <- function(value) {
  if (value > 0) 'Higher exit'
  else if (value < 0) 'Lower exit'
  else 'Same'
}
testForChange <- Vectorize(testForChange)

testForOverlap <- function(col1,col2){
  if (col1 == col2) col1
  else NA 
}
testForOverlap <- Vectorize(testForOverlap)

# Charts leaving higher or lower 
billboardcombinedsummised <- billboardcombined %>% group_by(artistTrack) %>% summarise(entryWeek = min(actualWeeks),entryPosition = Position[which.min(actualWeeks)],exitWeek=max(actualWeeks),exitPosition = Position[which.max(actualWeeks)])
billboardcombinedsummised <- billboardcombinedsummised %>% mutate(positionDiff = entryPosition-exitPosition, hOrL = testForChange(positionDiff))
billboardgeomtext <- billboardcombinedsummised %>% group_by(hOrL) %>% summarise(Count=n(),averageChange = mean(positionDiff))

# Charts leaving higher or lower 
ggplot(billboardcombinedsummised) + geom_bar(aes(x=hOrL,fill=hOrL),show.legend=FALSE)+
  labs(title = str_wrap('More songs leave the chart at a lower position than their entry position',width = 40),x='',y='Count')+
  theme_classic()+theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(x=hOrL,y=Count,label=Count),data=billboardgeomtext,nudge_y = 12)

# Average change in chart entry and exit position 
ggplot(billboardgeomtext) + geom_bar(aes(x=hOrL,y=averageChange,fill=hOrL),stat='identity',show.legend = FALSE)+
  labs(title = str_wrap('Higher exiting charts move higher than lower exiting charts move lower',width = 40),x='',y='Average position change')+
  theme_classic()+theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(x=hOrL,y=averageChange,label=format(averageChange,digits=2)),data=billboardgeomtext,nudge_x = 0.52,check_overlap = TRUE)

# Top chart movers (entry vs exit position)
ggplot(head(arrange(billboardcombinedsummised,desc(positionDiff)),20)) + geom_bar(aes(y=reorder(artistTrack,positionDiff),x=positionDiff,fill=artistTrack),show.legend=FALSE,stat='identity')+
  labs(title = str_wrap('Top 20 chart movers (entry vs exit position)',width = 40),x='Change in position',y='Artist - Track')+
  theme_classic()+theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(y=reorder(artistTrack,positionDiff),x=positionDiff,label=positionDiff),data=head(arrange(billboardcombinedsummised,desc(positionDiff)),20),nudge_x = -2,check_overlap = TRUE)


# Bottom chart movers (entry vs exit)
ggplot(head(arrange(billboardcombinedsummised,positionDiff),20)) + geom_bar(aes(y=reorder(artistTrack,-positionDiff),x=positionDiff,fill=artistTrack),show.legend=FALSE,stat='identity')+
  labs(title = str_wrap('Bottom 20 chart movers (entry vs exit position)',width = 40),x='Change in position',y='Artist - Track')+
  theme_classic()+theme(plot.title = element_text(hjust=0.5))+
  geom_text(aes(y=reorder(artistTrack,-positionDiff),x=positionDiff,label=positionDiff),data=head(arrange(billboardcombinedsummised,positionDiff),20),nudge_x =4.5 ,check_overlap = TRUE)

  

# time spent on chart vs exit position 
billboardsummisedgrpd <- left_join(billboardcombinedsummised,billboardgrpdplots,by=c('artistTrack'='artistTrack'))

ggplot(billboardsummisedgrpd) + geom_point(aes(x=noOfWeeksOnChart,y=positionDiff,color=hOrL))+
  labs(title=str_wrap('Songs that stay longer on chart tend to exit at a higher position than entry',width=40),x='Number of weeks on chart',y='Change in position',color='Exit relative to entry')+
  theme_classic() +theme(plot.title = element_text(hjust=0.5))


# Highest Rating vs exit position 
ggplot(billboardsummisedgrpd) + geom_point(aes(x=Highest_rating,y=positionDiff,color=hOrL))+
  labs(title=str_wrap('Higher rated songs tend to exit at a higher position than entry',width=40),x='Highest rating attained',y='Change in position',color='Exit relative to entry')+
  theme_classic() +theme(plot.title = element_text(hjust=0.5))


# Average Rating vs exit position 
ggplot(billboardsummisedgrpd) + geom_point(aes(x=Average_rating,y=positionDiff,color=hOrL))+
  labs(title=str_wrap('Higher rated songs tend to exit at a higher position than entry',width=40),x='Average rating ',y='Change in position',color='Exit relative to entry')+
  theme_classic() +theme(plot.title = element_text(hjust=0.5))


# Various chart positions for the top 20 longest reigning charts
billboardsummisedgrpd <- billboardsummisedgrpd %>% mutate(entryAndLowest = testForOverlap(entryPosition,Lowest_rating),exitAndLowest=testForOverlap(exitPosition,Lowest_rating))
billboardsummisedgrpdplot <- billboardsummisedgrpd %>% select(artistTrack,noOfWeeksOnChart,entryPosition,exitPosition,Highest_rating,Lowest_rating,Average_rating,exitAndLowest,entryAndLowest)
billboardsummisedgrpdplot <- billboardsummisedgrpdplot %>% gather(entryPosition:entryAndLowest,key='indicator',value='numbers') %>% drop_na(numbers)

# Various chart positions for the top 20 longest reigning charts
ggplot(head(arrange(billboardsummisedgrpdplot,desc(noOfWeeksOnChart),artistTrack),117)) + 
  geom_line(aes(y=reorder(artistTrack,noOfWeeksOnChart),x=numbers))+
  geom_point(aes(y=artistTrack,x=numbers,color=indicator))+
  theme_minimal()+
  labs(title =str_wrap('Chart positions for the top 20 longest reigning songs',width=40),color='Chart Positions',x='Position',y='Artist - Track')+
  theme(plot.title = element_text(hjust=0.5))


# Various chart positions for the top 20 shortest reigning charts
ggplot(head(arrange(billboardsummisedgrpdplot,noOfWeeksOnChart,artistTrack),129)) + 
  geom_line(aes(y=reorder(artistTrack,desc(noOfWeeksOnChart)),x=numbers))+
  geom_point(aes(y=artistTrack,x=numbers,color=indicator))+
  theme_minimal()+
  labs(title =str_wrap('Chart positions for the top 20 shortest reigning songs',width=40),color='Chart Positions',x='Position',y='Artist - Track')+
  theme(plot.title = element_text(hjust=0.5))


#Histograms
ggplot(data=billboardgrpdplots) + geom_histogram(aes(x=Average_rating))
ggplot(data=billboard2) + geom_histogram(aes(x=Position))



#other code experimentation 
lm(noOfWeeksOnChart~Average_rating, data=billboardgrpdplots)
quantile(billboardgrpdplots$Average_rating)

seq.Date(ymd(20220101),by='week',length.out = 6)
billboard2 %>% chop(Position)
billboard2$Position[1:34]
month.name[7]
top_n()
View(dff)
billboard3 %$% mutate(actualWeeks=seq.Date(from=ymd(dateEntered),by='week',length.out = noOfWeeksOnChart))
billboard2 %$% unique(c(track,date.entered))
billboard99 <- billboard %>% mutate(Year=year(date.entered))
billboard99 %>% count(Year)
