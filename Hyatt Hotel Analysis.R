library(openintro)
library(ggplot2)
library(ggmap)
library(tm)
library(stringr)
library(wordcloud)
library(treemap)
library(Hmisc)
library(corrgram)
library(sqldf)
library(maps)
library(rworldmap)
library(data.table)
library(arules)
library(arulesViz)
#
#Reading the dataset
#
FebDataHyattHotel <- read.csv("out-
                              201402.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
MarchDataHyattHotel <- read.csv("out-
                                201403.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                                191,202:205,214:216,218,221,232)]
AprilDataHyattHotel <- read.csv("out-
                                201404.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                                191,202:205,214:216,218,221,232)]
MayDataHyattHotel <- read.csv("out-
                              201405.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
JuneDataHyattHotel <- read.csv("out-
                               201406.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                               191,202:205,214:216,218,221,232)]
JulyDataHyattHotel <- read.csv("out-
                               201407.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                               191,202:205,214:216,218,221,232)]
AugDataHyattHotel <- read.csv("out-
                              201408.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
SepDataHyattHotel <- read.csv("out-
                              201409.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
OctDataHyattHotel <- read.csv("out-
                              201410.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
NovDataHyattHotel <- read.csv("out-
                              201411.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
DecDataHyattHotel <- read.csv("out-
                              201412.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
JanDataHyattHotel <- read.csv("out-
                              201501.csv")[,c(19,23,55,56,70,137:145,147,167,171,172,175:177,179,187,
                                              191,202:205,214:216,218,221,232)]
#
#Combining all the data sets into a data set
#
YearlyDataSet <-
  rbind(JanDataHyattHotel,FebDataHyattHotel,MarchDataHyattHotel,AprilData
        HyattHotel,MayDataHyattHotel,JuneDataHyattHotel,JulyDataHyattHotel,AugD
        ataHyattHotel,SepDataHyattHotel,OctDataHyattHotel,NovDataHyattHotel,Dec
        DataHyattHotel)
#
#Removing NA's
#
YearlyDataSet <- na.omit(YearlyDataSet)
#
#Renaming row names
#
row.names(YearlyDataSet) <- NULL
#
#Renaming the columns
#
colnames(YearlyDataSet) <-
  c("LengthofCustomerStay","PurposeofCustomerVisit","StateofCustomer","Co
    untryofCustomer","FlagforOfferUsed","CustomerMetric_LikelihoodtoRecomme
    nd","CustomerMetric_OverallStatisfaction","CustomerMetric_GuestRoomStat
    isfaction","CustomerMetric_Tranquility","CustomerMetric_ConditionofHote
    l","CusotmerMetric_QualityofCustomerServivce","CustomerMetric_StaffCare
    ","CustomerMetric_InternetSatisfaction","CustomerMetric_CheckInProcessQ
    uality","CustomerMetric_F&BOverallExperience","CityofHotel","CountryofH
    otel","OperationRegionofHotel","LatitudeofHotel","LongitudeofHotel","Ho
    telLocalCurrency","Hotel'sNPSGoals","TotalMeetingSpaceinHotel","Regiono
    fHotel","HotelFlag_BusinessCenter","HotelFlag_Casino","HotelFlag_Nearby
    ConferenceCenter","HotelFlag_ConventionSpace","HotelFlag_MiniBar","Hote
    lFlag_IndoorPool","HotelFlag_OutdoorPool","HotelFlag_Resort","HotelFlag
    _ShuttleService","CustomerType_NPS")
#
#
#Converting CustomerType_NPS column to character vector
#
YearlyDataSet$CustomerType_NPS <-
  as.character(YearlyDataSet$CustomerType_NPS)
#
#Segregating the dataset into different datasets
#
AmenitiesDataSetYearly <- subset(YearlyDataSet,select = c(25:33))
MetricDataSetYearly <- subset(YearlyDataSet,select = c(19,20,6:15))
LocationDataSetYearly <- subset(YearlyDataSet,select =c(16:20))
UserInfoDataSetYearly <- subset(YearlyDataSet,select = c(19,20,1:5,34))
#
#Changing row names
#
row.names(AmenitiesDataSetYearly) <- NULL
row.names(MetricDataSetYearly) <- NULL
row.names(LocationDataSetYearly) <- NULL
row.names(UserInfoDataSetYearly) <- NULL
#
#Converting state abbreviations to full names using library openintro
#
UserInfoDataSetYearly$StateofCustomerFullName <-
  abbr2state(UserInfoDataSetYearly$StateofCustomer)
#
#In ameneties data set replacing all Y with 1 and N with 0
#
AmenitiesDataSetYearly <-
  as.data.frame(ifelse(AmenitiesDataSetYearly=="Y",1,0))
#
#Converting user state and country name to character vector to find out
geocodes
#
UserInfoDataSetYearly$StateofCustomerFullName <-
  as.character(UserInfoDataSetYearly$StateofCustomerFullName)
UserInfoDataSetYearly$CountryofCustomer <-
  as.character(UserInfoDataSetYearly$CountryofCustomer)
UserInfoDataSetYearly$StateofCustomer <-
  as.character(UserInfoDataSetYearly$StateofCustomer)
#
#Counting number of 1's in Amenities data set
#
AmenitiesDataSetYearly$Countof1 <- rowSums(AmenitiesDataSetYearly==1)
#
#Converting Customer State Full Name to charcater vector to convert it
into a word corpus
#
UserInfoDataSetYearly$StateofCustomer <-
  as.character(UserInfoDataSetYearly$StateofCustomer)
#
#Clearing blankspaces in Country ans State Names
#
UserInfoDataSetYearly$CountryofCustomer <-
  str_replace(UserInfoDataSetYearly$CountryofCustomer," ","")
UserInfoDataSetYearly$StateofCustomerFullName <-
  str_replace_all(UserInfoDataSetYearly$StateofCustomerFullName," ","")
#
#Adding hotel latitude and longitude to amenities data set
#
AmenitiesDataSetYearly$LatitudeofHotel <-
  LocationDataSetYearly$LatitudeofHotel
AmenitiesDataSetYearly$LongitudeofHotel <-
  LocationDataSetYearly$LongitudeofHotel
#
#Adding statefullname column to each data set
#
AmenitiesDataSetYearly$CountryofHotel <-
  LocationDataSetYearly$CountryofHotel
LocationDataSetYearly$StateofCustomerFullName <-
  UserInfoDataSetYearly$StateofCustomerFullName
MetricDataSetYearly$StateofCustomerFullName <-
  UserInfoDataSetYearly$StateofCustomerFullName
MetricDataSetYearly$CountryofHotel <-
  LocationDataSetYearly$CountryofHotel
#
#create a map-shaped window
#
mapDevice('x11')
#
#Creating a function to generate a frequency data frame
#
SpecifiedFreqMatrix <- function(x){
  VectorSource <- VectorSource(x)
  WordCorpus <- Corpus(VectorSource)
  TDM <- TermDocumentMatrix(WordCorpus)
  WordMatrix <- as.matrix(TDM)
  WordCount <- rowSums(WordMatrix)
  WordCount <- sort(WordCount,decreasing = TRUE)
  CloudFrame <- data.frame(Name=names(WordCount),freq=WordCount)
  return(CloudFrame)
}
#
#Creating a data frame to find out frequency of most frequent state of
booking
#
StateCloudFrame <-
  SpecifiedFreqMatrix(UserInfoDataSetYearly$StateofCustomerFullName)
#
#Creating a treemap to find out number of reservations made from each
state
#
UserStateTreeMap<- treemap(StateCloudFrame,index =
                             c("Name"),vSize="freq",type="index",palette = "Dark2",title = "State
                           wise Customer distribution",fontsize.title = 14,fontsize.labels =
                             12,border.col = "white")
#
#Creating a data frame to find out frequency of most frequent country
of booking
#
CountryCloudFrame <-
  SpecifiedFreqMatrix(UserInfoDataSetYearly$CountryofCustomer)
#
#Creating a treemap to find out number of reservations made from each
country
#
UserCountryTreeMap<- treemap(CountryCloudFrame,index =
                               c("Name"),vSize="freq",type="index",palette = "Dark2",title = "Country
                             wise Customer distribution",fontsize.title = 14,fontsize.labels =
                               12,border.col = "white")
#
#Converting CityofHotel, CountryofHotel and OperationRegionofHotel in
LocationDataSet to character vector
#
LocationDataSetYearly$CityofHotel <-
  as.character(LocationDataSetYearly$CityofHotel)
LocationDataSetYearly$CountryofHotel <-
  as.character(LocationDataSetYearly$CountryofHotel)
LocationDataSetYearly$OperationRegionofHotel <-
  as.character(LocationDataSetYearly$OperationRegionofHotel)
#
#Removing spaces from CityofHotel column in Location Data Set
#
LocationDataSetYearly$CityofHotel <-
  str_replace_all(LocationDataSetYearly$CityofHotel," ","")
LocationDataSetYearly$CountryofHotel <-
  str_replace_all(LocationDataSetYearly$CountryofHotel," ","")
#
#Creating a word cloud to find out most visted hotel citywise
#
HotelCityWordCloudFrame <-
  SpecifiedFreqMatrix(LocationDataSetYearly$CityofHotel)
HotelCityWordCloud <-
  wordcloud(HotelCityWordCloudFrame$Name,HotelCityWordCloudFrame$freq,rot
            .per = 0.35,colors = brewer.pal(7,"Dark2"),scale = c(2,0.25))
#
#Creating a word cloud to find out most visted hotel country wise
#
HotelCountryWordCloudFrame <-
  SpecifiedFreqMatrix(LocationDataSetYearly$CountryofHotel)
HotelCountryWordCloud <-
  wordcloud(HotelCountryWordCloudFrame$Name,HotelCountryWordCloudFrame$fr
            eq,colors = brewer.pal(3,"Dark2"),scale=c(2,1))
#
#Creating a word cloud to find out most visted hotel region wise
#
HotelRegionWordCloudFrame <-
  SpecifiedFreqMatrix(LocationDataSetYearly$OperationRegionofHotel)
HotelRegionWordCloud <-
  wordcloud(HotelRegionWordCloudFrame$Name,HotelRegionWordCloudFrame$freq
            ,colors = brewer.pal(7,"Accent"))
#
#Adding amenitites score in metric data
#
MetricDataSetYearly$AmenitiesScore <- AmenitiesDataSetYearly$Countof1
#
#subsetting metric data set to fnd out correlation
#
correlationMatrixYearly <-
  subset(MetricDataSetYearly,select=c(3:12,15))
correlationDataYearly<-cor(correlationMatrixYearly)
correlationPlotYearly<-corrgram(correlationDataYearly,upper.panel =
                                  panel.cor)
#
#Performing regression analysis to find out significant variables
#
MetricsLinearModelYearly<-
  lm(CustomerMetric_LikelihoodtoRecommend~.,data =
       correlationMatrixYearly)
summary(MetricsLinearModelYearly)
#
#Plotting regression model
#
plot(MetricsLinearModelYearly,col="blue",cex=2)
#
#Finding out NPS type countrywise
#
CountryWisePromoterDataYearly<- sqldf("select
                                      count(CustomerType_NPS),CountryofCustomer,CustomerType_NPS from
                                      UserInfoDataSetYearly where CustomerType_NPS = 'Promoter'group by
                                      CountryofCustomer order by count(CustomerType_NPS) DESC")
#
#Creating subset to calculate NPS type state wise after selecting
United States as a state with maximum NPS type
#
StatePromoterSubsetYearly <-
  subset(UserInfoDataSetYearly,CountryofCustomer=="UNITEDSTATES",select =
           c(8:9))
#
#Grouping with respect to NPS type in United States
#
UnitedStatesPromoterCountYearly <- sqldf("select
                                         count(CustomerType_NPS) PromoterCount, StateofCustomerFullName from
                                         StatePromoterSubsetYearly where CustomerType_NPS= 'Promoter' group by
                                         StateofCustomerFullName order by count(CustomerType_NPS) DESC")
UnitedStatesPassiveCountYearly <- sqldf("select count(CustomerType_NPS)
                                        PassiveCount, StateofCustomerFullName from StatePromoterSubsetYearly
                                        where CustomerType_NPS= 'Passive' group by StateofCustomerFullName
                                        order by count(CustomerType_NPS) DESC")
UnitedStatesDetractorCountYearly <- sqldf("select
                                          count(CustomerType_NPS) DetractorCount, StateofCustomerFullName from
                                          StatePromoterSubsetYearly where CustomerType_NPS= 'Detractor' group by
                                          StateofCustomerFullName order by count(CustomerType_NPS) DESC")
#
#Merging Data Frames
#
UnitedStatesNPSTypeYearly <-
  merge(UnitedStatesPromoterCountYearly,UnitedStatesPassiveCountYearly,by
        ="StateofCustomerFullName")
UnitedStatesNPSTypeYearly <-
  merge(UnitedStatesNPSTypeYearly,UnitedStatesDetractorCountYearly,by="St
        ateofCustomerFullName")
UnitedStatesNPSTypeYearly <- na.omit(UnitedStatesNPSTypeYearly)
TotalCountYearly <-
  aggregate(UnitedStatesNPSTypeYearly$PromoterCount+UnitedStatesNPSTypeYe
            arly$PassiveCount+UnitedStatesNPSTypeYearly$DetractorCount,FUN=mean,dat
            a=UnitedStatesNPSTypeYearly,by=list(UnitedStatesNPSTypeYearly$StateofCu
                                                stomerFullName))
UnitedStatesNPSTypeYearly$TotalCount <- TotalCountYearly$x
#
#Plotting UnitedStates Promoter Type
#
PromoterUnitedStatesPlotYearly <-
  ggplot(UnitedStatesNPSTypeYearly,aes(x=StateofCustomerFullName,y=Promot
                                       erCount))+geom_bar(stat =
                                                            "identity",col="white",fill="yellow")+theme(axis.text.x =
                                                                                                          element_text(angle=90,hjust=1))
PromoterUnitedStatesPlotYearly
#
#Plotting UnitedStates Detractor Type
#
DetractorUnitedStatesPlotYearly <-
  ggplot(UnitedStatesNPSTypeYearly,aes(x=StateofCustomerFullName,y=Detrac
                                       torCount),fill="red")+geom_bar(stat =
                                                                        "identity",col="white",fill="blue")+theme(axis.text.x =
                                                                                                                    element_text(angle=90,hjust=1))
DetractorUnitedStatesPlotYearly
#
#Removing NA
#
MetricDataSetYearly <- na.omit(MetricDataSetYearly)
OverallSatisfactionDataSetYearly<-sqldf("select
                                        avg(CustomerMetric_OverallStatisfaction)
                                        AverageOverallSatisfaction,StateofCustomerFullName from
                                        MetricDataSetYearly group by StateofCustomerFullName")
#
#PLotting Overall Customer Experience vs State
#
OverallCustomerExperienceUnitedStatesPlotYearly <-
  ggplot(OverallSatisfactionDataSetYearly,aes(x=StateofCustomerFullName,y
                                              =AverageOverallSatisfaction))+geom_bar(stat="identity",col="grey",fill=
                                                                                       "red")+theme(axis.text.x = element_text(angle = 90,hjust = 1))
OverallCustomerExperienceUnitedStatesPlotYearly
#
#Plotting Likelihood to Recommend vs State
#
LikelihoodtoRecommendDataSetYearly<-sqldf("select
                                          avg(CustomerMetric_LikelihoodtoRecommend)
                                          AverageLikelihoodtoRecommend,StateofCustomerFullName region from
                                          MetricDataSetYearly group by StateofCustomerFullName")
#
#Loading US Map
#
USMap <- map_data("state")
#
#Finding latitude and longitude to plot a heat map
#
LikelihoodtoRecommendGeoData <-
  geocode(LikelihoodtoRecommendDataSetYearly$region)
LikelihoodtoRecommendDataSetYearly$Latitude <-
  LikelihoodtoRecommendGeoData$lat
LikelihoodtoRecommendDataSetYearly$Longitude <-
  LikelihoodtoRecommendGeoData$lon
LikelihoodtoRecommendDataSetYearly$region <-
  tolower(LikelihoodtoRecommendDataSetYearly$region)
#
#plotting heatmap using ggplot
#
LikelihoodtoRecommendHeatMapYearly <-
  ggplot(LikelihoodtoRecommendDataSetYearly,aes(map_id=region))+geom_map(
    map=USMap,aes(fill=AverageLikelihoodtoRecommend),col="white")+expand_li
mits(x=USMap$long,y=USMap$lat)+coord_map()+ggtitle("State wise
                                                   Likelihood of Recommendation")
LikelihoodtoRecommendHeatMapYearly
#
#Plotting world map with hotel locations
#
mapWorldHotelLocationsYearly <- borders("world", colour="gray50",
                                        fill="gray50")
HotelLocationPlotYearly <- ggplot() + mapWorldHotelLocationsYearly
HotelLocationPlotYearly <- HotelLocationPlotYearly+
  geom_point(aes(x=LocationDataSetYearly$LongitudeofHotel,
                 y=LocationDataSetYearly$LatitudeofHotel) ,color="blue", size=1)
HotelLocationPlotYearly
#
#Code to Compute the NPS of each Country
#
UserInfoDataSetYearly$CountryofHotel <-
  LocationDataSetYearly$CountryofHotel
NPS_Dummy <- sqldf('select count(*) as No_of_promoters ,CountryofHotel
                   from UserInfoDataSetYearly where CustomerType_NPS = "Promoter" Group By
                   CountryofHotel')
str(NPS_Dummy)
NPS_Dummy1 <- sqldf('select count(*) as No_of_detractors
                    ,CountryofHotel from UserInfoDataSetYearly where CustomerType_NPS =
                    "Detractor" Group By CountryofHotel')
NPS_Dummy2 <- sqldf('Select
                    No_of_promoters,No_of_detractors,a.CountryofHotel from NPS_Dummy a ,
                    NPS_Dummy1 b where a.CountryofHotel = b.CountryofHotel')
sqldf('insert into NPS_Dummy2 select No_of_promoters, 0,
      CountryofHotel from NPS_Dummy a where not exists (select * from
      NPS_Dummy1 b where a.CountryofHotel = b.CountryofHotel)')
NPS_Dummy3 <- sqldf('select a.No_of_promoters, b.No_of_detractors,
                    a.CountryofHotel from NPS_Dummy a LEFT JOIN NPS_Dummy1 b ON
                    a.CountryofHotel = b.CountryofHotel')
NPS_Dummy3
NPS_Dummy4 <- sqldf('select count(*) as No_of_Passive ,CountryofHotel
                    from UserInfoDataSetYearly where CustomerType_NPS = "Passive" Group By
                    CountryofHotel')
NPS_Dummy4
NPS_Dummy5 <- sqldf('select a.No_of_promoters, a.No_of_detractors,
                    a.CountryofHotel,b.No_of_Passive from NPS_Dummy3 a LEFT JOIN NPS_Dummy4
                    b ON a.CountryofHotel = b.CountryofHotel')
NPS_Dummy5
NPS_Dummy5$No_of_promoters[is.na(NPS_Dummy5$No_of_promoters)] <- 0
NPS_Dummy5$No_of_detractors[is.na(NPS_Dummy5$No_of_detractors)] <- 0
NPS_Dummy5$No_of_Passive[is.na(NPS_Dummy5$No_of_Passive)] <- 0
sqldf('select * from NPS_Dummy1 a where not exists (select * from
      NPS_Dummy b where a.CountryofHotel = b.CountryofHotel)')
NPS_Dummy5$Total_Voters <- NPS_Dummy5$No_of_promoters +
  NPS_Dummy5$No_of_detractors + NPS_Dummy5$No_of_Passive
NPS_Dummy5$Promoter_Perc <- NPS_Dummy5$No_of_promoters/
  NPS_Dummy5$Total_Voters * 100
NPS_Dummy5$Detractor_Perc <- NPS_Dummy5$No_of_detractors/
  NPS_Dummy5$Total_Voters * 100
NPS_Dummy5$NPS <- NPS_Dummy5$Promoter_Perc - NPS_Dummy5$Detractor_Perc
NPS_Dummy5
sqldf('select * from NPS_Dummy5 order by Total_Voters ')
#
#Scatter Plot of NPS for each country
#
ggplot(NPS_Dummy5, aes(x= CountryofHotel, y = NPS)) + geom_point() +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))
#
#Bar Plot of NPS for each Country
#
ggplot(NPS_Dummy5,aes(x = CountryofHotel, y = NPS )) + geom_bar(stat =
                                                                  "identity",col="white",fill="orange") + theme(axis.text.x =
                                                                                                                  element_text(angle = 90,hjust = 1))
min(NPS_Dummy5$NPS)
sqldf('select min(NPS) from NPS_Dummy5 ')
#
#
#join to a coarse resolution map
#
NPS_Dummy5$CountryofHotel<- gsub("([a-z])([A-Z])", "\\1 \\2",
                                 NPS_Dummy5$CountryofHotel)
spdf <- joinCountryData2Map(NPS_Dummy5, joinCode="NAME",
                            nameJoinColumn="CountryofHotel")
mapCountryData(spdf, nameColumnToPlot="NPS", catMethod="fixedWidth")
#
#PLotting a scatter of likelihood vs overall satisaction for different
NPS type
#
ScatterLikelihoodOverallSatData <- subset(YearlyDataSet,select
                                          =c(6,7,34))
GGScatterPLot <-
  ggplot(data=ScatterLikelihoodOverallSatData,aes(x=CustomerMetric_Overal
                                                  lStatisfaction,y=CustomerMetric_LikelihoodtoRecommend))
GGScatterPLot<-
  GGScatterPLot+geom_point(aes(colour=CustomerType_NPS),shape=19,alpha=0.
                           1,position = position_jitter(w=0.3,h=0.3))
GGScatterPLot
#
#PLotting a scatter of likelihood vs amenities for different NPS type
#
YearlyDataSet$AmenitiesScore <- AmenitiesDataSetYearly$Countof1
GGScatterPLotAme <-
  ggplot(data=YearlyDataSet,aes(x=CustomerMetric_StaffCare,y=CusotmerMetr
                                ic_QualityofCustomerServivce))
GGScatterPLotAme<-
  GGScatterPLotAme+geom_point(aes(colour=CustomerType_NPS),shape=19,alpha
                              =0.1,position = position_jitter(w=0.3,h=0.3))
GGScatterPLotAme
#
#Performing Data Mining to find out association rules on metrics
#
MiningDataSet <- subset(MetricDataSetYearly, select=c(3:12,15))
MiningDataSet$CustomerMetric_LikelihoodtoRecommend <-
  as.factor(MiningDataSet$CustomerMetric_LikelihoodtoRecommend)
MiningDataSet$CustomerMetric_OverallStatisfaction <-
  as.factor(MiningDataSet$CustomerMetric_OverallStatisfaction)
MiningDataSet$CustomerMetric_GuestRoomStatisfaction <-
  as.factor(MiningDataSet$CustomerMetric_GuestRoomStatisfaction)
MiningDataSet$CustomerMetric_Tranquility <-
  as.factor(MiningDataSet$CustomerMetric_Tranquility)
MiningDataSet$CustomerMetric_ConditionofHotel <-
  as.factor(MiningDataSet$CustomerMetric_ConditionofHotel)
MiningDataSet$CusotmerMetric_QualityofCustomerServivce <-
  as.factor(MiningDataSet$CusotmerMetric_QualityofCustomerServivce)
MiningDataSet$CustomerMetric_StaffCare <-
  as.factor(MiningDataSet$CustomerMetric_StaffCare)
MiningDataSet$CustomerMetric_InternetSatisfaction <-
  as.factor(MiningDataSet$CustomerMetric_InternetSatisfaction)
MiningDataSet$CustomerMetric_CheckInProcessQuality <-
  as.factor(MiningDataSet$CustomerMetric_CheckInProcessQuality)
MiningDataSet$`CustomerMetric_F&BOverallExperience` <-
  as.factor(MiningDataSet$`CustomerMetric_F&BOverallExperience`)
MiningDataSet$AmenitiesScore <- as.factor(MiningDataSet$AmenitiesScore)
MiningRuleSet <- apriori(MiningDataSet,parameter =
                           list(support=0.005,confidence=0.5,maxlen=11))
inspect(MiningRuleSet)
MinigGoodRules <- MiningRuleSet[quality(MiningRuleSet)$lift >10.9]
MinigGoodRulesGraph<-
  plot(MinigGoodRules,method="graph",measure="support",shading="lift",int
       eractive=TRUE)
inspect(MinigGoodRules)
#
#Analyzing California Data Set
#
CalDataSet <- sqldf("select count(PurposeofCustomerVisit) Total_Count,
                    PurposeofCustomerVisit from YearlyDataSet where
                    StateofCustomer='CA'group by PurposeofCustomerVisit")
ggg <- ggplot(CalDataSet, aes(x =
                                PurposeofCustomerVisit))+geom_bar(aes(y=Total_Count),stat="identity",fi
                                                                  ll="pink")
ggg <- ggg+ggtitle("Purpose of Visit for California")
ggg