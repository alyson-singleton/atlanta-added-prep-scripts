## Pre-Processing: Load required packages for data processing, analysis, and visualization
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)
library(grid)
library(lattice)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

## Pre-Processing: Import basic reports for each scenario into lists for data processing
setwd("~/Desktop/research/atlanta.prevention.packages/titan.output/A3/main.analysis/Aggregate_Basic_Report_Black_A3/")
FILE_LIST<-list.files(pattern = "*.txt")
BLACK_BASIC_REPORTS<-list()
for (i in 1:length(FILE_LIST)){BLACK_BASIC_REPORTS[[i]]<-read.table(FILE_LIST[i], header = TRUE)}
names(BLACK_BASIC_REPORTS)<-FILE_LIST
BLACK_REPORTS<-rbindlist(BLACK_BASIC_REPORTS, idcol = "origin")
rm(BLACK_BASIC_REPORTS)

setwd("~/Desktop/research/atlanta.prevention.packages/titan.output/A3/main.analysis/Aggregate_Basic_Report_White_A3/")
WHITE_BASIC_REPORTS<-list()
for (i in 1:length(FILE_LIST)){WHITE_BASIC_REPORTS[[i]]<-read.table(FILE_LIST[i], header = TRUE)}
names(WHITE_BASIC_REPORTS)<-FILE_LIST
WHITE_REPORTS<-rbindlist(WHITE_BASIC_REPORTS, idcol = "origin")
rm(WHITE_BASIC_REPORTS)
rm(FILE_LIST)
rm(i)

## Pre-Processing: Create fields with explicit PrEP and ART levels for subsequent grouping
BLACK_REPORTS$PREP_COVERAGE<-substr(BLACK_REPORTS$origin, 1, 4)
BLACK_REPORTS$ART_COVERAGE<-substr(BLACK_REPORTS$origin, 10, 12)

WHITE_REPORTS$PREP_COVERAGE<-substr(WHITE_REPORTS$origin, 1, 4)
WHITE_REPORTS$ART_COVERAGE<-substr(WHITE_REPORTS$origin, 10, 12)

## Data Analysis: Create key outcome measures for each scenario and run, stratified by race/ethnicity
BLACK_INCIDENCE<-BLACK_REPORTS %>%
  group_by(PREP_COVERAGE, ART_COVERAGE, rseed, add = TRUE) %>%
  summarise(#BLACK_PNR = mean(BLACK_PNR),
            #WHITE_PNR = mean(WHITE_PNR), 
            BLACK_INFECTIONS = sum(Incid), 
            BLACK_NIA = 1738.0400-sum(Incid), #mean number of infections @ baseline art and zero prep (1863.45)
            BLACK_PREP_YEARS = sum(PrEP)/12,
            BLACK_NNT = (sum(PrEP)/12)/BLACK_NIA,
            BLACK_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            BLACK_PERSON_TIME = (sum(Total-HIV)/12),
            BLACK_PREVALENT_INF_T0 = ((HIV[1]+AIDS[1])),#/Total[1]),
            BLACK_PREVALENT_INF_T120 = ((HIV[n()]+AIDS[n()])),#/Total[n()]),
            BLACK_PREVALENT_INF_CHANGE = ((HIV[n()]+AIDS[n()]) - ((HIV[1]+AIDS[1]))),
            BLACK_PREVALENCE_CHANGE = (((HIV[n()]+AIDS[n()])/Total[n()]) - ((HIV[1]+AIDS[1])/Total[1])),
            BLACK_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-5.9462920)/5.9462920), #mean incidence rate at baseline art and zero prep (6.103133)
            BLACK_RATE_CHANGE_ABSOLUTE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-5.9462920))) #mean incidence rate at baseline art and zero prep (6.103133)
BLACK_INCIDENCE$RUN_ID<-1:nrow(BLACK_INCIDENCE)

WHITE_INCIDENCE<-WHITE_REPORTS %>%
  group_by(PREP_COVERAGE, ART_COVERAGE, rseed, add = TRUE) %>%
  summarise(WHITE_INFECTIONS = sum(Incid), 
            WHITE_NIA = 1456.8700-sum(Incid), #1390.94
            WHITE_PREP_YEARS = sum(PrEP)/12,
            WHITE_NNT = (sum(PrEP)/12)/WHITE_NIA,
            WHITE_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            WHITE_PERSON_TIME = (sum(Total-HIV)/12),
            WHITE_PREVALENT_INF_T0 = ((HIV[1]+AIDS[1])),#/Total[1]),
            WHITE_PREVALENT_INF_T120 = ((HIV[n()]+AIDS[n()])),#/Total[n()]),
            WHITE_PREVALENT_INF_CHANGE = (HIV[n()] - HIV[1]),
            WHITE_PREVALENCE_CHANGE = ((HIV[n()]/Total[n()]) - (HIV[1]/Total[1])),
            WHITE_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-1.7069786)/1.7069786),#1.599996
            WHITE_RATE_CHANGE_ABSOLUTE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-1.7069786)))#1.599996
WHITE_INCIDENCE$RUN_ID<-1:nrow(WHITE_INCIDENCE)

## Data Analysis: Create key outcome measures for each scenario, stratified by race/ethnicity
SUMMARY_BY_RUN_MAIN<-merge(BLACK_INCIDENCE, WHITE_INCIDENCE, by = "RUN_ID")
SUMMARY_BY_RUN_MAIN$ART_COVERAGE.x <- factor(SUMMARY_BY_RUN_MAIN$ART_COVERAGE.x , levels=c("Bas", "90W", "95W", "90A", "95A","100"))
#SUMMARY_BY_RUN_MAIN<-SUMMARY_BY_RUN_MAIN[!(SUMMARY_BY_RUN_MAIN$PREP_COVERAGE=="0.00"),]

#rm(BLACK_REPORTS)
#rm(WHITE_REPORTS)
#rm(BLACK_INCIDENCE)
#rm(WHITE_INCIDENCE)

SUMMARY_BY_RUN_MAIN$RATE_RATIO<-SUMMARY_BY_RUN_MAIN$BLACK_RATE/SUMMARY_BY_RUN_MAIN$WHITE_RATE
SUMMARY_BY_RUN_MAIN$RATE_DIFFERENCE<-SUMMARY_BY_RUN_MAIN$BLACK_RATE-SUMMARY_BY_RUN_MAIN$WHITE_RATE
SUMMARY_BY_RUN_MAIN$PREVALENCE_T0<-(SUMMARY_BY_RUN_MAIN$BLACK_PREVALENT_INF_T0 + SUMMARY_BY_RUN_MAIN$WHITE_PREVALENT_INF_T0)/17440
SUMMARY_BY_RUN_MAIN$PREVALENCE_T120<-(SUMMARY_BY_RUN_MAIN$BLACK_PREVALENT_INF_T120 + SUMMARY_BY_RUN_MAIN$WHITE_PREVALENT_INF_T120)/17440
SUMMARY_BY_RUN_MAIN$INCIDENCE_RATE<-((SUMMARY_BY_RUN_MAIN$WHITE_INFECTIONS+SUMMARY_BY_RUN_MAIN$BLACK_INFECTIONS)/(SUMMARY_BY_RUN_MAIN$WHITE_PERSON_TIME+SUMMARY_BY_RUN_MAIN$BLACK_PERSON_TIME)*100)
SUMMARY_BY_RUN_MAIN$PREVALENCE_CHANGE<-(SUMMARY_BY_RUN_MAIN$BLACK_PREVALENT_INF_CHANGE+SUMMARY_BY_RUN_MAIN$WHITE_PREVALENT_INF_CHANGE)/17440
SUMMARY_BY_RUN_MAIN$RATE_RATIO_CHANGE<-((SUMMARY_BY_RUN_MAIN$RATE_RATIO-3.487303)/3.487303) #3.818515
SUMMARY_BY_RUN_MAIN$RATE_DIFFERENCE_CHANGE<-((SUMMARY_BY_RUN_MAIN$RATE_DIFFERENCE-4.2393134)/4.2393134) #4.503137
SUMMARY_BY_RUN_MAIN$RATE_CHANGE<-((SUMMARY_BY_RUN_MAIN$INCIDENCE_RATE-2.7882249)/2.7882249)
  
MAIN_RESULTS_SUMMARY<-SUMMARY_BY_RUN_MAIN %>%
  group_by(PREP_COVERAGE.x, ART_COVERAGE.x) %>%
  summarise(BLACK_INFECTIONS_MEAN = mean(BLACK_INFECTIONS), BLACK_INFECTIONS_LL = quantile(BLACK_INFECTIONS, probs = 0.025), BLACK_INFECTIONS_UL = quantile(BLACK_INFECTIONS, probs = 0.975),
            BLACK_NIA_MEAN = mean(BLACK_NIA), BLACK_NIA_LL = quantile(BLACK_NIA, probs = 0.025), BLACK_NIA_UL = quantile(BLACK_NIA, probs = 0.975),
            BLACK_NNT_MEAN = mean(BLACK_NNT), BLACK_NNT_LL = quantile(BLACK_NNT, probs = 0.025), BLACK_NNT_UL = quantile(BLACK_NNT, probs = 0.975),
            BLACK_RATE_MEAN = mean(BLACK_RATE), BLACK_RATE_LL = quantile(BLACK_RATE, probs = 0.025), BLACK_RATE_UL = quantile(BLACK_RATE, probs = 0.975),
            BLACK_PREV_T0_MEAN = mean(BLACK_PREVALENT_INF_T0), BLACK_PREV_T0_LL = quantile(BLACK_PREVALENT_INF_T0, probs = 0.025), BLACK_PREV_T0_UL = quantile(BLACK_PREVALENT_INF_T0, probs = 0.975),
            BLACK_PREV_T120_MEAN = mean(BLACK_PREVALENT_INF_T120), BLACK_PREV_T120_LL = quantile(BLACK_PREVALENT_INF_T120, probs = 0.025), BLACK_PREV_T120_UL = quantile(BLACK_PREVALENT_INF_T120, probs = 0.975),
            BLACK_RATE_CHANGE_MEAN = mean(BLACK_RATE_CHANGE), BLACK_RATE_CHANGE_LL = quantile(BLACK_RATE_CHANGE, probs = 0.025), BLACK_RATE_CHANGE_UL = quantile(BLACK_RATE_CHANGE, probs = 0.975),
            BLACK_PREVALENCE_CHANGE_MEAN = mean(BLACK_PREVALENCE_CHANGE), BLACK_PREVALENCE_CHANGE_LL = quantile(BLACK_PREVALENCE_CHANGE, probs = 0.025), BLACK_PREVALENCE_CHANGE_UL = quantile(BLACK_PREVALENCE_CHANGE, probs = 0.975),
            WHITE_INFECTIONS_MEAN = mean(WHITE_INFECTIONS), WHITE_INFECTIONS_LL = quantile(WHITE_INFECTIONS, probs = 0.025), WHITE_INFECTIONS_UL = quantile(WHITE_INFECTIONS, probs = 0.975),
            WHITE_NIA_MEAN = mean(WHITE_NIA), WHITE_NIA_LL = quantile(WHITE_NIA, probs = 0.025), WHITE_NIA_UL = quantile(WHITE_NIA, probs = 0.975),
            WHITE_NNT_MEAN = mean(WHITE_NNT), WHITE_NNT_LL = quantile(WHITE_NNT, probs = 0.025), WHITE_NNT_UL = quantile(WHITE_NNT, probs = 0.975),
            WHITE_RATE_MEAN = mean(WHITE_RATE), WHITE_RATE_LL = quantile(WHITE_RATE, probs = 0.025), WHITE_RATE_UL = quantile(WHITE_RATE, probs = 0.975),
            WHITE_PREV_T0_MEAN = mean(WHITE_PREVALENT_INF_T0), WHITE_PREV_T0_LL = quantile(WHITE_PREVALENT_INF_T0, probs = 0.025), WHITE_PREV_T0_UL = quantile(WHITE_PREVALENT_INF_T0, probs = 0.975),
            WHITE_PREV_T120_MEAN = mean(WHITE_PREVALENT_INF_T120), WHITE_PREV_T120_LL = quantile(WHITE_PREVALENT_INF_T120, probs = 0.025), WHITE_PREV_T120_UL = quantile(WHITE_PREVALENT_INF_T120, probs = 0.975),
            WHITE_RATE_CHANGE_MEAN = mean(WHITE_RATE_CHANGE), WHITE_RATE_CHANGE_LL = quantile(WHITE_RATE_CHANGE, probs = 0.025), WHITE_RATE_CHANGE_UL = quantile(WHITE_RATE_CHANGE, probs = 0.975),
            WHITE_PREVALENCE_CHANGE_MEAN = mean(WHITE_PREVALENCE_CHANGE), WHITE_PREVALENCE_CHANGE_LL = quantile(WHITE_PREVALENCE_CHANGE, probs = 0.025), WHITE_PREVALENCE_CHANGE_UL = quantile(WHITE_PREVALENCE_CHANGE, probs = 0.975),
            RATE_RATIO_MEAN = mean(RATE_RATIO), RATE_RATIO_LL = quantile(RATE_RATIO, probs = 0.025), RATE_RATIO_UL = quantile(RATE_RATIO, probs = 0.975),
            RATE_RATIO_CHANGE_MEAN = mean(RATE_RATIO_CHANGE), RATE_RATIO_CHANGE_LL = quantile(RATE_RATIO_CHANGE, probs = 0.025), RATE_RATIO_CHANGE_UL = quantile(RATE_RATIO_CHANGE, probs = 0.975),
            RATE_DIFFERENCE_MEAN = mean(RATE_DIFFERENCE), RATE_DIFFERENCE_LL = quantile(RATE_DIFFERENCE, probs = 0.025), RATE_DIFFERENCE_UL = quantile(RATE_DIFFERENCE, probs = 0.975),
            RATE_DIFFERENCE_CHANGE_MEAN = mean(RATE_DIFFERENCE_CHANGE), RATE_DIFFERENCE_CHANGE_LL = quantile(RATE_DIFFERENCE_CHANGE, probs = 0.025), RATE_DIFFERENCE_CHANGE_UL = quantile(RATE_DIFFERENCE_CHANGE, probs = 0.975),
            INCIDENCE_RATE_MEAN = mean(INCIDENCE_RATE), INCIDENCE_RATE_LL = quantile(INCIDENCE_RATE, probs = 0.025), INCIDENCE_RATE_UL = quantile(INCIDENCE_RATE, probs = 0.975),
            PREVALENCE_T0_MEAN = mean(PREVALENCE_T0), PREVALENCE_T0_LL = quantile(PREVALENCE_T0, probs = 0.025), PREVALENCE_T0_UL = quantile(PREVALENCE_T0, probs = 0.975),
            PREVALENCE_T120_MEAN = mean(PREVALENCE_T120), PREVALENCE_T120_LL = quantile(PREVALENCE_T120, probs = 0.025), PREVALENCE_T120_UL = quantile(PREVALENCE_T120, probs = 0.975),
            PREVALENCE_CHANGE_MEAN = mean(PREVALENCE_CHANGE), PREVALENCE_CHANGE_LL = quantile(PREVALENCE_CHANGE, probs = 0.025), PREVALENCE_CHANGE_UL = quantile(PREVALENCE_CHANGE, probs = 0.975),
            RATE_CHANGE_MEAN = mean(RATE_CHANGE), RATE_CHANGE_LL = quantile(RATE_CHANGE, probs = 0.025), RATE_CHANGE_UL = quantile(RATE_CHANGE, probs = 0.975))


## Data Analysis: Create new calculations for "relative" measures, i.e. within treament scenarios
BLACK_INCIDENCE_within<-BLACK_INCIDENCE
for (i in 1:dim(BLACK_INCIDENCE)[1]) {
  if (BLACK_INCIDENCE_within$ART_COVERAGE[i]=="Bas") {
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE[i] <- (BLACK_INCIDENCE_within$BLACK_RATE[i] - 5.9462920)/5.9462920
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE_ABSOLUTE[i] <- BLACK_INCIDENCE_within$BLACK_RATE[i] - 5.9462920
    BLACK_INCIDENCE_within$BLACK_NIA[i] <- 1738.0400-BLACK_INCIDENCE_within$BLACK_INFECTIONS[i]
    BLACK_INCIDENCE_within$BLACK_NNT[i] <- BLACK_INCIDENCE_within$BLACK_PREP_YEARS[i] / BLACK_INCIDENCE_within$BLACK_NIA[i]
  } else if (BLACK_INCIDENCE_within$ART_COVERAGE[i]=="90W") {
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE[i] <- (BLACK_INCIDENCE_within$BLACK_RATE[i] - 5.9123345)/5.9123345
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE_ABSOLUTE[i] <- BLACK_INCIDENCE_within$BLACK_RATE[i] - 5.9123345
    BLACK_INCIDENCE_within$BLACK_NIA[i] <- 1726.6600-BLACK_INCIDENCE_within$BLACK_INFECTIONS[i]
    BLACK_INCIDENCE_within$BLACK_NNT[i] <- BLACK_INCIDENCE_within$BLACK_PREP_YEARS[i] / BLACK_INCIDENCE_within$BLACK_NIA[i]
  } else if (BLACK_INCIDENCE_within$ART_COVERAGE[i]=="95W") {
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE[i] <- (BLACK_INCIDENCE_within$BLACK_RATE[i] - 5.9113906)/5.9113906
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE_ABSOLUTE[i] <- BLACK_INCIDENCE_within$BLACK_RATE[i] - 5.9113906
    BLACK_INCIDENCE_within$BLACK_NIA[i] <- 1732.8200-BLACK_INCIDENCE_within$BLACK_INFECTIONS[i]
    BLACK_INCIDENCE_within$BLACK_NNT[i] <- BLACK_INCIDENCE_within$BLACK_PREP_YEARS[i] / BLACK_INCIDENCE_within$BLACK_NIA[i]
  } else if (BLACK_INCIDENCE_within$ART_COVERAGE[i]=="90A") {
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE[i] <- (BLACK_INCIDENCE_within$BLACK_RATE[i] - 4.7798558)/4.7798558
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE_ABSOLUTE[i] <- BLACK_INCIDENCE_within$BLACK_RATE[i] - 4.7798558
    BLACK_INCIDENCE_within$BLACK_NIA[i] <- 1460.1700-BLACK_INCIDENCE_within$BLACK_INFECTIONS[i]
    BLACK_INCIDENCE_within$BLACK_NNT[i] <- BLACK_INCIDENCE_within$BLACK_PREP_YEARS[i] / BLACK_INCIDENCE_within$BLACK_NIA[i]
  } else if (BLACK_INCIDENCE_within$ART_COVERAGE[i]=="95A") {
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE[i] <- (BLACK_INCIDENCE_within$BLACK_RATE[i] - 4.4443777)/4.4443777
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE_ABSOLUTE[i] <- BLACK_INCIDENCE_within$BLACK_RATE[i] - 4.4443777
    BLACK_INCIDENCE_within$BLACK_NIA[i] <- 1370.0000-BLACK_INCIDENCE_within$BLACK_INFECTIONS[i]
    BLACK_INCIDENCE_within$BLACK_NNT[i] <- BLACK_INCIDENCE_within$BLACK_PREP_YEARS[i] / BLACK_INCIDENCE_within$BLACK_NIA[i]
  } else {
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE[i] <- (BLACK_INCIDENCE_within$BLACK_RATE[i] - 4.0168432)/4.0168432
    BLACK_INCIDENCE_within$BLACK_RATE_CHANGE_ABSOLUTE[i] <- BLACK_INCIDENCE_within$BLACK_RATE[i] - 4.0168432
    BLACK_INCIDENCE_within$BLACK_NIA[i] <- 1258.6200-BLACK_INCIDENCE_within$BLACK_INFECTIONS[i]
    BLACK_INCIDENCE_within$BLACK_NNT[i] <- BLACK_INCIDENCE_within$BLACK_PREP_YEARS[i] / BLACK_INCIDENCE_within$BLACK_NIA[i]
  }
}

WHITE_INCIDENCE_within<-WHITE_INCIDENCE
for (i in 1:dim(WHITE_INCIDENCE)[1]) {
  if (WHITE_INCIDENCE_within$ART_COVERAGE[i]=="Bas") {
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE[i] <- (WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.7069786)/1.7069786
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE_ABSOLUTE[i] <- WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.7069786
    WHITE_INCIDENCE_within$WHITE_NIA[i] <- 1456.8700-WHITE_INCIDENCE_within$WHITE_INFECTIONS[i]
    WHITE_INCIDENCE_within$WHITE_NNT[i] <- WHITE_INCIDENCE_within$WHITE_PREP_YEARS[i] / WHITE_INCIDENCE_within$WHITE_NIA[i]
  } else if (WHITE_INCIDENCE_within$ART_COVERAGE[i]=="90W") {
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE[i] <- (WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.5827795)/1.5827795
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE_ABSOLUTE[i] <- WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.5827795
    WHITE_INCIDENCE_within$WHITE_NIA[i] <- 1357.7300-WHITE_INCIDENCE_within$WHITE_INFECTIONS[i]
    WHITE_INCIDENCE_within$WHITE_NNT[i] <- WHITE_INCIDENCE_within$WHITE_PREP_YEARS[i] / WHITE_INCIDENCE_within$WHITE_NIA[i]
  } else if (WHITE_INCIDENCE_within$ART_COVERAGE[i]=="95W") {
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE[i] <- (WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.5346042)/1.5346042
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE_ABSOLUTE[i] <- WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.5346042
    WHITE_INCIDENCE_within$WHITE_NIA[i] <- 1317.4800-WHITE_INCIDENCE_within$WHITE_INFECTIONS[i]
    WHITE_INCIDENCE_within$WHITE_NNT[i] <- WHITE_INCIDENCE_within$WHITE_PREP_YEARS[i] / WHITE_INCIDENCE_within$WHITE_NIA[i]
  } else if (WHITE_INCIDENCE_within$ART_COVERAGE[i]=="90A") {
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE[i] <- (WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.3756062)/1.3756062
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE_ABSOLUTE[i] <- WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.3756062
    WHITE_INCIDENCE_within$WHITE_NIA[i] <- 1188.4300-WHITE_INCIDENCE_within$WHITE_INFECTIONS[i]
    WHITE_INCIDENCE_within$WHITE_NNT[i] <- WHITE_INCIDENCE_within$WHITE_PREP_YEARS[i] / WHITE_INCIDENCE_within$WHITE_NIA[i]
  } else if (WHITE_INCIDENCE_within$ART_COVERAGE[i]=="95A") {
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE[i] <- (WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.2649718)/1.2649718
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE_ABSOLUTE[i] <- WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.2649718
    WHITE_INCIDENCE_within$WHITE_NIA[i] <- 1096.9400-WHITE_INCIDENCE_within$WHITE_INFECTIONS[i]
    WHITE_INCIDENCE_within$WHITE_NNT[i] <- WHITE_INCIDENCE_within$WHITE_PREP_YEARS[i] / WHITE_INCIDENCE_within$WHITE_NIA[i]
  } else {
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE[i] <- (WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.1634715)/1.1634715
    WHITE_INCIDENCE_within$WHITE_RATE_CHANGE_ABSOLUTE[i] <- WHITE_INCIDENCE_within$WHITE_RATE[i] - 1.1634715
    WHITE_INCIDENCE_within$WHITE_NIA[i] <- 1012.8400-WHITE_INCIDENCE_within$WHITE_INFECTIONS[i]
    WHITE_INCIDENCE_within$WHITE_NNT[i] <- WHITE_INCIDENCE_within$WHITE_PREP_YEARS[i] / WHITE_INCIDENCE_within$WHITE_NIA[i]
  }
}

SUMMARY_BY_RUN_MAIN_WITHIN<-merge(BLACK_INCIDENCE_within, WHITE_INCIDENCE_within, by = "RUN_ID")
SUMMARY_BY_RUN_MAIN_WITHIN$ART_COVERAGE.x <- factor(SUMMARY_BY_RUN_MAIN_WITHIN$ART_COVERAGE.x , levels=c("Bas", "90W", "95W", "90A", "95A","100"))
SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO<-SUMMARY_BY_RUN_MAIN_WITHIN$BLACK_RATE/SUMMARY_BY_RUN_MAIN_WITHIN$WHITE_RATE
SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE<-SUMMARY_BY_RUN_MAIN_WITHIN$BLACK_RATE-SUMMARY_BY_RUN_MAIN_WITHIN$WHITE_RATE
SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE<-((SUMMARY_BY_RUN_MAIN_WITHIN$WHITE_INFECTIONS+SUMMARY_BY_RUN_MAIN_WITHIN$BLACK_INFECTIONS)/(SUMMARY_BY_RUN_MAIN_WITHIN$WHITE_PERSON_TIME+SUMMARY_BY_RUN_MAIN_WITHIN$BLACK_PERSON_TIME)*100)

for (i in 1:dim(SUMMARY_BY_RUN_MAIN_WITHIN)[1]) {
  if (SUMMARY_BY_RUN_MAIN_WITHIN$ART_COVERAGE.x[i]=="Bas") {
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO[i]-3.487303)/3.487303)
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE[i]-4.2393134)/4.2393134)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE_ABSOLUTE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.7882249))#/2.7882249)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.7882249)/2.7882249)
  } else if (SUMMARY_BY_RUN_MAIN_WITHIN$ART_COVERAGE.x[i]=="90W") {
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO[i]-3.737565)/3.737565)
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE[i]-4.3295550)/4.3295550) 
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE_ABSOLUTE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.6822330))#/2.6822330)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.6822330)/2.6822330)
  } else if (SUMMARY_BY_RUN_MAIN_WITHIN$ART_COVERAGE.x[i]=="95W") {
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO[i]-3.857049)/3.857049)
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE[i]-4.3767864)/4.3767864)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE_ABSOLUTE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.6483576))#/2.6483576)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.6483576)/2.6483576)
  } else if (SUMMARY_BY_RUN_MAIN_WITHIN$ART_COVERAGE.x[i]=="90A") {
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO[i]-3.479135)/3.479135)
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE[i]-3.4042496)/3.4042496)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE_ABSOLUTE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.2646640))#/2.2646640)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.2646640)/2.2646640)
  } else if (SUMMARY_BY_RUN_MAIN_WITHIN$ART_COVERAGE.x[i]=="95A") {
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO[i]-3.518733)/3.518733)
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE[i]-3.1794059)/3.1794059)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE_ABSOLUTE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.0985575))#/2.0985575)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-2.0985575)/2.0985575)
  } else {
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_RATIO[i]-3.456927)/3.456927)
    SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$RATE_DIFFERENCE[i]-2.8533717)/2.8533717)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE_ABSOLUTE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-1.9184949))#/1.9184949)
    SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE_CHANGE[i]<-((SUMMARY_BY_RUN_MAIN_WITHIN$INCIDENCE_RATE[i]-1.9184949)/1.9184949)
  }
}

MAIN_RESULTS_SUMMARY_WITHIN<-SUMMARY_BY_RUN_MAIN_WITHIN %>%
  group_by(PREP_COVERAGE.x, ART_COVERAGE.x) %>%
  summarise(BLACK_INFECTIONS_MEAN = mean(BLACK_INFECTIONS), BLACK_INFECTIONS_LL = quantile(BLACK_INFECTIONS, probs = 0.025), BLACK_INFECTIONS_UL = quantile(BLACK_INFECTIONS, probs = 0.975),
    BLACK_NIA_MEAN = mean(BLACK_NIA), BLACK_NIA_LL = quantile(BLACK_NIA, probs = 0.025), BLACK_NIA_UL = quantile(BLACK_NIA, probs = 0.975),
    BLACK_NNT_MEAN = mean(BLACK_NNT, na.rm=T), BLACK_NNT_LL = quantile(BLACK_NNT, probs = 0.025, na.rm=T), BLACK_NNT_UL = quantile(BLACK_NNT, probs = 0.975, na.rm=T),
    BLACK_RATE_MEAN = mean(BLACK_RATE), BLACK_RATE_LL = quantile(BLACK_RATE, probs = 0.025), BLACK_RATE_UL = quantile(BLACK_RATE, probs = 0.975),
    BLACK_RATE_CHANGE_MEAN = mean(BLACK_RATE_CHANGE), BLACK_RATE_CHANGE_LL = quantile(BLACK_RATE_CHANGE, probs = 0.025), BLACK_RATE_CHANGE_UL = quantile(BLACK_RATE_CHANGE, probs = 0.975),
    BLACK_RATE_CHANGE_ABSOLUTE_MEAN = mean(BLACK_RATE_CHANGE_ABSOLUTE), BLACK_RATE_CHANGE_ABSOLUTE_LL = quantile(BLACK_RATE_CHANGE_ABSOLUTE, probs = 0.025), BLACK_RATE_CHANGE_ABSOLUTE_UL = quantile(BLACK_RATE_CHANGE_ABSOLUTE, probs = 0.975),
    BLACK_PREVALENCE_CHANGE_MEAN = mean(BLACK_PREVALENCE_CHANGE), BLACK_PREVALENCE_CHANGE_LL = quantile(BLACK_PREVALENCE_CHANGE, probs = 0.025), BLACK_PREVALENCE_CHANGE_UL = quantile(BLACK_PREVALENCE_CHANGE, probs = 0.975),
    WHITE_INFECTIONS_MEAN = mean(WHITE_INFECTIONS), WHITE_INFECTIONS_LL = quantile(WHITE_INFECTIONS, probs = 0.025), WHITE_INFECTIONS_UL = quantile(WHITE_INFECTIONS, probs = 0.975),
    WHITE_NIA_MEAN = mean(WHITE_NIA), WHITE_NIA_LL = quantile(WHITE_NIA, probs = 0.025), WHITE_NIA_UL = quantile(WHITE_NIA, probs = 0.975),
    WHITE_NNT_MEAN = mean(WHITE_NNT), WHITE_NNT_LL = quantile(WHITE_NNT, probs = 0.025), WHITE_NNT_UL = quantile(WHITE_NNT, probs = 0.975),
    WHITE_RATE_MEAN = mean(WHITE_RATE), WHITE_RATE_LL = quantile(WHITE_RATE, probs = 0.025), WHITE_RATE_UL = quantile(WHITE_RATE, probs = 0.975),
    WHITE_RATE_CHANGE_MEAN = mean(WHITE_RATE_CHANGE), WHITE_RATE_CHANGE_LL = quantile(WHITE_RATE_CHANGE, probs = 0.025), WHITE_RATE_CHANGE_UL = quantile(WHITE_RATE_CHANGE, probs = 0.975),
    WHITE_RATE_CHANGE_ABSOLUTE_MEAN = mean(WHITE_RATE_CHANGE_ABSOLUTE), WHITE_RATE_CHANGE_ABSOLUTE_LL = quantile(WHITE_RATE_CHANGE_ABSOLUTE, probs = 0.025), WHITE_RATE_CHANGE_ABSOLUTE_UL = quantile(WHITE_RATE_CHANGE_ABSOLUTE, probs = 0.975),
    WHITE_PREVALENCE_CHANGE_MEAN = mean(WHITE_PREVALENCE_CHANGE), WHITE_PREVALENCE_CHANGE_LL = quantile(WHITE_PREVALENCE_CHANGE, probs = 0.025), WHITE_PREVALENCE_CHANGE_UL = quantile(WHITE_PREVALENCE_CHANGE, probs = 0.975),
    RATE_RATIO_MEAN = mean(RATE_RATIO), RATE_RATIO_LL = quantile(RATE_RATIO, probs = 0.025), RATE_RATIO_UL = quantile(RATE_RATIO, probs = 0.975),
    RATE_RATIO_CHANGE_MEAN = mean(RATE_RATIO_CHANGE), RATE_RATIO_CHANGE_LL = quantile(RATE_RATIO_CHANGE, probs = 0.025), RATE_RATIO_CHANGE_UL = quantile(RATE_RATIO_CHANGE, probs = 0.975),
    RATE_DIFFERENCE_MEAN = mean(RATE_DIFFERENCE), RATE_DIFFERENCE_LL = quantile(RATE_DIFFERENCE, probs = 0.025), RATE_DIFFERENCE_UL = quantile(RATE_DIFFERENCE, probs = 0.975),
    RATE_DIFFERENCE_CHANGE_MEAN = mean(RATE_DIFFERENCE_CHANGE), RATE_DIFFERENCE_CHANGE_LL = quantile(RATE_DIFFERENCE_CHANGE, probs = 0.025), RATE_DIFFERENCE_CHANGE_UL = quantile(RATE_DIFFERENCE_CHANGE, probs = 0.975),
    INCIDENCE_RATE_CHANGE_MEAN = mean(INCIDENCE_RATE_CHANGE), INCIDENCE_RATE_CHANGE_LL = quantile(INCIDENCE_RATE_CHANGE, probs = 0.025), INCIDENCE_RATE_CHANGE_UL = quantile(INCIDENCE_RATE_CHANGE, probs = 0.975),
    INCIDENCE_RATE_MEAN = mean(INCIDENCE_RATE), INCIDENCE_RATE_LL = quantile(INCIDENCE_RATE, probs = 0.025), INCIDENCE_RATE_UL = quantile(INCIDENCE_RATE, probs = 0.975),
    INCIDENCE_RATE_CHANGE_ABSOLUTE_MEAN = mean(INCIDENCE_RATE_CHANGE_ABSOLUTE), INCIDENCE_RATE_CHANGE_ABSOLUTE_LL = quantile(INCIDENCE_RATE_CHANGE_ABSOLUTE, probs = 0.025), INCIDENCE_RATE_CHANGE_ABSOLUTE_UL = quantile(INCIDENCE_RATE_CHANGE_ABSOLUTE, probs = 0.975),
    )

## _________________________________________________________________________________________________________________________________________________
## _________________________________________________________________________________________________________________________________________________
## _________________________________________________________________________________________________________________________________________________

## Data Visualization: Create Manuscript Figures

## Figure 2: Boxplots showing overall and race-specific incidence rates on y-axis with 
# treatment scenarios (baseline, 90W, 95W, 90A, 95A) along x-axis (3 panels with common legend)

# Create data frame with only PrEP @ 0% runs and no 100
SUMMARY_BY_RUN_MAIN_0PrEP<-SUMMARY_BY_RUN_MAIN[(SUMMARY_BY_RUN_MAIN$PREP_COVERAGE.x=="0.00"),]
SUMMARY_BY_RUN_MAIN_0PrEP<-SUMMARY_BY_RUN_MAIN_0PrEP[!(SUMMARY_BY_RUN_MAIN_0PrEP$ART_COVERAGE.x=="90W"),]
SUMMARY_BY_RUN_MAIN_0PrEP<-SUMMARY_BY_RUN_MAIN_0PrEP[!(SUMMARY_BY_RUN_MAIN_0PrEP$ART_COVERAGE.x=="95W"),]
SUMMARY_BY_RUN_MAIN_0PrEP<-SUMMARY_BY_RUN_MAIN_0PrEP[!(SUMMARY_BY_RUN_MAIN_0PrEP$ART_COVERAGE.x=="100"),]
#SUMMARY_BY_RUN_MAIN_0PrEP$ART_COVERAGE.x <- ifelse(SUMMARY_BY_RUN_MAIN_0PrEP$ART_COVERAGE.x=="Bas", "1", SUMMARY_BY_RUN_MAIN_0PrEP$ART_COVERAGE.x)

# Plot for black msm absolute incidence rate
f1.1<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_0PrEP, aes(x = ART_COVERAGE.x, y = BLACK_RATE), color = "black", fill="lightgrey", size=0.6) +
  scale_y_continuous(limits = c(1, 6.5)) + 
  scale_x_discrete(name = "Treatment Scenario", labels = c("Current", "90-90-90", "95-95-95")) +
  labs(x = "Treatment Scenario", y = "", title="Black/African American MSM") +
  guides(fill = guide_legend(label.position = "bottom")) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 15, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

# Plot for white msm absolute incidence rate
f1.2<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_0PrEP, aes(x = ART_COVERAGE.x, y = WHITE_RATE), color = "black", fill="lightgrey", size=0.6) +
  scale_y_continuous(limits = c(1, 6.5)) + 
  scale_x_discrete(name = "", labels = c("Current", "90-90-90", "95-95-95")) +
  labs(x = "", y = "", title="White MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 15, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))
# Plot for overall msm absolute incidence rate
f1.3<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_0PrEP, aes(x = ART_COVERAGE.x, y = INCIDENCE_RATE), color = "black", fill="lightgrey", size=0.6) +
  scale_y_continuous(limits = c(1, 6.5)) + 
  scale_x_discrete(name = "", labels = c("Current", "90-90-90", "95-95-95")) +
  labs(x = "", y = "HIV incidence (per 100 person-years)", title="All MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "none",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 15, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

# Plot all three together with a common legend
#legend <- get_legend(f1.1)
#f1.1 <- f1.1 + theme(legend.position="none")
grid.arrange(arrangeGrob(f1.3, left = textGrob("A)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))), 
             arrangeGrob(f1.1, left = textGrob("B)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             arrangeGrob(f1.2, left = textGrob("C)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             ncol=3, nrow=1,
             layout_matrix = rbind(c(1,2,3)),
             widths = c(2.7, 2.7, 2.7))


## _________________________________________________________________________________________________________________________________________________

## Figure 3: Heatmaps showing changes in overall and race-specific incidence rates 
# (relative to 0% PrEP in a given treatment scenario, i.e., 0% PrEP = 0) (3 panels, 
# not necessarily common legend if the scales of changes are really different but try 
# to match colors across scales)

# Create data frame with no PrEP @ 0% runs and no 100
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED <- MAIN_RESULTS_SUMMARY_WITHIN[!(MAIN_RESULTS_SUMMARY_WITHIN$PREP_COVERAGE.x=="0.00"),]
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED<-MAIN_RESULTS_SUMMARY_WITHIN_REDUCED[!(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$ART_COVERAGE.x=="90W"),]
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED<-MAIN_RESULTS_SUMMARY_WITHIN_REDUCED[!(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$ART_COVERAGE.x=="95W"),]
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED<-MAIN_RESULTS_SUMMARY_WITHIN_REDUCED[!(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$ART_COVERAGE.x=="100"),]
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$ART_COVERAGE.x <- ifelse(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$ART_COVERAGE.x=="Bas", "1", MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$ART_COVERAGE.x)
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x <- ifelse(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x=="0.15", "15%", MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x)
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x <- ifelse(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x=="0.30", "30%", MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x)
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x <- ifelse(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x=="0.45", "45%", MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x)
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x <- ifelse(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x=="0.60", "60%", MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x)
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x <- ifelse(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x=="0.75", "75%", MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x)
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x <- ifelse(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x=="0.90", "90%", MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$PREP_COVERAGE.x)

# Plot for black msm change in incidence rate
f2.1 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_RATE_CHANGE_ABSOLUTE_MEAN), colour = "white", size = 0.4) +
  scale_fill_gradient2(name = "Absolute Change in HIV Incidence\n(per 100 person-years)",
                       limits = c(-4.614553, -0.2253672), breaks = c(-4.614553, -2.41996, -0.2253672), labels = function(x) round(x,1), midpoint = -2.41996,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  scale_x_discrete(labels=c("1" = "Current", "4" = "90-90-90", "5" = "95-95-95")) + 
  labs(x = "Treatment Scenario", y = "", title="Black/African American MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 1.75, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.text = element_text(size = 12),
        legend.box.just = "center",
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))
f2.1
# Plot for white msm change in incidence rate
f2.2 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_RATE_CHANGE_ABSOLUTE_MEAN), colour = "white", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate White MSM",
                       limits = c(-4.614553, -0.2253671), breaks = c(-4.614553, -2.41996, -0.2253672), labels = function(x) round(x,1), midpoint = -2.41996,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  scale_x_discrete(labels=c("1" = "Current", "4" = "90-90-90", "5" = "95-95-95")) + 
  labs(x = "", y = "", title="White MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "none",
        legend.title = element_text(vjust=0.0,hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

# Plot for all msm change in incidence rate
f2.3 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = INCIDENCE_RATE_CHANGE_ABSOLUTE_MEAN), colour = "white", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate White MSM",
                       limits = c(-4.614553, -0.2253672), breaks = c(-4.614553, -2.41996, -0.2253672), labels = function(x) round(x,1), midpoint = -2.41996,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  scale_x_discrete(labels=c("1" = "Current", "4" = "90-90-90", "5" = "95-95-95")) + 
  labs(x = "", y = "PrEP Coverage (%)", title="All MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "none",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

# Plot all three together with a common legend
legend <- get_legend(f2.1)
f2.1 <- f2.1 + theme(legend.position="none")
grid.arrange(arrangeGrob(f2.3, left = textGrob("A)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))), 
             arrangeGrob(f2.1, left = textGrob("B)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             arrangeGrob(f2.2, left = textGrob("C)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             legend, ncol=3, nrow=2,
             layout_matrix = rbind(c(1,2,3), c(4,4,4)),
             widths = c(2.7, 2.7, 2.7), heights = c(2.5, 0.2))

## _________________________________________________________________________________________________________________________________________________

## Original Figure 3 (now not included, here for reference): Heatmaps showing actual values for IRR and 
# IRD with colorbar centered at baseline value of IRR and IRD (2 panels, no common legend)

# Plot Incidence Rate Ratio Between Race Groups
f3.1 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = RATE_RATIO_MEAN), colour = "white", size = 0.4) +
  scale_fill_gradient2(name = "Incidence Rate Ratio",
                       limits = c(3.549516, 5.74), breaks = c(3.549516, 4.66, 5.74), labels = function(x) round(x,1), midpoint = 4.66,
                       low = "#edf8b1", mid = "#7fcdbb", high = "#2c7fb8", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage (%)", title = "Incidence Rate Ratio") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))

### Plot Incidence Rate Difference Between Race Groups
f3.2 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = RATE_DIFFERENCE_MEAN), colour = "white", size = 0.4) +
  scale_fill_gradient2(name = "Incidence Rate Difference",
                       limits = c(0.75, 3.77), breaks = c(0.75, 2.26, 3.77), labels = function(x) round(x,1), midpoint = 2.26,
                       low = "#e0ecf4", mid = "#9ebcda", high = "#8856a7", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "", title = "Incidence Rate Difference") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))

# Plot both together with each legend
grid.arrange(arrangeGrob(f3.1, left = textGrob("A)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))), 
             arrangeGrob(f3.2, left = textGrob("B)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             ncol=2)

## _________________________________________________________________________________________________________________________________________________

## Figure 4: Heatmaps showing actual race-specific values of person-years on PrEP per HIV infection averted 
# (relative to 0% PrEP in a given treatment scenario) (2 panels, no common legend because scales are different)

# Plot 'NNT' black msm
f4.1 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_NNT_MEAN), colour = "white", size = 0.4) +
  scale_fill_gradient2(name = "Person-years on PrEP\nper HIV infection averted",
                       limits = c(20, 30), breaks = c(20, 25, 30), labels = function(x) round(x,1), midpoint = 25,
                       low = "#c51b8a", mid = "#fa9fb5", high = "#fde0dd", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Black/African American MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("1" = "Current", "4" = "90-90-90", "5" = "95-95-95"))

# Plot 'NNT' white msm
f4.2 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_NNT_MEAN), colour = "white", size = 0.4) +
  scale_fill_gradient2(name = "Person-years on PrEP\nper HIV infection averted",
                       limits = c(50, 85), breaks = c(50, (50 + 85)/2, 85), labels = function(x) round(x,1), midpoint = (50 + 85)/2,
                       low = "#d95f0e", mid = "#fec44f", high = "#fff7bc", guide = guide_colorbar(frame.colour = "black")) +
    labs(x = "Treatment Scenario", y = "", title="White MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("1" = "Current", "4" = "90-90-90", "5" = "95-95-95"))

# Plot both together with each legend
grid.arrange(arrangeGrob(f4.1, left = textGrob("A)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))), 
             arrangeGrob(f4.2, left = textGrob("B)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             ncol=2)

## _________________________________________________________________________________________________________________________________________________

## Supplemental Figure 1: Box Plot with Error Bars showing validation to calibration targets (race-specific incidence rates)
# build dataframe of calibration targets data
calibration.data <- data.frame(name=c("Black/African American", "White", "Black/African American", "White"),
                               source=c("A","A","B","B"),
                               value=c(6.5, 1.7, 5.95, 1.7069),
                               bottom.int=c(2.3, 1, 0.4, 0.12),
                               top.int=c(3.2, 1.6, 0.47, 0.13))

ggplot(calibration.data, aes(x=name, y=value, fill=source)) +
  geom_bar(stat="identity", color="black", alpha=0.7, position=position_dodge()) +
  geom_errorbar(aes(ymin=value-bottom.int, ymax=value+top.int), width=0.2, colour="black", position=position_dodge(0.9)) +  
  labs(x = "", y = "Incidence Rate (per 100 person-years)", title="") +
  scale_fill_brewer(palette="Paired", name = "", guide = guide_legend(), labels = c("InvolveMENt Cohort","The TITAN Model"))+ #values = c("#d95f0e", "#fff7bc")) +
  #guides(fill = guide_legend(label.position = "bottom")) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))


## _________________________________________________________________________________________________________________________________________________
## _________________________________________________________________________________________________________________________________________________

## Figure 2A: Heatmaps showing changes in overall and race-specific incidence rates 
# (relative to 0% PrEP in a given treatment scenario, i.e., 0% PrEP = 0) (3 panels, 
# not necessarily common legend if the scales of changes are really different but try 
# to match colors across scales)

# Create data frame with no PrEP @ 0% runs and no 100
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED <- MAIN_RESULTS_SUMMARY_WITHIN[!(MAIN_RESULTS_SUMMARY_WITHIN$PREP_COVERAGE.x=="0.00"),]
MAIN_RESULTS_SUMMARY_WITHIN_REDUCED<-MAIN_RESULTS_SUMMARY_WITHIN_REDUCED[!(MAIN_RESULTS_SUMMARY_WITHIN_REDUCED$ART_COVERAGE.x=="100"),]

# Plot for black msm change in incidence rate
f2.1 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_RATE_CHANGE_MEAN), colour = "white", size = 0.4) +
  scale_fill_gradient2(name = "Percent Change in\nIncidence Rate Black MSM",
                       limits = c(-0.8508364, -0.1432687), breaks = c(-0.8508364, -0.497052, -0.1432688), labels = percent, midpoint = -0.497052,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Added Percent Change in\nIncidence Rate Due to PrEP\nBlack MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.box.just = "center",
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

# Plot for white msm change in incidence rate
f2.2 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_RATE_CHANGE_MEAN), colour = "white", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate White MSM",
                       limits = c(-0.8508364, -0.1432688), breaks = c(-0.8508364, -0.497052, -0.1432688), labels = percent, midpoint = -0.497052,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "", title="Added Percent Change in\nIncidence Rate Due to PrEP\nWhite MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "none",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

# Plot for all msm change in incidence rate
f2.3 <- ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN_REDUCED, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = INCIDENCE_RATE_CHANGE_MEAN), colour = "white", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate White MSM",
                       limits = c(-0.8508364, -0.1432688), breaks = c(-0.8508364, -0.497052, -0.1432688), labels = percent, midpoint = -0.497052,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "", title="Added Percent Change in\nIncidence Rate Due to PrEP\nAll MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "none",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

# Plot all three together with a common legend
legend <- get_legend(f2.1)
f2.1 <- f2.1 + theme(legend.position="none")
grid.arrange(arrangeGrob(f2.1, left = textGrob("D)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))), 
             arrangeGrob(f2.2, left = textGrob("E)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             arrangeGrob(f2.3, left = textGrob("F)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             legend, ncol=3, nrow=2,
             layout_matrix = rbind(c(1,2,3), c(4,4,4)),
             widths = c(2.7, 2.7, 2.7), heights = c(2.5, 0.2))

## _________________________________________________________________________________________________________________________________________________
## _________________________________________________________________________________________________________________________________________________
## _________________________________________________________________________________________________________________________________________________

## Data Visualization: Explore results by creating figures summarizing each outcome measure
### Change in Incidence Rate stratified by race and by all scenarios, percent changes
p1<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN, aes(x = PREP_COVERAGE.x, y = BLACK_RATE_CHANGE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "Treatment Scenario", labels = c("Baseline", "90-90-90 White", "95-95-95 White", "90-90-90 All", "95-95-95 All", "100 All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170", "#fcfdfe")) +
  labs(x = "PrEP Coverage", y = "Change in Incidence Rate Black MSM", title="Change in Incidence Rate Black MSM") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

p2<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN, aes(x = PREP_COVERAGE.x, y = WHITE_RATE_CHANGE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "Treatment Scenario", labels = c("Baseline", "90-90-90 White", "95-95-95 White", "90-90-90 All", "95-95-95 All", "100 All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170", "#fcfdfe")) +
  labs(x = "PrEP Coverage", y = "Change in Incidence Rate White MSM", title="Change in Incidence Rate White MSM") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

grid.arrange(p1, p2, ncol = 2)

### Incidence Rates stratified by race and by all scenarios, absolute values
p1a<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN, aes(x = PREP_COVERAGE.x, y = BLACK_RATE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  #scale_y_continuous(breaks = seq(0,6,by=0.5)) + 
  scale_fill_manual(name = "Treatment Scenario", labels = c("Baseline", "90-90-90 White", "95-95-95 White", "90-90-90 All", "95-95-95 All", "100-100-100 All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170", "#fcfdfe")) +
  labs(x = "PrEP Coverage", y = "Incidence Rate Black MSM", title="Incidence Rate Black MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

p2a<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN, aes(x = PREP_COVERAGE.x, y = WHITE_RATE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.02)) + 
  scale_fill_manual(name = "Treatment Scenario", labels = c("Baseline", "90-90-90 White", "95-95-95 White", "90-90-90 All", "95-95-95 All", "100-100-100 All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170", "#fcfdfe")) +
  labs(x = "PrEP Coverage", y = "Incidence Rate White MSM", title="Incidence Rate White MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

grid.arrange(p1a, p2a, ncol = 2)

### Change in Incidence Rate stratified by race and by all scenarios, absolute changes WITHIN treatment scenarios "added val of prep"
p1w<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_WITHIN, aes(x = PREP_COVERAGE.x, y = BLACK_RATE_CHANGE_ABSOLUTE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "Treatment Scenario", labels = c("Baseline", "90-90-90 White", "95-95-95 White", "90-90-90 All", "95-95-95 All", "100-100-100 All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170", "#fcfdfe")) +
  labs(x = "PrEP Coverage", y = "Relative Change in Incidence Rate\nBlack MSM", title="Relative Change in Incidence Rate\nBlack MSM") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

p2w<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_WITHIN, aes(x = PREP_COVERAGE.x, y = WHITE_RATE_CHANGE_ABSOLUTE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "Treatment Scenario", labels = c("Baseline", "90-90-90 White", "95-95-95 White", "90-90-90 All", "95-95-95 All", "100-100-100 All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170", "#fcfdfe")) +
  labs(x = "PrEP Coverage", y = "Relative Change in Incidence Rate\nWhite MSM", title="Relative Change in Incidence Rate\nWhite MSM") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

grid.arrange(p1w, p2w, ncol = 2)

### Incidence Rate stratified by race and by all scenarios, absolute values, treatments on xaxis (blue)
p3<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_WITHIN, aes(x = ART_COVERAGE.x, y = BLACK_RATE, fill = PREP_COVERAGE.x), color = "black", size=0.6) +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "PrEP Coverage", labels = c("0.00 Coverage", "0.15 Coverage", "0.30 Coverage", "0.45 Coverage", "0.60 Coverage", "0.75 Coverage", "0.90 Coverage"), values = c("#fcfdfe", "#afc7e9", "#719dd9", "#3672c6", "#295798", "#1d3d69", "#1d3d69")) +
  labs(x = "Treatment Scenario", y = "Incidence Rate Black MSM", title="Incidence Rate Black MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))
p4<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_WITHIN, aes(x = ART_COVERAGE.x, y = BLACK_RATE, fill = PREP_COVERAGE.x), color = "black", size=0.6) +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "PrEP Coverage", labels = c("0.00 Coverage", "0.15 Coverage", "0.30 Coverage", "0.45 Coverage", "0.60 Coverage", "0.75 Coverage", "0.90 Coverage"), values = c("#fcfdfe", "#afc7e9", "#719dd9", "#3672c6", "#295798", "#1d3d69", "#1d3d69")) +
  labs(x = "Treatment Scenario", y = "Incidence Rate White MSM", title="Incidence Rate White MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))
grid.arrange(p3, p4, ncol = 2)

### RELATIVE Change in Incidence Rate stratified by race and by all scenarios, percent values, treatments on xaxis (blue)
p3w<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_WITHIN, aes(x = ART_COVERAGE.x, y = BLACK_RATE_CHANGE, fill = PREP_COVERAGE.x), color = "black", size=0.6) +
  scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "PrEP Coverage", labels = c("0.00 Coverage", "0.15 Coverage", "0.30 Coverage", "0.45 Coverage", "0.60 Coverage", "0.75 Coverage", "0.90 Coverage"), values = c("#fcfdfe", "#afc7e9", "#719dd9", "#3672c6", "#295798", "#1d3d69", "#1d3d69")) +
  labs(x = "Treatment Scenario", y = "Change in Incidence Rate Black MSM", title="Relative Change in Incidence Rate\nBlack MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))
p4w<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN_WITHIN, aes(x = ART_COVERAGE.x, y = BLACK_RATE_CHANGE, fill = PREP_COVERAGE.x), color = "black", size=0.6) +
  scale_y_continuous(labels = percent, limits = c(-1, 0.1)) + 
  scale_fill_manual(name = "PrEP Coverage", labels = c("0.00 Coverage", "0.15 Coverage", "0.30 Coverage", "0.45 Coverage", "0.60 Coverage", "0.75 Coverage", "0.90 Coverage"), values = c("#fcfdfe", "#afc7e9", "#719dd9", "#3672c6", "#295798", "#1d3d69", "#1d3d69")) +
  labs(x = "Treatment Scenario", y = "Change in Incidence Rate White MSM", title="Relative Change in Incidence Rate\nWhite MSM") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 10),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 18, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))
grid.arrange(p3w, p4w, ncol = 2)

### Change in Incidence Rate Black MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_RATE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate Ratio",
                       limits = c(-0.8224718, -0.1471274), breaks = c(-0.8224717, -0.4847996, -0.1471275), labels = percent, midpoint = -0.4847996,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in Incidence Rate Black MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

### Change in Incidence Rate White MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_RATE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate Ratio",
                       limits = c(-0.8747, -0.1768), breaks = c(-0.8747, -0.5258, -0.1768), labels = percent, midpoint = -0.5258,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in Incidence Rate White MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

### Change in Incidence Rate Ratio Between Race Groups
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = RATE_RATIO_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate Ratio",
                       limits = c(-0.02, 0.644464), breaks = c(-0.02, 0.332232, 0.644464), labels = percent, midpoint = 0.332232,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title = "Change in Incidence Rate Ratio") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
        scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))

### Change in Incidence Rate Difference Between Race Groups
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = RATE_DIFFERENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate Difference",
                       limits = c(-0.8413837, 0.03242813), breaks = c(-0.8413837, -0.4044778, 0.03242813), labels = percent, midpoint = -0.4044778,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title = "Change in Incidence Rate Difference") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### Change in Incidence Rate Ratio Between Race Groups RELATIVE
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = RATE_RATIO_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate Ratio",
                       limits = c(-0.01756481, 0.4868217), breaks = c(-0.01756481, 0.2346284, 0.4868217), labels = percent, midpoint = 0.2346284,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title = "Change in Incidence Rate Ratio RELATIVE") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### Change in Incidence Rate Difference Between Race Groups RELATIVE
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = RATE_DIFFERENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate Difference",
                       limits = c(-0.7643404, 0.001), breaks = c(-0.7643404, -0.3821701, 0.001), labels = percent, midpoint = -0.3821701,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title = "Change in Incidence Rate Difference RELATIVE") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### Change in HIV Prevalence among Black MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_PREVALENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nPrevalence",
                       limits = c(min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN), max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN), (min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN))/2, max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in Prevalence Black MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### Change in HIV Prevalence among White MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_PREVALENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nPrevalence",
                       limits = c(min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN), max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN), (min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN))/2, max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in Prevalence White MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### Change in HIV Prevalence among Full Population
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = PREVALENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nPrevalence",
                       limits = c(min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN), max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN), (min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN))/2, max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in Prevalence Full Population") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### NNT among Black MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_NNT_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "NNT",
                       limits = c(5, max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN)), breaks = c(5, (5 + max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN))/2, max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN)), labels = function(x) round(x,1), midpoint = (5 + max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="NNT Black MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### NNT among White MSM 
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_NNT_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "NNT",
                       limits = c(18, max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN)), breaks = c(18, (18 + max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN))/2, max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN)), labels = function(x) round(x,1), midpoint = (18 + max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="NNT White MSM") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))

### NNT among Black MSM RELATIVE
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_NNT_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "NNT",
                       limits = c(20, max(MAIN_RESULTS_SUMMARY_WITHIN$BLACK_NNT_MEAN)), breaks = c(20, (20 + max(MAIN_RESULTS_SUMMARY_WITHIN$BLACK_NNT_MEAN))/2, max(MAIN_RESULTS_SUMMARY_WITHIN$BLACK_NNT_MEAN)), labels = function(x) round(x,1), midpoint = (20 + max(MAIN_RESULTS_SUMMARY_WITHIN$BLACK_NNT_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="NNT Black MSM RELATIVE") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))


### NNT among White MSM RELATIVE
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY_WITHIN, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_NNT_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "NNT",
                       limits = c(50, max(MAIN_RESULTS_SUMMARY_WITHIN$WHITE_NNT_MEAN)), breaks = c(50, (50 + max(MAIN_RESULTS_SUMMARY_WITHIN$WHITE_NNT_MEAN))/2, max(MAIN_RESULTS_SUMMARY_WITHIN$WHITE_NNT_MEAN)), labels = function(x) round(x,1), midpoint = (50 + max(MAIN_RESULTS_SUMMARY_WITHIN$WHITE_NNT_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="NNT White MSM RELATIVE") +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 2, frame.colour = "black", frame.linewidth = 2)) +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.text = element_text(size = 15),
        legend.spacing.x = unit(1.25, 'cm'),
        legend.key = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.title = element_text(size = 20, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 22, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))+
  scale_x_discrete(labels=c("Base" = "Baseline", "90White" = "90-90-90 White","95White" = "95-95-95 White", "90All" = "90-90-90 All", "95All" = "95-95-95 All"))

