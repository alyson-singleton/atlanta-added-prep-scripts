## Pre-Processing: Load required packages for data processing, analysis, and visualization
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)

## Pre-Processing: Import basic reports for each scenario into lists for data processing
setwd("~/Desktop/research/atlanta_prevention_packages/titan.output/sensitivity.analysis/Aggregate_Basic_Report_Black/")
FILE_LIST<-list.files(pattern = "*.txt")
BLACK_BASIC_REPORTS<-list()
for (i in 1:length(FILE_LIST)){BLACK_BASIC_REPORTS[[i]]<-read.table(FILE_LIST[i], header = TRUE)}
names(BLACK_BASIC_REPORTS)<-FILE_LIST
BLACK_REPORTS<-rbindlist(BLACK_BASIC_REPORTS, idcol = "origin")
rm(BLACK_BASIC_REPORTS)

setwd("~/Desktop/research/atlanta_prevention_packages/titan.output/sensitivity.analysis/Aggregate_Basic_Report_White/")
WHITE_BASIC_REPORTS<-list()
for (i in 1:length(FILE_LIST)){WHITE_BASIC_REPORTS[[i]]<-read.table(FILE_LIST[i], header = TRUE)}
names(WHITE_BASIC_REPORTS)<-FILE_LIST
WHITE_REPORTS<-rbindlist(WHITE_BASIC_REPORTS, idcol = "origin")
rm(WHITE_BASIC_REPORTS)
rm(FILE_LIST)
rm(i)

## Pre-Processing: Create field to represent the PrEP-to-need ratio and PrEP continuum values in each group
#BLACK_REPORTS$BLACK_PNR<-recode(BLACK_REPORTS$BLACK_PREP_COVERAGE,
 #                               "0.068 = 1; 0.136 = 2; 0.205 = 3; 0.273 = 4; 0.341 = 5; 0.409 = 6; 0.477 = 7; 0.546 = 8; 0.614 = 9; 0.682 = 10")
#BLACK_REPORTS$WHITE_PREP_COVERAGE<-as.character(substr(BLACK_REPORTS$origin, 19, 23))
#BLACK_REPORTS$WHITE_PNR<-recode(BLACK_REPORTS$WHITE_PREP_COVERAGE,
 #                               "0.017 = 1; 0.033 = 2; 0.050 = 3; 0.066 = 4; 0.083 = 5; 0.100 = 6; 0.116 = 7; 0.133 = 8; 0.149 = 9; 0.166 = 10")
#BLACK_REPORTS$BLACK_RETENTION<-as.numeric(substr(BLACK_REPORTS$origin, 35, 38))

BLACK_REPORTS$PREP_COVERAGE<-as.numeric(substr(BLACK_REPORTS$origin, 1, 4))
BLACK_REPORTS$ART_COVERAGE<-substr(BLACK_REPORTS$origin, 10, 12)
BLACK_REPORTS$BLACK_ADHERENCE<-as.numeric(substr(BLACK_REPORTS$origin, 18, 22))


WHITE_REPORTS$PREP_COVERAGE<-as.numeric(substr(WHITE_REPORTS$origin, 1, 4))
WHITE_REPORTS$ART_COVERAGE<-substr(WHITE_REPORTS$origin, 10, 12)
WHITE_REPORTS$BLACK_ADHERENCE<-as.numeric(substr(WHITE_REPORTS$origin, 18, 22))

## Data Analysis: Create key outcome measures for each scenario and run, stratified by race/ethnicity
BLACK_INCIDENCE<-BLACK_REPORTS %>%
  group_by(origin, rseed, add = TRUE) %>%
  summarise(#BLACK_PNR = mean(BLACK_PNR),
            #WHITE_PNR = mean(WHITE_PNR), 
            #BLACK_RETENTION = mean(BLACK_RETENTION),
            PREP_COVERAGE = mean(PREP_COVERAGE),
            ART_COVERAGE = ART_COVERAGE[1],
            BLACK_ADHERENCE = mean(BLACK_ADHERENCE),
            BLACK_INFECTIONS = sum(Incid),
            BLACK_NIA = 1742.31-sum(Incid), 
            BLACK_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            BLACK_RATE_FULL = (sum(Incid)/(sum(17440-HIV)/12))*100,
            BLACK_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-5.953189)/5.953189))
BLACK_INCIDENCE$RUN_ID<-1:2000

WHITE_INCIDENCE<-WHITE_REPORTS %>%
  group_by(origin, rseed, add = TRUE) %>%
  summarise(PREP_COVERAGE = mean(PREP_COVERAGE),
            ART_COVERAGE = ART_COVERAGE[1],
            BLACK_ADHERENCE = mean(BLACK_ADHERENCE),
            WHITE_INFECTIONS = sum(Incid), 
            WHITE_NIA = 1453.71-sum(Incid),
            WHITE_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            WHITE_RATE_FULL = (sum(Incid)/(sum(17440-HIV)/12))*100,
            WHITE_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-1.7028580)/1.7028580))
WHITE_INCIDENCE$RUN_ID<-1:2000

SUMMARY_BY_RUN_SENSITIVITY<-merge(BLACK_INCIDENCE, WHITE_INCIDENCE, by = "RUN_ID")
SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x <- factor(SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x , levels=c("Bas", "90W", "95W", "90A", "95A"))
rm(BLACK_REPORTS)
rm(WHITE_REPORTS)
rm(BLACK_INCIDENCE)
rm(WHITE_INCIDENCE)

#SUMMARY_BY_RUN_SENSITIVITY$RATE_RATIO<-SUMMARY_BY_RUN_SENSITIVITY$BLACK_RATE/SUMMARY_BY_RUN_SENSITIVITY$WHITE_RATE
#SUMMARY_BY_RUN_SENSITIVITY$RATE_DIFFERENCE<-SUMMARY_BY_RUN_SENSITIVITY$BLACK_RATE-SUMMARY_BY_RUN_SENSITIVITY$WHITE_RATE
SUMMARY_BY_RUN_SENSITIVITY$INCIDENCE_RATE<-SUMMARY_BY_RUN_SENSITIVITY$WHITE_RATE_FULL + SUMMARY_BY_RUN_SENSITIVITY$BLACK_RATE_FULL
#SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE <- SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE
#SUMMARY_BY_RUN_SENSITIVITY$PREP_COVERAGE <- SUMMARY_BY_RUN_SENSITIVITY$PREP_COVERAGE
#SUMMARY_BY_RUN_SENSITIVITY$RATE_RATIO_CHANGE<-((SUMMARY_BY_RUN_SENSITIVITY$RATE_RATIO-3.818515)/3.818515)
#SUMMARY_BY_RUN_SENSITIVITY$RATE_DIFFERENCE_CHANGE<-((SUMMARY_BY_RUN_SENSITIVITY$RATE_DIFFERENCE-4.503137)/4.503137)

#SUMMARY_BY_RUN_SENSITIVITY$SCENARIO[SUMMARY_BY_RUN_SENSITIVITY$BLACK_PNR==1 & SUMMARY_BY_RUN_SENSITIVITY$WHITE_PNR==1]<-"Black/African American: 1\nWhite: 1"
#SUMMARY_BY_RUN_SENSITIVITY$SCENARIO[SUMMARY_BY_RUN_SENSITIVITY$BLACK_PNR==10 & SUMMARY_BY_RUN_SENSITIVITY$WHITE_PNR==1]<-"Black/African American: 10\nWhite: 1"
#SUMMARY_BY_RUN_SENSITIVITY$SCENARIO[SUMMARY_BY_RUN_SENSITIVITY$BLACK_PNR==1 & SUMMARY_BY_RUN_SENSITIVITY$WHITE_PNR==10]<-"Black/African American: 1\nWhite: 10"
#SUMMARY_BY_RUN_SENSITIVITY$SCENARIO[SUMMARY_BY_RUN_SENSITIVITY$BLACK_PNR==10 & SUMMARY_BY_RUN_SENSITIVITY$WHITE_PNR==10]<-"Black/African American: 10\nWhite: 10"

SUMMARY_BY_RUN_SENSITIVITY$RUN_TYPE[SUMMARY_BY_RUN_SENSITIVITY$BLACK_ADHERENCE.x==0.586]<-"Main Analysis"
SUMMARY_BY_RUN_SENSITIVITY$RUN_TYPE[SUMMARY_BY_RUN_SENSITIVITY$BLACK_ADHERENCE.x==0.911]<-"Sensitivity Analysis"
SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x <- factor(SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x , levels=c("Bas", "90W", "95W", "90A", "95A"))

## SUBSET TO SET 1: VARY ADHERENCE ONLY
HIGH_PREP_SCENARIOS<-SUMMARY_BY_RUN_SENSITIVITY[SUMMARY_BY_RUN_SENSITIVITY$PREP_COVERAGE.x==0.90,]
LOW_PREP_SCENARIOS<-SUMMARY_BY_RUN_SENSITIVITY[SUMMARY_BY_RUN_SENSITIVITY$PREP_COVERAGE.x==0.15,]

p1a<-ggplot() +
  geom_boxplot(data = LOW_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = INCIDENCE_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-0.75, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "ART Scenario", y = "Incidence Rate in Full Population", title = "0.15 PrEP Levels") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 14, colour = "black", face = "bold", hjust=0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

p1b<-ggplot() +
  geom_boxplot(data = HIGH_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = INCIDENCE_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "ART Scenario", y = "Incidence Rate in Full Population", title = "0.90 PrEP Levels") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 14, colour = "black", face = "bold", hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

grid.arrange(p1a, p1b, ncol = 1)



p1c<-ggplot() +
  geom_boxplot(data = LOW_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = BLACK_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-0.75, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "ART Scenario", y = "Incidence Rate in Black MSM", title = "0.15 PrEP Levels") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 14, colour = "black", face = "bold", hjust=0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

p1d<-ggplot() +
  geom_boxplot(data = HIGH_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = BLACK_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "ART Scenario", y = "Incidence Rate in Black MSM", title = "0.90 PrEP Levels") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 14, colour = "black", face = "bold", hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

grid.arrange(p1c, p1d, ncol = 1)





p1e<-ggplot() +
  geom_boxplot(data = LOW_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = WHITE_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-0.75, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "ART Scenario", y = "Incidence Rate in White MSM", title = "0.15 PrEP Levels") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 14, colour = "black", face = "bold", hjust=0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

p1f<-ggplot() +
  geom_boxplot(data = HIGH_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = WHITE_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "ART Scenario", y = "Incidence Rate in White MSM", title = "0.90 PrEP Levels") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 14, colour = "black", face = "bold", hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

grid.arrange(p1e, p1f, ncol = 1)
