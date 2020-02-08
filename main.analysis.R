## Pre-Processing: Load required packages for data processing, analysis, and visualization
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)

## Pre-Processing: Import basic reports for each scenario into lists for data processing
setwd("~/Desktop/research/atlanta_prevention_packages/titan.output/main.analysis/Aggregate_Basic_Report_Black/")
FILE_LIST<-list.files(pattern = "*.txt")
BLACK_BASIC_REPORTS<-list()
for (i in 1:length(FILE_LIST)){BLACK_BASIC_REPORTS[[i]]<-read.table(FILE_LIST[i], header = TRUE)}
names(BLACK_BASIC_REPORTS)<-FILE_LIST
BLACK_REPORTS<-rbindlist(BLACK_BASIC_REPORTS, idcol = "origin")
rm(BLACK_BASIC_REPORTS)

setwd("~/Desktop/research/atlanta_prevention_packages/titan.output/main.analysis/Aggregate_Basic_Report_White/")
WHITE_BASIC_REPORTS<-list()
for (i in 1:length(FILE_LIST)){WHITE_BASIC_REPORTS[[i]]<-read.table(FILE_LIST[i], header = TRUE)}
names(WHITE_BASIC_REPORTS)<-FILE_LIST
WHITE_REPORTS<-rbindlist(WHITE_BASIC_REPORTS, idcol = "origin")
rm(WHITE_BASIC_REPORTS)
rm(FILE_LIST)
rm(i)

## Pre-Processing: Create fields with explicit PrEP and ART levels for subsequent grouping, forgoe Will's PNR field
BLACK_REPORTS$PREP_COVERAGE<-substr(BLACK_REPORTS$origin, 1, 4)
BLACK_REPORTS$PREP_COVERAGE <- gsub("P","0", BLACK_REPORTS$PREP_COVERAGE)
#BLACK_REPORTS$BLACK_PNR<-recode(BLACK_REPORTS$PREP_COVERAGE,
 #                               "0.00 = 0; 0.15 = 1; 0.30 = 2; 0.45 = 3; 0.60 = 4; 0.75 = 5; 0.90 = 6")
BLACK_REPORTS$ART_COVERAGE<-substr(BLACK_REPORTS$origin, 9, 13)
BLACK_REPORTS$ART_COVERAGE <- gsub("90Whi","90White", BLACK_REPORTS$ART_COVERAGE)
BLACK_REPORTS$ART_COVERAGE <- gsub("95Whi","95White", BLACK_REPORTS$ART_COVERAGE)
BLACK_REPORTS$ART_COVERAGE <- gsub("Basel","Base", BLACK_REPORTS$ART_COVERAGE)
BLACK_REPORTS$ART_COVERAGE <- gsub("_90Al","90All", BLACK_REPORTS$ART_COVERAGE)
BLACK_REPORTS$ART_COVERAGE <- gsub("_95Al","95All", BLACK_REPORTS$ART_COVERAGE)
BLACK_REPORTS$ART_COVERAGE <- gsub("_90Wh","90White", BLACK_REPORTS$ART_COVERAGE)
BLACK_REPORTS$ART_COVERAGE <- gsub("_95Wh","95White", BLACK_REPORTS$ART_COVERAGE)
BLACK_REPORTS$ART_COVERAGE <- gsub("_Base","Base", BLACK_REPORTS$ART_COVERAGE)

WHITE_REPORTS$PREP_COVERAGE<-substr(WHITE_REPORTS$origin, 1, 4)
WHITE_REPORTS$PREP_COVERAGE <- gsub("P","0", WHITE_REPORTS$PREP_COVERAGE)
#WHITE_REPORTS$BLACK_PNR<-recode(WHITE_REPORTS$PREP_COVERAGE,
#                               "0.00 = 0; 0.15 = 1; 0.30 = 2; 0.45 = 3; 0.60 = 4; 0.75 = 5; 0.90 = 6")
WHITE_REPORTS$ART_COVERAGE<-substr(WHITE_REPORTS$origin, 9, 13)
WHITE_REPORTS$ART_COVERAGE <- gsub("90Whi","90White", WHITE_REPORTS$ART_COVERAGE)
WHITE_REPORTS$ART_COVERAGE <- gsub("95Whi","95White", WHITE_REPORTS$ART_COVERAGE)
WHITE_REPORTS$ART_COVERAGE <- gsub("Basel","Base", WHITE_REPORTS$ART_COVERAGE)
WHITE_REPORTS$ART_COVERAGE <- gsub("_90Al","90All", WHITE_REPORTS$ART_COVERAGE)
WHITE_REPORTS$ART_COVERAGE <- gsub("_95Al","95All", WHITE_REPORTS$ART_COVERAGE)
WHITE_REPORTS$ART_COVERAGE <- gsub("_90Wh","90White", WHITE_REPORTS$ART_COVERAGE)
WHITE_REPORTS$ART_COVERAGE <- gsub("_95Wh","95White", WHITE_REPORTS$ART_COVERAGE)
WHITE_REPORTS$ART_COVERAGE <- gsub("_Base","Base", WHITE_REPORTS$ART_COVERAGE)

## Storage from Will's code
## Pre-Processing: Create field to represent the PrEP-to-need ratio in each group
#BLACK_REPORTS$PREP_COVERAGE<-as.character(substr(BLACK_REPORTS$origin, 19, 23))
#BLACK_REPORTS$WHITE_PNR<-recode(BLACK_REPORTS$PREP_COVERAGE,
   #                             "0.0 = 0; 0.15 = 1; 0.30 = 2; 0.45 = 3; 0.60 = 4; 0.75 = 5; 0.90 = 6")
#BLACK_REPORTS$WHITE_PNR[BLACK_REPORTS$WHITE_PNR==0.05]<-3
#BLACK_REPORTS$WHITE_PNR[BLACK_REPORTS$WHITE_PNR==0.1]<-6

## Data Analysis: Create key outcome measures for each scenario and run, stratified by race/ethnicity
BLACK_INCIDENCE<-BLACK_REPORTS %>%
  group_by(PREP_COVERAGE, ART_COVERAGE, rseed, add = TRUE) %>%
  summarise(#BLACK_PNR = mean(BLACK_PNR),
            #WHITE_PNR = mean(WHITE_PNR), 
            BLACK_INFECTIONS = sum(Incid), 
            BLACK_NIA = 1742.31-sum(Incid), #mean number of infections @ baseline art and zero prep (1863.45)
            BLACK_NNT = (sum(PrEP)/12)/BLACK_NIA,
            BLACK_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            BLACK_PREVALENT_INF_CHANGE = ((HIV[n()]+AIDS[n()]) - ((HIV[1]+AIDS[1]))),
            BLACK_PREVALENCE_CHANGE = (((HIV[n()]+AIDS[n()])/Total[n()]) - ((HIV[1]+AIDS[1])/Total[1])),
            BLACK_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-5.953189)/5.953189)) #mean incidence rate at baseline art and zero prep (6.103133)
BLACK_INCIDENCE$RUN_ID<-1:nrow(BLACK_INCIDENCE)

WHITE_INCIDENCE<-WHITE_REPORTS %>%
  group_by(PREP_COVERAGE, ART_COVERAGE, rseed, add = TRUE) %>%
  summarise(WHITE_INFECTIONS = sum(Incid), 
            WHITE_NIA = 1453.71-sum(Incid), #1390.94
            WHITE_NNT = (sum(PrEP)/12)/WHITE_NIA,
            WHITE_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            WHITE_PREVALENT_INF_CHANGE = ((HIV[n()]+AIDS[n()]) - ((HIV[1]+AIDS[1]))),
            WHITE_PREVALENCE_CHANGE = (((HIV[n()]+AIDS[n()])/Total[n()]) - ((HIV[1]+AIDS[1])/Total[1])),
            WHITE_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-1.7028580)/1.7028580)) #1.599996
WHITE_INCIDENCE$RUN_ID<-1:nrow(WHITE_INCIDENCE)

## Data Analysis: Create key outcome measures for each scenario, stratified by race/ethnicity
SUMMARY_BY_RUN_MAIN<-merge(BLACK_INCIDENCE, WHITE_INCIDENCE, by = "RUN_ID")
SUMMARY_BY_RUN_MAIN <- SUMMARY_BY_RUN_MAIN[-c(12,13,14)]
SUMMARY_BY_RUN_MAIN$ART_COVERAGE.x <- factor(SUMMARY_BY_RUN_MAIN$ART_COVERAGE.x , levels=c("Base", "90White", "95White", "90All", "95All"))
SUMMARY_BY_RUN_MAIN<-SUMMARY_BY_RUN_MAIN[!(SUMMARY_BY_RUN_MAIN$PREP_COVERAGE=="0.00"),]

rm(BLACK_REPORTS)
rm(WHITE_REPORTS)
rm(BLACK_INCIDENCE)
rm(WHITE_INCIDENCE)

SUMMARY_BY_RUN_MAIN$RATE_RATIO<-SUMMARY_BY_RUN_MAIN$BLACK_RATE/SUMMARY_BY_RUN_MAIN$WHITE_RATE
SUMMARY_BY_RUN_MAIN$RATE_DIFFERENCE<-SUMMARY_BY_RUN_MAIN$BLACK_RATE-SUMMARY_BY_RUN_MAIN$WHITE_RATE
SUMMARY_BY_RUN_MAIN$PREVALENCE_CHANGE<-(SUMMARY_BY_RUN_MAIN$BLACK_PREVALENT_INF_CHANGE+SUMMARY_BY_RUN_MAIN$WHITE_PREVALENT_INF_CHANGE)/17440
SUMMARY_BY_RUN_MAIN$RATE_RATIO_CHANGE<-((SUMMARY_BY_RUN_MAIN$RATE_RATIO-3.499277)/3.499277) #3.818515
SUMMARY_BY_RUN_MAIN$RATE_DIFFERENCE_CHANGE<-((SUMMARY_BY_RUN_MAIN$RATE_DIFFERENCE-4.2503308)/4.2503308) #4.503137
SUMMARY_BY_RUN_MAIN$NNT_TOTAL<-SUMMARY_BY_RUN_MAIN$WHITE_NNT + SUMMARY_BY_RUN_MAIN$BLACK_NNT

MAIN_RESULTS_SUMMARY<-SUMMARY_BY_RUN_MAIN %>%
  group_by(PREP_COVERAGE.x, ART_COVERAGE.x) %>%
  summarise(#BLACK_PNR = mean(BLACK_PNR), WHITE_PNR = mean(WHITE_PNR),
            BLACK_INFECTIONS_MEAN = mean(BLACK_INFECTIONS), BLACK_INFECTIONS_LL = quantile(BLACK_INFECTIONS, probs = 0.025), BLACK_INFECTIONS_UL = quantile(BLACK_INFECTIONS, probs = 0.975),
            BLACK_NIA_MEAN = mean(BLACK_NIA), BLACK_NIA_LL = quantile(BLACK_NIA, probs = 0.025), BLACK_NIA_UL = quantile(BLACK_NIA, probs = 0.975),
            BLACK_NNT_MEAN = mean(BLACK_NNT), BLACK_NNT_LL = quantile(BLACK_NNT, probs = 0.025), BLACK_NNT_UL = quantile(BLACK_NNT, probs = 0.975),
            BLACK_RATE_MEAN = mean(BLACK_RATE), BLACK_RATE_LL = quantile(BLACK_RATE, probs = 0.025), BLACK_RATE_UL = quantile(BLACK_RATE, probs = 0.975),
            BLACK_RATE_CHANGE_MEAN = mean(BLACK_RATE_CHANGE), BLACK_RATE_CHANGE_LL = quantile(BLACK_RATE_CHANGE, probs = 0.025), BLACK_RATE_CHANGE_UL = quantile(BLACK_RATE_CHANGE, probs = 0.975),
            BLACK_PREVALENCE_CHANGE_MEAN = mean(BLACK_PREVALENCE_CHANGE), BLACK_PREVALENCE_CHANGE_LL = quantile(BLACK_PREVALENCE_CHANGE, probs = 0.025), BLACK_PREVALENCE_CHANGE_UL = quantile(BLACK_PREVALENCE_CHANGE, probs = 0.975),
            WHITE_INFECTIONS_MEAN = mean(WHITE_INFECTIONS), WHITE_INFECTIONS_LL = quantile(WHITE_INFECTIONS, probs = 0.025), WHITE_INFECTIONS_UL = quantile(WHITE_INFECTIONS, probs = 0.975),
            WHITE_NIA_MEAN = mean(WHITE_NIA), WHITE_NIA_LL = quantile(WHITE_NIA, probs = 0.025), WHITE_NIA_UL = quantile(WHITE_NIA, probs = 0.975),
            WHITE_NNT_MEAN = mean(WHITE_NNT), WHITE_NNT_LL = quantile(WHITE_NNT, probs = 0.025), WHITE_NNT_UL = quantile(WHITE_NNT, probs = 0.975),
            WHITE_RATE_MEAN = mean(WHITE_RATE), WHITE_RATE_LL = quantile(WHITE_RATE, probs = 0.025), WHITE_RATE_UL = quantile(WHITE_RATE, probs = 0.975),
            WHITE_RATE_CHANGE_MEAN = mean(WHITE_RATE_CHANGE), WHITE_RATE_CHANGE_LL = quantile(WHITE_RATE_CHANGE, probs = 0.025), WHITE_RATE_CHANGE_UL = quantile(WHITE_RATE_CHANGE, probs = 0.975),
            WHITE_PREVALENCE_CHANGE_MEAN = mean(WHITE_PREVALENCE_CHANGE), WHITE_PREVALENCE_CHANGE_LL = quantile(WHITE_PREVALENCE_CHANGE, probs = 0.025), WHITE_PREVALENCE_CHANGE_UL = quantile(WHITE_PREVALENCE_CHANGE, probs = 0.975),
            RATE_RATIO_MEAN = mean(RATE_RATIO), RATE_RATIO_LL = quantile(RATE_RATIO, probs = 0.025), RATE_RATIO_UL = quantile(RATE_RATIO, probs = 0.975),
            RATE_RATIO_CHANGE_MEAN = mean(RATE_RATIO_CHANGE), RATE_RATIO_CHANGE_LL = quantile(RATE_RATIO_CHANGE, probs = 0.025), RATE_RATIO_CHANGE_UL = quantile(RATE_RATIO_CHANGE, probs = 0.975),
            RATE_DIFFERENCE_MEAN = mean(RATE_DIFFERENCE), RATE_DIFFERENCE_LL = quantile(RATE_DIFFERENCE, probs = 0.025), RATE_DIFFERENCE_UL = quantile(RATE_DIFFERENCE, probs = 0.975),
            RATE_DIFFERENCE_CHANGE_MEAN = mean(RATE_DIFFERENCE_CHANGE), RATE_DIFFERENCE_CHANGE_LL = quantile(RATE_DIFFERENCE_CHANGE, probs = 0.025), RATE_DIFFERENCE_CHANGE_UL = quantile(RATE_DIFFERENCE_CHANGE, probs = 0.975),
            PREVALENCE_CHANGE_MEAN = mean(PREVALENCE_CHANGE), PREVALENCE_CHANGE_LL = quantile(PREVALENCE_CHANGE, probs = 0.025), PREVALENCE_CHANGE_UL = quantile(PREVALENCE_CHANGE, probs = 0.975),
            NNT_TOTAL = mean(NNT_TOTAL), NNT_TOTAL_LL = quantile(NNT_TOTAL, probs = 0.025), NNT_TOTAL_UL = quantile(NNT_TOTAL, probs = 0.975)
            )
MAIN_RESULTS_SUMMARY$ART_COVERAGE.x <- factor(MAIN_RESULTS_SUMMARY$ART_COVERAGE.x , levels=c("Base", "90White", "95White", "90All", "95All"))
MAIN_RESULTS_SUMMARY<-MAIN_RESULTS_SUMMARY[2:37]
#MAIN_RESULTS_SUMMARY$BLACK_PNR<-as.factor(MAIN_RESULTS_SUMMARY$BLACK_PNR)
#MAIN_RESULTS_SUMMARY$WHITE_PNR<-as.factor(MAIN_RESULTS_SUMMARY$WHITE_PNR)


## Data Visualization: Create figures summarizing each outcome measure
### Change in Incidence Rate stratified by race and by all scenarios
p1<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN, aes(x = PREP_COVERAGE.x, y = BLACK_RATE_CHANGE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  scale_y_continuous(labels = percent, limits = c(-1, 0.3)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("Baseline", "90White", "95White", "90All", "95All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170")) +
  labs(x = "PrEP Coverage", y = "Change in Incidence Rate\n Black MSM") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "right",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

p2<-ggplot() +
  geom_boxplot(data = SUMMARY_BY_RUN_MAIN, aes(x = PREP_COVERAGE.x, y = WHITE_RATE_CHANGE, fill = ART_COVERAGE.x), color = "black", size=0.6) +
  scale_y_continuous(labels = percent, limits = c(-1, 0.3)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("Baseline", "90White", "95White", "90All", "95All"), values = c("#C05640", "#003D73", "#0878A4", "#1ECFD6", "#EDD170")) +
  labs(x = "PrEP Coverage", y = "Change in Incidence Rate\n White MSM") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "right",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "vertical",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

grid.arrange(p1, p2, ncol = 2)

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
                       limits = c(0.01501537, 0.5711334), breaks = c(0.01501537, 0.2930744, 0.5711334), labels = percent, midpoint = 0.2930744,
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
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

### Change in Incidence Rate Difference Between Race Groups
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = RATE_DIFFERENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nIncidence Rate Difference",
                       limits = c(-0.8015579, 0.02079265), breaks = c(-0.8015579, -0.3903822, 0.02079265), labels = percent, midpoint = -0.3903822,
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
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

### Change in HIV Prevalence among Black MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_PREVALENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nHIV Prevalence",
                       limits = c(min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN), max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN), (min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN))/2, max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$BLACK_PREVALENCE_CHANGE_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in HIV Prevalence Black MSM") +
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

### Change in HIV Prevalence among White MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_PREVALENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nHIV Prevalence",
                       limits = c(min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN), max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN), (min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN))/2, max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$WHITE_PREVALENCE_CHANGE_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in HIV Prevalence White MSM") +
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

### Change in HIV Prevalence among Full Population
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = PREVALENCE_CHANGE_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "Change in\nHIV Prevalence",
                       limits = c(min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN), max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN), (min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN))/2, max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN) + max(MAIN_RESULTS_SUMMARY$PREVALENCE_CHANGE_MEAN))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="Change in HIV Prevalence Full Population") +
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

### NNT among Black MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = BLACK_NNT_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "NNT",
                       limits = c(min(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN), max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN), (min(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN) + max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN))/2, max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN) + max(MAIN_RESULTS_SUMMARY$BLACK_NNT_MEAN))/2,
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
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

### NNT among White MSM
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = WHITE_NNT_MEAN), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "NNT",
                       limits = c(min(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN), max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN)), breaks = c(min(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN), (min(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN) + max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN))/2, max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN) + max(MAIN_RESULTS_SUMMARY$WHITE_NNT_MEAN))/2,
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
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 16, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5))

### NNT among Full Population
ggplot() +
  geom_tile(data = MAIN_RESULTS_SUMMARY, aes(x = ART_COVERAGE.x, y = PREP_COVERAGE.x, fill = NNT_TOTAL), colour = "black", size = 0.3) +
  scale_fill_gradient2(name = "NNT",
                       limits = c(min(MAIN_RESULTS_SUMMARY$NNT_TOTAL), max(MAIN_RESULTS_SUMMARY$NNT_TOTAL)), breaks = c(min(MAIN_RESULTS_SUMMARY$NNT_TOTAL), (min(MAIN_RESULTS_SUMMARY$NNT_TOTAL) + max(MAIN_RESULTS_SUMMARY$NNT_TOTAL))/2, max(MAIN_RESULTS_SUMMARY$NNT_TOTAL)), labels = percent, midpoint = (min(MAIN_RESULTS_SUMMARY$NNT_TOTAL) + max(MAIN_RESULTS_SUMMARY$NNT_TOTAL))/2,
                       low = "#003D73", mid = "#1ECFD6", high = "#EDD170", guide = guide_colorbar(frame.colour = "black")) +
  labs(x = "Treatment Scenario", y = "PrEP Coverage", title="NNT Full Population") +
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

