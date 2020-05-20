## Pre-Processing: Load required packages for data processing, analysis, and visualization
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)

## Pre-Processing: Import basic reports for each scenario into lists for data processing
setwd("~/Desktop/research/atlanta.prevention.packages/titan.output/A3/sensitivity.analysis/Aggregate_Basic_Report_Black_A3_Sens/")
FILE_LIST<-list.files(pattern = "*.txt")
BLACK_BASIC_REPORTS<-list()
for (i in 1:length(FILE_LIST)){BLACK_BASIC_REPORTS[[i]]<-read.table(FILE_LIST[i], header = TRUE)}
names(BLACK_BASIC_REPORTS)<-FILE_LIST
BLACK_REPORTS<-rbindlist(BLACK_BASIC_REPORTS, idcol = "origin")
rm(BLACK_BASIC_REPORTS)

setwd("~/Desktop/research/atlanta.prevention.packages/titan.output/A3/sensitivity.analysis/Aggregate_Basic_Report_White_A3_Sens/")
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
BLACK_REPORTS$BLACK_ADHERENCE<-substr(BLACK_REPORTS$origin, 17, 20)


WHITE_REPORTS$PREP_COVERAGE<-as.numeric(substr(WHITE_REPORTS$origin, 1, 4))
WHITE_REPORTS$ART_COVERAGE<-substr(WHITE_REPORTS$origin, 10, 12)
WHITE_REPORTS$BLACK_ADHERENCE<-substr(WHITE_REPORTS$origin, 17, 20)

## Data Analysis: Create key outcome measures for each scenario and run, stratified by race/ethnicity
BLACK_INCIDENCE<-BLACK_REPORTS %>%
  group_by(origin, rseed, add = TRUE) %>%
  summarise(#BLACK_PNR = mean(BLACK_PNR),
            #WHITE_PNR = mean(WHITE_PNR), 
            #BLACK_RETENTION = mean(BLACK_RETENTION),
            PREP_COVERAGE = mean(PREP_COVERAGE),
            ART_COVERAGE = ART_COVERAGE[1],
            BLACK_ADHERENCE = BLACK_ADHERENCE[1],
            BLACK_INFECTIONS = sum(Incid),
            BLACK_NIA = 1738.0400-sum(Incid), 
            BLACK_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            BLACK_RATE_FULL = (sum(Incid)/(sum(17440-HIV)/12))*100,
            BLACK_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-5.9462920)/5.9462920))
BLACK_INCIDENCE$RUN_ID<-1:3595

WHITE_INCIDENCE<-WHITE_REPORTS %>%
  group_by(origin, rseed, add = TRUE) %>%
  summarise(PREP_COVERAGE = mean(PREP_COVERAGE),
            ART_COVERAGE = ART_COVERAGE[1],
            BLACK_ADHERENCE = BLACK_ADHERENCE[1],
            WHITE_INFECTIONS = sum(Incid), 
            WHITE_NIA = 1456.8700-sum(Incid),
            WHITE_RATE = (sum(Incid)/(sum(Total-HIV)/12))*100,
            WHITE_RATE_FULL = (sum(Incid)/(sum(17440-HIV)/12))*100,
            WHITE_RATE_CHANGE = ((((sum(Incid)/(sum(Total-HIV)/12))*100)-1.7069786)/1.7069786))
WHITE_INCIDENCE$RUN_ID<-1:3595

SUMMARY_BY_RUN_SENSITIVITY<-merge(BLACK_INCIDENCE, WHITE_INCIDENCE, by = "RUN_ID")
SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x <- factor(SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x , levels=c("Bas", "90W", "95W", "90A", "95A", "100"))
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

SUMMARY_BY_RUN_SENSITIVITY$RUN_TYPE[SUMMARY_BY_RUN_SENSITIVITY$BLACK_ADHERENCE.x=="Main"]<-"Main Analysis"
SUMMARY_BY_RUN_SENSITIVITY$RUN_TYPE[SUMMARY_BY_RUN_SENSITIVITY$BLACK_ADHERENCE.x=="Sens"]<-"Sensitivity Analysis"
SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x <- factor(SUMMARY_BY_RUN_SENSITIVITY$ART_COVERAGE.x , levels=c("Bas", "90W", "95W", "90A", "95A"))

## SUBSET TO SET 1: VARY ADHERENCE ONLY
HIGH_PREP_SCENARIOS<-SUMMARY_BY_RUN_SENSITIVITY[SUMMARY_BY_RUN_SENSITIVITY$PREP_COVERAGE.x==0.90,]
LOW_PREP_SCENARIOS<-SUMMARY_BY_RUN_SENSITIVITY[SUMMARY_BY_RUN_SENSITIVITY$PREP_COVERAGE.x==0.15,]

SENSITIVITY_RESULTS_SUMMARY <- SUMMARY_BY_RUN_SENSITIVITY %>%
  group_by(PREP_COVERAGE.x, ART_COVERAGE.x, RUN_TYPE) %>%
  summarise(BLACK_INFECTIONS_MEAN = mean(BLACK_INFECTIONS), BLACK_INFECTIONS_LL = quantile(BLACK_INFECTIONS, probs = 0.025), BLACK_INFECTIONS_UL = quantile(BLACK_INFECTIONS, probs = 0.975),
            BLACK_NIA_MEAN = mean(BLACK_NIA), BLACK_NIA_LL = quantile(BLACK_NIA, probs = 0.025), BLACK_NIA_UL = quantile(BLACK_NIA, probs = 0.975),
            BLACK_RATE_MEAN = mean(BLACK_RATE), BLACK_RATE_LL = quantile(BLACK_RATE, probs = 0.025), BLACK_RATE_UL = quantile(BLACK_RATE, probs = 0.975),
            BLACK_RATE_CHANGE_MEAN = mean(BLACK_RATE_CHANGE), BLACK_RATE_CHANGE_LL = quantile(BLACK_RATE_CHANGE, probs = 0.025), BLACK_RATE_CHANGE_UL = quantile(BLACK_RATE_CHANGE, probs = 0.975),
            WHITE_INFECTIONS_MEAN = mean(WHITE_INFECTIONS), WHITE_INFECTIONS_LL = quantile(WHITE_INFECTIONS, probs = 0.025), WHITE_INFECTIONS_UL = quantile(WHITE_INFECTIONS, probs = 0.975),
            WHITE_NIA_MEAN = mean(WHITE_NIA), WHITE_NIA_LL = quantile(WHITE_NIA, probs = 0.025), WHITE_NIA_UL = quantile(WHITE_NIA, probs = 0.975),
            WHITE_RATE_MEAN = mean(WHITE_RATE), WHITE_RATE_LL = quantile(WHITE_RATE, probs = 0.025), WHITE_RATE_UL = quantile(WHITE_RATE, probs = 0.975),
            WHITE_RATE_CHANGE_MEAN = mean(WHITE_RATE_CHANGE), WHITE_RATE_CHANGE_LL = quantile(WHITE_RATE_CHANGE, probs = 0.025), WHITE_RATE_CHANGE_UL = quantile(WHITE_RATE_CHANGE, probs = 0.975))
            

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
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, colour = "black", face = "bold"),
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
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

grid.arrange(p1a, p1b, ncol = 1)



p1c<-ggplot() +
  geom_boxplot(data = LOW_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = BLACK_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-0.75, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "", y = "HIV incidence (per 100 person-years)", title = "Black/African American MSM (15% PrEP Use)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

p1d<-ggplot() +
  geom_boxplot(data = HIGH_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = BLACK_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in PrEP Adherence", "No Disparities in PrEP Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "Treatment Scenario", y = "HIV incidence (per 100 person-years)", title = "Black/African American MSM (90% PrEP Use)") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

grid.arrange(p1c, p1d, ncol = 1)





p1e<-ggplot() +
  geom_boxplot(data = LOW_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = WHITE_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-0.75, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "", y = "", title = "White MSM (15% PrEP Use)") +
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
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

p1f<-ggplot() +
  geom_boxplot(data = HIGH_PREP_SCENARIOS, aes(x = ART_COVERAGE.x, y = WHITE_RATE, fill = RUN_TYPE), color = "black") +
  #scale_y_continuous(labels = percent, limits = c(-1, 0.5)) + 
  scale_fill_manual(name = "Analysis Type", labels = c("'As Observed' Disparities in Adherence", "No Disparities in Adherence"), values = c("#1ECFD6", "#EDD170")) +
  labs(x = "Treatment Scenario", y = "", title = "White MSM (90% PrEP Use)") +
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
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 14, colour = "black", face = "bold"),
        axis.text = element_text(size = 14, colour = "black"))

grid.arrange(p1c, p1e, p1d, p1f, ncol = 2)

legend <- get_legend(p1d)
p1d <- p1d + theme(legend.position="none")
p1f <- p1f + theme(legend.position="none")

grid.arrange(arrangeGrob(p1c, left = textGrob("A)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))), 
             arrangeGrob(p1e, left = textGrob("B)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             arrangeGrob(p1d, left = textGrob("C)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             arrangeGrob(p1f, left = textGrob("D)", x = unit(1, "npc"), 
                                              y = unit(.95, "npc"),gp=gpar(fontsize=20))),
             legend, ncol=2, nrow=3,
             layout_matrix = rbind(c(1,2), c(3,4), c(5,5)),
             widths = c(2.7, 2.7), heights = c(2.5, 2.5, 0.4))
