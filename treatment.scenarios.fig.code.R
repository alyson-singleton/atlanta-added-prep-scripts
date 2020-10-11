## Pre-Processing: Load required packages for data processing, analysis, and visualization
library(ggplot2)
library(grid)
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# baseline parameters
treatment.prep.data <- data.frame(c(.818,.488,.432, .655,.431,.352), 
                                  c("White MSM", "White MSM", "White MSM", "Black/African American MSM", "Black/African American MSM", "Black/African American MSM"),
                                  c("Diagnosed","ART\nUse","Virally\nSuppressed", "Diagnosed","ART\nUse","Virally\nSuppressed"))
colnames(treatment.prep.data) <- c("Percentages", "Race", "Continuum")
# 909090 treatment scenario
treatment.prep.data.90 <- data.frame(c(.90001,.81,.729, .90,.81,.729), 
                                     c("White MSM", "White MSM", "White MSM", "Black/African American MSM", "Black/African American MSM", "Black/African American MSM"),
                                     c("Diagnosed","ART\nUse","Virally\nSuppressed", "Diagnosed","ART\nUse","Virally\nSuppressed"))
colnames(treatment.prep.data.90) <- c("Percentages", "Race", "Continuum")

# 959595 treatment scenario
treatment.prep.data.95 <- data.frame(c(.95001,.9025,.857, .95,.9025,.857), 
                                     c("White MSM", "White MSM", "White MSM", "Black/African American MSM", "Black/African American MSM", "Black/African American MSM"),
                                     c("Diagnosed","ART\nUse","Virally\nSuppressed", "Diagnosed","ART\nUse","Virally\nSuppressed"))
colnames(treatment.prep.data.95) <- c("Percentages", "Race", "Continuum")


f1.1 <- ggplot(treatment.prep.data) + 
  geom_bar(aes(x= reorder(Continuum,-Percentages), y=Percentages, fill=reorder(Race,-Percentages)), alpha=0.6,
           stat="identity", position=position_dodge(width = 0.9), color="black") + 
  scale_fill_grey(start=0.3, end=0.8, name="", guide = guide_legend(), labels = c("White MSM","Black/African American MSM")) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  labs(x="", y="Percent of all Agents with HIV infection", title="Current Levels") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 11, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

f1.2 <- ggplot(treatment.prep.data.90) + 
  geom_bar(aes(x= reorder(Continuum,-Percentages), y=Percentages, fill=reorder(Race,-Percentages)), alpha=0.6, 
           stat="identity", position=position_dodge(width = 0.9), color="black") + 
  scale_fill_grey(start=0.3, end=0.8, name="", guide = guide_legend(), labels = c("White MSM","Black/African American MSM")) +
  labs(x="", y="", title="90-90-90") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 11, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))

f1.3 <- ggplot(treatment.prep.data.95) + 
  geom_bar(aes(x= reorder(Continuum,-Percentages), y=Percentages, fill=reorder(Race,-Percentages)), alpha=0.6, 
           stat="identity", position=position_dodge(width = 0.9), color="black") + 
  scale_fill_grey(start=0.3, end=0.8, name="", guide = guide_legend(), labels = c("White MSM","Black/African American MSM")) +
  labs(x="", y="", title="95-95-95") +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.direction = "horizontal",
        legend.box.just = "center",
        legend.spacing.x = unit(0.5, 'cm'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title = element_text(size = 16, colour = "black", face = "bold"),
        axis.text = element_text(size = 11, colour = "black"),
        plot.title = element_text(size = 16, colour = "black", face = "bold", hjust=0.5, vjust = 0.8))


legend <- get_legend(f1.1)
f1.1 <- f1.1 + theme(legend.position="none")
grid.arrange(arrangeGrob(f1.1, left = textGrob("A)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=16))), 
             arrangeGrob(f1.2, left = textGrob("B)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=16))),
             arrangeGrob(f1.3, left = textGrob("C)", x = unit(1, "npc"), 
                                               y = unit(.95, "npc"),gp=gpar(fontsize=16))),
             legend, ncol=3, nrow=2,
             layout_matrix = rbind(c(1,2,3), c(4,4,4)),
             widths = c(2.7, 2.7, 2.7), heights = c(2.5, 0.2))

