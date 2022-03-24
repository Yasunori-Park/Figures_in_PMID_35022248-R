library(readr)
library(ggplot2)
library(reshape2)

#Supplementary Figure 1: List of all Countries flagged in Gene
CRFY <- read_csv("GENECountryFlaggedYear.csv")
View

ggplot(CRFY, mapping = aes(Year, fill=Country))+
  geom_bar(width = 0.9)+
  theme_bw()+
  facet_wrap(~Country, ncol=4, nrow = 8, scales = "fixed")+
  scale_x_continuous(breaks = seq(2007,2018, by=2))+
  scale_y_continuous(breaks = seq(0,26, by = 5))+
  theme(axis.text.x = element_text(size = 10), 
        legend.position = "none")+
  ylab("Problematic Gene Publications (n=)\n")+
  xlab("Publication Year")

#Supplementary Figure 2: List of all Countries flagged in OR
ORFY <- read_csv("ORFlaggedCountryYear.csv")

ggplot(ORFY, mapping = aes(Year, fill=Country))+
  geom_bar(width = 0.6)+
  theme_bw()+
  facet_wrap(~Country, ncol=4, nrow = 5, scales = "fixed")+
  scale_x_continuous(breaks = seq(2014,2018, by=1))+
  scale_y_continuous(breaks = seq(0,100, by = 10))+
  theme(axis.text.x = element_text(size = 10), 
        legend.position = "none")+
  ylab("Problematic Oncology Reports Publications (n=)\n")+
  xlab("Publication Year")

#Supplementary Figure 3: Proportion of Gene Countries to Hospitals
CFY <- read_csv("ChinaWorldGene2.csv")

CFY$Hospital <- (factor(CFY$Hospital,
                        levels = c("No", "Yes")))


ggplot(CFY, mapping = aes(Year, Number, 
                          fill=Hospital))+
  geom_col(width = 0.9)+
  theme_bw()+
  facet_wrap(~Country, ncol=2, nrow = 4, scales = "fixed")+
  scale_x_continuous(breaks = seq(2007,2018, by=1))+
  scale_y_continuous(breaks = seq(0,1, by = 0.25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10), 
        legend.position = "top")+
  ylab("Hospital affiliation status (%)\n")+
  xlab("Publication Year")+
  scale_fill_manual(values = c("#5baede", "#e8b84f"), 
                    labels = c("Publications not from hospitals",
                               "Publications from hospitals"),
                    name = "Affiliation")+
  geom_text(aes(label = Number2), y = -0.03, size = 3)+
  geom_text(aes(label = Number), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3)

#Supplementary Figure 4: Proportion of OR Countries to Hospitals
OFY <- read_csv("ChinaWorldOR2.csv")


OFY$Hospital <- (factor(OFY$Hospital,
                        levels = c("No", "Yes")))


ggplot(OFY, mapping = aes(Year, Number, 
                          fill=Hospital))+
  geom_col(width = 0.9)+
  theme_bw()+
  facet_wrap(~Country, ncol=2, nrow = 4, scales = "fixed")+
  scale_x_continuous(breaks = seq(2014,2018, by=1))+
  scale_y_continuous(breaks = seq(0,1, by = .25))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 10), 
        legend.position = "top")+
  ylab("Hospital affiliation status (%)\n")+
  xlab("Publication Year")+
  scale_fill_manual(values = c("#5baede", "#e8b84f"), 
                    labels = c("Publications not from hospitals",
                               "Publications from hospitals"),
                    name = "Affiliation")+
  geom_text(aes(label = Number2), y = -0.03, size = 3)+
  geom_text(aes(label = Number), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3)
  


