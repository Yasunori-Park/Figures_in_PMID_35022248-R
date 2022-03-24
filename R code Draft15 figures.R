#Other Tables Draft 10 
library(readr)
library(ggplot2)
library(waffle)
library(reshape2)
library(scales)
library(eulerr)
library(data.table)

#Figure 2- ErrorProportions of all Collections
#Files in R_WD > Casestudy > Error Proportion
#Change colours to be consistent
C <- read_csv("ErrorProportionSeriesJournal.csv")
View(C)

C2 <- melt(C, id.vars = "Corpus", measure.vars = c("GoF", "LoF", "CoF"))
View(C2)

View(C)
View(C2)

C2$Corpus <- (factor(C2$Corpus,
                     levels = c("Single Gene Knockdown (n = 115)",
                                "miR-145 (n = 49)",
                                "Drug (n = 109)",
                                "Gene (n = 279)",
                                "Oncology Reports (n = 995)")))

ggplot(C2, mapping = aes(Corpus, value, fill = variable, 
                         label = (round(value, digits = 2))))+
  geom_col(position = "stack", width = 0.4, alpha = 1)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+ 
  ylab("Proportion of Error Types (%)\n")+
  xlab("")+
  scale_x_discrete(labels = c("Single Gene Knockdown\n(n = 115)",
                              "miR-145\n(n = 49)",
                              "C+G\n(n = 109)",
                              "Gene\n(n = 284)",
                              "Oncology Reports\n(n = 995)"))+
  geom_text(aes(label=sprintf("%1.f%%", value)), 
            position = position_stack(vjust = 0.5))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12), 
        legend.position = "top")+
  scale_fill_manual(values = c("#58d690", "#5baede", "#e8b84f"), 
                    name = "Error Types", 
                    labels = c(
                      "'Non-targeting' yet targeting",
                      "'Targeting' yet non-targeting",
                      "Targeting wrong gene or genomic sequence"))

#Fig 3
#Pie Chart of repeated sequences 

#SGK = 67 Repeated Sequences, 48 Unique, Total = 115
#SGKPercentage = 58% Repeated, 42% Unique = 100%
#mIR-145 = 6 Repeated Sequences, 43 Unique, Total = 49
#miR-145Percentage = 12% Repeated, 88% Unique = 100%
#Drug = 2 Repeated Sequences, 107 Unique, Total = 109
#Drug Percentage = 2% Repeated, 98% Unique = 100%
#Gene = 23 Repeated Sequences, 261 Unique, Total = 284
#Gene Percentage = 8% Repeated, 92% Unique = 100%
#Oncology Reports = 179 Repeated Sequences, 816 Unique, Total = 995
#OR Percentage = 18% Repeated, 82% Unique



APDPerc <- data.frame(
  Corpus = c("SGK", "miR-145", "C+G", "Gene", "Oncology Reports"),
  RawN = c("n = 115", "n = 49", "n = 109", "n = 284", "n = 995", 
           "n = 115", "n = 49", "n = 109", "n = 284", "n = 995"),
  Category = c("Repeated Sequences", "Unique Sequences",
               "Repeated Sequences", "Unique Sequences",
               "Repeated Sequences", "Unique Sequences",
               "Repeated Sequences", "Unique Sequences",
               "Repeated Sequences", "Unique Sequences"),
  Count = c(58, 88, 2, 92, 18,
            42, 12, 98, 8, 82),
  Countlabel = c("58%", NA, "2%", NA, "18%", NA,
                 "12%", NA, "8%", NA)
)

View(APD)

APDPerc$Corpus <- (factor(APDPerc$Corpus,
                          levels = c("SGK", 
                                     "miR-145", 
                                     "C+G",
                                     "Gene", 
                                     "Oncology Reports")))
base = 3.5
expand = 1.7

ggplot(APDPerc, aes(x = "", y = Count, fill = factor(Category))) + 
  geom_col(width = 1, position = "stack") +
  facet_wrap( ~ Corpus, ncol=5)+
  theme_void()+
  theme(legend.position = "bottom")+
  scale_fill_manual(labels = c("Repeated incorrect sequences", 
                               "Unique incorrect sequences"),
                    values = c("#e8b84f", "#5baede"))+
  labs(fill = "")+
  coord_polar(theta = "y")+
  geom_text(aes(x=1.7, label=Countlabel), 
            position = position_stack(vjust = 0.8),
            size = 4)+
  geom_text(aes(label = RawN), 
            x = -Inf, y = Inf, hjust = 0.5, vjust = 7.9)


#Figure 4- Affiliation of problematic authors China vs World OR + G
#Need to add scales::percentage for stacked to 100%
L <- data.frame("AllG07" = c(91.53, 8.47),
                "ChinaG07" = c(21.73, 78.26),
                "AllG" = c(96.88, 3.13),
                "ChinaG" = c(22.22, 77.78),
                "AllOR" = c(88.37, 11.63),
                "ChinaOR" = c(12.98, 87.02),
                "Affil" = c("N", "Y"))
View(L)

M <- melt(L, id.vars = "Affil", measure.vars = c("AllG07", "ChinaG07",
                                                 "AllG", "ChinaG", 
                                                 "AllOR","ChinaOR"))

M$Label = c("Gene\n(2007-2018)", "Gene\n(2007-2018)", "Gene\n(2007-2018)", 
            "Gene\n(2007-2018)",
            "Gene\n(2014-2018)", "Gene\n(2014-2018)", "Gene\n(2014-2018)",
            "Gene\n(2014-2018)", 
            "Oncology Reports\n(2014-2018)", "Oncology Reports\n(2014-2018)", 
            "Oncology Reports\n(2014-2018)", "Oncology Reports\n(2014-2018)")
View(M)

ggplot(M, mapping = aes(variable, value, fill = Affil))+
  geom_col(width = 0.4, alpha = 0.85)+
  geom_text(aes(label=sprintf("%1.f%%", value/1)), colour = "Black", 
            position = position_stack(vjust = 0.5), size = 3.5)+
  facet_wrap(~Label, scales = "free_x")+
  xlab("\nAffiliated Country") + 
  ylab("Hospital affiliation status (%)")+
  scale_y_continuous(breaks = seq(0, 100, by = 5)) +
  scale_x_discrete(breaks = levels(factor(M$variable)), 
                   labels = c("All other Countries \n(n = 59)",
                              "China \n(n = 69)",
                              "All other Countries \n(n = 32)",
                              "China \n(n = 63)", 
                              "All other Countries \n(n = 43)",
                              "China \n(n = 393)"))+
  scale_fill_manual(values = c("#5baede", "#e8b84f"), 
                    labels = c("Publications not from hospitals", 
                               "Publications from hospitals"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank()) + 
  labs(fill = "")


#Figure 5 - Error rate in G and OR by China vs World
#File is in "Gene" R_WD folder
#need to fix second geom_text lbl values
GvORCountry <- read_csv("GvORErrorvCountryTrial.csv")
View(GvORCountry)

ggplot(data = GvORCountry, mapping = aes(Year, PERC, label=sprintf("%0.1f", round(PERC, digits = 1)),
                                         fill = Corp))+
  geom_col(position = "stack", alpha = 1, width = 0.9)+
  facet_wrap(~Journal, scales = "free_x")+
  geom_text(colour = "Black", 
            position = position_stack(vjust = 0.65), size = 3.1)+
  xlab("\nPublication Year") + 
  ylab("Problematic/ Original papers (%)\n")+
  scale_y_continuous(breaks = seq(0, 13, by = 1), expand = c(0.1, 0)) +
  scale_x_continuous(breaks = seq(2007, 2018, by = 1)) + 
  scale_fill_manual(values = c("#5baede", "#e8b84f","#5baede", "#e8b84f"),
                    name = "Country of Origin",
                    labels = c("All other Countries",
                               "China",
                               "China",
                               "China (Oncology Reports)"))+
  theme_bw()+
  theme(legend.position = "top", legend.title = element_blank())+
  geom_text(aes(label = lbl), y = -0.5, size = 3.5)