library(ggplot2)
library(reshape2)

#Load data from analysis separately done on excel
A <- data.frame(Corpus = c("Single Gene Knockdown \n(n = 115)", 
                           "miR-145 \n(n = 49)", 
                           "C+G \n(n = 109)", 
                           "Gene \n(n = 284)", 
                           "Oncology Reports \n(n = 995)"), 
                GOF = c(38, 4, 6, 3, 3),
                LOF = c(12, 18, 22, 15, 34),
                COF = c(50, 78, 72, 82, 63))
View(A)

#Melt dataframe for position stack
A2 <- melt(A, id.vars = "Corpus", measure.vars = c("GOF", "LOF", "COF"))
View(A2)

#Force levels to arrange corpora in order
A2$Corpus <- (factor(A2$Corpus,
                     levels = c("Single Gene Knockdown \n(n = 115)",
                                "miR-145 \n(n = 49)",
                                "C+G \n(n = 109)",
                                "Gene \n(n = 284)",
                                "Oncology Reports \n(n = 995)")))

#Append % sign for labels
A2$Value2 <- paste0(as.matrix(A2$value), "%")


#Plot figure 2
ggplot(A2, mapping = aes(Corpus, value, fill = variable))+
  geom_col(position = "stack", width = 0.4, alpha = 0.86)+
  scale_y_continuous(breaks = seq(0, 100, by = 10))+ 
  ylab("Proportion of Error Types (%)\n")+
  geom_text(aes(label=Value2), position = position_stack(vjust = 0.5))+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10), 
        legend.position = "top")+
  scale_fill_manual(values = c("seagreen3", "#0072B2", "#E69F00"), 
                    name = "Error Types", 
                    labels = c(
                      "'Non-targeting' yet targeting",
                      "'Targeting' yet non-targeting",
                      "Targeting wrong gene or genomic sequence"))
