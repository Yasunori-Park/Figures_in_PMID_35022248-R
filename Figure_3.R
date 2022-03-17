library(ggplot2)

Fig3 <- data.frame(
  Corpus = c("SGK", "miR-145", "C+G", "Gene", "Oncology Reports"),
  Category = c("Repeated incorrect sequences", "Unique incorrect sequences",
               "Repeated incorrect sequences", "Unique incorrect sequences",
               "Repeated incorrect sequences", "Unique incorrect sequences",
               "Repeated incorrect sequences", "Unique incorrect sequences",
               "Repeated incorrect sequences", "Unique incorrect sequences"),
  Count = c(58, 88, 2, 92, 18,
            42, 12, 98, 8, 82),
  RawN = c("n = 115", "n = 49", "n = 109", "n = 284", "n = 995", 
           "n = 115", "n = 49", "n = 109", "n = 284", "n = 995")
)


Fig3$Corpus <- (factor(Fig3$Corpus,
                     levels = c("SGK", "miR-145", "C+G", 
                                "Gene", "Oncology Reports")))

#Export pie chart as pdf for high quality.Adjust vjust = 9 and margin(b=10) as necessary
ggplot(Fig3, aes(x = "", y = Count, fill = factor(Category))) + 
  geom_col(width = 1, position = "stack") +
  facet_wrap( ~ Corpus, ncol=5)+
  theme_void()+
  theme(legend.position = "bottom")+
  scale_fill_manual(labels = c("Repeated incorrect sequences", 
                               "Unique incorrect sequences"),
                    values = c("black", "grey"))+
  labs(fill = "")+
  coord_polar(theta = "y")+
  geom_text(aes(label = RawN), 
            x = -Inf, y = Inf, hjust = 0.5, vjust = 9)+
  theme(strip.text.x=element_text(margin=margin(b=10)))
