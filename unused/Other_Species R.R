library(readr)
library(ggplot2)
A <- read_csv("Other_Species.csv")
View(A)

ggplot(A, mapping = aes(Family, PERC))+
  geom_col()+
  coord_flip()

#lollipop

ggplot(Adf, mapping = aes(x, y))+
  geom_segment(aes(x= reorder(x, +y), xend=x, y=0, yend=y), color="black") + 
  geom_point(color="black", size = 4) +
  coord_flip()+
  theme_light()+
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank())+
  scale_y_continuous(breaks = seq(0, 20, by = 1))+
  xlab("Order") +
  ylab("Percentage of all flagged sequences")


#E69F00

ggplot(Bdf, mapping = aes(x, y))+
  geom_segment(aes(x= reorder(x, +y), xend=x, y=0, yend=y), color="black") + 
  geom_point(color="black", size = 4) +
  coord_flip()+
  theme_light()+
  theme(panel.grid.major.y = element_blank())+
  scale_y_continuous(breaks = seq(0, 91, by = 5))+
  xlab("Order") +
  ylab("Percentage of non-human sequences")

Adf <- data.frame(
  x = c("Muridae", "Suidae", "Canidae", "Pipidae", "Phasianidae", "Other"), 
  y = c(18.83053, 0.49554, 0.49554, 0.198216, 0.198216, 0.594648)
)

Bdf <- data.frame(
  x = c("Muridae", "Suidae", "Canidae", "Pipidae", "Phasianidae", "Other"), 
  y = c(90.47619, 2.380952, 2.380952, 0.952381, 0.952381, 2.857143)
)


ggplot(Adf, mapping = aes(x = reorder(x, +y), y))+
  geom_col()+
  coord_flip()+
  theme_light()+
  theme(panel.grid.major.y = element_blank())+
  scale_y_continuous(breaks = seq(0, 20, by = 1))+
  scale_fill_manual(values = c("#E69F00", "#0072B2")) +
  xlab("Order") +
  ylab("Percentage")

  