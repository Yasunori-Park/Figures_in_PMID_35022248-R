library(ggplot2)
library(readr)
library(plotly)
install.packages("ggmap")
library(ggmap)

Pie <- read_csv("OR Other_Species Pie.csv")
View(Pie)
bar <- ggplot(Pie, aes("", C, fill = A))+
  geom_col(width = 1)
bar

bar + coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#0072B2", "#E69F00"))+
  theme_minimal()+
  theme_void()+
  geom_text(aes(y = C,label = C))
  
  
df <- data.frame(
  Group = c("Human", "Non-human"),
  Value = c(79.18, 20.81)
) %>% 
  mutate(Group = factor(Group, levels = c("Human", "Non-human")),
       cumulative = cumsum(Value),
       midpoint = cumulative - Value/2,
       label = paste0(Group, " ", round(value / sum(value) * 100, 1),
                      "%")
         
       )


df <- data.frame(value = c(79.18, 20.81),
                 Group = c("Human", "Non-human")) %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(Group = factor(Group, levels = c("Non-human", "Human")),
         cumulative = cumsum(value),
         midpoint = cumulative - value / 2,
         label = paste0(" ", round(value / sum(value) * 100, 1), "%"))

ggplot(df, aes(x = 1, weight = value, fill = Group)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.1, y = midpoint, label = label), size = 4.6, color = "white") +
  theme_void()+
  scale_y_reverse()+
  scale_fill_manual(values = c("#E69F00", "#0072B2"))

