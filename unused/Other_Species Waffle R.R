install.packages("waffle")
library(waffle)
install.packages("ggthemes")
library(ggthemes)
library(scales)
library(readr)
library(ggplot2)


vals <- c(239, 52, 9)
val_names <- sprintf("%s (%s)", c("Negative", "Positive", "Neutral"), 
                     scales::percent(round(vals/sum(vals), 2)))
names(vals) <- val_names

waffle::waffle(vals) +
  ggthemes::scale_fill_tableau(name=NULL)

value <- c(799, 190, 5, 5, 2, 2, 6)
value_names <- sprintf("%s (%s)", c("Human", "Muridae", "Suidae", 
                                    "Canidae", "Pipidae", "Phasianidae", 
                                    "Other"),
                       percent(round(value/sum(value), 3)))
names(value) <- value_names

waffle(value)


value2 <- c(79, 18.83, 0.49, 0.49, 0.19, 0.19, 0.59)
value_names2 <- sprintf("%s (%s)", c("Human", "Muridae", "Suidae", 
                                    "Canidae", "Pipidae", "Phasianidae", 
                                    "Other"),
                       percent(round(value2/sum(value2), 3)))
names(value2) <- value_names2
waffle(value2)

value3 <- c(79.9, 19.0, 2.0)
value_names3 <- sprintf("%s (%s)", c("Human", "Muridae", "Other"),
                       percent(round(value3/sum(value3), 4)))
names(value3) <- value_names3
waffle(value3)
waffle(value3, rows=5, size = 0, legend_pos = "right") +
  scale_fill_manual(values = c("#0072B2", "#E69F00","grey", "black"))+
  xlab("One square =  10.09 sequences")+
  labs(fill = "Oncology Reports")


Group4 <- c(79, 19, 2)
Group_names4 <- sprintf("%s (%s)", c("Human", "Rodent", "Other"),
                        percent(round(Group4/sum(Group4), 4)))
names(Group4) <- Group_names4
L <- waffle(Group4, rows=5, size = 0, legend_pos = "right") +
  scale_fill_manual(values = c("#0072B2", "#E69F00","grey"))+
  xlab("One square =  1% of 1004 sequences")+
  labs(fill = "Oncology Reports")

L$layers[[1]]$aes_params$colour <- 'black'
L


Group5 <- c(90, 10)
Group_names5 <- sprintf("%s (%s)", c("Human", "Rodent"),
                        percent(round(Group5/sum(Group5), 4)))
names(Group5) <- Group_names5
M <- waffle(Group5, rows=5, size = 0, legend_pos = "right") +
  scale_fill_manual(values = c("#0072B2", "#E69F00"))+
  xlab("One square =  1% of 296 sequences")+
  labs(fill = "Gene")

M$layers[[1]]$aes_params$colour <- 'black'
M
