library(readxl)
library(tidyverse)
library(ggplot2)

data <- read_xlsx("data.xlsx", sheet = "data")

prop.table(table(data$Condition))*100
# 48 % condition 0 and 51% condition 1

prop.table(table(data$Choice))*100
# 46% No's and 54% Yes

prop.table(table(data$Gender))*100
# 50% Men and 50% Women

data$Age <- as.numeric(data$Age)

str(data)
summary(data %>%
          filter(Gender == "Male") %>% select(Age))

condition_0 <- data %>% 
            filter(Condition == "0")

condition_1 <- data %>% 
  filter(Condition == "1")

prop.table(table(condition_0$Choice))*100
# 59% No's and 41% Yes

prop.table(table(condition_0$Gender))*100
# 42% Men and 58% Women

data %>% 
  filter(Age > 31) %>% 
  nrow() /
  data %>% 
  nrow()

condition_0 %>% 
  filter(Age > 31) %>% 
  nrow() /
  condition_0 %>% 
  nrow()

condition_1 %>% 
  filter(Age > 31) %>% 
  nrow() /
  condition_1 %>% 
  nrow()

summary(condition_0$Age)

summary(condition_1$Age)

prop.table(table(condition_1$Gender))*100
# 58% Men and 42% Women


prop.table(table(condition_1$Choice))*100
# 36% No's and 64% Yes

data %>% 
  group_by(Condition) %>%
  summarise(Yes = sum(Choice == "Yes"),
            No = sum(Choice == "No"),
            Total = n()) %>%
  mutate(Percentage_Yes = Yes / Total * 100,
         Percentage_No = No / Total * 100) %>% 
  pivot_longer(cols = starts_with("Percentage"),
               names_to = "Choice",
               values_to = "Percent") %>%
  mutate(Choice = case_when(
    Choice == "Percentage_Yes" ~ "Alternate Choice",
    Choice == "Percentage_No" ~ "Original Offering",
    TRUE ~ as.character(Choice)
  )) %>% 
  ggplot(aes(x = Condition, y = Percent, fill = Choice)) +
  geom_col(width = 0.5, position = "dodge",just = 0 )+
  ylab("Percent of Participants")+
  xlab("Original Offered Choice")+
  labs(title = "Particpant Acceptance of the Alternative Choice",
       subtitle = "58% want to stay with 5 Days work week
       
       64% employees intent to turnover to 5 Days from 4 Work Days per week")+
  scale_x_continuous(labels = c("5 Days", "4 Days"), breaks = c(0,1))+
  scale_fill_manual(values = c("#56B4E9", "#000066"))+
#  scale_y_continuous(limits = c(0,11), breaks = c(2,4,6,8,10))+
#  scale_x_continuous(breaks = c(0,1))+
  theme(panel.background = element_blank(),
        axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
        axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
        legend.position = "top", 
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(size = 1),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.spacing = unit(2, 'cm'),
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic"))

 #overall

  

condition_0 %>%
  group_by(Gender) %>%
  summarise(Yes = sum(Choice == "Yes"),
            No = sum(Choice == "No"),
            Total = n()) %>%
  mutate(Percentage_Yes = Yes / Total * 100,
         Percentage_No = No / Total * 100) %>% 
  pivot_longer(cols = starts_with("Percentage"),
               names_to = "Choice",
               values_to = "Percent") %>%
  mutate(Choice = case_when(
    Choice == "Percentage_Yes" ~ "4 Days (Alternate Choice)",
    Choice == "Percentage_No" ~ "5 Days (Original Offer)",
    TRUE ~ as.character(Choice)
  )) %>% 
  ggplot(aes(x = Choice, y = Percent, fill = Gender)) +
  geom_col(width = 0.5, position = "dodge")+
  ylab("Percent")+
  labs(title = "Gender Analysis of 5 Days Work Week as the Original Choice",
       subtitle = "84% women participants want to stay with the Original condition of 5 Working Days
       
       60% men express to accept the alternate choice of 4 work days per week")+
  scale_fill_manual(values = c("#CC0066", "#000066"))+
#  scale_x_discrete(labels = c("4 Days (Alternate Choice)", "5 Days (Original Condition)"))+
#  scale_y_continuous(limits = c(0,11), breaks = c(2,4,6,8,10))+
  theme(panel.background = element_blank(),
        axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
        axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
        legend.position = "top", 
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(size = 1),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        legend.spacing = unit(2, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic"))

condition_1 %>% 
  group_by(Gender) %>%
  summarise(Yes = sum(Choice == "Yes"),
            No = sum(Choice == "No"),
            Total = n()) %>%
  mutate(Percentage_Yes = Yes / Total * 100,
         Percentage_No = No / Total * 100) %>% 
  pivot_longer(cols = starts_with("Percentage"),
               names_to = "Choice",
               values_to = "Percent") %>%
  mutate(Choice = case_when(
    Choice == "Percentage_Yes" ~ "5 Days (Alternate Choice)",
    Choice == "Percentage_No" ~ "4 Days (Original Offer)",
    TRUE ~ as.character(Choice)
  )) %>% 
  ggplot(aes(x = Choice, y = Percent, fill = Gender)) +
  geom_col(width = 0.5, position = "dodge")+
  ylab("Percent")+
  labs(title = "Gender Analysis of 4 Days Work Week as the Original Choice",
       subtitle = "More than 60% of both the genders express their intent of turning over
       to 5 work days per week")+
  scale_fill_manual(values = c("#CC0066", "#000066"))+
#  scale_x_discrete(labels = c("4 Days (Original Offer)", "5 Days (Alternate Choice)"))+
  #  scale_y_continuous(limits = c(0,11), breaks = c(2,4,6,8,10))+
  theme(panel.background = element_blank(),
        axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
        axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
        legend.position = "top", 
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(size = 1),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 11),
        legend.spacing = unit(2, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic"))



# by age distribution of  data

data %>%
  filter(Gender == "Female") %>% 
  ggplot(aes(x = Age)) +
  geom_bar(binwidth = 1, position = 'dodge')+
  xlab("Age") +
  ylab("Participant Count") +
#  ggtitle("Age Distribution of Participants")+
  scale_x_continuous(limits = c(20,50), breaks = c(20,25,30,35,40,45,50))+
  (data %>%
  filter(Gender == "Male") %>%
  geom_vline(xintercept = mean(Age), linetype = "dashed"))+
  theme(panel.background = element_blank(),
        axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)), # adjusting margin of axis label from the axis values
        axis.title.y = element_text(face="italic", margin = margin(00, 8, 00, 8)),
        legend.position = "none", 
        legend.background = element_blank(),
        #legend.title = element_text(size = 10),
        axis.line = element_line(linewidth = 1),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        #legend.spacing = unit(2, 'cm'),
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold', family = "Verdana"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, face = "italic", family = "Verdana"))

data %>%
  filter(Choice == 'Yes') %>% 
  ggplot(aes(x = Age)) +
  geom_histogram()

condition_0 %>%
  ggplot(aes(x = Age)) +
  geom_histogram()

condition_1 %>%
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 5)


# BY AGE

data %>% 
  filter(Age < 31) %>% 
  group_by(Condition) %>%
  summarise(Yes = sum(Choice == "Yes"),
            No = sum(Choice == "No"),
            Total = n()) %>%
  mutate(Percentage_Yes = Yes / Total * 100,
         Percentage_No = No / Total * 100) %>% 
  pivot_longer(cols = starts_with("Percentage"),
               names_to = "Choice",
               values_to = "Percent") %>%
  mutate(Choice = case_when(
    Choice == "Percentage_Yes" ~ "Alternate Choice",
    Choice == "Percentage_No" ~ "Original Offering",
    TRUE ~ as.character(Choice)
  )) %>% 
  ggplot(aes(x = Condition, y = Percent, fill = Choice)) +
  geom_col(width = 0.5, position = "dodge",just = 0 )+
  ylab("Percent of Participants")+
  xlab("Original Offered Choice")+
  labs(title = "Particpant Acceptance of the Alternative Choice
       Age Group (20-30 Years)",
       subtitle = "64% want to stay with 5 Days work week
       
       66% intent to turnover to of 5 Days from 4 Work Days per week")+
  scale_x_continuous(labels = c("5 Days", "4 Days"), breaks = c(0,1))+
  scale_fill_manual(values = c("#56B4E9", "#000066"))+
  #  scale_y_continuous(limits = c(0,11), breaks = c(2,4,6,8,10))+
  #  scale_x_continuous(breaks = c(0,1))+
  theme(panel.background = element_blank(),
        axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
        axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
        legend.position = "top", 
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(size = 1),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.spacing = unit(2, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic"))

data %>% 
  filter(Age > 31) %>% 
  group_by(Condition) %>%
  summarise(Yes = sum(Choice == "Yes"),
            No = sum(Choice == "No"),
            Total = n()) %>%
  mutate(Percentage_Yes = Yes / Total * 100,
         Percentage_No = No / Total * 100) %>% 
  pivot_longer(cols = starts_with("Percentage"),
               names_to = "Choice",
               values_to = "Percent") %>%
  mutate(Choice = case_when(
    Choice == "Percentage_Yes" ~ "Alternate Choice",
    Choice == "Percentage_No" ~ "Original Offering",
    TRUE ~ as.character(Choice)
  )) %>% 
  ggplot(aes(x = Condition, y = Percent, fill = Choice)) +
  geom_col(width = 0.5, position = "dodge",just = 0 )+
  ylab("Percent of Participants")+
  xlab("Original Offered Choice")+
  labs(title = "Particpant Acceptance of the Alternative Choice
       Age Group (31-50 Years)",
       subtitle = "52% intent to turn over from 5 Days work week
       
       60% intent to turnover to 5 Days from 4 Work Days per week")+
  scale_x_continuous(labels = c("5 Days", "4 Days"), breaks = c(0,1))+
  scale_fill_manual(values = c("#56B4E9", "#000066"))+
  #  scale_y_continuous(limits = c(0,11), breaks = c(2,4,6,8,10))+
  #  scale_x_continuous(breaks = c(0,1))+
  theme(panel.background = element_blank(),
        axis.title.x = element_text(face="italic", margin = margin(8, 00, 8, 00)),
        axis.title.y = element_text(face="italic", margin = margin(0, 08, 0, 08)),
        legend.position = "top", 
        legend.background = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(size = 1),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12),
        legend.spacing = unit(2, 'cm'),
        plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic"))
