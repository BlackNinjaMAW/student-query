library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

df_Register <- read_xlsx("Registration.xlsx", .name_repair = "universal")
df_Student <- read_xlsx("Student.xlsx", .name_repair = "universal")
df_Course <- read_xlsx("Course.xlsx", .name_repair = "universal")

df_Combo <- left_join(df_Register, df_Course, by = "Instance.ID") %>%
  left_join(df_Student, by = "Student.ID") %>%
  mutate(Age = trunc((Birth.Date %--% today()) / years(1))) %>%
  mutate(Quarter.Start = paste0("Q", as.character(quarter(Start.Date)))) %>%
  mutate(Quarter.End = paste0("Q", as.character(quarter(End.Date))))

df_CourseQuartered <- df_Course %>%
  mutate(Quarter.Start = paste0("Q", as.character(quarter(Start.Date)))) %>%
  mutate(Quarter.End = paste0("Q", as.character(quarter(End.Date))))

df_CompSci <- filter(df_Combo, Title == "Computer Science")
df_PaymentPlans <- filter(df_Combo, Payment.Plan == TRUE)
df_FirstQuarterClassOptions <- filter(df_CourseQuartered, Quarter.Start == "Q1")

df_TotalBalanceByPlan <- df_combo %>%
  group_by(Payment.Plan) %>%
  summarize(SumCost = sum(Total.Cost))

df_TotalCostByMajor <- df_combo %>%
  group_by(Title) %>%
  summarize(SumCost = sum(Total.Cost))

plot = ggplot(df_Combo) +
  geom_bar(aes(Title), fill = c("darkred", "darkblue", "darkcyan", "darkgreen", "gold", "darkmagenta"))

show(plot)
