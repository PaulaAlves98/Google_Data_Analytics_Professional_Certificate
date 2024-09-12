library("tidyverse")
library(janitor)
library(skimr)

activity_day <- read.csv("C:/Users/paula/OneDrive/Documentos/Programação/Google Análise de Dados/CapstoneProject/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv",
                           header = TRUE, sep = ",")

calories_day <- read.csv("C:/Users/paula/OneDrive/Documentos/Programação/Google Análise de Dados/CapstoneProject/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv",
                           header = TRUE, sep = ",")

intensities_day<- read.csv("C:/Users/paula/OneDrive/Documentos/Programação/Google Análise de Dados/CapstoneProject/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv",
                              header = TRUE, sep = ",")

steps_day <- read.csv("C:/Users/paula/OneDrive/Documentos/Programação/Google Análise de Dados/CapstoneProject/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv",
                        header = TRUE, sep = ",")

sleep_day <- read.csv("C:/Users/paula/OneDrive/Documentos/Programação/Google Análise de Dados/CapstoneProject/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv",
                        header = TRUE, sep = ",")

heartbeat <- read.csv("~/Programação/Google Análise de Dados/CapstoneProject/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv",
                      header = TRUE, sep = ",")

weight_log <- read.csv("C:/Users/paula/OneDrive/Documentos/Programação/Google Análise de Dados/CapstoneProject/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv",
                        header = TRUE, sep = ",")

n_distinct(activity_day$Id)
sum(duplicated(activity_day))
n_distinct(sleep_day$Id)
sum(duplicated(sleep_day))
sleep_day <- unique(sleep_day)
sum(duplicated(sleep_day))
n_distinct(weight_log$Id)
sum(duplicated(weight_log))

n_distinct(heartbeat$Id)

sleep_day_summary <- sleep_day %>% group_by(Id) %>% summarise(TotalMinutesAsleep=sum(TotalMinutesAsleep,na.rm = TRUE),
                                                              TotalTimeInBed=sum(TotalTimeInBed, na.rm = TRUE))

ggplot(data=sleep_day_summary, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point() + geom_smooth()


activity_day_summary <- activity_day %>%
  group_by(Id)%>%
  summarise(TotalSteps = sum(TotalSteps, na.rm = TRUE),
            TotalCalories = sum(Calories, na.rm = TRUE),
            TotalDistance = sum(TotalDistance, na.rm = TRUE),
            TotalSedenteryMinutes=sum(SedentaryMinutes, na.rm = TRUE))

ggplot(data = activity_day_summary, aes(x=TotalDistance, y=TotalSteps)) + geom_point() + geom_smooth()

ggplot(data = activity_day_summary, aes(x=TotalCalories, y=TotalSteps)) + geom_point() + geom_smooth() +
  ylim(0, max(activity_day_summary$TotalSteps))

ggplot(data = activity_day_summary, aes(x = TotalDistance, y = TotalCalories)) + geom_point() + geom_smooth() +
  
  

weight_log_summary <- weight_log %>% group_by(Id) %>% summarise(mean_weight=mean(WeightKg),
                                                                mean_BMI=mean(BMI),
                                                                mean_fat=mean(Fat,na.rm = TRUE))

combined_activity_weight <- merge(activity_day_summary, weight_log_summary, by='Id')

ggplot(combined_activity_weight, aes(x = mean_BMI, y = TotalSteps)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  labs(title = "Relação entre Total de Passos e IMC",
       x = "IMC Médio",
       y = "Total de Passos") +
  ylim(0, max(combined_activity_weight$TotalSteps))

ggplot(combined_activity_weight, aes(x = mean_BMI, y = TotalCalories)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  labs(title = "Relação entre Total de Calorias e IMC",
       x = "IMC Médio",
       y = "Total de Calorias") +
  ylim(0, max(combined_activity_weight$TotalCalories))  

ggplot(combined_activity_weight, aes(x = mean_BMI, y = TotalDistance)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  labs(title = "Relação entre Distância Total e IMC",
       x = "IMC Médio",
       y = "Distância Total") +
  ylim(0, max(combined_activity_weight$TotalDistance))

ggplot(combined_activity_weight, aes(x = TotalDistance, y = TotalCalories)) +
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  labs(title = "Relação entre Distância Total e Calorias",
       x = "Distância Total",
       y = "Total de Calorias") +
  ylim(0, max(combined_activity_weight$TotalCalories))


sleep_day_summary <- sleep_day %>% group_by(Id) %>% summarise(AvgMinutesAsleep=mean(TotalMinutesAsleep,na.rm = TRUE),
                                                              AvgTimeInBed=mean(TotalTimeInBed, na.rm = TRUE))


activity_day_summary <- activity_day %>%
  group_by(Id)%>%
  summarise(AvgSteps = mean(TotalSteps, na.rm = TRUE),
            AvgCalories = mean(Calories, na.rm = TRUE),
            AvgDistance = mean(TotalDistance, na.rm = TRUE),
            AvgSedenteryMinutes=mean(SedentaryMinutes, na.rm = TRUE))

combined_activity_weight_sleep <- merge(activity_day_summary, sleep_day_summary, by='Id')  

ggplot(data = combined_activity_weight_sleep, aes(x=AvgMinutesAsleep,y=AvgSteps)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + ylim(0, max(combined_activity_weight_sleep$AvgSteps))

ggplot(data = combined_activity_weight_sleep, aes(x=AvgMinutesAsleep,y=AvgCalories)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  ylim(0, max(combined_activity_weight_sleep$AvgCalories))

ggplot(data = combined_activity_weight_sleep, aes(x=AvgMinutesAsleep,y=AvgDistance)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  ylim(0, max(combined_activity_weight_sleep$AvgDistance))

ggplot(data = combined_activity_weight_sleep, aes(x=AvgMinutesAsleep,y=AvgSteps)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) + ylim(0, max(combined_activity_weight_sleep$AvgSteps))

ggplot(data = combined_activity_weight_sleep, aes(x=AvgMinutesAsleep,y=AvgCalories)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  ylim(0, max(combined_activity_weight_sleep$AvgCalories))

ggplot(data = combined_activity_weight_sleep, aes(x=AvgTimeInBed,y=AvgDistance)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  ylim(0, max(combined_activity_weight_sleep$AvgDistance))

ggplot(data = combined_activity_weight_sleep, aes(x=AvgTimeInBed,y=AvgCalories)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  ylim(0, max(combined_activity_weight_sleep$AvgCalories))

ggplot(data = combined_activity_weight_sleep, aes(x=AvgTimeInBed,y=AvgSteps)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) 
+ ylim(0, max(combined_activity_weight_sleep$AvgSteps))


