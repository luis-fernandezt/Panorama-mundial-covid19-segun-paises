#Load library
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

# Load Data Frama from OWD
owid_covid_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
owid_covid_data <- as.data.frame(owid_covid_data)
names(owid_covid_data)
max(owid_covid_data$date)

owid_covid_data <- owid_covid_data[owid_covid_data$total_deaths_per_million != 0,  ] #ejecutar para gg1
owid_covid_data <- owid_covid_data[owid_covid_data$total_deaths_per_million != 0,  ] #ejecutar para gg2

#filter by countries (run all again to gg2)
Chile <- owid_covid_data %>%  filter(location %in% c("Chile"))
Brazil <- owid_covid_data %>%  filter(location %in% c("Brazil"))
Argentina <- owid_covid_data %>%  filter(location %in% c("Argentina"))
Peru <- owid_covid_data %>%  filter(location %in% c("Peru"))
USA <- owid_covid_data %>%  filter(location %in% c("United States"))

Spain <- owid_covid_data %>%  filter(location %in% c("Spain"))
UK <- owid_covid_data %>%  filter(location %in% c("United Kingdom"))
France <- owid_covid_data %>%  filter(location %in% c("France"))
Belgium <- owid_covid_data %>%  filter(location %in% c("Belgium"))
San_Marino <- owid_covid_data %>%  filter(location %in% c("San Marino"))
Italy <- owid_covid_data %>%  filter(location %in% c("Italy"))

China <- owid_covid_data %>%  filter(location %in% c("China"))
Korea <- owid_covid_data %>%  filter(location %in% c("South Korea"))
Andorra <- owid_covid_data %>%  filter(location %in% c("Andorra"))
Qatar <- owid_covid_data %>%  filter(location %in% c("Qatar"))
Bahrain <- owid_covid_data %>%  filter(location %in% c("Bahrain"))
  
# elapsed time in days
Chile$elapsed_days <- as.numeric(as.character(difftime(Chile$date,min(Chile$date), units = c("days")))) 
Brazil$elapsed_days <- as.numeric(as.character(difftime(Brazil$date,min(Brazil$date), units = c("days")))) 
Argentina$elapsed_days <- as.numeric(as.character(difftime(Argentina$date,min(Argentina$date), units = c("days")))) 
Peru$elapsed_days <- as.numeric(as.character(difftime(Peru$date,min(Peru$date), units = c("days")))) 
USA$elapsed_days <- as.numeric(as.character(difftime(USA$date,min(USA$date), units = c("days")))) 

Spain$elapsed_days <- as.numeric(as.character(difftime(Spain$date,min(Spain$date), units = c("days")))) 
UK$elapsed_days <- as.numeric(as.character(difftime(UK$date,min(UK$date), units = c("days")))) 
France$elapsed_days <- as.numeric(as.character(difftime(France$date,min(France$date), units = c("days")))) 
Belgium$elapsed_days <- as.numeric(as.character(difftime(Belgium$date,min(Belgium$date), units = c("days")))) 
San_Marino$elapsed_days <- as.numeric(as.character(difftime(San_Marino$date,min(San_Marino$date), units = c("days")))) 
Italy$elapsed_days <- as.numeric(as.character(difftime(Italy$date,min(Italy$date), units = c("days")))) 

China$elapsed_days <- as.numeric(as.character(difftime(China$date,min(China$date), units = c("days")))) 
Korea$elapsed_days <- as.numeric(as.character(difftime(Korea$date,min(Korea$date), units = c("days")))) 
Andorra$elapsed_days <- as.numeric(as.character(difftime(Andorra$date,min(Andorra$date), units = c("days")))) 
Qatar$elapsed_days <- as.numeric(as.character(difftime(Qatar$date,min(Qatar$date), units = c("days")))) 
Bahrain$elapsed_days <- as.numeric(as.character(difftime(Bahrain$date,min(Bahrain$date), units = c("days")))) 

#total_deaths_per_million ####
#join selected countries
sdt_deaths_per_million <- rbind(Chile, Argentina, USA, Peru, Italy, France, Belgium,  China, Korea, San_Marino, Andorra)
names(sdt_deaths_per_million)

gg1 <- 
  ggplot(sdt_deaths_per_million, aes(x=elapsed_days, y=total_deaths_per_million, group=location, color=location)) +
  geom_line(size = 1.3, data = sdt_deaths_per_million) + 
  geom_text(aes(label = iso_code), size= 4, hjust = -0.1, data = sdt_deaths_per_million 
            %>% filter(date == max(date)), show.legend = F) +
  
  geom_line(color='darkred', size = 1.5, data = Chile) +
  geom_text(aes(label = iso_code), color='darkred', size= 4, hjust = -0.1, 
            data = Chile %>% filter(date == max(date))) +
  
  scale_colour_viridis_d(name = "", option = 'D', begin = 0, end = 0.8, direction = -1, alpha=0.9) +
  
  theme_classic() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
        legend.text = element_text(color = "black", size = 14)) +
  
  scale_x_continuous(trans = 'sqrt', breaks = c(1,2,3,4,5,10,20,50,100,172,200,250)) + 
  scale_y_continuous(trans = 'sqrt', breaks = c(10, 50, 100, 250, 500, 550, 616, 700, 800, 900, 1000, 1500)) + 
  
  geom_hline(aes(yintercept = max(Chile$total_deaths_per_million)), color="red", linetype = 'dashed') +
  geom_vline(aes(xintercept = max(Chile$elapsed_days)), color="red", linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  labs(x = "elapsed_days", 
       y = "Total deaths per million", 
       title = "Total deaths per million", 
       subtitle = "September 11, 2020", 
       caption = "Fuente: ourworldindata.org/coronavirus - github.com/owid")
gg1

ggsave(plot = gg1, filename = './Graficos/total_deaths_per_million.png', #guardar como imagen
       units = 'mm', width = 216, height = 279, dpi = 300)


#total_cases_per_million ####
#join selected countries
sdt_cases_per_million <- rbind(Chile, Brazil, Argentina,  Peru, Italy, France, Belgium,  China, Korea, Bahrain, Qatar)

gg2 <- 
  ggplot(sdt_cases_per_million, aes(x=elapsed_days, y=total_cases_per_million, group=location, color=location)) +
  geom_line(size = 1.3, data = sdt_cases_per_million) + 
  geom_text(aes(label = iso_code), size= 4, hjust = -0.1, data = sdt_cases_per_million 
            %>% filter(date == max(date)), show.legend = F) +
  
  geom_line(color='darkred', size = 1.5, data = Chile) +
  geom_text(aes(label = iso_code), color='darkred', size= 4, hjust = -0.1, 
            data = Chile %>% filter(date == max(date))) +
  
  scale_colour_viridis_d(name = "", option = 'D', begin = 0, end = 0.8, direction = -1, alpha=0.9) +
  
  theme_classic() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
        legend.text = element_text(color = "black", size = 14)) +
  
  scale_x_continuous(trans = 'sqrt', breaks = c(1,2,3,4,5,10,20,50,100,172, 200,250)) + 
  scale_y_continuous(trans = 'sqrt', breaks = c(1,10,100,1000,10000,20000,22424,30000,40000)) + 
  
  geom_hline(aes(yintercept = max(Chile$total_cases_per_million)), color="red", linetype = 'dashed') +
  geom_vline(aes(xintercept = max(Chile$elapsed_days)), color="red", linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "date", 
       y = "Total cases per million", 
       title = "Total cases per million", 
       subtitle = "September 11, 2020", 
       caption = "Fuente: ourworldindata.org/coronavirus - github.com/owid")

gg2

ggsave(plot = gg2, filename = './Graficos/total_cases_per_million.png', #guardar como imagen
       units = 'mm', width = 216, height = 279, dpi = 300)

