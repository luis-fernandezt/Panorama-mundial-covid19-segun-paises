#Load library
library(readxl)
library(ggplot2)
library(dplyr)
library(readr)

# Load Data Frama from OWD
owid_covid_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
owid_covid_data <- as.data.frame(owid_covid_data)
names(owid_covid_data)
max(owid_covid_data$date)

#filter by countries
Chile <- owid_covid_data %>%  filter(location %in% c("Chile"))
Brazil <- owid_covid_data %>%  filter(location %in% c("Brazil"))
Argentina <- owid_covid_data %>%  filter(location %in% c("Argentina"))
Peru <- owid_covid_data %>%  filter(location %in% c("Peru"))

Spain <- owid_covid_data %>%  filter(location %in% c("Spain"))
USA <- owid_covid_data %>%  filter(location %in% c("United States"))
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

#total_deaths_per_million ####

#join selected countries
sdt_deaths_per_million <- rbind(Chile, Argentina, USA, Peru, Spain, France, Belgium,  China, Korea, San_Marino, Andorra)

gg1 <- 
  
  ggplot(sdt_deaths_per_million, aes(x=date, y=total_deaths_per_million, group=location, color=location)) +
  geom_line(color='grey95', size = 1.5, data = owid_covid_data) +
  geom_line(size = 1.3, data = sdt_deaths_per_million %>% filter(total_deaths_per_million > 0)) + 
  geom_text(aes(label = iso_code), size= 4, hjust = -0.1, data = sdt_deaths_per_million 
            %>% filter(date == max(date)), show.legend = F) +
  geom_line(color='darkred', size = 1.5, data = Chile %>% filter(total_deaths_per_million > 0)) +
  geom_text(aes(label = iso_code), color='darkred', size= 4, hjust = -0.1, 
            data = Chile %>% filter(date == max(date))) +
  
  scale_colour_viridis_d(name = "", option = 'D', begin = 0, end = 0.8, direction = -1, alpha=0.9) +
       theme_classic() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
        legend.text = element_text(color = "black", size = 14)) +
  scale_x_date(date_breaks = '7 day', date_labels = "%b %d") +
  scale_y_continuous(trans = 'sqrt', breaks = c(10, 50, 100, 250, 500, 550, 600, 650, 700, 800, 900, 1000, 1500)) + 
  geom_hline(aes(yintercept = max(Chile$total_deaths_per_million)), color="red", linetype = 'dashed') +
  geom_vline(aes(xintercept = max(Chile$date)), color="red", linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "date", 
       y = "Total deaths per million", 
       title = "Total deaths per million", 
       subtitle = "September 10, 2020", 
       caption = "Fuente: ourworldindata.org/coronavirus - github.com/owid")

#gg1

ggsave(plot = gg1, filename = './Graficos/total_deaths_per_million.png', #guardar como imagen
       units = 'mm', width = 216, height = 279, dpi = 300)


#total_cases_per_million ####
#join selected countries
sdt_cases_per_million <- rbind(Chile, Brazil, Argentina,  Peru, Spain, Italy, France, Belgium,  China, Korea, Bahrain, Qatar)

gg2 <-
  
  ggplot(sdt_cases_per_million, aes(x=date, y=total_cases_per_million, group=location, color=location)) +
  geom_line(color='grey95', size = 1.5, data = owid_covid_data) +
  geom_line(size = 1.3, data = sdt_cases_per_million %>% filter(total_cases_per_million > 0)) +  
  geom_text(aes(label = iso_code), size= 4, hjust = -0.1, data = sdt_cases_per_million 
            %>% filter(date == max(date)), show.legend = F) +
  geom_line(color='darkred', size = 1.5, data = Chile %>% filter(total_cases_per_million > 0)) + 
  geom_text(aes(label = iso_code), color='darkred', size= 4, hjust = -0.1, 
            data = Chile %>% filter(date == max(date))) +
  
  scale_colour_viridis_d(name = "", option = 'D', begin = 0, end = 0.8, direction = -1, alpha=0.9) +
  theme_classic() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
        legend.text = element_text(color = "black", size = 14)) +
  scale_x_date(date_breaks = '7 day', date_labels = "%b %d") +
  scale_y_continuous(trans = 'sqrt', breaks = c(10, 100, 500, 1000, 50000, 10000, 20000, 30000, 40000)) + 
  geom_hline(aes(yintercept = max(Chile$total_cases_per_million)), color="red", linetype = 'dashed') +
  geom_vline(aes(xintercept = max(Chile$date)), color="red", linetype = 'dashed') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "date", 
       y = "Total cases per million", 
       title = "Total cases per million", 
       subtitle = "September 10, 2020", 
       caption = "Fuente: ourworldindata.org/coronavirus - github.com/owid")

#gg2

ggsave(plot = gg2, filename = './Graficos/total_cases_per_million.png', #guardar como imagen
       units = 'mm', width = 216, height = 279, dpi = 300)

