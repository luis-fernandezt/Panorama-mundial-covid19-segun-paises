#cargamos librerías
library(readxl)
library(ggplot2)
library(dplyr)

# cargamos base de datos
# descargamos data desde https://ourworldindata.org/coronavirus-source-data
# creamos una columna en excel llamada "elapsed_days" con la formula: "=ABS(DIAS(MIN.SI.CONJUNTO($C$2:$C$13493;$B$2:$B$13493;B2);C2))"


#total_cases_per_million
#load data
full_data <- read_excel("./Data/total_cases_per_million.xlsx")

c_compare_cases <- full_data %>% 
  filter(location %in% c("Ecuador","Brazil", "Argentina","Italy", "Spain", "United States",
                         "United Kingdom", "France", "Germany", "China", "India")) #"Andorra", "San Marino", "India"

chile <- full_data %>%  filter(location %in% c("Chile"))

#plot
ggplot(full_data, aes(x=elapsed_days, y=total_cases_per_million, group=location, color=location)) +
  geom_line(colour = 'grey95', size = 1.1) +
  geom_line(size = 1.1, data = c_compare_cases) +
  geom_line(color='darkred', size = 1.5, data = chile) +
  geom_text(aes(label = iso_code), size= 4, hjust = -0.1, data = c_compare_cases %>% filter(date == max(date))) +
  scale_colour_viridis_d(option = 'D', begin = 0,end = 0.8, direction = -1,alpha=0.8) +
  geom_text(aes(label = iso_code), color='darkred', size= 4, hjust = -0.1, data = chile %>% filter(date == max(date))) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = 'right',
        axis.line = element_line(colour = 'grey'), axis.ticks = element_line(colour = 'grey'), 
        axis.text = element_text(size = 10),
        panel.grid=element_line(color="#00000000")) +
  scale_y_continuous(trans = 'log1p', breaks = c(1,2,3,4,5,10,50, 100, 250, 500, 700, 1000, 2500, 5000, 10000)) + 
  scale_x_continuous(trans = 'identity', breaks = c(1,15,30,45, 60,75, 90,105,120)) +
  geom_hline(aes(yintercept = max(chile$total_cases_per_million)), color="red", linetype = 'dashed') +
  geom_vline(aes(xintercept = max(chile$elapsed_days)), color="red", linetype = 'dashed') +
  ylab("Cantidad de casos positivos cada millon de habitantes") +
  xlab("Días transcurridos desde el primer caso de covid19") +
  labs(caption = "Autor: L.Fernández, Data: https://ourworldindata.org/coronavirus-source-data")


#plot total_deaths_per_million

full_data_2 <- read_excel("./Data/total_deaths_per_million.xlsx")

c_compare_deaths <- full_data_2 %>% 
  filter(location %in% c("Ecuador","Brazil", "Argentina","Italy","United States",
                         "United Kingdom", "France", "Germany", "China", "India", "Spain")) #"San Marino", "India"

chile2 <- full_data_2 %>%  filter(location %in% c("Chile"))


ggplot(data=full_data_2, aes(x=elapsed_days, y=total_deaths_per_million, group=location, color=location)) +
  geom_line(colour = 'grey95', size = 1.1) +
  geom_line(size = 1.1, data = c_compare_deaths) +
  geom_line(color='darkred', size = 1.5, data = chile2) +
  geom_text(aes(label = iso_code), size= 4,hjust = -0.1, data = c_compare_deaths %>% filter(date == max(date))) +
  scale_colour_viridis_d(option = 'D', begin = 0,end = 0.8, direction = -1,alpha=0.8) +
  geom_text(aes(label = iso_code), color='darkred', size= 4, hjust = -0.1,  data = chile2 %>% filter(date == max(date))) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = 'right',
        axis.line = element_line(colour = 'grey'), axis.ticks = element_line(colour = 'grey'), 
        axis.text = element_text(size = 10),
        panel.grid=element_line(color="#00000000")) +
  scale_y_continuous(trans = 'log1p',breaks = c(1,2,3,4,5,10,25, 50,100,250, 500, 1250)) + 
  scale_x_continuous(trans = 'identity',breaks = c(1,5,10,20,30,40, 50, 60, 70, 80, 90,105, 120)) +
  geom_hline(aes(yintercept = max(chile2$total_deaths_per_million)), color="red", linetype = 'dashed') +
  geom_vline(aes(xintercept = max(chile2$elapsed_days)), color="red", linetype = 'dashed') +
  ylab("Cantidad de fallecidos cada millon de habitantes") +
  xlab("Días transcurridos desde el primer fallecido por covid19") +
  labs(caption = "Autor: L.Fernández, Data: https://ourworldindata.org/coronavirus-source-data") 

# For continuous scales, the name of a transformation object or the object itself. 
# Built-in transformations include "asn", "atanh", "boxcox", "date", "exp", "hms", "identity", 
# "log", "log10", "log1p", "log2", "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal", 
# "reverse", "sqrt" and "time".
