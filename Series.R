#cargamos librerías
library(readxl)
library(ggplot2)
library(dplyr)

# cargamos base de datos
# descargamos data desde https://ourworldindata.org/coronavirus-source-data
# creamos una columna en excel llamada "elapsed_days" con la formula: "=ABS(DIAS(MIN.SI.CONJUNTO($C$2:$C$13493;$B$2:$B$13493;B2);C2))"

full_data <- read_excel("./owid_covid_data.xlsx")
summary(full_data)

pais_compare <- full_data %>% 
  filter(location %in% c("Ecuador","Brazil", "Argentina","Italy", "Spain","United States",
                         "United Kingdom", "Germany", "China"))

chile <- full_data %>%  filter(location %in% c("Chile"))
max(chile$elapsed_days)
max(chile$total_cases_per_million)
max(chile$total_deaths_per_million)

#plot total_cases_per_million
ggplot(full_data, aes(x=elapsed_days, y=total_cases_per_million, group=location, color=location)) +
  geom_line(colour = 'grey88', size = 1.1) +
  geom_line(size = 1.1, data = pais_compare) +
  geom_line(color='darkred', size = 1.5, data = chile) +
  geom_text(aes(label = location), size= 4, hjust = -0.1, data = pais_compare %>% filter(date == max(date))) +
  scale_colour_viridis_d(option = 'D', begin = 0,end = 0.8, direction = -1,alpha=0.8) +
  geom_text(aes(label = location), color='darkred', size= 4, hjust = -0.1, data = chile %>% filter(date == max(date))) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = 'none',
        axis.line = element_line(colour = 'grey'), axis.ticks = element_line(colour = 'grey'), 
        axis.text = element_text(size = 10),
        panel.grid=element_line(color="#00000000")) +
  scale_y_continuous(trans = 'log2', breaks = c(0.1,0.5,1,10,100, 500, 700, 1000, 5000, 10000, 16000)) + 
  scale_x_continuous(trans = 'sqrt', breaks = c(1,2,3,4,5,10,15,20,25,30,54,90,120,150)) +
  ylab("Cantidad de casos positivos cada millon de habitantes") +
  xlab("Días transcurridos desde el primer caso de covid19") +
  labs(caption = "Autor: L.Fernández, Data: https://ourworldindata.org/coronavirus-source-data")


#plot total_deaths_per_million
ggplot(data=full_data, aes(x=elapsed_days, y=total_deaths_per_million, group=location, color=location)) +
  geom_line(colour = 'grey88', size = 1.1) +
  geom_line(size = 1.1, data = pais_compare) +
  geom_line(color='darkred', size = 1.5, data = chile) +
  geom_text(aes(label = location), size= 4,hjust = -0.1, data = pais_compare %>% filter(date == max(date))) +
  scale_colour_viridis_d(option = 'D', begin = 0,end = 0.8, direction = -1,alpha=0.8) +
  geom_text(aes(label = location), color='darkred', size= 4, hjust = -0.1,  data = chile %>% filter(date == max(date))) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold"),
        legend.position = 'none',
        axis.line = element_line(colour = 'grey'), axis.ticks = element_line(colour = 'grey'), 
        axis.text = element_text(size = 10),
        panel.grid=element_line(color="#00000000")) +
  scale_y_continuous(trans = 'log2', breaks = c(0.1, 0.5, 1, 10, 50, 100, 250, 500, 1000,1250)) + 
  scale_x_continuous(trans = 'sqrt', breaks = c(1,2,3,4,5,10,15,18,25,30,60,90,120,150)) +
  ylab("Cantidad de fallecidos cada millon de habitantes") +
  xlab("Días transcurridos desde el primer fallecido por covid19") +
  labs(caption = "Autor: L.Fernández, Data: https://ourworldindata.org/coronavirus-source-data") 
