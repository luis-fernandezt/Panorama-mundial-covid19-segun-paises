## data covid19 - descargamos en la carpeta de trabajo
## data disponible en: https://ourworldindata.org/coronavirus-source-data
## mapa mundial - descargamos y descomprimimos en la carpeta de trabajo
## naturalearthdata.com mapa disponible en: https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
## script guardado en formato Latin1 para no perder caracteres

## cargamos librerias ####
require(raster)  
require(tidyverse)
require(sf)
library(readxl)
library(ggplot2)
require(ggspatial)
library(ggrepel)
library(ggpubr)
library(classInt)
library(viridis)
 
# cargamos mapa ####
shp <- shapefile('./Countries/ne_10m_admin_0_countries_lakes.shp') 
shp@data$NAME_EN <- iconv(shp@data$NAME_EN, from = 'UTF-8', to = 'latin1')

Contry <- st_as_sf(shp) #convertimos en un objeto sf

Contry$NAME_EN <-  c("Indonesia", #cambiamos nombres de países para que coincidan con csv
    "Malaysia",
    "Chile",
    "Bolivia",
    "Peru",
    "Argentina",
    "Dhekelia Cantonment",
    "Cyprus",
    "India",
    "China",
    "Israel",
    "Palestine",
    "Lebanon",
    "Ethiopia",
    "South Sudan",
    "Somalia",
    "Kenya",
    "Pakistan",
    "Malawi",
    "Tanzania",
    "Syria",
    "Somaliland",
    "France",
    "Suriname",
    "Guyana",
    "South Korea",
    "North Korea",
    "Morocco",
    "Western Sahara",
    "Costa Rica",
    "Nicaragua",
    "Congo",
    "Democratic Republic of Congo",
    "Bhutan",
    "Ukraine",
    "Belarus",
    "Namibia",
    "South Africa",
    "Saint Martin",
    "Bonaire Sint Eustatius and Saba",
    "Oman",
    "Uzbekistan",
    "Kazakhstan",
    "Tajikistan",
    "Lithuania",
    "Brazil",
    "Uruguay",
    "Mongolia",
    "Russia",
    "Czech Republic",
    "Germany",
    "Estonia",
    "Latvia",
    "Norway",
    "Sweden",
    "Finland",
    "Vietnam",
    "Cambodia",
    "Luxembourg",
    "United Arab Emirates",
    "Belgium",
    "Georgia",
    "Macedonia",
    "Albania",
    "Azerbaijan",
    "Kosovo",
    "Turkey",
    "Spain",
    "Laos",
    "Kyrgyzstan",
    "Armenia",
    "Denmark",
    "Libya",
    "Tunisia",
    "Romania",
    "Hungary",
    "Slovakia",
    "Poland",
    "Ireland",
    "United Kingdom",
    "Greece",
    "Zambia",
    "Sierra Leone",
    "Guinea",
    "Liberia",
    "Central African Republic",
    "Sudan",
    "Djibouti",
    "Eritrea",
    "Austria",
    "Iraq",
    "Italy",
    "Switzerland",
    "Iran",
    "Netherlands",
    "Liechtenstein",
    "Cote d'Ivoire",
    "Serbia",
    "Mali",
    "Senegal",
    "Nigeria",
    "Benin",
    "Angola",
    "Croatia",
    "Slovenia",
    "Qatar",
    "Saudi Arabia",
    "Botswana",
    "Zimbabwe",
    "Bulgaria",
    "Thailand",
    "San Marino",
    "Haiti",
    "Dominican Republic",
    "Chad",
    "Kuwait",
    "El Salvador",
    "Guatemala",
    "Timor",
    "Brunei",
    "Monaco",
    "Algeria",
    "Mozambique",
    "Swaziland",
    "Burundi",
    "Rwanda",
    "Myanmar",
    "Bangladesh",
    "Andorra",
    "Afghanistan",
    "Montenegro",
    "Bosnia and Herzegovina",
    "Uganda",
    "Guantanamo Bay Naval Base",
    "Cuba",
    "Honduras",
    "Ecuador",
    "Colombia",
    "Paraguay",
    "Portugal",
    "Moldova",
    "Turkmenistan",
    "Jordan",
    "Nepal",
    "Lesotho",
    "Cameroon",
    "Gabon",
    "Niger",
    "Burkina Faso",
    "Togo",
    "Ghana",
    "Guinea-Bissau",
    "Gibraltar",
    "United States",
    "Canada",
    "Mexico",
    "Belize",
    "Panama",
    "Venezuela",
    "Papua New Guinea",
    "Egypt",
    "Yemen",
    "Mauritania",
    "Equatorial Guinea",
    "Gambia",
    "Hong Kong",
    "Vatican",
    "Turkish Republic of Northern Cyprus",
    "United Nations Buffer Zone in Cyprus",
    "Siachen Glacier",
    "Baikonur",
    "Akrotiri",
    "Antarctica",
    "Australia",
    "Greenland",
    "Fiji",
    "New Zealand",
    "New Caledonia",
    "Madagascar",
    "Philippines",
    "Sri Lanka",
    "Curacao",
    "Aruba",
    "Bahamas",
    "Turks and Caicos Islands",
    "Taiwan",
    "Japan",
    "Saint Pierre and Miquelon",
    "Iceland",
    "Pitcairn Islands",
    "French Polynesia",
    "French Southern and Antarctic Lands",
    "Seychelles",
    "Kiribati",
    "Marshall Islands",
    "Trinidad and Tobago",
    "Grenada",
    "Saint Vincent and the Grenadines",
    "Barbados",
    "Saint Lucia",
    "Dominica",
    "United States Minor Outlying Islands",
    "Montserrat",
    "Antigua and Barbuda",
    "Saint Kitts and Nevis",
    "United States Virgin Islands",
    "Saint-Barthélemy",
    "Puerto Rico",
    "Anguilla",
    "British Virgin Islands",
    "Jamaica",
    "Cayman Islands",
    "Bermuda",
    "Heard Island and McDonald Islands",
    "Saint Helena",
    "Mauritius",
    "Comoros",
    "Sao Tome and Principe",
    "Cape Verde",
    "Malta",
    "Jersey",
    "Guernsey",
    "Isle of Man",
    "Åland Islands",
    "Faeroe Islands",
    "Australian Indian Ocean Territories",
    "British Indian Ocean Territory",
    "Singapore",
    "Norfolk Island",
    "Cook Islands",
    "Tonga",
    "Wallis and Futuna",
    "Samoa",
    "Solomon Islands",
    "Tuvalu",
    "Maldives",
    "Nauru",
    "Federated States of Micronesia",
    "South Georgia and the South Sandwich Islands",
    "Falkland Islands",
    "Vanuatu",
    "Niue",
    "American Samoa",
    "Palau",
    "Guam",
    "Northern Mariana Islands",
    "Bahrain",
    "Coral Sea Islands",
    "Spratly Islands",
    "Clipperton Island",
    "Macau",
    "Ashmore and Cartier Islands",
    "Bajo Nuevo Bank",
    "Serranilla Bank",
    "Scarborough Shoal")

## load cvs
library(readr)
owid_covid_data <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
names(owid_covid_data)

data <- owid_covid_data %>% filter(date == "2020-07-19") # usar max(date) con precaución.
View(data$location)

# fortificamos los datos ####
mps_casos <- data %>% 
  group_by(location) %>%
  dplyr::summarise(total_cases = max(total_cases)) %>%  
  ungroup()

mps_casos_sft <- st_as_sf(Contry) %>% 
  inner_join(., y = mps_casos, by = c('NAME_EN' = 'location')) 

## quantiles para casos totales segun pais ####
# quiebre por quantiles rangos ingresados manualmente
quantiles <- quantile(mps_casos_sft$total_cases, 
                      probs = c(0, 0.17, 0.33, 0.5, 0.6635, 0.8295, 0.9, 0.95, 0.975,0.99, 1),
                      type=6, names = FALSE)

labels <- c() # creamos etiqueta de clases

for(idx in 1:length(quantiles)){ #redondeamos los valores a miles "k" y dos digitos
  labels <- c(labels,paste0(round(quantiles[idx + 1] / 1000, 2), #dejando solo limite superior para visualizar
                            "k"))}

labels <- labels[1:length(labels)-1] #borramos el valor NA

mps_casos_sft$total_cases_qt <- cut(mps_casos_sft$total_cases, #guardamos la nueva variable
                     breaks = quantiles,
                     labels = labels,
                     include.lowest = TRUE)

## gg1 -plotiamos Casos_totales_qt por pais y guardamos ####
gg1 <- ggplot() +
  
  geom_sf(data = Contry, color= 'white', size=0.2, fill = 'grey') +
  geom_sf(data = mps_casos_sft, color= 'white', size=0.2,
          aes(fill = total_cases_qt, colour = total_cases_qt)) +
  
  coord_sf() +
  theme_void() +
  
  theme(plot.title=element_text(face = 'bold', hjust = 0.5, color = 'black'),
        plot.subtitle = element_text(hjust = 0.5, color = 'black'),
        legend.title = element_text(hjust = 0.5, color = 'black'),
        plot.caption = element_text(size = 6, hjust = 0.9, color = 'grey'), 
        legend.position = c(0.5, 0.05),
        legend.direction = "horizontal") +
  
  labs(x = NULL, 
       y = NULL,
       title = "Panorama Mundial Covid19 según países", 
       subtitle = "2020-07-19", 
       caption = "Autor: L. Fernández, Data: ourworldindata.org, Mapa: naturalearthdata.com, Recursos ggplot: timogrossenbacher.ch") +
  
  scale_fill_viridis(option = "magma",
                     name = "Casos covid19",
                     alpha = 1,
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(2, units = "mm"),
                                          keywidth = unit(130 / length(labels), units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 1,
                                          nrow = 1,
                                          byrow = T,
                                          reverse = F,
                                          label.position = "bottom")) +
  ylim(-67, 83)

gg1

## gguardamos gg1 como imagen (opcional)
ggsave(plot = gg1, filename = './Situacion_mundial_covid19.png', 
       units = 'mm', width = 279, height = 216, dpi = 300)
