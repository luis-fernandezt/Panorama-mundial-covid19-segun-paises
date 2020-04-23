## data covid19 - descargamos en la carpeta de trabajo
## data disponible en: chttps://ourworldindata.org/coronavirus-source-data
## mapa mundial - descargamos y descomprimimos en la carpeta de trabajo
## gadm.org mapa disponible en: https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_shp.zip

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

## cargamos mapa mundi ####
shp <- shapefile('./gadm36_levels_shp/gadm36_0.shp') 
shp@data$NAME_0 <- iconv(shp@data$NAME_0, from = 'UTF-8', to = 'latin1')

shp <- shp[shp@data$NAME_0 != "Antarctica" ,  ] # extraemos Antártica

Contry <- st_as_sf(shp) #convertimos en un objeto sf (lento)
view(Contry$NAME_0)

## cargamos datos desde el archivo *.csv ####

full_data <- read.csv("./full_data_21abr.csv")
tail(full_data)

tbl <- full_data %>% filter(date == "2020-04-21") #fecha de interés
table(tbl$location)

## cambiamos los nombres de países para que shp coincida con los del archivo *.csv ####
Contry$NAME_0 <- c(
"Aruba",
"Afghanistan",
"Angola",
"Anguilla",
"Åland",
"Albania",
"Andorra",
"United Arab Emirates",
"Argentina",
"Armenia",
"American Samoa",
"French Southern Territories",
"Antigua and Barbuda",
"Australia",
"Austria",
"Azerbaijan",
"Burundi",
"Belgium",
"Benin",
"Bonaire Sint Eustatius and Saba",
"Burkina Faso",
"Bangladesh",
"Bulgaria",
"Bahrain",
"Bahamas",
"Bosnia and Herzegovina",
"Saint-Barthélemy",
"Belarus",
"Belize",
"Bermuda",
"Bolivia",
"Brazil",
"Barbados",
"Brunei",
"Bhutan",
"Bouvet Island",
"Botswana",
"Central African Republic",
"Canada",
"CocosIslands",
"Switzerland",
"Chile",
"China",
"Cote d'Ivoire",
"Cameroon",
"Democratic Republic of Congo",
"Congo",
"Cook Islands",
"Colombia",
"Comoros",
"Cape Verde",
"Costa Rica",
"Cuba",
"Curacao",
"Christmas Island",
"Cayman Islands",
"Cyprus",
"Czech Republic",
"Germany",
"Djibouti",
"Dominica",
"Denmark",
"Dominican Republic",
"Algeria",
"Ecuador",
"Egypt",
"Eritrea",
"Western Sahara",
"Spain",
"Estonia",
"Ethiopia",
"Finland",
"Fiji",
"Falkland Islands",
"France",
"Faeroe Islands",
"Micronesia",
"Gabon",
"United Kingdom",
"Georgia",
"Guernsey",
"Ghana",
"Gibraltar",
"Guinea",
"Guadeloupe",
"Gambia",
"Guinea-Bissau",
"Equatorial Guinea",
"Greece",
"Grenada",
"Greenland",
"Guatemala",
"French Guiana",
"Guam",
"Guyana",
"Hong Kong",
"Heard Island and McDonald Islands",
"Honduras",
"Croatia",
"Haiti",
"Hungary",
"Indonesia",
"Isle of Man",
"India",
"British Indian Ocean Territory",
"Ireland",
"Iran",
"Iraq",
"Iceland",
"Israel",
"Italy",
"Jamaica",
"Jersey",
"Jordan",
"Japan",
"Kazakhstan",
"Kenya",
"Kyrgyzstan",
"Cambodia",
"Kiribati",
"Saint Kitts and Nevis",
"South Korea",
"Kuwait",
"Laos",
"Lebanon",
"Liberia",
"Libya",
"Saint Lucia",
"Liechtenstein",
"Sri Lanka",
"Lesotho",
"Lithuania",
"Luxembourg",
"Latvia",
"Macao",
"Saint-Martin",
"Morocco",
"Monaco",
"Moldova",
"Madagascar",
"Maldives",
"Mexico",
"Marshall Islands",
"Macedonia",
"Mali",
"Malta",
"Myanmar",
"Montenegro",
"Mongolia",
"Northern Mariana Islands",
"Mozambique",
"Mauritania",
"Montserrat",
"Martinique",
"Mauritius",
"Malawi",
"Malaysia",
"Mayotte",
"Namibia",
"New Caledonia",
"Niger",
"Norfolk Island",
"Nigeria",
"Nicaragua",
"Niue",
"Netherlands",
"Norway",
"Nepal",
"Nauru",
"New Zealand",
"Oman",
"Pakistan",
"Panama",
"Pitcairn Islands",
"Peru",
"Philippines",
"Palau",
"Papua New Guinea",
"Poland",
"Puerto Rico",
"North Korea",
"Portugal",
"Paraguay",
"Palestina",
"French Polynesia",
"Qatar",
"Reunion",
"Romania",
"Russia",
"Rwanda",
"Saudi Arabia",
"Sudan",
"Senegal",
"Singapore",
"South Georgia and the South Sandwich Islands",
"Saint Helena",
"Svalbard and Jan Mayen",
"Solomon Islands",
"Sierra Leone",
"El Salvador",
"San Marino",
"Somalia",
"Saint Pierre and Miquelon",
"Serbia",
"South Sudan",
"Sao Tome and Principe",
"Suriname",
"Slovakia",
"Slovenia",
"Sweden",
"Swaziland",
"Sint Maarten (Dutch part)",
"Seychelles",
"Syria",
"Turks and Caicos Islands",
"Chad",
"Togo",
"Thailand",
"Tajikistan",
"Tokelau",
"Turkmenistan",
"Timor",
"Tonga",
"Trinidad and Tobago",
"Tunisia",
"Turkey",
"Tuvalu",
"Taiwan",
"Tanzania",
"Uganda",
"Ukraine",
"United States Minor Outlying Islands",
"Uruguay",
"United States",
"Uzbekistan",
"Vatican",
"Saint Vincent and the Grenadines",
"Venezuela",
"British Virgin Islands",
"United States Virgin Islands",
"Vietnam",
"Vanuatu",
"Wallis and Futuna",
"Samoa",
"Akrotiri and Dhekelia",
"Caspian Sea",
"Clipperton Island",
"Kosovo",
"Northern Cyprus",
"Paracel Islands",
"Spratly Islands",
"Yemen",
"South Africa",
"Zambia",
"Zimbabwe")

# fortificamos los datos ####
mps_casos <- tbl %>% 
  group_by(location) %>%
  dplyr::summarise(total_cases = max(total_cases)) %>%  
  ungroup()

mps_casos_sft <- st_as_sf(Contry) %>% 
  inner_join(., y = mps_casos, by = c('NAME_0' = 'location')) 

## quantiles para casos totales según país ####
# quiebre por quantiles rangos ingresados manualmente
quantiles <- quantile(mps_casos_sft$total_cases, 
                      probs = c(0, 0.17, 0.33, 0.5, 0.6635, 0.8295, 0.9, 0.95, 0.975,0.99, 1),
                      type=6, names = FALSE)

labels <- c() # creamos etiqueta de clases

for(idx in 1:length(quantiles)){ #redondeamos los valores a miles "k" y dos digitos
  labels <- c(labels,paste0(round(quantiles[idx + 1] / 1000, 2), #dejando solo límite superior para visualizar
                            "k"))
}

labels <- labels[1:length(labels)-1] #borramos el valor NA

mps_casos_sft$total_cases_qt <- cut(mps_casos_sft$total_cases, #guardamos la nueva variable
                     breaks = quantiles,
                     labels = labels,
                     include.lowest = TRUE)

## gg1 -plotiamos Casos_totales_qt por país y guardamos ####
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
        legend.position = c(0.5, 0.06),
        legend.direction = "horizontal") +
  
  labs(x = NULL, 
       y = NULL,
       title = "Panorama Mundial Covid19 según países", 
       subtitle = "21 de abril de 2020", 
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

## gguardamos gg1 como imagen
ggsave(plot = gg1, filename = './Situacion_mundial_covid19.png', 
       units = 'mm', width = 279, height = 216, dpi = 300)
