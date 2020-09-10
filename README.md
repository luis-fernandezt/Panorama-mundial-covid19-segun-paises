## Panorama mundial Covid19 según países.
En este repositorio puede encontrar dos scrips en lenguaje R, para visualizar la situación y evolución del Covid19 según países, con data proveniente desde [ourworldindata.org](https://ourworldindata.org/coronavirus-source-data). Los scrips son los siguientes:
* [Wold script.R](https://github.com/luis-fernandezt/Panorama-mundial-covid19-segun-paises/blob/master/Wold_script.R) - Primer script con código e instrucciones para construir un mapa mundial con los casos positivos de covid19 según países, utilizando principalmente las librerías de ggplot2, mapa mundi en formato shp disponible en [naturalearthdata.com](https://www.naturalearthdata.com/downloads/10m-cultural-vectors/).
![mapa](https://github.com/luis-fernandezt/Panorama-mundial-covid19-segun-paises/blob/master/Graficos/Situacion_mundial_covid19.png)
* [Series de tiempo.R](https://github.com/luis-fernandezt/Panorama-mundial-covid19-segun-paises/blob/master/Series_de_tiempo.R) - Script dedicado a la construcción de una serie de tiempo desde el primer caso confirmado como contagiado y fallecido por causa del covid19, dando enfoque a países con altas incidencias y la posición general de Chile dentro del panorama mundial. 
![casos_mill](https://github.com/luis-fernandezt/Panorama-mundial-covid19-segun-paises/blob/master/Graficos/total_cases_per_million.png)
![fall_mill](https://github.com/luis-fernandezt/Panorama-mundial-covid19-segun-paises/blob/master/Graficos/total_deaths_per_million.png)

Fuente: Elaboración propia.

#### **Versión de Rstudio:**
"R version 3.6.3 (2020-02-29)"
