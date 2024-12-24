library(dplyr)
library(tidyr)
library(readr)
library(jsonlite)
library(sf)
library(geojsonsf)
library(ggplot2)
library(ggthemes)
# Задание 1. Агбафа Ж. С. вариант 1 
# для района Бибирево докажите что диаметр стволов родов Тополь и Ива значимо отличаются.
## Начало - указываем рабочую директорию
setwd ("C:/Modé li/dose")
# Проверим рабочую директорию
getwd()
# Считаем файл greendb в переменную date
date= read.csv('greendb.csv', sep = ',', dec = '.');
date
map = sf::read_sf("moscow.geojson")
# Установим нужные для работы пакеты
# install.packages('dplyr')
# install.packages('stringr')
# install.packages('car')
# Подключим установленные пакеты
library(dplyr)
library(stringr)
library(car)

# Зададим параметр species, в который запишем все названия деревьев на русском
species = date$species_ru; 
species

# Посчитаем количество деревьев разных видов
date %>% group_by(species_ru) %>% summarise(N = n()) %>%
  arrange(desc(N)) 

# Выделим из общего названия род дерева
genre= stringr::str_split(species, pattern = ' ', simplify = TRUE)[,1]; 
genre

#добавим новый столбец Genus
datagenre = date %>% mutate(Genus = genre); 
datagenre

# Фильтруем данные район Бибирево,Тополь , Ива
datarégion = datagenre %>% filter(Genus %in% c('Тополь','Ива')) %>% filter(adm_region == 'район Бибирево'); 
datarégion
datarégion$Genus=as.factor(datarégion$Genus)

dominant_diameter  = datarégion %>% group_by(species_ru)%>%
  summarise(count = n()) %>%
  arrange(desc(count))
# Объединяем данные с картой
map = map %>% mutate(dominant_diameter= NAME)
map = left_join(map, max_width, by = "adm_region")

# Построение картосхемы для Липы
ggplot(map) +
  geom_sf(aes(fill = "Тополь")) + 
  theme() 


# Построение картосхемы для Ели
ggplot(map) +
  geom_sf(aes(fill = "Ива")) + 
  theme

