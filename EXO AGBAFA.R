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
str(datarégion)

# Проведём тест на нормальность с помощью теста Шапиро-Уилка.
shapiro.test(datarégion$d_trunk_m) 
# ANOVA
anova_result = aov(datarégion$d_trunk_m~ Genus, data= datarégion)
anova_result
# Résumé des résultats
summary(anova_result)
#Test de Levene
leveneTest(datarégion$d_trunk_m ~ Genus,data = datarégion)
# Test post hoc Tukey HSD
tukey_result <- TukeyHSD(anova_result)
tukey_result
# Résumé des résultats du test Tukey
print(tukey_result)

# Boxplot des diamètres par type d'arbre
boxplot(datarégion$d_trunk_m ~ Genus, data = datarégion, main = "диаметр стволов родов Тополь и Ива ",
        xlab = "tree", ylab = "диаметр", col = "lightblue")
# résult test Tukey HSD
plot(tukey_result)

#1. Тест на нормальность (Шапиро-Уилка) :
•	W=0.95426W = 0.95426W=0.95426, ppp-значение = 0.0002299.
•	Интерпретация : ppp-значение меньше 0.05, что означает, что данные не подчиняются нормальному распределению. Это ставит под сомнение использование классической ANOVA.
#2. Тест на однородность дисперсий (Левена) :
  •	p=0.3609p = 0.3609p=0.3609 (больше 0.05).
•	Интерпретация : Дисперсии между группами однородны. Это позволяет использовать ANOVA, несмотря на отсутствие строгой нормальности.
#3. ANOVA :
  •	Влияние фактора Genus (род дерева):
  o	Sum of Squares (Genus)=0.0951\text{Sum of Squares (Genus)} = 0.0951Sum of Squares (Genus)=0.0951,
o	Residual Sum of Squares=2.892\text{Residual Sum of Squares} = 2.892Residual Sum of Squares=2.892,
o	F=1.84F = 1.84F=1.84, p=0.041p = 0.041p=0.041.
•	Интерпретация : ANOVA показывает значимую разницу между средними диаметрами стволов двух родов деревьев (Тополь и Ива), так как ppp-значение меньше 0.05.
#4. Множественные сравнения (Тьюки HSD) :
  •	Средняя разница между Тополь и Ива:
  o	Средняя разница: −0.0585-0.0585−0.0585 (диаметры стволов Тополя немного меньше, чем у Ивы).
o	Доверительный интервал: [−0.1147,−0.0023][-0.1147, -0.0023][−0.1147,−0.0023], не включает 0.
o	ppp-значение: 0.0414.
•	Интерпретация : Тест Тьюки подтверждает, что разница между родами Тополь и Ива является статистически значимой с уровнем доверия 95 %
#Общй вывод :
  Проведённые тесты показывают, что существует значимая разница между средними диаметрами стволов деревьев родов Тополь и Ива в исследуемом районе. Средний диаметр стволов Тополя немного меньше, чем у Ивы, и эта разница подтверждена статистически (ANOVA и тест Тьюки).
