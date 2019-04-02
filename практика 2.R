# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Аналитический пакет R: Приктика 2

# Часть 1: Загрузка пакетов  -----------------------------------------------

# загрузка пакетов

library('dplyr')
library('lattice')
library('ggplot2')
library('data.table')
# Часть 2: Формирование массива данных -----------------------------------------------

# загружаем файл с данными по импорту масла в РФ (из прошлой практики)
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'

# создаём директорию для данных, если она ещё не существует:
if (!file.exists('./data')) {
    dir.create('./data')
}
# создаём файл с логом загрузок, если он ещё не существует:
if (!file.exists('./data/download.log')) {
    file.create('./data/download.log')
}
# загружаем файл, если он ещё не существует,
#  и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
    download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
    # сделать запись в лог
    write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
          file = './data/download.log', append = T)
}
# читаем данные из загруженного .csv во фрейм, если он ещё не существует
if (!exists('DT')){
    DT.import <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', 
                                     stringsAsFactors = F))
}
# предварительный просмотр
dim(DT.import)            # размерность таблицы
str(DT.import)            # структура (характеристики столбцов)
DT.import          # удобный просмотр объекта data.table


#фильтруем данные для получения необходимого среза данных
unique(DT.import$Reporter)
unique(DT.import$Period.Desc)
#фильтруем данные для получения необходимого среза данных
DT.import <- data.table(filter(DT.import, startsWith(Period.Desc, "January ") | startsWith(Period.Desc, "February ")
                               |startsWith(Period.Desc, "March ") |startsWith(Period.Desc, "April ")
                               |startsWith(Period.Desc, "May ") |startsWith(Period.Desc, "June ")
                               |startsWith(Period.Desc, "July ") |startsWith(Period.Desc, "August ")))

# Часть 3: Заполнение пропусков средними значениями =====================================

#проверим количество пропусков
na.num <- apply(DT.import, 2, function(x) length(which(is.na(x)))) 
sort(na.num[na.num > 0], decreasing = T) #получили один пропуск

# явное преобразование типа, чтобы избежать проблем 
#  при заполнении пропусков
DT.import[, Netweight.kg := as.double(Netweight.kg) ]
DT.import[, round(mean(.SD$Netweight.kg, na.rm = T), 0),
          by = Year]

# заменяем пропуски на средние
DT.import[, Netweight.kg.mean := round(mean(.SD$Netweight.kg,
                                            na.rm = T), 0),
          by = Year]
DT.import[!is.na(Netweight.kg), Netweight.kg.mean := Netweight.kg]
# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.mean]

# Часть 4: Формирование массива по странам -----------------------------------------------

#Разделим страны по группам
unique(DT.import$Reporter)
#страны таможенного союза
customs_union <- c('Armenia',"Belarus", "Kazakhstan", "Kyrgyzstan", "Russian Federation")
#страны СНГ, не входящий в таможенный союз
cis <- c("Azerbaijan")
#остальные страны
other_countries <- c("EU-27","Finland","Georgia","Germany","United States of America",
                     "Estonia","Ukraine","Lithuania","Latvia","Mongolia","New Zealand",
                     "United Arab Emirates","Slovenia","Egypt")
cls <- palette(rainbow(3))# обозначили цвета для графиков
#(inform.customs_union <- filter(inform, Reporter=='Armenia'|Reporter=="Belarus"|
#                                  Reporter=="Kazakhstan"|Reporter=="Kyrgyzstan"|
#                                  Reporter=="Russian Federation"))
#plot(x=inform.customs_union$Year, y=inform.customs_union$Netweight.kg.median)

#сделаем поле Reporter ключевым, чтобы сделать возможной фильтрацию
setkey(DT.import, Reporter)
#inform.customs_union <- inform[c('Armenia',"Belarus", 
#                                 "Kazakhstan", "Kyrgyzstan", "Russian Federation")]

#формируем таблицы по признаку принадлежности стран к тому или иному союзу
DT.import.customs_union <- DT.import[customs_union]
DT.import.cis <- DT.import[cis]
DT.import.other_countries <- DT.import[other_countries]

#добавим столбец фактор принадлежности к союзу
union1 <- mutate(DT.import.customs_union, c_fact='Таможенный союз')
cis2 <- mutate(DT.import.cis, c_fact='Страны СНГ')
other3 <- mutate(DT.import.other_countries, c_fact='Другие страны')

#объединяем таблицы в одну новую
DT.import<- data.table()
DT.import <- full_join(union1,cis2)
DT.import <- full_join(DT.import, other3)


year_in <- as.factor(unique(DT.import$c_fact))


#считаем суммарные постаки по годам и союзу
res <- select(DT.import, Netweight.kg.mean, c_fact,Year) %>%
  group_by(c_fact, Year) %>%
  mutate(Netweight.kg.total = sum(Netweight.kg.mean))
res1 <- na.omit(res)
# Часть 5: Построение графиков  -----------------------------------------------


#график, построенный с помощью пакета base------ 
png(filename = 'Pic-1.png', width = 500, height = 500)
plot(x = res1$Year, y = res1$Netweight.kg.total, type = 'p',
     pch = 21, bg = cls[as.factor(res1$c_fact)],
     axes = F, ylim=c(0, 1000000),
     xlim=c(2010,2018),
     xlab = 'Год продажи',
     ylab = 'Количество проданной продукции',
     main = 'пакет base')
# легенда
legend('topright', legend = year_in, fill = cls[year_in])

axis(side = 1, pos = 0, at = seq(0, 150, by = 50),
     labels = seq(0, 150, by = 50))

     
# горизонтальная ось
axis(side = 1, pos = 0, at = seq(2010, 2018, by = 1),
     labels = seq(2010, 2018, by = 1))



dev.off()

#график, построенный с помощью пакета lattice-----
pic2 <- xyplot(Netweight.kg.total~Year, data=res1,
               key = list(text = list(lab = as.character(year_in)),
                          points = list(pch = 21,
                                        col = palette(rainbow(3))[year_in],
                                        fill = palette(rainbow(3))[year_in]),
                          title = 'Группы стран',
                          cex.title = 1),
               fill.color = cls[as.factor(res1$c_fact)],
               xlab = "Год продажи",
               ylab = 'Количество проданной продукции',
               main = 'пакет lattice',
               panel = function(x,y,fill.color,...,subscipts){
                 fill = fill.color[subscipts]
                 panel.xyplot(x,y,pch = 19,
                              col = fill)})
xyplot(Netweight.kg.total~Year, data=res1)

png(filename = 'Pic-2.png', width = 500, height = 500)
dev.off()
#график построенный с помощью ggplot2-----------
qplot(Year, Netweight.kg.total, data = res1)
png(filename = 'Pic-3.png', width = 500, height = 500)
qplot(Year, Netweight.kg.total, data = res1, color = as.factor(c_fact),
      xlab = 'Год продажи',
      ylab = 'Количество проданной продукции',
      main = 'пакет ggplot2')
dev.off()

