#Меньшова С.С. Д-А 132.Вариант 8.

#работа с пакетами
install.packages("tidyverse")
install.packages("rnoaa")
library(tidyverse)
library(rnoaa)
library(dplyr)
library(lubridate)

#данные по метеостанциям
station_data = ghcnd_stations()
write.csv(station_data, file = "station_data.csv")
station_data = read_csv("station_data.csv")

#данные по ближайшим метеостанциям
yoshkarola = data.frame(id="YOSHKAROLA", latitude = 56.631600, longitude = 47.886178)
yoshkarola_around = meteo_nearby_stations(lat_lon_df = yoshkarola,
                                          station_data = station_data,
                                          limit = 13, var = c("PRCP","TAVG"),
                                          year_min = 1999, year_max = 2000)


yoshkarola_id = yoshkarola_around$YOSHKAROLA$id[1]
all_yoshkarola_data = meteo_tidy_ghcnd(stationid = yoshkarola_id)

summary(all_yoshkarola_data)

all_yoshkarola_data$date
class(all_yoshkarola_data$date)
all_yoshkarola_data$date+1
as.numeric(all_yoshkarola_data$date)

#работа с табличными данными
all_yoshkarola_data1 = all_yoshkarola_data %>% mutate(
  year = year(all_yoshkarola_data$date),
  month = month(all_yoshkarola_data$date),
  day = yday(all_yoshkarola_data$date)
) %>% select(year, month, day, tavg)

all_yoshkarola_data2 = all_yoshkarola_data1 %>% mutate(tavg = case_when(TRUE ~ tavg/10))
all_yoshkarola_data2 = filter(all_yoshkarola_data2,year > 1998 & year < 2000)


all_yoshkarola_data2[is.na(all_yoshkarola_data2$tavg),"tavg"] = 0
all_yoshkarola_data2[all_yoshkarola_data2$tavg<7, "tavg"] = 0
summary(all_yoshkarola_data2) 

#сгруппироуем по году и месяцу и просуммируем температуры
group_meteodata =all_yoshkarola_data2 %>% group_by(year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

# подготовка данных
y = 1.0 #коэффициент для экспозиции склона
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
#константа, из табл.1
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)

#расчет di
tdi = all_yoshkarola_data %>% mutate(date=ymd(date), year=year(date),
                                  month=month(date)) %>%group_by(year,month) %>%
                                 mutate(dm = n(),
                                 gd = case_when(
                                 tavg >= 70 ~ T,
                                  tavg <70 ~ F
              )) %>% summarise(di = sum(gd)/mean(dm))

tdi = tdi %>% filter(year>1998 & year < 2000) %>% ungroup() %>% group_by(month) %>%
  summarise(di = mean(di))
tdi$di[0:3]=0
tdi = tdi %>% mutate(tdis = cumsum(di))
tdi = tdi %>% mutate(di = case_when(tdis > 4 ~ di - (tdis-4),TRUE ~ di))
tdi = tdi %>% mutate(di = case_when(di < 0 ~ 0,TRUE ~ di))

di = tdi$di

Kf = 300 # коэффициент использования ФАР посевом
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 # коэффициент «Сумма частей основной и побочной продукции
Ej = 25 # коэффициент «Стандартная влажность культуры


#Расчитаем Fi по месяцам
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Расчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))

#Расчитаем урожайность
Yield = (sum(sumT_month$Yi))

#Урожайность 17,35 ц/га
