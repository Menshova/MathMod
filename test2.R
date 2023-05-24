# Меньшова С.С.
# создайте модель множественной линейной регрессии ночных потоков 
# углекислого газа за весенний период 2013 года по данным измерений методом турбулентной пульсации

rm(list=ls())
library("tidyverse")
library("readr")
library("stringr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stringr")
library("lubridate")

tbl = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))

tbl = tbl[-1,]; tbl
tbl = select(tbl, -(roll))

tbl$date <- as.Date (tbl$date, format = "%Y-%m-%d")

tbl = mutate(tbl, month = months(tbl$date))
tbl = mutate(tbl, year = year(tbl$date))


tbl = tbl %>% mutate_if(is.character, factor)
glimpse(tbl)

names(tbl) = names(tbl) %>%
  str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%
  str_replace_all("[*]","_star_") %>%
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(tbl)


tbl = filter(tbl, daytime %in% FALSE)
# Оставляем необходимый год
tbl = filter(tbl, year %in% 2013)
# Оставляем необходимые месяцы
tbl = filter(tbl, month %in% c("Май"))
unique(tbl$month) # проверяем какие месяца остались
# Убираем пустые значения
tbl = drop_na(tbl)



tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
cor_td = cor(drop_na(tbl_numeric), method = "spearman") %>% as.data.frame %>% select(co2_flux)
cor_td
cor_td = drop_na(cor_td)
cor_td
vars = row.names(cor_td)[cor_td$co2_flux^2 > 0.2] %>% na.exclude

formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""));formula

#создание моделей
model0=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + w_div_co2_cov + co2...125 + co2...127), data = tbl_numeric)
coef(model0)
resid(model0)
confint(model0)
summary(model0)
anova(model0)

model1=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + co2...125), data = tbl_numeric)
coef(model1)
resid(model1)
confint(model1)
summary(model1)
anova(model1)
anova(model0, model1)

model2=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + co2...125), data = tbl_numeric)
coef(model2)
resid(model2)
confint(model2)
summary(model2)
anova(model2)
anova(model1, model2)

