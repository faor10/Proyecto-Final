rm(list=ls())
require(pacman)
p_load(here , tidyverse, haven, gtsummary)
path = here('')

train <- read_dta(here(path,"stores/train.dta"))
test <- read_dta(here(path,"stores/test.dta"))

table(is.na(train$codmpio))
table(is.na(train$pobl_tot))
table(is.na(train$areaoficialkm2))
table(is.na(train$discapital))
table(is.na(train$g_cap))
table(is.na(train$finan_credito))
table(is.na(train$vrf_peq_productor))
table(is.na(train$lights_mean))
table(is.na(train$pib_cons))

table(is.na(test$codmpio))
table(is.na(test$pobl_tot))
table(is.na(test$areaoficialkm2))
table(is.na(test$discapital))
table(is.na(test$g_cap))
table(is.na(test$finan_credito))
table(is.na(test$vrf_peq_productor))
table(is.na(test$lights_mean))

saveRDS(train,paste0(path,"/stores/trainR.rds"))
saveRDS(test,paste0(path,"/stores/testR.rds"))

modelo1 <- lm(pib_cons ~ pobl_tot+areaoficialkm2+discapital+g_cap+lights_mean+finan_credito+vrf_peq_productor, data = train)
summary(modelo1)
modelo2 <- lm(pib_total ~ pobl_tot+areaoficialkm2+discapital+g_cap+lights_mean+finan_credito+vrf_peq_productor, data = train)
summary(modelo2)

summary <- train %>% select(pib_cons,pobl_tot,areaoficialkm2,discapital,g_cap,lights_mean,finan_credito,vrf_peq_productor)
summary %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                               all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = list(pib_cons~"PIB Constante", pobl_tot ~ "Poblaci�n total", 
                                     areaoficialkm2~"�rea oficial km2",discapital~"Distancia a la capital",
                                     g_cap~"Gastos de capital", g_cap~"Promedio de luminosidad",
                                     finan_credito~"Cr�dito interno y externo",vrf_peq_productor~"Valor cr�dito productores")
                        
)

summary <- train %>% select(pib_cons,pobl_tot,areaoficialkm2,discapital,g_cap,lights_mean,finan_credito,vrf_peq_productor)
summary %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = list(pib_cons~"PIB Constante", pobl_tot ~ "Poblaci�n total", 
                                     areaoficialkm2~"�rea oficial km2",discapital~"Distancia a la capital",
                                     g_cap~"Gastos de capital", lights_mean~"Promedio de luminosidad",
                                     finan_credito~"Cr�dito interno y externo",vrf_peq_productor~"Valor cr�dito productores"))

summary <- test %>% select(,pobl_tot,areaoficialkm2,discapital,g_cap,lights_mean,finan_credito,vrf_peq_productor)
summary %>% tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                         all_categorical() ~ "{n} / {N} ({p}%)"),
                        label = list( pobl_tot ~ "Poblaci�n total", 
                                     areaoficialkm2~"�rea oficial km2",discapital~"Distancia a la capital",
                                     g_cap~"Gastos de capital", lights_mean~"Promedio de luminosidad",
                                     finan_credito~"Cr�dito interno y externo",vrf_peq_productor~"Valor cr�dito productores"))

