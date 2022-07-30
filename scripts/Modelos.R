## Limpiamos
rm(list=ls())


require(pacman) 

## Llama/instala-llama las librer?as listadas
p_load(tidyverse,rio,
       sf, # Leer/escribir/manipular datos espaciales
       leaflet, # Visualizaciones din?micas
       tmaptools, # geocode_OSM()
       osmdata,
       spdep,
       secr,
       osmdata,
       here) # Get OSM's data
path = here('')
##Librerias requeridas
rm(list=ls())
require("pacman")
p_load("here")
p_load("readr")
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(rio) # Librer?a para importar datos 
p_load(tidyverse) # Librer?a para limpiar datos
p_load(e1071) # Tiene la funci?n para calcular skewness
p_load(EnvStats) # Transformaci?n Box-Cox
p_load(tidymodels) # Modelos ML
p_load(ggplot2) # Librer?a para visualizar datos
p_load(scales) # Formato de los ejes en las gr?ficas
p_load(ggpubr) # Combinar gr?ficas
p_load(knitr) # Tablas dentro de Rmarkdown
p_load(kableExtra) # Tablas dentro de Rmarkdown
p_load(dplyr)
p_load(caret)
p_load(glmnet)
p_load(pls)
p_load(tidyr)
p_load(tibble)
p_load(gtsummary)

#Cargamos las bases de test y train
testR <- readRDS("C:/Users/francisco.alejandro1/Documents/BD/Proyecto Final/Proyecto-Final/stores/testR.rds")
trainR <- readRDS("C:/Users/francisco.alejandro1/Documents/BD/Proyecto Final/Proyecto-Final/stores/trainR.rds")

#Analizamos la correlación de nuestras variables dependientes
cor(trainR$pobl_tot, trainR$areaoficialkm2)
cor(trainR$pobl_tot, trainR$discapital)
cor(trainR$pobl_tot, trainR$g_cap)
cor(trainR$pobl_tot, trainR$finan_credito)
cor(trainR$pobl_tot, trainR$vrf_peq_productor)
cor(trainR$pobl_tot, trainR$lights_mean)
cor(trainR$discapital, trainR$g_cap)
cor(trainR$discapital, trainR$finan_credito)
cor(trainR$discapital, trainR$vrf_peq_productor)
cor(trainR$discapital, trainR$lights_mean)
cor(trainR$discapital, trainR$areaoficialkm2)



#Modelo 1 Tradicional****************************************

mod1 <- lm(pib_cons ~ pobl_tot + areaoficialkm2 + discapital + g_cap + finan_credito + vrf_peq_productor + lights_mean , data = trainR)
summary(mod1)

##CORREMOS OLS 
model1 <- train(pib_cons ~ pobl_tot + areaoficialkm2 + discapital + g_cap + finan_credito + vrf_peq_productor + lights_mean,
                # model to fit
                data = trainR,
                trControl = trainControl(method = "cv", number = 5), method = "lm")

model1 
564814.1^2
##MSE=3.19015e+11

##Coeficientes model 1 OLS
df_coeficientes <- model1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

##Gráfica importancia coeficientes model 1 OLS
df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 5, angle = 45))

# Predicciones de test
predicciones_test_ols <- predict(model1, newdata = testR)
predicciones_ols<-as.data.frame(predicciones_test_ols)


#Modelo 2 Ridge****************************************

x_train <- model.matrix( ~ pobl_tot + areaoficialkm2 + discapital + g_cap + finan_credito + vrf_peq_productor + lights_mean, data = trainR)[, -1]
y_train <- trainR$pib_cons

#scale(x_train)
model2_ridge <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  nlambda     = 100,
  standardize = T
)

regularizacion <- model2_ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model2_ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funci?n de la regularizaci?n") +
  theme_bw() +
  theme(legend.position = "none")

cv_error_ridge <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = T
)

plot(cv_error_ridge)
paste("Mejor valor de lambda encontrado:", cv_error_ridge$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviaci?n est?ndar:", cv_error_ridge$lambda.1se)
cv_error_ridge ##Lambda min=323067, MSE=8.329e+10 

modelo2_ridge_lambdamin <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error_ridge$lambda.min,
  standardize = TRUE
)

modelo2_ridge_lambdamin


df_coeficientes_ridge <- coef(modelo2_ridge_lambdamin) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

##Coeficientes modelo ridge
df_coeficientes_ridge

##Gráfica importancia coeficientes ridge
df_coeficientes_ridge %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

##Predicciones en test
x.test <- model.matrix( ~ pobl_tot + areaoficialkm2 + discapital + g_cap + finan_credito + vrf_peq_productor + lights_mean, testR)[, -1]
predict_test_ridge <- predict(modelo2_ridge_lambdamin, newx = x.test)
predict_test_ridge

#MSE del ridge= 9.833e+10 

#Modelo 3 Lasso****************************************

model_lasso <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 100,
  standardize = TRUE
)

regularizacion <- model_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = model_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en funci?n de la regularizaci?n") +
  theme_bw() +
  theme(legend.position = "none")


cv_error_lasso <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

cv_error_lasso #min Lambda=7639, MSE=8.786e+10
plot(cv_error_lasso)
paste("Mejor valor de lambda encontrado:", cv_error_lasso$lambda.min)
paste("Mejor valor de lambda encontrado + 1 desviaci?n est?ndar:", cv_error_lasso$lambda.1se)


model_lasso_min <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_lasso$lambda.min,
  standardize = TRUE
)

model_lasso_min

df_coeficientes_lasso <- coef(model_lasso_min) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

#Coeficientes modelo lasso
df_coeficientes_lasso

#Gráfica importancia coeficientes lasso
df_coeficientes_lasso %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo lasso") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# Predicciones en test
predict_test_lasso <- predict(model_lasso_min, newx = x.test)
predict_test_lasso
# MSE de entrenamiento= 8.786e+10

#Modelo 4--Elastic Net-----------------------------------------------------------
el <- train(pib_cons ~ pobl_tot + areaoficialkm2 + discapital + g_cap + finan_credito + vrf_peq_productor + lights_mean, data = trainR, method = "glmnet",
            trControl = trainControl("cv", number = 10), preProcess = c("center", "scale"))

el ##The final values used for the model were alpha = 0.1 and lambda = 6461.332
## RMSE= 520262.2
520262.2^2
##MSE= 270672756749

# Model Prediction en test
price_predict_el <- predict(el, testR)
price_predict_el

require(randomForest)
#Modelo 5 Superlearners------------------------------------------------------------
require("tidyverse")
require("ranger")
require("SuperLearner")
install.packages("VGAM", dependencies = FALSE)
require("VGAM")
# set the seed for reproducibility
set.seed(123)
XS <- data.frame(trainR$pobl_tot, trainR$areaoficialkm2, trainR$discapital, trainR$g_cap, trainR$finan_credito, trainR$vrf_peq_productor, trainR$lights_mean)
XS<-rename(XS, pobl_tot =trainR.pobl_tot)
XS<-rename(XS, areaoficialkm2 =trainR.areaoficialkm2)
XS<-rename(XS, discapital =trainR.discapital)
XS<-rename(XS, g_cap =trainR.g_cap)
XS<-rename(XS, finan_credito =trainR.finan_credito)
XS<-rename(XS, vrf_peq_productor =trainR.vrf_peq_productor)
XS<-rename(XS, lights_mean =trainR.lights_mean)

YS <- trainR$pib_cons

str(XS)
folds = 5
index <- split(1:1000, 1:folds)
##Aquí hacemos un SuperLearnes por medio de OLS, RF y Glmnet
fitY <- SuperLearner(Y = YS, X = XS,
                     method = "method.NNLS", SL.library = c("SL.mean","SL.lm", "SL.ranger", "SL.glmnet"),
                     cvControl = list(V = folds))

fitY ## Nos dice que el mejor modelo es ranger_All:0.95785557;glmnet_All:0.04214443

# Now predict the outcome for all possible x
yS <- predict(fitY, newdata = data.frame(XS),onlySL = T)$pred
# Create a dataframe of all x and predicted SL responses
Dl1 <- data.frame(XS, yS)


#RMSE de todos los modelos realizados en el train, esto nos dice que el de menor MSE
#es el Superlearner
#OLS MSE=3.19015e+11
#Ridge min=323067, MSE=8.329e+10 
#Lasso Lambda min =7639, MSE=8.786e+10
#Elastic Net MSE=270672756749= 2.70e+11
#Superlearner Ranger_MSE:2.459992e+11; glmnet_MSE:3.105874e+11

##Predicciones en test
x.test <- model.matrix( ~ pobl_tot + areaoficialkm2 + discapital + g_cap + finan_credito + vrf_peq_productor + lights_mean, testR)[, -1]
predict_test_ridge <- predict(modelo2_ridge_lambdamin, newx = x.test)
predict_test_ridge


# Create a dataframe of all x and predicted SL responses
predictionsDB <- data.frame(testR, predict_test_ridge)

#Estadisticas descriptivas del precio predicho
summary(predictionsDB$s0)


#Submission file
submission<-data.frame(testR$municipio,testR$depto,testR$ano,predictionsDB$s0)
write.csv(submission,"C:/Users/francisco.alejandro1/Documents/BD/Taller 3/result_pred.csv", row.names = FALSE)



