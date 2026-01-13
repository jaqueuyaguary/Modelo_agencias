# Cargar librerías
library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(openxlsx)
library(data.table)
library(plm)
library(car)
library(lmtest)
library(sandwich)

# Limpiar memoria
rm(list=ls())

# fijamos el directorio

setwd("/Users/luis/Downloads/proyecto_agencias")

#####################################################################################
#             PREPARACIÓN DE LOS DATOS                                              #
#####################################################################################


# Cargar el archivo Excel
datos <- read_excel("datos_agencias.xlsx")

setDT(datos) # Asegura que el objeto es un data.table

# --- CONVERTIR TODOS LOS BALANCES A POSITIVOS ---

# Identificar las columnas de balance (las que empiezan con 'AGENCIA_') y aplicar la función abs() (valor absoluto) a esas columnas.
columnas_agencia <- grep("^AGENCIA_", names(datos), value = TRUE)

# Aplicar la función abs() usando lapply sobre las columnas identificadas
datos[, (columnas_agencia) := lapply(.SD, abs), .SDcols = columnas_agencia]


# Transformar a la estructura de Panel (Dos Pasos con data.table)

# PASO 1: Agrupar las Agencias (Melting) - ESPECIFICAR data.table::melt
datos_largo <- data.table::melt(
  datos,
  id.vars = c("FECHA", "CUENTA"),
  measure.vars = patterns("^AGENCIA_"), # Identifica las columnas AGENCIA_X
  variable.name = "Agencia",
  value.name = "Valor"
)

# PASO 2: Desplegar las Cuentas (Dcasting) - ESPECIFICAR data.table::dcast
datos_panel <- data.table::dcast(
  datos_largo,
  FECHA + Agencia ~ CUENTA,
  value.var = "Valor"
)

# 3. Liberar memoria si el archivo es muy grande
rm(datos, datos_largo, columnas_agencia)
gc() # Ejecuta el recolector de basura

b<-datos_panel

b[is.na(b)] <- 0

b$mes=month(b$FECHA)

#Fondos disponibles netos
b$fon_disp_n = b$X11 - b$X1105
#Inversiones líquidas
b$inv_liq = b$X1301 + b$X1302 + b$X1303 + b$X1304 + b$X1305 + b$X1306
#Activos líquidos
b$act_liq = b$fon_disp_n + b$inv_liq
#Pasivos exigibles
b$pas_exigibles = b$X2101 + b$X2103 + b$X2105 + b$X23 + b$X26 + b$X27 + b$X2903
#Índice de liquidez general
b$liquid_gen = b$act_liq/b$pas_exigibles

#Cartera que no devenga intereses
b$car_qndi= b$X1425+ b$X1426+ b$X1427+ b$X1428+ b$X1432+ b$X1433+ b$X1434+ b$X1435+ b$X1436+ b$X1440+ b$X1441+ b$X1442+ b$X1443+ b$X1444+ b$X1448+ b$X1479+ b$X1481+ b$X1483
#Cartera Vencida
b$car_vencida = b$X1449+ b$X1450+ b$X1451+ b$X1452+ b$X1456+ b$X1457+ b$X1458+ b$X1459+ b$X1460+ b$X1464+ b$X1465+ b$X1466+ b$X1467+ b$X1468+ b$X1472+ b$X1485+ b$X1487+ b$X1489
#Cartera improductiva bruta
b$car_impro_b = b$car_qndi + b$car_vencida
#Cartera Bruta
b$car_b = b$X14 + b$X1499
#Morosidad ampliada
b$mor_amp = b$car_impro_b/b$car_b

#Cartera por vencer
b$car_x_ven = b$X1401+ b$X1402+ b$X1403+ b$X1404+ b$X1408+ b$X1409+ b$X1410+ b$X1411+ b$X1412+ b$X1416+ b$X1417+ b$X1418+ b$X1419+ b$X1420+ b$X1424+ b$X1473+ b$X1475+ b$X1477
#Activos productivos
b$act_prod = b$car_x_ven+b$X1103+b$X1201+b$X13+b$X1901+b$X1902
#Pasivos con costo
b$pas_costo= b$X2101+ b$X2102+ b$X2103+ b$X2105+ b$X22+ b$X2601+ b$X2602+ b$X2603+ b$X2606+ b$X2607+ b$X2690+ b$X27- b$X2790
#Utilización pasivos con costo
b$utiliz_pas_cost = b$act_prod/b$pas_costo

#Proporción de activos productivos
b$propor_act_prod = b$act_prod/b$X1
#Proporción de activos improductivos
b$propor_act_improd = 1 - b$propor_act_prod

b$X5_a = b$X5*12/b$mes
b$X4_a=b$X4*12/b$mes
b$X45_a=b$X45*12/b$mes
b$X51_a = b$X51*12/b$mes
b$X41_a=b$X41*12/b$mes
b$X52_a = b$X52*12/b$mes
b$X54_a = b$X54*12/b$mes
b$X42_a=b$X42*12/b$mes
b$X53_a = b$X53*12/b$mes
b$X43_a=b$X43*12/b$mes
b$X44_a=b$X44*12/b$mes
b$utilidades = b$X5_a-b$X4_a
b$ROA=b$utilidades/b$X1

#Margen financiero bruto
#b$marg_fin_b = (b$X51_a-b$X41_a+b$X52_a+b$X54_a-b$X42_a+b$X53_a-b$X43_a)
b$marg_fin_b = (b$X51-b$X41+b$X52+b$X54-b$X42+b$X53-b$X43)
#Margen financiero neto
#b$marg_fin_n = b$marg_fin_b-(b$X44_a)
b$marg_fin_n = b$marg_fin_b-(b$X44)
#Grado de absorción del margen financiero bruto
#b$grad_absor_marg_fin_b = (b$X45_a)/b$marg_fin_b
b$grad_absor_marg_fin_b = (b$X45)/b$marg_fin_b

#Grado de absorción del margen financiero neto
b$grad_absor_marg_fin = (b$X45_a)/b$marg_fin_n

#Cobertutra de cartera improductiva
b$Cob_car_impro<- ifelse(b$car_impro_b>0,b$X1499/b$car_impro_b,0)

#porcentaje de cartera bruta
b$Porc_car_bruta <- (b$X14+b$X1499)/b$X1

b$Porcentaje_consumo= (b$X1402+	b$X1407+	b$X1410+	b$X1415+	b$X1418+	b$X1423+	b$X1426+	b$X1431+	b$X1434+	b$X1439+	b$X1442+	b$X1447+	b$X1450+	b$X1455+	b$X1458+	b$X1463+	b$X1466+	b$X1471)/b$car_b
b$Porcentaje_vivienda=(b$X1403+	b$X1411+	b$X1419+	b$X1427+	b$X1435+	b$X1443+	b$X1451+	b$X1459+	b$X1467)/b$car_b
b$Porcentaje_micro=(b$X1404+	b$X1412+	b$X1420+	b$X1428+	b$X1436+	b$X1444+	b$X1452+	b$X1460+	b$X1468)/b$car_b

b <- b %>% group_by(FECHA) %>% mutate(total_sistema_fecha = sum(X1, na.rm = TRUE),tamaño = (X1 / total_sistema_fecha)) %>% ungroup()

b1<-b[,c("FECHA","Agencia","ROA","liquid_gen","Porc_car_bruta","Porcentaje_consumo","Porcentaje_vivienda","Porcentaje_micro","propor_act_prod","mor_amp","grad_absor_marg_fin_b","utiliz_pas_cost","Cob_car_impro","tamaño")]
b1$FECHA<-as.Date(b1$FECHA,format="%d/%m/%Y")
write.xlsx(b1, file = "b1.xlsx", colNames = T)

summary(b1)
x<-summary(b1[,c("ROA","liquid_gen","Porc_car_bruta","Porcentaje_consumo","Porcentaje_vivienda","Porcentaje_micro","propor_act_prod","mor_amp","grad_absor_marg_fin_b","utiliz_pas_cost","Cob_car_impro","tamaño")])
write.xlsx(x, file = "resumen.xlsx", colNames = T)

variables_modelo <- c("ROA", "liquid_gen", "Porc_car_bruta","Porcentaje_consumo","Porcentaje_vivienda","Porcentaje_micro","propor_act_prod", "mor_amp", "grad_absor_marg_fin_b", "utiliz_pas_cost", "Cob_car_impro","tamaño")

datos_corr <- b1[, variables_modelo]
corr <- cor(datos_corr, use = "pairwise.complete.obs")
write.xlsx(round(corr, 4), file = "correlacion.xlsx", colNames = T)
cor(b1$propor_act_prod,b1$ROA)

b1$Cob_car_impro <- ifelse(b1$Cob_car_impro>1.5,1.5,b1$Cob_car_impro)

#plot(b1$Porc_car_bruta,b1$ROA)
#plot(b1$Cob_car_impro,b1$ROA)
plot(b1$utiliz_pas_cost,b1$ROA)
summary(b1)
b1 <- pdata.frame(b1, index = c("Agencia","FECHA"))

#####################################################################################
#                         ELABORACIÓN DEL MODELO                                    #
#####################################################################################

#reg <- plm(ROA ~ liquid_gen+Porc_car_bruta+mor_amp+grad_absor_marg_fin_b+utiliz_pas_cost+Cob_car_impro, data = b1, model = "within")
reg <- plm(ROA ~ Porcentaje_consumo+Porcentaje_vivienda+mor_amp+grad_absor_marg_fin_b+utiliz_pas_cost+Cob_car_impro, data = b1, model = "within")
summary(reg)

# Modelo con efectos fijos de individuo (agencia) y tiempo (meses)
reg_twoways <- plm(ROA ~ Porcentaje_consumo + Porcentaje_vivienda + 
                     mor_amp + grad_absor_marg_fin_b + utiliz_pas_cost + 
                     Cob_car_impro, 
                   data = b1, model = "within", effect = "twoways")

summary(reg_twoways)

# Compara el modelo de solo agencias vs el de agencias + tiempo
pFtest(reg_twoways, reg)
reg <- plm(ROA ~ Porcentaje_consumo+mor_amp+grad_absor_marg_fin_b+utiliz_pas_cost+Cob_car_impro, data = b1, model = "within",effect = "twoways")
summary(reg)

# Test de Pesaran CD
pcdtest(reg)
# Test de autocorrelación en paneles
pbgtest(reg)
# Test de Pesaran CD (Cross-sectional Dependence)
pcdtest(reg)
#prueba de homoscedasticidad
bptest(reg)

# Aplicamos Driscoll-Kraay (vcovSCC) sobre tu modelo Twoways
# Usamos type = "HC1" para ser consistentes con lo que venías haciendo
final_robusto <- coeftest(reg, vcov = vcovSCC(reg, type = "HC1"))
print(final_robusto)

b1$ROA_ajustado<-fitted(reg)
b1$residuos<-residuals(reg)

residuos <- residuals(reg)
#prueba de normalidad
shapiro.test(residuos)
par(mfrow=c(1,2))
hist(residuos, main="Histograma de Residuos", col="lightblue", breaks=50)
qqnorm(residuos)
qqline(residuos, col="red")

#Prueba de multicolinealidad
# Corremos un modelo OLS simple solo para calcular el VIF
reg_vif <- lm(ROA ~ Porcentaje_consumo+mor_amp+grad_absor_marg_fin_b+utiliz_pas_cost+Cob_car_impro, data = b1)
# Ahora sí calculamos el VIF
vif(reg_vif)

# Correct to heteroskedasticity-consistent covariance matrix using White's method
coeftest(reg, vcovHC(reg, method = "arellano", type = "HC1"))

# Compara el modelo original con uno que no tenga esas dos variables
reg_reducido <- update(reg, . ~ . - Porcentaje_vivienda - Cob_car_impro)
waldtest(reg, reg_reducido, vcov = vcovHC(reg, method = "arellano", type = "HC1"))

reg <- plm(ROA ~ Porcentaje_consumo+mor_amp+grad_absor_marg_fin_b, data = b1, model = "within")
summary(reg)
bptest(reg)
coeftest(reg, vcovHC(reg, method = "arellano", type = "HC1"))

modelo_fe <- plm(ROA ~ liquid_gen+propor_act_prod + mor_amp + grad_absor_marg_fin_b + utiliz_pas_cost + Cob_car_impro, data = b1, model = "within")
modelo_re <- plm(ROA ~ liquid_gen+propor_act_prod + mor_amp + grad_absor_marg_fin_b + utiliz_pas_cost + Cob_car_impro, data = b1, model = "random")
hausman_test <- phtest(modelo_fe, modelo_re)
print(hausman_test)



############################################################################################
#                           ANALISIS GRÁFICO DE LAS VARIABLES                              #     
############################################################################################

############# ROA

hist(
  b1$ROA,
  main = "Distribución del ROA con Curva de Densidad",  # Nuevo título para reflejar la curva
  xlab = "ROA (Retorno sobre Activos)",
  ylab = "Densidad",                                    # El eje Y ahora es Densidad
  col = "#4A708B99",                                    # Hacemos el color de las barras un poco transparente (99 al final)
  border = "black",
  breaks = 15,
  freq = FALSE,                                         # ¡CRUCIAL! Escala el eje Y a densidad
  xlim = c(min(b1$ROA, na.rm = TRUE), max(b1$ROA, na.rm = TRUE)),
  ylim = NULL
)

# 2. Calcular y Dibujar la Curva de Densidad
# Calcula la estimación de densidad para los datos no NA
densidad_roa <- density(b1$ROA, na.rm = TRUE)

# Dibuja la línea de la curva sobre el histograma
lines(
  densidad_roa,
  col = "red",           # Color de la curva de densidad
  lwd = 3,               # Grosor de la línea
  lty = 1                # Tipo de línea (1 es línea sólida)
)

# Opcional: Añadir una línea para la media
abline(
  v = mean(b1$ROA, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 2 # Línea punteada
)

# Opcional: Añadir una leyenda para clarificar la línea
legend(
  "topright",
  legend = c("Curva de Densidad", "Media"),
  col = c("red", "darkgreen"),
  lty = c(1, 2),
  lwd = c(3, 2),
  bty = "n"
)


############ LIQUIDEZ GENERAL

# 1. Calcular el límite máximo deseado (e.g., hasta el 0.4)
max_x_limit <- 0.4

# 2. Recalcular la densidad solo para los datos dentro del nuevo límite
# (Esto asegura que la curva no se extienda fuera del rango visible si el original iba hasta 0.6+)
datos_filtrados <- b1$liquid_gen[b1$liquid_gen <= max_x_limit & !is.na(b1$liquid_gen)]
densidad_filtrada <- density(datos_filtrados, na.rm = TRUE, adjust = 1) # adjust=1 es el valor por defecto

# 3. Crear el Histograma con el Eje X Ajustado
hist(
  b1$liquid_gen,
  main = "Distribución de la liquidez con Curva de Densidad",
  xlab = "Liquidez General",
  ylab = "Densidad",
  col = "#4A708B99",
  border = "black",
  breaks = 50,
  freq = FALSE,
  xlim = c(0, max_x_limit), # Límites del Eje X ajustados
  ylim = c(0,18)              # Dejamos que R decida el límite Y
)

# 4. Dibujar la Curva de Densidad (usando el cálculo original para mantener la forma)
# Es mejor usar el cálculo original para que la curva represente todos los datos
densidad_original <- density(b1$liquid_gen, na.rm = TRUE) 
lines(
  densidad_original,
  col = "red",
  lwd = 3,
  lty = 1
)

# 5. Añadir Media y Leyenda (igual que antes)
abline(v = mean(b1$liquid_gen, na.rm = TRUE), col = "darkgreen", lwd = 2, lty = 2)
legend("topright", legend = c("Curva de Densidad", "Media"),
       col = c("red", "darkgreen"), lty = c(1, 2), lwd = c(3, 2), bty = "n")




################################ Porc_car_bruta

hist(
  b1$Porc_car_bruta,
  main = "Distribución de Porcentaje de Cartera Bruta con Curva de Densidad",  # Nuevo título para reflejar la curva
  xlab = "Porcentaje de Cartera Bruta",
  ylab = "Densidad",                                    # El eje Y ahora es Densidad
  col = "#4A708B99",                                    # Hacemos el color de las barras un poco transparente (99 al final)
  border = "black",
  breaks = 15,
  freq = FALSE,                                         # ¡CRUCIAL! Escala el eje Y a densidad
  xlim = c(min(b1$Porc_car_bruta, na.rm = TRUE), max(b1$Porc_car_bruta, na.rm = TRUE)),
  ylim = NULL
)

# 2. Calcular y Dibujar la Curva de Densidad
# Calcula la estimación de densidad para los datos no NA
densidad_roa <- density(b1$Porc_car_bruta, na.rm = TRUE)

# Dibuja la línea de la curva sobre el histograma
lines(
  densidad_roa,
  col = "red",           # Color de la curva de densidad
  lwd = 3,               # Grosor de la línea
  lty = 1                # Tipo de línea (1 es línea sólida)
)

# Opcional: Añadir una línea para la media
abline(
  v = mean(b1$Porc_car_bruta, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 2 # Línea punteada
)

# Opcional: Añadir una leyenda para clarificar la línea
legend(
  "topleft",
  legend = c("Curva de Densidad", "Media"),
  col = c("red", "darkgreen"),
  lty = c(1, 2),
  lwd = c(3, 2),
  bty = "n"
)


######### propor_act_prod

hist(
  b1$propor_act_prod,
  main = "Distribución de Proporción de Activos Productivos con Curva de Densidad",  # Nuevo título para reflejar la curva
  xlab = "Proporción de Activos Productivos",
  ylab = "Densidad",                                    # El eje Y ahora es Densidad
  col = "#4A708B99",                                    # Hacemos el color de las barras un poco transparente (99 al final)
  border = "black",
  breaks = 15,
  freq = FALSE,                                         # ¡CRUCIAL! Escala el eje Y a densidad
  xlim = c(min(b1$propor_act_prod, na.rm = TRUE), max(b1$propor_act_prod, na.rm = TRUE)),
  ylim = NULL
)

# 2. Calcular y Dibujar la Curva de Densidad
# Calcula la estimación de densidad para los datos no NA
densidad_roa <- density(b1$propor_act_prod, na.rm = TRUE)

# Dibuja la línea de la curva sobre el histograma
lines(
  densidad_roa,
  col = "red",           # Color de la curva de densidad
  lwd = 3,               # Grosor de la línea
  lty = 1                # Tipo de línea (1 es línea sólida)
)

# Opcional: Añadir una línea para la media
abline(
  v = mean(b1$propor_act_prod, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 2 # Línea punteada
)

# Opcional: Añadir una leyenda para clarificar la línea
legend(
  "topleft",
  legend = c("Curva de Densidad", "Media"),
  col = c("red", "darkgreen"),
  lty = c(1, 2),
  lwd = c(3, 2),
  bty = "n"
)


################## mor_amp


hist(
  b1$mor_amp,
  main = "Distribución de Morosidad con Curva de Densidad",  # Nuevo título para reflejar la curva
  xlab = "Morosidad",
  ylab = "Densidad",                                    # El eje Y ahora es Densidad
  col = "#4A708B99",                                    # Hacemos el color de las barras un poco transparente (99 al final)
  border = "black",
  breaks = 15,
  freq = FALSE,                                         # ¡CRUCIAL! Escala el eje Y a densidad
  xlim = c(min(b1$mor_amp, na.rm = TRUE), max(b1$mor_amp, na.rm = TRUE)),
  ylim = NULL
)

# 2. Calcular y Dibujar la Curva de Densidad
# Calcula la estimación de densidad para los datos no NA
densidad_roa <- density(b1$mor_amp, na.rm = TRUE)

# Dibuja la línea de la curva sobre el histograma
lines(
  densidad_roa,
  col = "red",           # Color de la curva de densidad
  lwd = 3,               # Grosor de la línea
  lty = 1                # Tipo de línea (1 es línea sólida)
)

# Opcional: Añadir una línea para la media
abline(
  v = mean(b1$mor_amp, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 2 # Línea punteada
)

# Opcional: Añadir una leyenda para clarificar la línea
legend(
  "topright",
  legend = c("Curva de Densidad", "Media"),
  col = c("red", "darkgreen"),
  lty = c(1, 2),
  lwd = c(3, 2),
  bty = "n"
)


################## grad_absor_marg_fin_b

# 1. Calcular el límite máximo deseado (e.g., hasta el 0.4)
max_x_limit <- 2.5

# 2. Recalcular la densidad solo para los datos dentro del nuevo límite
# (Esto asegura que la curva no se extienda fuera del rango visible si el original iba hasta 0.6+)
datos_filtrados <- b1$grad_absor_marg_fin_b[b1$grad_absor_marg_fin_b <= max_x_limit & !is.na(b1$grad_absor_marg_fin_b)]
densidad_filtrada <- density(datos_filtrados, na.rm = TRUE, adjust = 1) # adjust=1 es el valor por defecto


hist(
  b1$grad_absor_marg_fin_b,
  main = "Distribución del Grado de Absorción del Margen Financiero",  # Nuevo título para reflejar la curva
  xlab = "Grado de Absorción del Margen Financiero",
  ylab = "Densidad",                                    # El eje Y ahora es Densidad
  col = "#4A708B99",                                    # Hacemos el color de las barras un poco transparente (99 al final)
  border = "black",
  breaks = 70,
  freq = FALSE,                                         # ¡CRUCIAL! Escala el eje Y a densidad
  xlim = c(0,max_x_limit),
  ylim = c(0,2.7)
)

# 2. Calcular y Dibujar la Curva de Densidad
# Calcula la estimación de densidad para los datos no NA
densidad_roa <- density(b1$grad_absor_marg_fin_b, na.rm = TRUE)

# Dibuja la línea de la curva sobre el histograma
lines(
  densidad_roa,
  col = "red",           # Color de la curva de densidad
  lwd = 3,               # Grosor de la línea
  lty = 1                # Tipo de línea (1 es línea sólida)
)

# Opcional: Añadir una línea para la media
abline(
  v = mean(b1$grad_absor_marg_fin_b, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 2 # Línea punteada
)

# Opcional: Añadir una leyenda para clarificar la línea
legend(
  "topright",
  legend = c("Curva de Densidad", "Media"),
  col = c("red", "darkgreen"),
  lty = c(1, 2),
  lwd = c(3, 2),
  bty = "n"
)

################## utiliz_pas_cost

# 1. Calcular el límite máximo deseado (e.g., hasta el 0.4)
max_x_limit <- 9

# 2. Recalcular la densidad solo para los datos dentro del nuevo límite
# (Esto asegura que la curva no se extienda fuera del rango visible si el original iba hasta 0.6+)
datos_filtrados <- b1$utiliz_pas_cost[b1$utiliz_pas_cost <= max_x_limit & !is.na(b1$utiliz_pas_cost)]
densidad_filtrada <- density(datos_filtrados, na.rm = TRUE, adjust = 1) # adjust=1 es el valor por defecto


hist(
  b1$utiliz_pas_cost,
  main = "Distribución de Utilización de Pasivos con Costo",  # Nuevo título para reflejar la curva
  xlab = "Utilización de Pasivos con Costo",
  ylab = "Densidad",                                    # El eje Y ahora es Densidad
  col = "#4A708B99",                                    # Hacemos el color de las barras un poco transparente (99 al final)
  border = "black",
  breaks = 70,
  freq = FALSE,                                         # ¡CRUCIAL! Escala el eje Y a densidad
  xlim = c(min(b1$utiliz_pas_cost, na.rm = TRUE), max_x_limit),
  ylim = c(0,1)
)

# 2. Calcular y Dibujar la Curva de Densidad
# Calcula la estimación de densidad para los datos no NA
densidad_roa <- density(b1$utiliz_pas_cost, na.rm = TRUE)

# Dibuja la línea de la curva sobre el histograma
lines(
  densidad_roa,
  col = "red",           # Color de la curva de densidad
  lwd = 3,               # Grosor de la línea
  lty = 1                # Tipo de línea (1 es línea sólida)
)

# Opcional: Añadir una línea para la media
abline(
  v = mean(b1$utiliz_pas_cost, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 2 # Línea punteada
)

# Opcional: Añadir una leyenda para clarificar la línea
legend(
  "topright",
  legend = c("Curva de Densidad", "Media"),
  col = c("red", "darkgreen"),
  lty = c(1, 2),
  lwd = c(3, 2),
  bty = "n"
)



################## Cob_car_impro

# 1. Calcular el límite máximo deseado (e.g., hasta el 0.4)
max_x_limit <- 3

# 2. Recalcular la densidad solo para los datos dentro del nuevo límite
# (Esto asegura que la curva no se extienda fuera del rango visible si el original iba hasta 0.6+)
datos_filtrados <- b1$Cob_car_impro[b1$Cob_car_impro <= max_x_limit & !is.na(b1$Cob_car_impro)]
densidad_filtrada <- density(datos_filtrados, na.rm = TRUE, adjust = 1) # adjust=1 es el valor por defecto

hist(
  b1$Cob_car_impro,
  main = "Distribución de Cobertura de cartera Improductiva",  # Nuevo título para reflejar la curva
  xlab = "Cobertura de cartera Improductiva",
  ylab = "Densidad",                                    # El eje Y ahora es Densidad
  col = "#4A708B99",                                    # Hacemos el color de las barras un poco transparente (99 al final)
  border = "black",
  breaks = 70,
  freq = FALSE,                                         # ¡CRUCIAL! Escala el eje Y a densidad
  xlim = c(min(b1$Cob_car_impro, na.rm = TRUE), max_x_limit),
  ylim = c(0,2.7)
)

# 2. Calcular y Dibujar la Curva de Densidad
# Calcula la estimación de densidad para los datos no NA
densidad_roa <- density(b1$Cob_car_impro, na.rm = TRUE)

# Dibuja la línea de la curva sobre el histograma
lines(
  densidad_roa,
  col = "red",           # Color de la curva de densidad
  lwd = 3,               # Grosor de la línea
  lty = 1                # Tipo de línea (1 es línea sólida)
)

# Opcional: Añadir una línea para la media
abline(
  v = mean(b1$Cob_car_impro, na.rm = TRUE),
  col = "darkgreen",
  lwd = 2,
  lty = 2 # Línea punteada
)

# Opcional: Añadir una leyenda para clarificar la línea
legend(
  "topright",
  legend = c("Curva de Densidad", "Media"),
  col = c("red", "darkgreen"),
  lty = c(1, 2),
  lwd = c(3, 2),
  bty = "n"
)
