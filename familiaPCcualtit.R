# Cargar las bibliotecas necesarias
library(ggplot2)
library(lattice)
library(pROC)
library(caret)
library(readxl)
library(pscl)
library(vcd)
library(ResourceSelection)

# Leer los datos desde el archivo Excel
variables <- read_excel("C:/Users/igaag/OneDrive/Documentos/ECONOMETRIA2/EXCEL_DATABASE/econometriaII.xlsx")

# Ajustar el modelo logit
modelo <- glm(Y ~ IF + HU + PP, data = variables, family = binomial(link = "logit"))
summary(modelo)

# Calcular el McFadden R-squared
mcfadden_r2 <- pR2(modelo)
mcfadden_r2

# Obtener las predicciones y la matriz de confusión
predicciones <- ifelse(modelo$fitted.values > 0.5, 1, 0)
matriz_confusion <- table(variables$Y, predicciones, dnn = c("Observaciones", "Predicciones"))
matriz_confusion

# Realizar la prueba de bondad de ajuste Hosmer-Lemeshow
hl <- hoslem.test(variables$Y, fitted(modelo), g = 10)
cbind(hl$expected, hl$observed)
hl
