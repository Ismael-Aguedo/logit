---
author: "Ismael Giancarlo Aguedo Aguilar"
title: "Familias con Probabilidad de Tener Pc en casa"
date: "<center>2023-06-16</center>"
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    collapsed: true
    smooth_scroll: true
    theme: journal
    highlight: kate
    df_print: paged
    code_folding: show
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
En esta oportunidad analizaremos el modelo con respuesta cualitativa que indica la probabilidad de que las familias tengan o no una pc en su casa, cuyo modelo es:

$$ Y = \beta_{0}+\beta_{1}If+\beta_{2}HU+\beta_{3}PP$$

# Base de datos 60 familias 

Las variables explicativas del modelos son las siguientes:

Yi=(1: si la familia tiene una PC en su hogar, 0: si la familia no tiene una PC en su hogar)

If: ingreso familiar

HU=> (1: si la familia tiene hijos universitarios, 0: si la familia no tiene hijos universitarios)

PP=> (1: si los padres son profesionales, 0: si los padres no son profesionales)


```{r}
library(readxl)
variables<-read_excel("C:/Users/igaag/OneDrive/Documentos/ECONOMETRIA2/EXCEL_DATABASE/econometriaII.xlsx")
variables
```

# Ajustando el modelo Logit

Ahora bien dado que el modelo mostrado anteriormente no puede ser evaluado por MCO, dado las respuestas cualitativas de la variable dependiente, y dado los problemas que presenta necesitamos evaluar mediante otro modelo el cual es llamado Logit por su distribución logística.

$$Ln\left(\frac{P_{i}}{1-P_{i}}\right)=\beta_{0}+\beta_{1}If+\beta_{2}HU+\beta_{3}PP+\mu$$

```{r, comment=""}
modelo <- glm(Y ~ IF+HU+PP, data = variables, family = binomial(link = "logit"))
summary(modelo)
```

## McFadden R-squared

Uno de los estadísticos que en este modelo no funcionan es el que nos menciona la bondad de ajuste del modelo R-squared, sin embargo no indica que el modelo tenga falencias sino que tendremos que hacer uso de otro estadístico que remplace esa función mas adelante.

```{r, comment="", include=FALSE}
library(pscl)
mcfadden_r2 <- pR2(modelo)
```
```{r, comment=""}
mcfadden_r2
```
entonces observamos que $R^{2}=0.3069321$ lo cual como ya habíamos mencionado era muy bajo, pero que no usaremos en esta oportunidad.

# $R^{2}$ de Conteo 

Ahora obtenemos las observaciones de la variable dependiente, de la misma estimada por el modelo Logit y sus residuos para encontrar el $R^{2}$ **de Conteo** el cual remplazara al $R^{2}$

```{r, include=FALSE}
actual <- modelo$model$Y
ajustados <- modelo$fitted.values
residual <- modelo$residuals
```
```{r, include=FALSE}
datosRconteo <- data.frame(yactual=modelo$model$Y, yestimada=modelo$fitted.values, residuos=modelo$residuals)
```
```{r}
datosRconteo
```

## Obteniendo VP, VN, FP, FN

Para obtener estos valores solo necesitamos contar, comparando el yactual con el yestimado, de forma estandar es suficiente tomar como referencia a yestimado al 50% es decir 0.50, donde:

VP=Verdadero Positivo, el $\left(yestimado>0.50\right)$  y coincide con lo observado: $(yactual=1)$

VN=Verdadero Negativo, el $\left(yestimado\leq0.50\right)$  y coincide con lo observado: $(yactual=0)$

FP=Falso Positivo, el $\left(yestimado\leq0.50\right)$  y coincide con lo observado: $(yactual=1)$

FN=Falso Negativo, el $\left(yestimado>0.50\right)$  y coincide con lo observado: $(yactual=0)$



```{r, comment=""}
conteoVP <- sum(datosRconteo$yactual == 1 & datosRconteo$yestimada > 0.50)
```
Este código muestra para el primer resultado VP, asi sucesivamente hacemos para los demas:
```{r, include=FALSE}
conteoVN <- sum(datosRconteo$yactual == 0 & datosRconteo$yestimada <= 0.50)
conteoFP <- sum(datosRconteo$yactual == 1 & datosRconteo$yestimada <= 0.50)
conteoFN <- sum(datosRconteo$yactual == 0 & datosRconteo$yestimada > 0.50)
```
```{r, comment=""}
conteoVP; conteoVN; conteoFP; conteoFN
```
### Presición Predictiva 

```{r, include=FALSE}
library(vcd)
library(vcdExtra)
library(mosaic)
```


```{r, comment=""}
library(vcd)
predicciones <- ifelse(modelo$fitted.values > 0.5, 1, 0)
matriz_confusion <- table(variables$Y, predicciones, dnn = c("Observaciones", "Predicciones"))
```
Se carga la biblioteca **vcd** para utilizar la función **table()** y calcular la matriz de confusión. A continuación, se generan las predicciones binarias utilizando un umbral de **0.5** basado en las probabilidades ajustadas del modelo logit. Posteriormente, se calcula la matriz de confusión utilizando la función **table()**, pasando las observaciones reales y las predicciones como argumentos. Se utiliza la opción **dnn** para asignar nombres a las dimensiones de la matriz de confusión.

```{r, comment=""}
matriz_confusion
```
La matriz de confusión proporciona información detallada sobre el rendimiento del modelo, incluyendo los verdaderos positivos, verdaderos negativos, falsos positivos y falsos negativos. Estos valores se utilizan para evaluar métricas de rendimiento como la precisión, sensibilidad, especificidad y otras medidas de evaluación del modelo de clasificación.

```{r, comment=""}
mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("cyan2", "magenta2", "magenta2", "cyan2"), 2, 2)))
```

# Calculo de $R^{2}$ de Conteo

Calcular mediante el $R^{2}$ de conteo el porcentaje total de predicciones correctas

**Solución:**

$$R^{2} \ de \ Conteo \ =\frac{Predicciones  \ correctas}{Total \ de \ observaciones}=\frac{VP+VN}{Total \ de \ observaciones}$$
$$R^{2} \ de \ Conteo \ =\frac{28+18}{60}=0.7\hat{6}$$
Lo cual significa que 76.67% de las predicciones serán correctas en el modelo logit estimado.

El $R^2$ de conteo varía entre $0$ y $1$. Un valor de $R^2$ cercano a $0$ indica que el modelo no explica bien la variabilidad en los datos de conteo, mientras que un valor cercano a $1$ indica un buen ajuste del modelo y una mayor capacidad de explicación de la variabilidad.

## Pred. Correctas Positivas

Calcular mediante el $R^{2}$ de Conteo el porcentaje de predicciones correctas positivas 

$$PC_{positivas}= \frac{VP}{VP+FP}=\frac{VP}{P}$$

### Solución

$$PC_{positivas}= \frac{28}{28+6}=\frac{28}{34}=0.8235$$

Lo que significa que el 82.35% de las predicciones positivas serán correctas o acertadas.

## Pred. Correctas Negativas 

calcular mediante el $R^{2}$ de Conteo, el porcentaje de predicciones correctas negativas
$$PC_{negativas}= \frac{VN}{VN+FN}= \frac{VN}{N}$$

### Solución

$$PC_{positivas}= \frac{18}{18+8}=\frac{18}{26}=0.6923$$

lo que significa que el 69.23% de las predicciones negativas serán correctas o acertadas.

# Hosmer Lemeshow

```{r, include=FALSE}
library(ResourceSelection)
```


```{r, comment=""}
hl <- hoslem.test(variables$Y, fitted(modelo), g = 10)
cbind(hl$expected, hl$observed)
```
Entonces, cada fila de la tabla muestra los valores esperados y observados para un intervalo de predicción y las categorías 0 y 1. Por ejemplo, la primera fila indica que se esperaban 5.47 observaciones en el intervalo [0.053, 0.125] con categoría 0 (y0), y se observaron 6 casos reales en esa categoría.

```{r, comment=""}
hl
```
La prueba de Hosmer-Lemeshow es una forma de evaluar si un modelo logístico se ajusta adecuadamente a los datos observados. Proporciona una medida de la discrepancia entre las frecuencias observadas y esperadas.
## Hipótesis

Ho: El modelo es adecuado

H1: el modelo no es adecuado

## Estadístico de Prueba

HL=3.9386

Si $HL > X^{2}_{\left(5\% \ ; \  j-2\right)} \, \rightarrow$   se rechaza la Ho y se acepta la H1, lo que Significa que el modelo no es adecuado 

## Chi cuadrada

Calculamos la Chi-cuadrada, donde DF=8 lo obtenemos mediante j-2=8 número de grupos formados menos dos grados de libertad.

```{r}
nivel_significancia <- 0.05
grados_libertad <- 8
valor_critico <- qchisq(1 - nivel_significancia, df = grados_libertad)
valor_critico
```
En este caso se acepta Ho, dado que $HL<X^{2}_{\left(5\% \ ; \  j-2\right)}$  lo que significa que el modelo es adecuado.

# Casuistica

Dado que al evaluar mediante los test de $R^{2}$ de Conteo y el test de Hosmer-Lemeshow emos encontrado que el modelo es adecuado resolveremos los siguientes casos:

## Familia (i) ratio_odds

Determinar cuántas veces es más probable que la familia que tiene los siguientes atributos: IF= 5500, HU=1 y PP=1, tenga una PC en su hogar que la que no tenga.

```{r, comment=""}
Razon_de_probabilidades_i<-exp(-4.0792259 + 0.0004273 * 5500 + 2.8609796 * 1 + 1.3211503 * 1)
Razon_de_probabilidades_i
```
Lo que significa que 11.62379 veces es más probable que la familia (i) tenga una PC que la que no tenga.

## Familia (i) P-ocurrencia

Estimar la probabilidad de ocurrencia del evento para la familia (i) que tiene  los siguiente atributos: IF= 5500, HU=1 y PP=1:

```{r, comment=""}
funcion <- function(x) {
  return(x / (1 - x) - 11.62379)
  }
solucion <- uniroot(funcion, interval = c(0, 1))
pi<- solucion$root
pi
```
Entonces Pi o Probabilidad de ocurrencia, indica que la familia (i) tiene una probabilidad de 92.07% de tener una PC en casa.

## Familia (j) rodds y Pj

Estimar la probabilidad de ocurrencia del evento para la familia (j) que tiene  los siguiente atributos: IF= 7000, HU=1 y PP=0:

```{r, comment=""}
Razon_de_probabilidades_j<-exp(-4.0792259 + 0.0004273 * 7000 + 2.8609796 * 1 + 1.3211503 * 0)
Razon_de_probabilidades_j
```
Lo que significa que 5.887631 veces es más probable que la familia (i) tenga una PC que la que no tenga, luego.

```{r, comment=""}
funcion <- function(x) {
  return(x / (1 - x) - 5.887631)
  }
solucion <- uniroot(funcion, interval = c(0, 1))
pj<- solucion$root
pj
```
Entonces Pi o Probabilidad de ocurrencia, indica que la familia (i) tiene una probabilidad de 85.48% de tener una PC en casa.

## Familia (i) VS Familia(j)

¿Cuantas veces es más probable que la familia (i) tenga una PC que la familia (j)?

En este caso obtendremos la solución solo obteniendo la razón de la Pi sobre Pj, es decir:

$$Cociente \ entre \ odds=\frac{\frac{pi}{(1-pi)}}{\frac{pj}{(1-pj)}}$$
```{r, comment=""} 
cociente_entre_rodds = Razon_de_probabilidades_i / Razon_de_probabilidades_j
cociente_entre_rodds
```

1.97 veces es más probable que la familia (i) posea una PC que la familia (j).

