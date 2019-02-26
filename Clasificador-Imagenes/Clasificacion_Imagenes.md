---
title: "Clasificaci�n Im�genes"
author: "crsd"
date: "25 de febrero de 2019"
output: 
      html_document:
            code_folding: hide
            toc: true
            toc_depth: 3
            number_sections: false
            theme: united
            highlight: tango
            css: air.css
            keep_md: true
---


```r
suppressMessages(library(EBImage))
suppressMessages(library(readbitmap))
suppressMessages(library(fftwtools))
suppressMessages(library(rgl))
suppressMessages(library(ggplot2))
suppressMessages(library(caret))
suppressMessages(library(MASS))
suppressMessages(library(dplyr))

source("fftshift.R") # centra el espectro de frecuencias
source("PCE.R") # Calcula PCE
source("PSR.R") # Calcula PSR
source("DC.R") # Calcula DC
source("ExtCaract.R") # Hace pruebas y extrae caracteristicas PCE, PSR y DC para graficos
source("ExtCaract2.R") # Hace pruebas y extrae caracteristicas PCE, PSR y DC para modelado
source("reg_face.R") # Genera modelos de regresion y clasificacion
source("UnitTest.R") # Clasifica una imagen
source("F_random.R") # Crea un filtro POF aleatorio (filtros, imagen en frecuencia y autocorrelaciones)
```

# Resumen

Continuando con el trabajo de [Reconocimiento de Imagenes por Correlacion](https://rpubs.com/desareca/Reconocimiento-Imagenes-Correlacion), desarrollaremos un clasificador para los 13 sujetos.

EL objetivo es construir un clasificador a partir de s�lo una imagen de cada sujeto y para esto seguiremos la siguiente estrategia:

- Extraeremos las car�cter�sticas de PCE, PSR y DC de 13 filtros (uno por sujeto) aplicados a una imagen de prueba aleatoria.
- Con estas pruebas construiremos 13 modelos de regresi�n para estimar si la imagen de prueba pertenece a una clase.
- Estas regresiones se utilizar�n para desarrallar un clasificador final.
- Para finalizar se aplicar� ruido a algunas im�genes para evaluar el desempe�o del clasificador.


# Extraccion de cararter�sticas

Para extraer caracter�sticas se considera el filtro POF (ver [Reconocimiento de Imagenes por Correlacion](https://rpubs.com/desareca/Reconocimiento-Imagenes-Correlacion)), ya que se observa una clara posibilidad de separar en regiones distintas (sujeto de impostor) de acuerdo a las variables PCE, PSR y DC.

Con este fin, se considera realizar 1000 observaciones, donde cada observacion considera extraer el PSR, PCE y DC de una imagen con 13 filtros POF (uno por sujeto). Para el caso de la DC se considera como auto correlaci�n a la imagen con que se dise�� cada filtro.

Una vez obtenidas las observaciones, vamos a separar los datos en conjunto de entrenamiento, pruebas y validaci�n, con 50%, 30% y 20% respectivamente.


```r
dir1="./faceExpressionDatabase/"
Subject = LETTERS[1:13]
Number = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
           "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
           "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
           "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
           "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
           "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
           "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
           "70", "71", "72", "73", "74")
n=1000 # numero de pruebas (el numero de observaciones es 13*n) (100 muestras son 2 minutos)

PLOT <- ExtCaract(n=n, seed = 50, dir1 = dir1, Subject = Subject, Number = Number)
DATA <- ExtCaract2(n=n, seed = 50, dir1 = dir1, Subject = Subject, Number = Number)

set.seed(28916022)
Index1 <- createDataPartition(y = DATA$imagen, p = 0.5, list = FALSE)
TEST <- DATA[-Index1,]
TRAIN <- DATA[Index1,]
Index2 <- createDataPartition(y = TEST$imagen, p = 0.6, list = FALSE)
VAL <- TEST[-Index2,]
TEST <- TEST[Index2,]

saveRDS(TRAIN, "train_set_pof.rds")
saveRDS(TEST, "test_set_pof.rds")
saveRDS(VAL, "Valid_set_pof.rds")

remove(DC, fftshift, PCE, PSR, Index1, DATA, dir1, Index2, Number, Subject,n)

ggplot(PLOT, aes(PCE, PSR, color = imagen)) +
      geom_point(size = 2, alpha = 0.1) + facet_wrap(~filtro)
```

![](Clasificacion_Imagenes_files/figure-html/Extraccion-1.png)<!-- -->

```r
ggplot(PLOT, aes(DC^2, PSR, color = imagen)) +
      geom_point(size = 2, alpha = 0.1) + facet_wrap(~filtro)
```

![](Clasificacion_Imagenes_files/figure-html/Extraccion-2.png)<!-- -->

```r
ggplot(PLOT, aes(DC^2, PCE, color = imagen)) +
      geom_point(size = 2, alpha = 0.1) + facet_wrap(~filtro)
```

![](Clasificacion_Imagenes_files/figure-html/Extraccion-3.png)<!-- -->

Observando los gr�ficos podemos ver que para cada filtro hay una regi�n que se puede asociar al sujeto con que se dise�� el filtro y los impostores. Esto es en algunos casos m�s evidente como es el caso del sujeto **"A"** y en otros no tanto como en el sujeto **"B"**. En cualquier caso podemos aplicar alg�n modelo que prediga esto.

Si observamos el gr�fico de PCE VS PSR, tenemos que para cada filtro la imagen asociada a su clase tiene valores altos de PCE y PSR y los impostores al filtro tienen valores bajos, esto se debe a que valores altos en PSR y PCE indican una mayor relaci�n entre la informaci�n del filtro y la imagen aplicada.

En el caso de la DC, tenemos que las imagenes asociadas a la clase del filtro tienen valores peque�os y los impostores altos, esto es debido a que al filtrotiene menor capacidad para discrimanar enrte la imagen con que se dise�� el filtro y una imagen de la misma clase (reconoce a ambas como v�lidas).


# Modelos de regresi�n

Para los modelos de regresi�n utilizaremos el m�todo **Bayesglm**, este es una simple alteraci�n a un algoritmo de un modelos lineal generalizado (**glm**). Para mayor informaci�n revisar:

https://www.rdocumentation.org/packages/arm/versions/1.10-1/topics/bayesglm

En este caso se toman las variables extraidas anteriormente asociadas a la clase de un filtro en particular con la clase de imagen de prueba. Si la imagen de prueba pertenece a la clase del filtro se asigna un **1**, en caso contrario se asigna un **0**, esto se repite para cada filtro.

Para desarrollar los modelos de regresi�n consideraremos para entrenamiento al conjunto de pruebas utilizando validaci�n cruzada con 5 folds y para las pruebas el conjunto de pruebas determinados anteriormente.

A continuaci�n se muestran los resultados aplicando el conjunto de prueba a cada modelos de regresi�n. Se aprecia una clara tendencia a filtrar el rostro asociado a la clase para la que fue dise�ado y a rechazar el resto.


```r
Resultado <- reg_face(TRAIN, TEST, VAL, method1 = "bayesglm", method2="LogitBoost",
                    trC=trainControl(method="cv", number=5))

test <- readRDS("TestReg_bayesglm_LogitBoost.rds")
colnames(test) <- c("Regresi�n A", "Regresi�n B", "Regresi�n C", "Regresi�n D",
                    "Regresi�n E", "Regresi�n F", "Regresi�n G", "Regresi�n H",
                    "Regresi�n I", "Regresi�n J", "Regresi�n K", "Regresi�n L",
                    "Regresi�n M", "Imagen")
tt <- test[, c(1,14)]
tt <- cbind(tt, colnames(test)[1])
colnames(tt) <- c("Valor", "Imagen", "Regresion")
for (i in 2:13) {
      a <- test[, c(i,14)]
      a <- cbind(a, colnames(test)[i])
      colnames(a) <- c("Valor", "Imagen", "Regresion")
      tt <- rbind(tt, a)
}

ggplot(data=tt, aes(Imagen, Valor,fill=Imagen)) +  geom_boxplot() + facet_wrap(~Regresion)
```

![](Clasificacion_Imagenes_files/figure-html/clasificador-1.png)<!-- -->


# Clasificador

Para el calasificador utilizaremos el m�todo **LogitBoost**, este es un algoritmo basado en arboldes de decisi�n. Para mayor informaci�n revisar:

https://www.rdocumentation.org/packages/caTools/versions/1.17.1/topics/LogitBoost

Para dise�ar el clasificador, consideraremos para entrenamiento al conjunto de pruebas utilizando validaci�n cruzada con 5 folds.


```r
readRDS("FitClass_bayesglm_LogitBoost.rds")
```

```
Boosted Logistic Regression 

3900 samples
  13 predictor
  13 classes: 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 3120, 3120, 3120, 3120, 3120 
Resampling results across tuning parameters:

  nIter  Accuracy   Kappa    
  11     0.9984553  0.9983265
  21     0.9984606  0.9983323
  31     0.9982051  0.9980556

Accuracy was used to select the optimal model using the largest value.
The final value used for the model was nIter = 21.
```

Las pruebas de desempe�o se realizar�n con el conjunto de validaci�n, tal como se muestra acontinuaci�n:


```r
Resultado
```

```
Confusion Matrix and Statistics

          Reference
Prediction   A   B   C   D   E   F   G   H   I   J   K   L   M
         A 200   0   0   0   0   0   0   0   0   0   0   0   0
         B   0 199   0   0   0   0   0   0   0   0   0   0   0
         C   0   0 198   0   0   0   0   0   0   0   0   0   0
         D   0   0   0 200   0   0   0   0   0   0   0   0   0
         E   0   0   0   0 200   0   0   0   0   0   0   0   0
         F   0   0   0   0   0 200   0   0   0   0   0   0   0
         G   0   0   0   0   0   0 200   0   0   0   0   0   0
         H   0   0   0   0   0   0   0 200   0   0   0   0   0
         I   0   0   0   0   0   0   0   0 198   0   0   0   0
         J   0   0   0   0   0   0   0   0   0 200   0   0   0
         K   0   0   0   0   0   0   0   0   0   0 200   0   0
         L   0   0   0   0   0   0   0   0   0   0   0 200   0
         M   0   0   0   0   0   0   0   0   0   0   0   0 200

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9986, 1)
    No Information Rate : 0.0771     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E Class: F
Sensitivity           1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Prevalence            0.07707  0.07669   0.0763  0.07707  0.07707  0.07707
Detection Rate        0.07707  0.07669   0.0763  0.07707  0.07707  0.07707
Detection Prevalence  0.07707  0.07669   0.0763  0.07707  0.07707  0.07707
Balanced Accuracy     1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
                     Class: G Class: H Class: I Class: J Class: K Class: L
Sensitivity           1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
Prevalence            0.07707  0.07707   0.0763  0.07707  0.07707  0.07707
Detection Rate        0.07707  0.07707   0.0763  0.07707  0.07707  0.07707
Detection Prevalence  0.07707  0.07707   0.0763  0.07707  0.07707  0.07707
Balanced Accuracy     1.00000  1.00000   1.0000  1.00000  1.00000  1.00000
                     Class: M
Sensitivity           1.00000
Specificity           1.00000
Pos Pred Value        1.00000
Neg Pred Value        1.00000
Prevalence            0.07707
Detection Rate        0.07707
Detection Prevalence  0.07707
Balanced Accuracy     1.00000
```

# Pruebas con ruido

El clasificador muestra un muy buen desempe�o, para probarlo a�n m�s utilizaremos otro conjunto de pruebas adicional y le agregaremos ruido tal como se muestra en el siguiente ejemplo:


```r
# carga imagen de prueba
dir1="./faceExpressionDatabase/"
img = Image(rotate(read.bitmap(paste0(dir1,"A","22",".bmp"))/255,
                   angle = 90))
# genera ruido uniforme
img_noisy1 = img
img_noisy2 = img
img_noisy3 = img
set.seed(900)
img_noisy1[sample(length(img), length(img)/10)] = runif(length(img)/10, min=0, max=1)
img_noisy2[sample(length(img), length(img)/5)] = runif(length(img)/5, min=0, max=1)
img_noisy3[sample(length(img), length(img)/3)] = runif(length(img)/3, min=0, max=1)

# muestra imagenes
display(EBImage::combine(img, img_noisy1, img_noisy2, img_noisy3),
        all=TRUE, method = "raster")
text(x = 2, y = 2, label = "Img. sin ruido", adj = c(0,1),
     col = "red", cex = 1)
text(x = 64 + 2, y = 2, label = "Img. con ruido (1/10)", adj = c(0,1),
     col = "red", cex = 1)
text(x = 2, y = 64 + 2, label = "Img. con ruido (1/5)", adj = c(0,1),
     col = "red", cex = 1)
text(x = 64 + 2, y = 64 + 2, label = "Img. con ruido (1/3)", adj = c(0,1),
                               col = "red", cex = 1)
```

![](Clasificacion_Imagenes_files/figure-html/ImgRuido-1.png)<!-- -->

El ruido considera aplicar a una proporci�n de la imagen ruido uniforme, por ejemplo ruido (1/10) quiere decir que se aplicar� ruido uniforme al 1/10 de pixeles de la imagen.

Para el conjunto de pruebas con ruido consideraremos tomar 50 muestras aleatorias por cada clase y asignarle ruido en proporcion 1/10, 1/5 y 1/3.

Acontinuaci�n se muestra la matriz de confusi�n de las imagenes sin ruido, con una precisi�n de 100%.


```r
# Genera filtros aleatoriamente
Filtros <- F_random(seed = 750)
# Carga modelos de regresion
fitLOG_A <- readRDS("fitReg_bayesglm_LogitBoost_A.rds")
fitLOG_B <- readRDS("fitReg_bayesglm_LogitBoost_B.rds")
fitLOG_C <- readRDS("fitReg_bayesglm_LogitBoost_C.rds")
fitLOG_D <- readRDS("fitReg_bayesglm_LogitBoost_D.rds")
fitLOG_E <- readRDS("fitReg_bayesglm_LogitBoost_E.rds")
fitLOG_F <- readRDS("fitReg_bayesglm_LogitBoost_F.rds")
fitLOG_G <- readRDS("fitReg_bayesglm_LogitBoost_G.rds")
fitLOG_H <- readRDS("fitReg_bayesglm_LogitBoost_H.rds")
fitLOG_I <- readRDS("fitReg_bayesglm_LogitBoost_I.rds")
fitLOG_J <- readRDS("fitReg_bayesglm_LogitBoost_J.rds")
fitLOG_K <- readRDS("fitReg_bayesglm_LogitBoost_K.rds")
fitLOG_L <- readRDS("fitReg_bayesglm_LogitBoost_L.rds")
fitLOG_M <- readRDS("fitReg_bayesglm_LogitBoost_M.rds")
# Carga modelo de clasificacion
fitclass <- readRDS("FitClass_bayesglm_LogitBoost.rds")
# Genera pruebas para 50 rostros con cada tipo con ruido
n=50
Number = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
           "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
           "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
           "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
           "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
           "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
           "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
           "70", "71", "72", "73", "74")
set.seed(200)
Class_IMG <- data.frame()
Class_IMG1 <- data.frame()
Class_IMG2 <- data.frame()
Class_IMG3 <- data.frame()
l<-1
for (i in 1:13) {
      for (k in 1:n) {
            # carga indices aleatorios
            SubjectP <- LETTERS[i]
            NP <- sample(Number, 1, replace = TRUE)
            # lee imagen y agrega ruido
            img = Image(rotate(read.bitmap(paste0(dir1,SubjectP,NP,".bmp"))/255,angle = 90))
            img_noisy1 = img
            img_noisy2 = img
            img_noisy3 = img
            img_noisy1[sample(length(img), length(img)/10)] = runif(length(img)/10, min=0, max=1)
            img_noisy2[sample(length(img), length(img)/5)] = runif(length(img)/5, min=0, max=1)
            img_noisy3[sample(length(img), length(img)/3)] = runif(length(img)/3, min=0, max=1)
            # Clasifica las imagenes
            IMG <- UnitTest(img/64/64, fitLOG_A, fitLOG_B, fitLOG_C, fitLOG_D, fitLOG_E, fitLOG_F,
                            fitLOG_G, fitLOG_H, fitLOG_I, fitLOG_K, fitLOG_L, fitLOG_M,
                            fitclass, seed = 20)
            IMG1 <- UnitTest(img_noisy1/64/64, fitLOG_A, fitLOG_B, fitLOG_C, fitLOG_D, fitLOG_E, fitLOG_F,
                             fitLOG_G, fitLOG_H, fitLOG_I, fitLOG_K, fitLOG_L, fitLOG_M,
                             fitclass, seed = 20)
            IMG2 <- UnitTest(img_noisy2/64/64, fitLOG_A, fitLOG_B, fitLOG_C, fitLOG_D, fitLOG_E, fitLOG_F,
                             fitLOG_G, fitLOG_H, fitLOG_I, fitLOG_K, fitLOG_L, fitLOG_M,
                             fitclass, seed = 20)
            IMG3 <- UnitTest(img_noisy3/64/64, fitLOG_A, fitLOG_B, fitLOG_C, fitLOG_D, fitLOG_E, fitLOG_F,
                             fitLOG_G, fitLOG_H, fitLOG_I, fitLOG_K, fitLOG_L, fitLOG_M,
                             fitclass, seed = 20)
            # guarda la clasificacion
            Class_IMG[l,1] <- IMG
            Class_IMG[l,2] <- SubjectP
            Class_IMG1[l,1] <- IMG1
            Class_IMG1[l,2] <- SubjectP
            Class_IMG2[l,1] <- IMG2
            Class_IMG2[l,2] <- SubjectP
            Class_IMG3[l,1] <- IMG3
            Class_IMG3[l,2] <- SubjectP
            l<-l+1
      }
}
colnames(Class_IMG) <- c("Clasificacion", "Referencia")
colnames(Class_IMG1) <- c("Clasificacion", "Referencia")
colnames(Class_IMG2) <- c("Clasificacion", "Referencia")
colnames(Class_IMG3) <- c("Clasificacion", "Referencia")
Class_IMG$Clasificacion <- factor(Class_IMG$Clasificacion)
Class_IMG$Referencia <- factor(Class_IMG$Referencia)
Class_IMG1$Clasificacion <- factor(Class_IMG1$Clasificacion)
Class_IMG1$Referencia <- factor(Class_IMG1$Referencia)
Class_IMG2$Clasificacion <- factor(Class_IMG2$Clasificacion)
Class_IMG2$Referencia <- factor(Class_IMG2$Referencia)
Class_IMG3$Clasificacion <- factor(Class_IMG3$Clasificacion)
Class_IMG3$Referencia <- factor(Class_IMG3$Referencia)
# Genera matriz de confusion
CM0 <- confusionMatrix(Class_IMG$Clasificacion, Class_IMG$Referencia)
CM1 <- confusionMatrix(Class_IMG1$Clasificacion, Class_IMG1$Referencia)
CM2 <- confusionMatrix(Class_IMG2$Clasificacion, Class_IMG2$Referencia)
CM3 <- confusionMatrix(Class_IMG3$Clasificacion, Class_IMG3$Referencia)
CM0
```

```
Confusion Matrix and Statistics

          Reference
Prediction  A  B  C  D  E  F  G  H  I  J  K  L  M
         A 50  0  0  0  0  0  0  0  0  0  0  0  0
         B  0 50  0  0  0  0  0  0  0  0  0  0  0
         C  0  0 50  0  0  0  0  0  0  0  0  0  0
         D  0  0  0 50  0  0  0  0  0  0  0  0  0
         E  0  0  0  0 50  0  0  0  0  0  0  0  0
         F  0  0  0  0  0 50  0  0  0  0  0  0  0
         G  0  0  0  0  0  0 50  0  0  0  0  0  0
         H  0  0  0  0  0  0  0 50  0  0  0  0  0
         I  0  0  0  0  0  0  0  0 50  0  0  0  0
         J  0  0  0  0  0  0  0  0  0 50  0  0  0
         K  0  0  0  0  0  0  0  0  0  0 50  0  0
         L  0  0  0  0  0  0  0  0  0  0  0 50  0
         M  0  0  0  0  0  0  0  0  0  0  0  0 50

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9943, 1)
    No Information Rate : 0.0769     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E Class: F
Sensitivity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
                     Class: G Class: H Class: I Class: J Class: K Class: L
Sensitivity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
                     Class: M
Sensitivity           1.00000
Specificity           1.00000
Pos Pred Value        1.00000
Neg Pred Value        1.00000
Prevalence            0.07692
Detection Rate        0.07692
Detection Prevalence  0.07692
Balanced Accuracy     1.00000
```

Acontinuaci�n se muestran las matrices de confusi�n de las imagenes con ruido con proporci�n 1/10, 1/5 y 1/3 respectivamente, con una precisi�n de 92.31% confundiendo la clase I y asignandola a la clase B.


```r
CM1
```

```
Confusion Matrix and Statistics

          Reference
Prediction  A  B  C  D  E  F  G  H  I  J  K  L  M
         A 50  0  0  0  0  0  0  0  0  0  0  0  0
         B  0 50  0  0  0  0  0  0 50  0  0  0  0
         C  0  0 50  0  0  0  0  0  0  0  0  0  0
         D  0  0  0 50  0  0  0  0  0  0  0  0  0
         E  0  0  0  0 50  0  0  0  0  0  0  0  0
         F  0  0  0  0  0 50  0  0  0  0  0  0  0
         G  0  0  0  0  0  0 50  0  0  0  0  0  0
         H  0  0  0  0  0  0  0 50  0  0  0  0  0
         I  0  0  0  0  0  0  0  0  0  0  0  0  0
         J  0  0  0  0  0  0  0  0  0 50  0  0  0
         K  0  0  0  0  0  0  0  0  0  0 50  0  0
         L  0  0  0  0  0  0  0  0  0  0  0 50  0
         M  0  0  0  0  0  0  0  0  0  0  0  0 50

Overall Statistics
                                          
               Accuracy : 0.9231          
                 95% CI : (0.8998, 0.9424)
    No Information Rate : 0.0769          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9167          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E Class: F
Sensitivity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Specificity           1.00000  0.91667  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  0.50000  1.00000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.15385  0.07692  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  0.95833  1.00000  1.00000  1.00000  1.00000
                     Class: G Class: H Class: I Class: J Class: K Class: L
Sensitivity           1.00000  1.00000  0.00000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000      NaN  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  0.92308  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.00000  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.07692  0.00000  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  1.00000  0.50000  1.00000  1.00000  1.00000
                     Class: M
Sensitivity           1.00000
Specificity           1.00000
Pos Pred Value        1.00000
Neg Pred Value        1.00000
Prevalence            0.07692
Detection Rate        0.07692
Detection Prevalence  0.07692
Balanced Accuracy     1.00000
```


```r
CM2
```

```
Confusion Matrix and Statistics

          Reference
Prediction  A  B  C  D  E  F  G  H  I  J  K  L  M
         A 50  0  0  0  0  0  0  0  0  0  0  0  0
         B  0 50  0  0  0  0  0  0  0  0  0  0  0
         C  0  0 50  0  0  0  0  0  0  0  0  0  0
         D  0  0  0 50  0  0  0  0  0  0  0  0  0
         E  0  0  0  0 50  0  0  0  0  0  0  0  0
         F  0  0  0  0  0 50  0  0  0  0  0  0  0
         G  0  0  0  0  0  0 50  0  0  0  0  0  0
         H  0  0  0  0  0  0  0 50  0  0  0  0  0
         I  0  0  0  0  0  0  0  0  0  0  0  0  0
         J  0  0  0  0  0  0  0  0  0 50  0  0  0
         K  0  0  0  0  0  0  0  0  0  0 50  0  0
         L  0  0  0  0  0  0  0  0  0  0  0 50  0
         M  0  0  0  0  0  0  0  0  0  0  0  0 50

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9939, 1)
    No Information Rate : 0.0833     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
 Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E Class: F
Sensitivity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Prevalence            0.08333  0.08333  0.08333  0.08333  0.08333  0.08333
Detection Rate        0.08333  0.08333  0.08333  0.08333  0.08333  0.08333
Detection Prevalence  0.08333  0.08333  0.08333  0.08333  0.08333  0.08333
Balanced Accuracy     1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
                     Class: G Class: H Class: I Class: J Class: K Class: L
Sensitivity           1.00000  1.00000       NA  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000        1  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000       NA  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000       NA  1.00000  1.00000  1.00000
Prevalence            0.08333  0.08333        0  0.08333  0.08333  0.08333
Detection Rate        0.08333  0.08333        0  0.08333  0.08333  0.08333
Detection Prevalence  0.08333  0.08333        0  0.08333  0.08333  0.08333
Balanced Accuracy     1.00000  1.00000       NA  1.00000  1.00000  1.00000
                     Class: M
Sensitivity           1.00000
Specificity           1.00000
Pos Pred Value        1.00000
Neg Pred Value        1.00000
Prevalence            0.08333
Detection Rate        0.08333
Detection Prevalence  0.08333
Balanced Accuracy     1.00000
```


```r
CM3
```

```
Confusion Matrix and Statistics

          Reference
Prediction  A  B  C  D  E  F  G  H  I  J  K  L  M
         A 50  0  0  0  0  0  0  0  0  0  0  0  0
         B  0 50  0  0  0  0  0  0 50  0  0  0  0
         C  0  0 50  0  0  0  0  0  0  0  0  0  0
         D  0  0  0 50  0  0  0  0  0  0  0  0  0
         E  0  0  0  0 50  0  0  0  0  0  0  0  0
         F  0  0  0  0  0 50  0  0  0  0  0  0  0
         G  0  0  0  0  0  0 50  0  0  0  0  0  0
         H  0  0  0  0  0  0  0 50  0  0  0  0  0
         I  0  0  0  0  0  0  0  0  0  0  0  0  0
         J  0  0  0  0  0  0  0  0  0 50  0  0  0
         K  0  0  0  0  0  0  0  0  0  0 50  0  0
         L  0  0  0  0  0  0  0  0  0  0  0 50  0
         M  0  0  0  0  0  0  0  0  0  0  0  0 50

Overall Statistics
                                          
               Accuracy : 0.9231          
                 95% CI : (0.8998, 0.9424)
    No Information Rate : 0.0769          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9167          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E Class: F
Sensitivity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Specificity           1.00000  0.91667  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  0.50000  1.00000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.15385  0.07692  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  0.95833  1.00000  1.00000  1.00000  1.00000
                     Class: G Class: H Class: I Class: J Class: K Class: L
Sensitivity           1.00000  1.00000  0.00000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000      NaN  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  0.92308  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.00000  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.07692  0.00000  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  1.00000  0.50000  1.00000  1.00000  1.00000
                     Class: M
Sensitivity           1.00000
Specificity           1.00000
Pos Pred Value        1.00000
Neg Pred Value        1.00000
Prevalence            0.07692
Detection Rate        0.07692
Detection Prevalence  0.07692
Balanced Accuracy     1.00000
```

---

# Conclusiones

El clasificador presenta un buen desempe�o (99.846%) para todas las clases. Al aplicar ruido a las im�genes, el desempe�o decae a 92.31%, aunque todav�a es un buen desempe�o.

Para resolver esto, se puede aplicar un filtro para el ruido, en particular el filtro de mediana es una excelente opci�n para este tipo de ruido.

La ejecuci�n del filtro es bastante r�pido, pero requiere tener cargado de antemano los 13 modelos de regresi�n y el modelo de clasificaci�n. Adem�s de las transformadas de fourier de las imagenes de dise�o de los filtros y los filtros de correlaci�n.

Este filtro tiene la ventaja de poder ser dise�ado con s�lo una imagen para cada filtro. En caso de querer agregar otra clase se debe realizar la regresi�n para la nueva clase y entrenar nuevamente el clasificador con las clases anteriores y la nueva, por lo que ampliarlo es relativamente sencillo.

Una incertidumbre es el desempe�o del filtro para imagenes tomadas en distintas condiciones de luz, foco, contraste, etc. ya que en este caso todas las imagenes est�n tomadas con las mismas condiciones. Por lo que un paso siguiente ser�a evaluar el clasificador con imagenes en distintas condiciones.

Otro paso a seguir ser�a probar el clasificador ante cambios de posici�n, ya que los valores de los peak de correlaci�n cambian con la posici�n y es posible que el clasificador (y los modelos de regresi�n) deba ser reentrenado completamente.

---

# Referencia

Para revisar el c�digo de este trabajo ir a:

https://github.com/desareca/Procesamiento-Imagenes-R

---

# Informaci�n de sesi�n


```r
sessionInfo()
```

```
R version 3.5.2 (2018-12-20)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17134)

Matrix products: default

locale:
[1] LC_COLLATE=Spanish_Chile.1252  LC_CTYPE=Spanish_Chile.1252   
[3] LC_MONETARY=Spanish_Chile.1252 LC_NUMERIC=C                  
[5] LC_TIME=Spanish_Chile.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] dplyr_0.8.0.1    MASS_7.3-51.1    caret_6.0-81     lattice_0.20-38 
[5] ggplot2_3.1.0    rgl_0.99.16      fftwtools_0.9-8  readbitmap_0.1.5
[9] EBImage_4.24.0  

loaded via a namespace (and not attached):
 [1] jsonlite_1.6            splines_3.5.2          
 [3] foreach_1.4.4           prodlim_2018.04.18     
 [5] shiny_1.2.0             assertthat_0.2.0       
 [7] stats4_3.5.2            tiff_0.1-5             
 [9] yaml_2.2.0              ipred_0.9-8            
[11] pillar_1.3.1            glue_1.3.0             
[13] digest_0.6.18           manipulateWidget_0.10.0
[15] promises_1.0.1          colorspace_1.4-0       
[17] recipes_0.1.4           htmltools_0.3.6        
[19] httpuv_1.4.5.1          Matrix_1.2-15          
[21] plyr_1.8.4              timeDate_3043.102      
[23] pkgconfig_2.0.2         purrr_0.3.0            
[25] xtable_1.8-3            scales_1.0.0           
[27] webshot_0.5.1           jpeg_0.1-8             
[29] later_0.8.0             gower_0.1.2            
[31] lava_1.6.5              tibble_2.0.1           
[33] generics_0.0.2          withr_2.1.2            
[35] nnet_7.3-12             BiocGenerics_0.28.0    
[37] lazyeval_0.2.1          survival_2.43-3        
[39] magrittr_1.5            crayon_1.3.4           
[41] mime_0.6                evaluate_0.13          
[43] nlme_3.1-137            class_7.3-14           
[45] tools_3.5.2             data.table_1.12.0      
[47] bmp_0.3                 stringr_1.4.0          
[49] munsell_0.5.0           locfit_1.5-9.1         
[51] compiler_3.5.2          rlang_0.3.1            
[53] grid_3.5.2              RCurl_1.95-4.11        
[55] iterators_1.0.10        htmlwidgets_1.3        
[57] crosstalk_1.0.0         miniUI_0.1.1.1         
[59] bitops_1.0-6            rmarkdown_1.11         
[61] gtable_0.2.0            ModelMetrics_1.2.2     
[63] codetools_0.2-15        abind_1.4-5            
[65] reshape2_1.4.3          R6_2.4.0               
[67] lubridate_1.7.4         knitr_1.21             
[69] stringi_1.3.1           parallel_3.5.2         
[71] Rcpp_1.0.0              rpart_4.1-13           
[73] png_0.1-7               tidyselect_0.2.5       
[75] xfun_0.5               
```








