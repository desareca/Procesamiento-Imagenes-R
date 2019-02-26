reg_face <- function(TRAIN, TEST, VAL, method1, method2="LogitBoost", 
                     trC=trainControl(method="cv", number=5), seed = 50){
      
      # TRAIN: conjunto de entrenamiento para las 13 regresiones
      # TEST: conjunto de pruebas para entrenar el clasificador en base a los 
      #           de las 13 regresiones.
      # VAL: conjunto de validación para probar y mostrar los resultados.
      # method1: metodo con que se realizan las regresiones.
      # method2: metodo con que se realiza el clasificador.
      # trC: metodo para control de modelado.

      # carga conjunto de entrenamiento para hacer 1 regresion por rostro
      # pb <- txtProgressBar(min = 0, max = 17, style = 3, initial = 0)
      
      Train_A <- mutate(TRAIN[,c(1,14,27)], clase=1*(TRAIN$imagen=="A"))
      Train_B <- mutate(TRAIN[,c(2,15,28)], clase=1*(TRAIN$imagen=="B"))
      Train_C <- mutate(TRAIN[,c(3,16,29)], clase=1*(TRAIN$imagen=="C"))
      Train_D <- mutate(TRAIN[,c(4,17,30)], clase=1*(TRAIN$imagen=="D"))
      Train_E <- mutate(TRAIN[,c(5,18,31)], clase=1*(TRAIN$imagen=="E"))
      Train_F <- mutate(TRAIN[,c(6,19,32)], clase=1*(TRAIN$imagen=="F"))
      Train_G <- mutate(TRAIN[,c(7,20,33)], clase=1*(TRAIN$imagen=="G"))
      Train_H <- mutate(TRAIN[,c(8,21,34)], clase=1*(TRAIN$imagen=="H"))
      Train_I <- mutate(TRAIN[,c(9,22,35)], clase=1*(TRAIN$imagen=="I"))
      Train_J <- mutate(TRAIN[,c(10,23,36)], clase=1*(TRAIN$imagen=="J"))
      Train_K <- mutate(TRAIN[,c(11,24,37)], clase=1*(TRAIN$imagen=="K"))
      Train_L <- mutate(TRAIN[,c(12,25,38)], clase=1*(TRAIN$imagen=="L"))
      Train_M <- mutate(TRAIN[,c(13,26,39)], clase=1*(TRAIN$imagen=="M"))

      
      # setTxtProgressBar(pb, 1)
      # carga conjunto de prueba para hacer 1 regresion por rostro
      
      Test_A <- mutate(TEST[,c(1,14,27)], clase=1*(TEST$imagen=="A"))
      Test_B <- mutate(TEST[,c(2,15,28)], clase=1*(TEST$imagen=="B"))
      Test_C <- mutate(TEST[,c(3,16,29)], clase=1*(TEST$imagen=="C"))
      Test_D <- mutate(TEST[,c(4,17,30)], clase=1*(TEST$imagen=="D"))
      Test_E <- mutate(TEST[,c(5,18,31)], clase=1*(TEST$imagen=="E"))
      Test_F <- mutate(TEST[,c(6,19,32)], clase=1*(TEST$imagen=="F"))
      Test_G <- mutate(TEST[,c(7,20,33)], clase=1*(TEST$imagen=="G"))
      Test_H <- mutate(TEST[,c(8,21,34)], clase=1*(TEST$imagen=="H"))
      Test_I <- mutate(TEST[,c(9,22,35)], clase=1*(TEST$imagen=="I"))
      Test_J <- mutate(TEST[,c(10,23,36)], clase=1*(TEST$imagen=="J"))
      Test_K <- mutate(TEST[,c(11,24,37)], clase=1*(TEST$imagen=="K"))
      Test_L <- mutate(TEST[,c(12,25,38)], clase=1*(TEST$imagen=="L"))
      Test_M <- mutate(TEST[,c(13,26,39)], clase=1*(TEST$imagen=="M"))
      
      # setTxtProgressBar(pb, 2)
      # carga conjunto de validacion para hacer 1 regresion por rostro
      
      Val_A <- mutate(VAL[,c(1,14,27)], clase=1*(VAL$imagen=="A"))
      Val_B <- mutate(VAL[,c(2,15,28)], clase=1*(VAL$imagen=="B"))
      Val_C <- mutate(VAL[,c(3,16,29)], clase=1*(VAL$imagen=="C"))
      Val_D <- mutate(VAL[,c(4,17,30)], clase=1*(VAL$imagen=="D"))
      Val_E <- mutate(VAL[,c(5,18,31)], clase=1*(VAL$imagen=="E"))
      Val_F <- mutate(VAL[,c(6,19,32)], clase=1*(VAL$imagen=="F"))
      Val_G <- mutate(VAL[,c(7,20,33)], clase=1*(VAL$imagen=="G"))
      Val_H <- mutate(VAL[,c(8,21,34)], clase=1*(VAL$imagen=="H"))
      Val_I <- mutate(VAL[,c(9,22,35)], clase=1*(VAL$imagen=="I"))
      Val_J <- mutate(VAL[,c(10,23,36)], clase=1*(VAL$imagen=="J"))
      Val_K <- mutate(VAL[,c(11,24,37)], clase=1*(VAL$imagen=="K"))
      Val_L <- mutate(VAL[,c(12,25,38)], clase=1*(VAL$imagen=="L"))
      Val_M <- mutate(VAL[,c(13,26,39)], clase=1*(VAL$imagen=="M"))
      
      # setTxtProgressBar(pb, 3)
      
      remove(TRAIN)
      remove(TEST)
      remove(VAL)
      
      # Genera 13 modelos de regresion
      trC=trainControl(method="cv", number=5)
      m="RMSE"
      method = method1
      set.seed(seed)
      
      fitLOG_A <- train(clase ~ ., data=Train_A, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 3)
      fitLOG_B <- train(clase ~ ., data=Train_B, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 4)
      fitLOG_C <- train(clase ~ ., data=Train_C, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 5)
      fitLOG_D <- train(clase ~ ., data=Train_D, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 6)
      fitLOG_E <- train(clase ~ ., data=Train_E, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 7)
      fitLOG_F <- train(clase ~ ., data=Train_F, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 8)
      fitLOG_G <- train(clase ~ ., data=Train_G, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 9)
      fitLOG_H <- train(clase ~ ., data=Train_H, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 10)
      fitLOG_I <- train(clase ~ ., data=Train_I, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 11)
      fitLOG_J <- train(clase ~ ., data=Train_J, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 12)
      fitLOG_K <- train(clase ~ ., data=Train_K, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 13)
      fitLOG_L <- train(clase ~ ., data=Train_L, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 14)
      fitLOG_M <- train(clase ~ ., data=Train_M, method=method, metric=m, trControl=trC)
      # setTxtProgressBar(pb, 15)
      
      saveRDS(fitLOG_A, paste0("fitReg_",method1,"_",method2,"_A.rds"))
      saveRDS(fitLOG_B, paste0("fitReg_",method1,"_",method2,"_B.rds"))
      saveRDS(fitLOG_C, paste0("fitReg_",method1,"_",method2,"_C.rds"))
      saveRDS(fitLOG_D, paste0("fitReg_",method1,"_",method2,"_D.rds"))
      saveRDS(fitLOG_E, paste0("fitReg_",method1,"_",method2,"_E.rds"))
      saveRDS(fitLOG_F, paste0("fitReg_",method1,"_",method2,"_F.rds"))
      saveRDS(fitLOG_G, paste0("fitReg_",method1,"_",method2,"_G.rds"))
      saveRDS(fitLOG_H, paste0("fitReg_",method1,"_",method2,"_H.rds"))
      saveRDS(fitLOG_I, paste0("fitReg_",method1,"_",method2,"_I.rds"))
      saveRDS(fitLOG_J, paste0("fitReg_",method1,"_",method2,"_J.rds"))
      saveRDS(fitLOG_K, paste0("fitReg_",method1,"_",method2,"_K.rds"))
      saveRDS(fitLOG_L, paste0("fitReg_",method1,"_",method2,"_L.rds"))
      saveRDS(fitLOG_M, paste0("fitReg_",method1,"_",method2,"_M.rds"))
      
      # calcula predicciones de regresiones como entrada al clasificador (conjunto de pruebas)
      P_A <- predict(fitLOG_A, newdata=Test_A)
      P_B <- predict(fitLOG_B, newdata=Test_B)
      P_C <- predict(fitLOG_C, newdata=Test_C)
      P_D <- predict(fitLOG_D, newdata=Test_D)
      P_E <- predict(fitLOG_E, newdata=Test_E)
      P_F <- predict(fitLOG_F, newdata=Test_F)
      P_G <- predict(fitLOG_G, newdata=Test_G)
      P_H <- predict(fitLOG_H, newdata=Test_H)
      P_I <- predict(fitLOG_I, newdata=Test_I)
      P_J <- predict(fitLOG_J, newdata=Test_J)
      P_K <- predict(fitLOG_K, newdata=Test_K)
      P_L <- predict(fitLOG_L, newdata=Test_L)
      P_M <- predict(fitLOG_M, newdata=Test_M)
      # setTxtProgressBar(pb, 16)
      
      
      P <- cbind(P_A, P_B, P_C, P_D, P_E, P_F, P_G, P_H, P_I, P_J, P_K, P_L,
                 P_M, TEST$imagen)
      P <- data.frame(P, stringsAsFactors = FALSE)
      colnames(P) <- c("P_A", "P_B", "P_C", "P_D", "P_E", "P_F", "P_G", "P_H",
                       "P_I", "P_J", "P_K", "P_L", "P_M", "imagen")
      P$imagen <- as.factor(LETTERS[P$imagen])
      
      saveRDS(P, paste0("TestReg_",method1,"_",method2,".rds"))
      # crea clasificador con los 13 modelos de regresion

      fitRF <- train(imagen ~ ., data=P, method = method2, metric="Accuracy", trControl=trC)
      # CM_RF_Test <- confusionMatrix(predict(fitRF, newdata=P), P$imagen)
      saveRDS(fitRF, paste0("FitClass_",method1,"_",method2,".rds"))
      
      
      V_A <- predict(fitLOG_A, newdata=Val_A)
      V_B <- predict(fitLOG_B, newdata=Val_B)
      V_C <- predict(fitLOG_C, newdata=Val_C)
      V_D <- predict(fitLOG_D, newdata=Val_D)
      V_E <- predict(fitLOG_E, newdata=Val_E)
      V_F <- predict(fitLOG_F, newdata=Val_F)
      V_G <- predict(fitLOG_G, newdata=Val_G)
      V_H <- predict(fitLOG_H, newdata=Val_H)
      V_I <- predict(fitLOG_I, newdata=Val_I)
      V_J <- predict(fitLOG_J, newdata=Val_J)
      V_K <- predict(fitLOG_K, newdata=Val_K)
      V_L <- predict(fitLOG_L, newdata=Val_L)
      V_M <- predict(fitLOG_M, newdata=Val_M)
      
      V <- cbind(V_A, V_B, V_C, V_D, V_E, V_F, V_G, V_H, V_I, V_J, V_K, V_L,
                 V_M, VAL$imagen)
      V <- data.frame(V, stringsAsFactors = FALSE)
      colnames(V) <- c("P_A", "P_B", "P_C", "P_D", "P_E", "P_F", "P_G", "P_H",
                       "P_I", "P_J", "P_K", "P_L", "P_M", "imagen")
      V$imagen <- as.factor(LETTERS[V$imagen])
      CM_RF_Val <- confusionMatrix(predict(fitRF, newdata=V), V$imagen)
      saveRDS(CM_RF_Val, paste0("ValClass_",method1,"_",method2,".rds"))
      
      # setTxtProgressBar(pb, 17)
      CM_RF_Val
}