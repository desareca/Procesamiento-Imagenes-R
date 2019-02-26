UnitTest <- function(img, fitLOG_A, fitLOG_B, fitLOG_C, fitLOG_D, fitLOG_E, fitLOG_F,
                     fitLOG_G, fitLOG_H, fitLOG_I, fitLOG_K, fitLOG_L, fitLOG_M,
                     fitclass, seed = 50){
      
      source("fftshift.R") # centra el espectro de frecuencias
      source("PCE.R") # Calcula PCE
      source("PSR.R") # Calcula PSR
      source("DC.R") # Calcula DC
      source("ExtCaract2.R") # Hace pruebas
      
      
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
      
      
      # TRAIN: conjunto de entrenamiento para las 13 regresiones
      # TEST: conjunto de pruebas para entrenar el clasificador en base a los 
      #           de las 13 regresiones.
      # VAL: conjunto de validación para probar y mostrar los resultados.
      # method1: metodo con que se realizan las regresiones.
      # method2: metodo con que se realiza el clasificador.
      # trC: metodo para control de modelado.
      set.seed(seed)
      Nf <- sample(Number, 13, replace = TRUE)
      # lee imagenes de filtro y prueba
      # imgA = Image(rotate(read.bitmap(paste0(dir1,Subject[1],Nf[1],".bmp"))/255/64/64,angle = 90))
      # imgB = Image(rotate(read.bitmap(paste0(dir1,Subject[2],Nf[2],".bmp"))/255/64/64,angle = 90))
      # imgC = Image(rotate(read.bitmap(paste0(dir1,Subject[3],Nf[3],".bmp"))/255/64/64,angle = 90))
      # imgD = Image(rotate(read.bitmap(paste0(dir1,Subject[4],Nf[4],".bmp"))/255/64/64,angle = 90))
      # imgE = Image(rotate(read.bitmap(paste0(dir1,Subject[5],Nf[5],".bmp"))/255/64/64,angle = 90))
      # imgF = Image(rotate(read.bitmap(paste0(dir1,Subject[6],Nf[6],".bmp"))/255/64/64,angle = 90))
      # imgG = Image(rotate(read.bitmap(paste0(dir1,Subject[7],Nf[7],".bmp"))/255/64/64,angle = 90))
      # imgH = Image(rotate(read.bitmap(paste0(dir1,Subject[8],Nf[8],".bmp"))/255/64/64,angle = 90))
      # imgI = Image(rotate(read.bitmap(paste0(dir1,Subject[9],Nf[9],".bmp"))/255/64/64,angle = 90))
      # imgJ = Image(rotate(read.bitmap(paste0(dir1,Subject[10],Nf[10],".bmp"))/255/64/64,angle = 90))
      # imgK = Image(rotate(read.bitmap(paste0(dir1,Subject[11],Nf[11],".bmp"))/255/64/64,angle = 90))
      # imgL = Image(rotate(read.bitmap(paste0(dir1,Subject[12],Nf[12],".bmp"))/255/64/64,angle = 90))
      # imgM = Image(rotate(read.bitmap(paste0(dir1,Subject[13],Nf[13],".bmp"))/255/64/64,angle = 90))
      # 
      imgp = img
      
      # calcula FFT a las imagenes
      fftA <- readRDS("fftA.rds")
      fftB <- readRDS("fftB.rds")
      fftC <- readRDS("fftC.rds")
      fftD <- readRDS("fftD.rds")
      fftE <- readRDS("fftE.rds")
      fftF <- readRDS("fftF.rds")
      fftG <- readRDS("fftG.rds")
      fftH <- readRDS("fftH.rds")
      fftI <- readRDS("fftI.rds")
      fftJ <- readRDS("fftJ.rds")
      fftK <- readRDS("fftK.rds")
      fftL <- readRDS("fftL.rds")
      fftM <- readRDS("fftM.rds")
      
      fftp <- fftshift(fftw2d(imgp))
      
      # Filtros POF
      HA <- readRDS("HA.rds")
      HB <- readRDS("HB.rds")
      HC <- readRDS("HC.rds")
      HD <- readRDS("HD.rds")
      HE <- readRDS("HE.rds")
      HF <- readRDS("HF.rds")
      HG <- readRDS("HG.rds")
      HH <- readRDS("HH.rds")
      HI <- readRDS("HI.rds")
      HJ <- readRDS("HJ.rds")
      HK <- readRDS("HK.rds")
      HL <- readRDS("HL.rds")
      HM <- readRDS("HM.rds")

      # Hace las correlaciones
      img_corA <- fftshift(fftw2d(fftp*Conj(HA), inverse = TRUE))
      img_corB <- fftshift(fftw2d(fftp*Conj(HB), inverse = TRUE))
      img_corC <- fftshift(fftw2d(fftp*Conj(HC), inverse = TRUE))
      img_corD <- fftshift(fftw2d(fftp*Conj(HD), inverse = TRUE))
      img_corE <- fftshift(fftw2d(fftp*Conj(HE), inverse = TRUE))
      img_corF <- fftshift(fftw2d(fftp*Conj(HF), inverse = TRUE))
      img_corG <- fftshift(fftw2d(fftp*Conj(HG), inverse = TRUE))
      img_corH <- fftshift(fftw2d(fftp*Conj(HH), inverse = TRUE))
      img_corI <- fftshift(fftw2d(fftp*Conj(HI), inverse = TRUE))
      img_corJ <- fftshift(fftw2d(fftp*Conj(HJ), inverse = TRUE))
      img_corK <- fftshift(fftw2d(fftp*Conj(HK), inverse = TRUE))
      img_corL <- fftshift(fftw2d(fftp*Conj(HL), inverse = TRUE))
      img_corM <- fftshift(fftw2d(fftp*Conj(HM), inverse = TRUE))
      
      img_corHA <- readRDS("img_corHA.rds")
      img_corHB <- readRDS("img_corHB.rds")
      img_corHC <- readRDS("img_corHC.rds")
      img_corHD <- readRDS("img_corHD.rds")
      img_corHE <- readRDS("img_corHE.rds")
      img_corHF <- readRDS("img_corHF.rds")
      img_corHG <- readRDS("img_corHG.rds")
      img_corHH <- readRDS("img_corHH.rds")
      img_corHI <- readRDS("img_corHI.rds")
      img_corHJ <- readRDS("img_corHJ.rds")
      img_corHK <- readRDS("img_corHK.rds")
      img_corHL <- readRDS("img_corHL.rds")
      img_corHM <- readRDS("img_corHM.rds")
      
      # Hace pruebas imagenes sin filtro PA
      t <- data.frame()
      
      t[1, 1] <- PCE(img_corA)
      t[1, 2] <- PCE(img_corB)
      t[1, 3] <- PCE(img_corC)
      t[1, 4] <- PCE(img_corD)
      t[1, 5] <- PCE(img_corE)
      t[1, 6] <- PCE(img_corF)
      t[1, 7] <- PCE(img_corG)
      t[1, 8] <- PCE(img_corH)
      t[1, 9] <- PCE(img_corI)
      t[1, 10] <- PCE(img_corJ)
      t[1, 11] <- PCE(img_corK)
      t[1, 12] <- PCE(img_corL)
      t[1, 13] <- PCE(img_corM)
      
      t[1, 14] <- PSR(img_corA)
      t[1, 15] <- PSR(img_corB)
      t[1, 16] <- PSR(img_corC)
      t[1, 17] <- PSR(img_corD)
      t[1, 18] <- PSR(img_corE)
      t[1, 19] <- PSR(img_corF)
      t[1, 20] <- PSR(img_corG)
      t[1, 21] <- PSR(img_corH)
      t[1, 22] <- PSR(img_corI)
      t[1, 23] <- PSR(img_corJ)
      t[1, 24] <- PSR(img_corK)
      t[1, 25] <- PSR(img_corL)
      t[1, 26] <- PSR(img_corM)
      
      t[1, 27] <- DC(img_corHA, img_corA, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 28] <- DC(img_corHB, img_corB, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 29] <- DC(img_corHC, img_corC, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 30] <- DC(img_corHD, img_corD, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 31] <- DC(img_corHE, img_corE, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 32] <- DC(img_corHF, img_corF, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 33] <- DC(img_corHG, img_corG, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 34] <- DC(img_corHH, img_corH, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 35] <- DC(img_corHI, img_corI, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 36] <- DC(img_corHJ, img_corJ, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 37] <- DC(img_corHK, img_corK, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 38] <- DC(img_corHL, img_corL, Co = c(32,32), Cc = c(32,32), Vec = 15)
      t[1, 39] <- DC(img_corHM, img_corM, Co = c(32,32), Cc = c(32,32), Vec = 15)
      
      # TEST[l, 40] <- SubjectP
      
      colnames(t) <- c("PCE_A", "PCE_B", "PCE_C", "PCE_D", "PCE_E", "PCE_F", "PCE_G", "PCE_H",
                          "PCE_I", "PCE_J", "PCE_K", "PCE_L", "PCE_M", "PSR_A", "PSR_B", "PSR_C",
                          "PSR_D", "PSR_E", "PSR_F", "PSR_G", "PSR_H", "PSR_I", "PSR_J", "PSR_K",
                          "PSR_L", "PSR_M", "DC_A", "DC_B", "DC_C", "DC_D", "DC_E", "DC_F",
                          "DC_G", "DC_H", "DC_I", "DC_J", "DC_K", "DC_L", "DC_M")
      
      # Ajusta variables para ingresar en modelos de regresion
      Test_A <- t[,c(1,14,27)]
      Test_B <- t[,c(2,15,28)]
      Test_C <- t[,c(3,16,29)]
      Test_D <- t[,c(4,17,30)]
      Test_E <- t[,c(5,18,31)]
      Test_F <- t[,c(6,19,32)]
      Test_G <- t[,c(7,20,33)]
      Test_H <- t[,c(8,21,34)]
      Test_I <- t[,c(9,22,35)]
      Test_J <- t[,c(10,23,36)]
      Test_K <- t[,c(11,24,37)]
      Test_L <- t[,c(12,25,38)]
      Test_M <- t[,c(13,26,39)]
      
      
      # # Carga modelos de regresion
      # fitLOG_A <- readRDS("fitReg_bayesglm_LogitBoost_A.rds")
      # fitLOG_B <- readRDS("fitReg_bayesglm_LogitBoost_B.rds")
      # fitLOG_C <- readRDS("fitReg_bayesglm_LogitBoost_C.rds")
      # fitLOG_D <- readRDS("fitReg_bayesglm_LogitBoost_D.rds")
      # fitLOG_E <- readRDS("fitReg_bayesglm_LogitBoost_E.rds")
      # fitLOG_F <- readRDS("fitReg_bayesglm_LogitBoost_F.rds")
      # fitLOG_G <- readRDS("fitReg_bayesglm_LogitBoost_G.rds")
      # fitLOG_H <- readRDS("fitReg_bayesglm_LogitBoost_H.rds")
      # fitLOG_I <- readRDS("fitReg_bayesglm_LogitBoost_I.rds")
      # fitLOG_J <- readRDS("fitReg_bayesglm_LogitBoost_J.rds")
      # fitLOG_K <- readRDS("fitReg_bayesglm_LogitBoost_K.rds")
      # fitLOG_L <- readRDS("fitReg_bayesglm_LogitBoost_L.rds")
      # fitLOG_M <- readRDS("fitReg_bayesglm_LogitBoost_M.rds")
      # 
      # # Carga modelo de clasificacion
      # fitclass <- readRDS("FitClass_bayesglm_LogitBoost.rds")
      
      # Pruebas imagen modelos de regresion
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
      
      # Agrupa resultados de regresion
      P <- cbind(P_A, P_B, P_C, P_D, P_E, P_F, P_G, P_H, P_I, P_J, P_K, P_L, P_M)
      P <- data.frame(P, stringsAsFactors = FALSE)
      colnames(P) <- c("P_A", "P_B", "P_C", "P_D", "P_E", "P_F", "P_G", "P_H",
                       "P_I", "P_J", "P_K", "P_L", "P_M")
      
      # Calcula resultado de clasificacion
      Result <- predict(fitclass, newdata=P)
      # Result
      as.character(Result)
}