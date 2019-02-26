ExtCaract2 <- function(n=1000, seed = 30, dir1, Subject, Number){
      source("fftshift.R") # centra el espectro de frecuencias
      source("PCE.R") # Calcula PCE
      source("PSR.R") # Calcula PSR
      source("DC.R") # Calcula DC
      set.seed(seed)
      TEST <- data.frame()
      l=1
      # pb <- txtProgressBar(min = 0, max = 13*n, style = 3, initial = 0)
      for (i in 1:13) {
            for (k in 1:n) {
                  # carga indices aleatorios
                  SubjectP <- sample(Subject[i], 1)
                  Nf <- sample(Number, 13, replace = TRUE)
                  NP <- sample(Number[-i],1)
                  # lee imagenes de filtro y prueba
                  imgA = Image(rotate(read.bitmap(paste0(dir1,Subject[1],Nf[1],".bmp"))/255/64/64,angle = 90))
                  imgB = Image(rotate(read.bitmap(paste0(dir1,Subject[2],Nf[2],".bmp"))/255/64/64,angle = 90))
                  imgC = Image(rotate(read.bitmap(paste0(dir1,Subject[3],Nf[3],".bmp"))/255/64/64,angle = 90))
                  imgD = Image(rotate(read.bitmap(paste0(dir1,Subject[4],Nf[4],".bmp"))/255/64/64,angle = 90))
                  imgE = Image(rotate(read.bitmap(paste0(dir1,Subject[5],Nf[5],".bmp"))/255/64/64,angle = 90))
                  imgF = Image(rotate(read.bitmap(paste0(dir1,Subject[6],Nf[6],".bmp"))/255/64/64,angle = 90))
                  imgG = Image(rotate(read.bitmap(paste0(dir1,Subject[7],Nf[7],".bmp"))/255/64/64,angle = 90))
                  imgH = Image(rotate(read.bitmap(paste0(dir1,Subject[8],Nf[8],".bmp"))/255/64/64,angle = 90))
                  imgI = Image(rotate(read.bitmap(paste0(dir1,Subject[9],Nf[9],".bmp"))/255/64/64,angle = 90))
                  imgJ = Image(rotate(read.bitmap(paste0(dir1,Subject[10],Nf[10],".bmp"))/255/64/64,angle = 90))
                  imgK = Image(rotate(read.bitmap(paste0(dir1,Subject[11],Nf[11],".bmp"))/255/64/64,angle = 90))
                  imgL = Image(rotate(read.bitmap(paste0(dir1,Subject[12],Nf[12],".bmp"))/255/64/64,angle = 90))
                  imgM = Image(rotate(read.bitmap(paste0(dir1,Subject[13],Nf[13],".bmp"))/255/64/64,angle = 90))
                  
                  imgp = Image(rotate(read.bitmap(paste0(dir1,SubjectP,NP,".bmp"))/255/64/64,angle = 90))
                  
                  # calcula FFT a las imagenes
                  fftA <- fftshift(fftw2d(imgA))
                  fftB <- fftshift(fftw2d(imgB))
                  fftC <- fftshift(fftw2d(imgC))
                  fftD <- fftshift(fftw2d(imgD))
                  fftE <- fftshift(fftw2d(imgE))
                  fftF <- fftshift(fftw2d(imgF))
                  fftG <- fftshift(fftw2d(imgG))
                  fftH <- fftshift(fftw2d(imgH))
                  fftI <- fftshift(fftw2d(imgI))
                  fftJ <- fftshift(fftw2d(imgJ))
                  fftK <- fftshift(fftw2d(imgK))
                  fftL <- fftshift(fftw2d(imgL))
                  fftM <- fftshift(fftw2d(imgM))
                  
                  fftp <- fftshift(fftw2d(imgp))

                  
                  # Filtros POF
                  HA <- fftshift(fftw2d(imgA))/Mod(fftshift(fftw2d(imgA)))
                  HB <- fftshift(fftw2d(imgB))/Mod(fftshift(fftw2d(imgB)))
                  HC <- fftshift(fftw2d(imgC))/Mod(fftshift(fftw2d(imgC)))
                  HD <- fftshift(fftw2d(imgD))/Mod(fftshift(fftw2d(imgD)))
                  HE <- fftshift(fftw2d(imgE))/Mod(fftshift(fftw2d(imgE)))
                  HF <- fftshift(fftw2d(imgF))/Mod(fftshift(fftw2d(imgF)))
                  HG <- fftshift(fftw2d(imgG))/Mod(fftshift(fftw2d(imgG)))
                  HH <- fftshift(fftw2d(imgH))/Mod(fftshift(fftw2d(imgH)))
                  HI <- fftshift(fftw2d(imgI))/Mod(fftshift(fftw2d(imgI)))
                  HJ <- fftshift(fftw2d(imgJ))/Mod(fftshift(fftw2d(imgJ)))
                  HK <- fftshift(fftw2d(imgK))/Mod(fftshift(fftw2d(imgK)))
                  HL <- fftshift(fftw2d(imgL))/Mod(fftshift(fftw2d(imgL)))
                  HM <- fftshift(fftw2d(imgM))/Mod(fftshift(fftw2d(imgM)))

                  HA[is.na(HA)] <- 0
                  HB[is.na(HB)] <- 0
                  HC[is.na(HC)] <- 0
                  HD[is.na(HD)] <- 0
                  HE[is.na(HE)] <- 0
                  HF[is.na(HF)] <- 0
                  HG[is.na(HG)] <- 0
                  HH[is.na(HH)] <- 0
                  HI[is.na(HI)] <- 0
                  HJ[is.na(HJ)] <- 0
                  HK[is.na(HK)] <- 0
                  HL[is.na(HL)] <- 0
                  HM[is.na(HM)] <- 0
                  
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
                  
                  img_corHA <- fftshift(fftw2d(fftA*Conj(HA), inverse = TRUE))
                  img_corHB <- fftshift(fftw2d(fftB*Conj(HB), inverse = TRUE))
                  img_corHC <- fftshift(fftw2d(fftC*Conj(HC), inverse = TRUE))
                  img_corHD <- fftshift(fftw2d(fftD*Conj(HD), inverse = TRUE))
                  img_corHE <- fftshift(fftw2d(fftE*Conj(HE), inverse = TRUE))
                  img_corHF <- fftshift(fftw2d(fftF*Conj(HF), inverse = TRUE))
                  img_corHG <- fftshift(fftw2d(fftG*Conj(HG), inverse = TRUE))
                  img_corHH <- fftshift(fftw2d(fftH*Conj(HH), inverse = TRUE))
                  img_corHI <- fftshift(fftw2d(fftI*Conj(HI), inverse = TRUE))
                  img_corHJ <- fftshift(fftw2d(fftJ*Conj(HJ), inverse = TRUE))
                  img_corHK <- fftshift(fftw2d(fftK*Conj(HK), inverse = TRUE))
                  img_corHL <- fftshift(fftw2d(fftL*Conj(HL), inverse = TRUE))
                  img_corHM <- fftshift(fftw2d(fftM*Conj(HM), inverse = TRUE))
                  
                  
                  # Hace pruebas imagenes sin filtro PA
                  TEST[l, 1] <- PCE(img_corA)
                  TEST[l, 2] <- PCE(img_corB)
                  TEST[l, 3] <- PCE(img_corC)
                  TEST[l, 4] <- PCE(img_corD)
                  TEST[l, 5] <- PCE(img_corE)
                  TEST[l, 6] <- PCE(img_corF)
                  TEST[l, 7] <- PCE(img_corG)
                  TEST[l, 8] <- PCE(img_corH)
                  TEST[l, 9] <- PCE(img_corI)
                  TEST[l, 10] <- PCE(img_corJ)
                  TEST[l, 11] <- PCE(img_corK)
                  TEST[l, 12] <- PCE(img_corL)
                  TEST[l, 13] <- PCE(img_corM)
                  
                  TEST[l, 14] <- PSR(img_corA)
                  TEST[l, 15] <- PSR(img_corB)
                  TEST[l, 16] <- PSR(img_corC)
                  TEST[l, 17] <- PSR(img_corD)
                  TEST[l, 18] <- PSR(img_corE)
                  TEST[l, 19] <- PSR(img_corF)
                  TEST[l, 20] <- PSR(img_corG)
                  TEST[l, 21] <- PSR(img_corH)
                  TEST[l, 22] <- PSR(img_corI)
                  TEST[l, 23] <- PSR(img_corJ)
                  TEST[l, 24] <- PSR(img_corK)
                  TEST[l, 25] <- PSR(img_corL)
                  TEST[l, 26] <- PSR(img_corM)
                  
                  
                  TEST[l, 27] <- DC(img_corHA, img_corA, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 28] <- DC(img_corHB, img_corB, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 29] <- DC(img_corHC, img_corC, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 30] <- DC(img_corHD, img_corD, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 31] <- DC(img_corHE, img_corE, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 32] <- DC(img_corHF, img_corF, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 33] <- DC(img_corHG, img_corG, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 34] <- DC(img_corHH, img_corH, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 35] <- DC(img_corHI, img_corI, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 36] <- DC(img_corHJ, img_corJ, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 37] <- DC(img_corHK, img_corK, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 38] <- DC(img_corHL, img_corL, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[l, 39] <- DC(img_corHM, img_corM, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  
                  TEST[l, 40] <- SubjectP

                  l<-l+1
                  # setTxtProgressBar(pb, l)
            }
      }
      # close(pb)
      
      colnames(TEST) <- c("PCE_A", "PCE_B", "PCE_C", "PCE_D", "PCE_E", "PCE_F", "PCE_G", "PCE_H",
                          "PCE_I", "PCE_J", "PCE_K", "PCE_L", "PCE_M", "PSR_A", "PSR_B", "PSR_C",
                          "PSR_D", "PSR_E", "PSR_F", "PSR_G", "PSR_H", "PSR_I", "PSR_J", "PSR_K",
                          "PSR_L", "PSR_M", "DC_A", "DC_B", "DC_C", "DC_D", "DC_E", "DC_F",
                          "DC_G", "DC_H", "DC_I", "DC_J", "DC_K", "DC_L", "DC_M", "imagen"
                          )
      TEST$imagen <- factor(TEST$imagen)
      TEST
      
}