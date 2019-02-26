F_random <- function(seed = 50){
      
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

      # trC: metodo para control de modelado.
      set.seed(seed)
      Nf <- sample(Number, 13, replace = TRUE)
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
      
      saveRDS(fftA, "fftA.rds")
      saveRDS(fftB, "fftB.rds")
      saveRDS(fftC, "fftC.rds")
      saveRDS(fftD, "fftD.rds")
      saveRDS(fftE, "fftE.rds")
      saveRDS(fftF, "fftF.rds")
      saveRDS(fftG, "fftG.rds")
      saveRDS(fftH, "fftH.rds")
      saveRDS(fftI, "fftI.rds")
      saveRDS(fftJ, "fftJ.rds")
      saveRDS(fftK, "fftK.rds")
      saveRDS(fftL, "fftL.rds")
      saveRDS(fftM, "fftM.rds")
      
      
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
      
      saveRDS(HA, "HA.rds")
      saveRDS(HB, "HB.rds")
      saveRDS(HC, "HC.rds")
      saveRDS(HD, "HD.rds")
      saveRDS(HE, "HE.rds")
      saveRDS(HF, "HF.rds")
      saveRDS(HG, "HG.rds")
      saveRDS(HH, "HH.rds")
      saveRDS(HI, "HI.rds")
      saveRDS(HJ, "HJ.rds")
      saveRDS(HK, "HK.rds")
      saveRDS(HL, "HL.rds")
      saveRDS(HM, "HM.rds")
      
      # Hace las correlaciones

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
      
      saveRDS(img_corHA, "img_corHA.rds")
      saveRDS(img_corHB, "img_corHB.rds")
      saveRDS(img_corHC, "img_corHC.rds")
      saveRDS(img_corHD, "img_corHD.rds")
      saveRDS(img_corHE, "img_corHE.rds")
      saveRDS(img_corHF, "img_corHF.rds")
      saveRDS(img_corHG, "img_corHG.rds")
      saveRDS(img_corHH, "img_corHH.rds")
      saveRDS(img_corHI, "img_corHI.rds")
      saveRDS(img_corHJ, "img_corHJ.rds")
      saveRDS(img_corHK, "img_corHK.rds")
      saveRDS(img_corHL, "img_corHL.rds")
      saveRDS(img_corHM, "img_corHM.rds")

      Nf <- data.frame(t(Nf), stringsAsFactors = FALSE)
      colnames(Nf) <- Subject[1:13]
      Nf
}