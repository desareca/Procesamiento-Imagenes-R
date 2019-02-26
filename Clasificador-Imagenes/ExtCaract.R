ExtCaract <- function(n=1000, seed = 30, dir1, Subject, Number){
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
                  SubjectP <- sample(Subject[-i], 1)
                  Ns <- sample(Number, 3, replace = FALSE)
                  # lee imagenes de filtro y prueba
                  img = Image(rotate(read.bitmap(paste0(dir1,Subject[i],Ns[1],".bmp"))/255/64/64,angle = 90))
                  imgp = Image(rotate(read.bitmap(paste0(dir1,SubjectP,Ns[2],".bmp"))/255/64/64,angle = 90))
                  img2 = Image(rotate(read.bitmap(paste0(dir1,Subject[i],Ns[3],".bmp"))/255/64/64,angle = 90))
                  
                  # calcula FFT a las imagenes
                  fft1 <- fftshift(fftw2d(img))
                  fft2 <- fftshift(fftw2d(imgp))
                  fft3 <- fftshift(fftw2d(img2))
                  
                  # Filtros POF
                  H <- fftshift(fftw2d(img))/Mod(fftshift(fftw2d(img)))
                  H[is.na(H)] <- 0
                  
                  # Hace las correlaciones
                  img_cor1 <- fftshift(fftw2d(fft1*Conj(H), inverse = TRUE))
                  img_cor2 <- fftshift(fftw2d(fft2*Conj(H), inverse = TRUE))
                  img_cor3 <- fftshift(fftw2d(fft3*Conj(H), inverse = TRUE))
                  
                  # Hace pruebas imagenes sin filtro PA
                  TEST[2*l-1,1] <- PCE(img_cor2)
                  TEST[2*l,1] <- PCE(img_cor3)
                  
                  TEST[2*l-1,2] <- PSR(Mod(img_cor2), Vec = 15)
                  TEST[2*l,2] <- PSR(Mod(img_cor3), Vec = 15)
                  
                  TEST[2*l-1,3] <- DC(img_cor1, img_cor2, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  TEST[2*l,3] <- DC(img_cor1, img_cor3, Co = c(32,32), Cc = c(32,32), Vec = 15)
                  
                  TEST[2*l-1,4] <- SubjectP
                  TEST[2*l,4] <- Subject[i]
                  
                  TEST[2*l-1,5] <- Subject[i]
                  TEST[2*l,5] <- Subject[i]
                  l<-l+1
                  # setTxtProgressBar(pb, l)
            }
      }
      # close(pb)
      
      colnames(TEST) <- c("PCE", "PSR", "DC", "imagen", "filtro")
      TEST$imagen <- factor(TEST$imagen)
      TEST$filtro <- factor(TEST$filtro)
      TEST$PCE <- TEST$PCE*30
      TEST$PSR <- TEST$PSR/20
      TEST
      
}