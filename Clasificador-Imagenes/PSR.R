PSR <- function(fttImg, Co = c(32,32), Vec = 3) {
      xi = Co[1] - Vec
      xf = Co[1] + Vec
      yi = Co[2] - Vec
      yf = Co[2] + Vec

      M = matrix(fttImg[xi:xf, yi:yf], ncol = 1)
      pos = floor(dim(M)[1]/2) + 1
      M = M[-pos,]
      mu = mean(Mod(M))
      sdu = sd(Mod(M))
      
      Mo = Mod(fttImg[Co[1], Co[2]])
      PSR = (Mo - mu)/sdu
      PSR
}