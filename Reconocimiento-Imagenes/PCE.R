PCE <- function(fttImg, Co =c(32,32)) {
      Mo = Mod(fttImg[Co[1], Co[2]])^2
      M = sum(Mod(fttImg)^2)
      PCE = Mo/M
      PCE
}