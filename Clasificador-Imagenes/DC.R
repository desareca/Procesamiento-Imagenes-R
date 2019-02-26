DC <- function(imgCO, imgCC, Co = c(32,32), Cc = c(32,32), Vec = 15) {
      imgCO = imgCO[Co[1] - Vec:Co[1] + Vec,
                    Co[2] - Vec:Co[2] + Vec]
      imgCC = imgCC[Cc[1] - Vec:Cc[1] + Vec,
                    Cc[2] - Vec:Cc[2] + Vec]
      
      DC = 1 - (max(Mod(imgCC))/max(Mod(imgCO)))
      DC
}