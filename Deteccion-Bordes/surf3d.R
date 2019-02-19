surf3d <- function(img, n=10, amp=256, palette=NULL, back = "lines", norm = TRUE){
      x <- n*(1:dim(img)[1])
      y <- n*(1:dim(img)[2])
      if(norm){
            img <- (img - min(img))/max(img)
      }
      z <- amp*img
      
      zlim <- range(z)
      zlen <- zlim[2] - zlim[1] + 1
      if(is.null(palette)){
            colorlut <- terrain.colors(zlen)
      }
      if(!is.null(palette)){
            colorlut <- palette(zlen)
      }
      zz <-  z - zlim[1] + 1
      zz[zz<=0] <- 1
      col <- colorlut[zz]
      
      open3d()
      surface3d(x, y, z, color = col, back = back)
      # surface3d(x, y, z, back = back)
      
}
