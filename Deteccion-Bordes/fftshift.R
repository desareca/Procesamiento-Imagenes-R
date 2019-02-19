fftshift <- function(img){
      d1 <- dim(img)[1]
      d2 <- dim(img)[2]
      
      d1 <- c(1,floor(d1/2),floor(d1/2)+1,d1)
      d2 <- c(1,floor(d2/2),floor(d2/2)+1,d2)
      
      xx1 <- img[d1[1]:d1[2], d2[1]:d2[2]]
      xx2 <- img[d1[1]:d1[2], d2[3]:d2[4]]
      xx3 <- img[d1[3]:d1[4], d2[1]:d2[2]]
      xx4 <- img[d1[3]:d1[4], d2[3]:d2[4]]
      
      xx1 <- EBImage::flip(xx1)
      xx1 <- EBImage::flop(xx1)
      xx2 <- EBImage::flip(xx2)
      xx2 <- EBImage::flop(xx2)
      xx3 <- EBImage::flip(xx3)
      xx3 <- EBImage::flop(xx3)
      xx4 <- EBImage::flip(xx4)
      xx4 <- EBImage::flop(xx4)
      
      img[d1[1]:d1[2], d2[1]:d2[2]] <- xx1
      img[d1[1]:d1[2], d2[3]:d2[4]] <- xx2
      img[d1[3]:d1[4], d2[1]:d2[2]] <- xx3
      img[d1[3]:d1[4], d2[3]:d2[4]] <- xx4
      
      img
}