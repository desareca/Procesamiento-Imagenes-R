filtro <- function(img, w=NULL, size = 15, sigma = 10, inverse = TRUE){
      # w es el filtro, por defecto es un pasa bajo gaussiano
      # Se aplica en el dominio de distancia
      if(is.null(w)){
            w = EBImage::makeBrush(size = size,
                                   shape = 'Gaussian',
                                   sigma = sigma)
      }
      
      # Inverse, invierte el filtro
      if(inverse){
            w <- (mean(w)-w)/sd(w)
      }
      
      EBImage::filter2(img, w)
}