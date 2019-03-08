# Resumen

MNIST (“Instituto Nacional Modificado de Estándares y Tecnología”) es el conjunto de datos de facto de “visión mundial” de la visión de computadora. Desde su lanzamiento en 1999, este clásico conjunto de datos de imágenes manuscritas ha servido como base para los algoritmos de clasificación de referencia. A medida que surgen nuevas técnicas de aprendizaje automático, MNIST sigue siendo un recurso confiable para investigadores y estudiantes por igual.

El conjunto de datos mixto de Instituto Nacional de estándares y tecnología (MNIST) es una colección de 70.000 pequeñas imágenes de dígitos escritos a mano. Los datos fue creados para actuar como un referente para los algoritmos de reconocimiento de imagen. Aunque MNIST las imágenes son pequeñas (28 x 28 pixeles) y sólo hay 10 dígitos posibles (cero a nueve) a reconocer y hay 42.0000 imágenes de formación para la creación de un modelo de reconocimiento de imagen (con 28.000 imágenes tendidas a probar la exactitud de un modelo), la experiencia ha demostrado que reconocer las imágenes MNIST es un problema difícil.

Para lidiar con este problema vamos a extraer características de cada imagen y disminuir el tamaño de cada muestra. Con esto probaremos el desempeño de clasificadores. Consideraremos 3 estrategias:

1- [Sumas y diferencias de filas y columnas](http://rpubs.com/desareca/Clasificacion-Imagenes-Extraccion-Caracteristicas)

2- [Mínima distancia](http://rpubs.com/desareca/Min-dist)

3- [Correlación](http://rpubs.com/desareca/Class-corr-coef)
