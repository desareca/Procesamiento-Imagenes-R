## Resumen

El siguiente trabajo presenta una implementación simple de detección de bordes utilizando R.

Para el tratamiento de imagenes se utilizó el paquete [EBImage](https://www.bioconductor.org/packages/devel/bioc/vignettes/EBImage/inst/doc/EBImage-introduction.html), que proporciona una funcionalidad de propósito general para el procesamiento y análisis de imágenes.

Se consideró realizar la deteccion de bordes en 3 pasos:

- Filtrar ruido (suavizar imagen).
- Generar bordes.
- Eliminar información no relevante (mediante umbral).

Para mayor detalle revisar: https://rpubs.com/desareca/Deteccion-Bordes-en-R

