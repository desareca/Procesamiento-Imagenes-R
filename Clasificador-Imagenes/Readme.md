# Resumen

Continuando con el trabajo de [Reconocimiento de Imagenes por Correlacion](https://rpubs.com/desareca/Reconocimiento-Imagenes-Correlacion), desarrollaremos un clasificador para los 13 sujetos.

EL objetivo es construir un clasificador a partir de sólo una imagen de cada sujeto y para esto seguiremos la siguiente estrategia:

- Extraer carácterísticas de PCE, PSR y DC de 13 filtros (uno por sujeto).
- Construir 13 modelos de regresión para cada filtro.
- Construir un clasificador utilizando los 13 modelos de regresión.
- Aplicar imagenes con ruido para probar su desempeño.

Para ver más detalle sobre el trabajo revisar:

http://rpubs.com/desareca/Clasificador-Imagenes
