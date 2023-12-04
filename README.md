# PS_3_ML
El repositorio contiene las siguientes carpetas:

- `document`: contiene el archivo final en `.pdf` subido a Bloque Neón, pero adicionalmente los `.tex`. 
- `references`: contiene el enunciado y algunos documentos adicionales consultados por el equipo.
- `scripts`: contiene todos los scripts.
- `stores`: contiene la base de datos extraída de la pagina de kaggle de la competencia. 
- `views`: contiene las figuras y tablas exportadas al documento, contiene además las predicciones a evaluar por fuera de muestra.

Para ejecutar el código, basta con abrir el archivo `predicting_poverty.Rproj` ubicado en la raíz del repositorio. Una vez abierto, la librería `here` permite emplear direcciones relativas independiente de quién esté ejecutando el código. Así, el siguiente paso es abrir el archivo denominado `main.R` en la carpeta `scripts`, y este archivo 'invoca' a _scripts_ auxiliares que generarán los resultados presentados en el documento `.pdf`. 
