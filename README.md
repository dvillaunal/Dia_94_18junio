```{r, eval=FALSE, include=TRUE}
"Protocolo:
 
 1. Daniel Felipe Villa Rengifo
 
 2. Lenguaje: R
 
 3. Tema: MÉTODOS DE REMUESTREO Y VALIDACIÓN DE MODELOS: VALIDACIÓN CRUZADA Y BOOTSTRAP [Parte 2]
 
 4. Fuentes:  
    https://rpubs.com/rdelgado/405322
    https://tereom.github.io/est-computacional-2018/bootstrap-en-r.html"
```

# Leave One Out Cross-Validation (LOOCV)

Al igual que con la validación simple, LOOCV se puede emplear para estimar el test error y para determinar el grado óptimo de flexibilidad de un modelo

## Estimación del test error

Usando el ejemplo anterior, esta vez llevaremos a cabo el método LOOCV para estimar el test error. En este caso usamos `n–1` observaciones para entrenar el modelo en cada iteración (usando un `for`)

```{r}
# Guardamos los outputs:
sink("OUTPUTS.txt")

# cargamos el .RData:
"Con esto nos Ahoramos el Hecho de manejar muchas bases de datos, varibles y vectores, es decir este archivo es ahorrarse varias lineas de codigo"
#load(file = ".RData")

# Cargamos la libreria:
library(ISLR)
# Independedizamos las variables de la base de datos:
Weekly <- Weekly
attach(Weekly)

# Creamos una semilla:
set.seed(1)


# Vector que contendrá los aciertos (0) y errores (1) del modelo
loocv.error <- rep(0, nrow(Weekly))

# Las iteraciones serán igual al número de observaciones n
for (i in 1:nrow(Weekly)) {
  modelo.logistico <- glm(Direction ~ Lag2, data = Weekly[-i, ],  
                          family = "binomial")
  
  # si la probabilidad a posteriori es > 0.5 -> TRUE
  pred.up <- predict.glm(modelo.logistico, Weekly[i, ], type = "response") > 0.5
  
  # si el valor Direction de la i-ésima observación es “Up” -> TRUE
  true.up <- Weekly[i, ]$Direction == "Up"
  
  # si los TRUE/FALSE de ambos objetos (predicción y realidad) no coinciden -> se asigna error (1)
  if (pred.up != true.up){
    loocv.error[i] <- 1
  }
}

# Miramos la suma de los items del vector:
print("# Miramos la suma de los items del vector:")
sum(loocv.error)

# Resultado:
"Ha habido en total 480 errores de predicción de un total de 1089 observaciones con las que cuenta el set de datos. Si calculamos la media, obtenemos la estimación del test error:"


# test de error:
print("# test de error:")
mean(loocv.error)

# Resultado:
"LOOCV estima un test error del 44,07%, valor muy próximo al estimado por validación simple."
```


# Selección de flexibilidad del modelo

La función `cv.glm()` del paquete `boot` puede emplearse para llevar a cabo LOOCV (si el argumento __K__ no se especifica) de cualquier modelo lineal generalizado creado mediante la función `glm()`.


La función devuelve una lista con múltiples componentes. El vector delta es el que contiene el resultado de la validación cruzada (contiene dos valores, el primero es el estimador estándar y el segundo es la versión corregida por sesgo).


Para este ejemplo usaremos un set de datos simulados con dos variables __(p = 2)__ y 100 observaciones __(n = 100)__, según la siguiente ecuación: `Y = 2x-(2x^2) + e`


```{r}
# Creamos una semilla:
set.seed(1)

# Creamos dos vectores con distribución normal:
y <- rnorm(100)
x <- rnorm(100)

# Cambiamos y por:
y <- x - 2 * x^2 + rnorm(100)

# Creamos un dataframe con varibles x~y:
datos <- data.frame(x = x, y = y)

# Obsevamos los datos:
head(datos)


# Grafico de dispersión, de los datos anteriores:
png(filename = "DispXY.png")

plot(x, y)

dev.off()
```

Al representar los datos podemos observar como la relación entre x e y no es lineal.

Con lo cual, si quisiéramos ajustar un modelo, sería conveniente usar términos polinómicos de la variable `X`: En este caso, al llevar a cabo LOOCV no es necesario dividir los datos en grupos.


```{r}
# Cargamos la libreria:
library(boot)
library(ggplot2)

# En LOOCV el establecer una u otra semilla no cambiará el resultado ya que no hay reparto aleatorio de las observaciones
loocv.MSE <- rep(NA, 4)

for (i in 1:4) {
modelo <- glm(y ~ poly(x, i), data = datos)
# Almacenamos en el vector los valores estándar (delta[1])
loocv.MSE[i] <- cv.glm(data = datos, glmfit = modelo)$delta[1]
}

# Valores Estandar:
print("# Valores Estandar:")
print(loocv.MSE)

png(filename = "ValidacionCruzada.png")

ggplot(data = data.frame(polinomio = 1:4, loocv.MSE = loocv.MSE), 
       aes(x = polinomio, y = loocv.MSE))+
  geom_point(color = "orangered2")+
  geom_path()+
  labs(title = "MSE ~ Grado del polinomio")+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

# Resultado:
"El resultado de la validación cruzada indica que un polinomio de grado 2 mejora sustancialmente el ajuste del modelo (test error = 1,08%), no superándose esta mejora con grados mayores de polinomio."

"Esto es de esperar ya que la relación entre x e y (dada por la ecuación inicial) es cuadrática. Además, con el summary del modelo podemos obtener la significancia estadística de los coeficientes para cada grado de polinomio mediante el ajuste por mínimos cuadrados."

# significancia estadística de los coeficientes para cada grado de polinomio
print("# significancia estadística de los coeficientes para cada grado de polinomio")
print(summary(lm(y ~ poly(x, 4), data = datos)))

# Conclusión:
"Resulta significativo el ajuste hasta el grado de polinomio 2, con lo que las conclusiones coinciden."

```
