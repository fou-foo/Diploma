



















\begin{titlepage}
\begin{center}
\vspace*{0in}

\begin{figure}[htb]
\centering
\includegraphics[ width = 8cm ]{images/fesa.png}
\end{figure}
\vspace*{0.25in}
\large{Universidad Nacional Autónoma de México}\\
\vspace*{0.5in}

Facultad de Estudios Superiores Acatlán\\
\vspace*{0.5in}

Técnicas estadísticas y minería de datos\\
\vspace*{0.5in}

Módulo VI. Análisis de varianza, factorial y de correspondencias y técnicas de estadística multivariada \\
\vspace*{0.5in}

\begin{large}
Práctica 3: \\
\end{large}

\vspace*{0.2in}
\begin{Large}
\textbf{Índices de desarrollo urbano} \\
\end{Large}

\vspace*{0.5in}
Camacho Ortíz Yuly \\
García Ramírez José Antonio \\
Méndez Ramírez Kenny Yahir \\
Vera Luna Maximiliano \\ 

\vspace*{1in}
Septiembre 2016 \\
\vspace*{0.05in}
\rule{80mm}{0.1mm}\\
\vspace*{0.1in}

\end{center}

\end{titlepage}

\newpage
Objetivo
========

Identificar las variables que integran distintos índices de desarrollo urbano mediante análisis factorial.

Problemática
============

A pesar de que existen diversas variables economicas, demograficas y geograficas para describir a los paises y a las ciudades en su aspecto social estas variables por lo general se enfocan a cuantificar un sólo aspecto, por ejemplo la población de una ciudad o su extensión territorial. Los indices son variables que miden un factor, en el sentido del analisis factorial, cuyos pesos de las variables pueden ser ajustados por la teoría respectiva.

Los indicadores tienen el objetivo de representar la realidad de forma cuantitativa, sencilla y directa, para así poder elaborar un análisis claro.

Existen indicadores simples e indicadores complejos. Por ejemplo, la tasa de analfabetismo y el acceso al agua potable son indicadores simples, ya que se refieren a atributos que se puede constatar su presencia o nivel calidad en forma simple y empírica. Diferente es el caso de indicadores como indicador social, que requieren un marco conceptual más complejo, al ser ambos un constructo teórico y no tener una equivalencia empírica concreta. En la composición de indicadores se deben tener conceptos claros y precisos, que no requieran un gran desarrollo matemático o estadístico.

Actualmente, se están llevando a cabo numerosos estudios de investigación sobre los sistemas de indicadores urbanos como apoyo a la toma de decisiones en la gestión de las ciudades.

En este ejercicio partimos de los valores registrados para 21 ciudades en 11 variables que miden caracteristicas en forma particular (las variables son de tres tipos: un grupo enfocado a valores territoriales, otro a extension poblacional y el ultimo a servicios) con el fin dar una descripción de las ciudades que cuantifique diversos aspectos.

Marco teórico
-------------

El Análisis factorial es un método multivariante que pretende expresar \(p\) variables observables como una combinación lineal de \(m\) variables hipotéticas o latentes, denominadas factores. Tiene una formulación parecida al análisis de componentes principales, pero el modelo que relaciona variables y factores es diferente en AF. Si la matriz de correlaciones existe, las componentes principales también existen, mientras que el modelo factorial podría ser aceptado o no mediante un test estadístico.

El AF obtiene e interpreta los factores comunes a partir de la matriz de correlaciones entre las variables.

Modelo multifactorial
---------------------

El modelo del análisis factorial de \(m\) factores comunes considera que las \(p\) variables observables \(X_{1},...,X_{p}\) depende de \(m\) variables latentes \(F_{1},\ldots,F_{m}\) llamadas factores comunes, y \(p\) factores únicos \(U_{1},...,U_{p}\), de acuerdo con el modelo lineal:

\[X_{1} = a_{11}F_{1} + ... + a_{1m}F_{m} + d_{1}U_{1}\] \[X_{2} = a_{21}F_{1} + ... + a_{2m}F_{m} + d_{2}U_{2}\] \[                      ...                           \] \[X_{p} = a_{p1}F_{1} + ... + a_{pm}F_{m} + d_{p}U_{p}\]

Las hipótesis del modelo son:

\begin{itemize}
\item Los factores comunes y los factores únicos están incorrelacionados dos a dos.
\item Los factores comunes están incorrelacionados con los factores únicos.
\item Tanto los factores comunes como los factores únicos son variables reducidas (media cero y varianza uno).
\end{itemize}
Matriz factorial
----------------

Los coeficientes \(a_{ij}\) son las saturaciones entre cada variable \(X_{i}\) y el factor \(F_{j}\). La matriz \(p \times m\) que contiene estos coeficientes es la matriz factorial.

Si indicamos por \(\textbf{X} = (X_{1},...,X_{p})'\) el vector columna de las variables, y análogamente \(\textbf{F} = (F_{1},...,F_{m})'\), \(\textbf{U} = (U_{1},...,U_{p})'\), el modelo factorial en expresión matricial es

\[\textbf{X = AF + DU},\]

donde \(\textbf{D} = diag(d_{1},...,d_{p})\) es la matriz diagonal con las saturaciones entre variables y factores únicos. El AF tiene com principal objetivo encontrar e interpretar la matriz factorial \(\textbf{A}\).

``` r
setwd('/home/fou/Desktop/proyectos-diplomado-2016/6Modulo/')
ciudades<-read.csv("ciudades.csv")
```

Análisis descriptivo de las variables
=====================================

Para el Análisis factorial contamos con 11 variables, las cuales corresponden a la caracteristicas de 21 ciudades. Las ciudades de las que se recaba la información son: Buenos Aires, Dhaka, Rio de Janeiro, Sao Paolo, Beijing, Shanghai, Tianjin, Cairo, Bombay, Calcutta, Delhi, Jakarta, Osaka, Tokyo, Seoul, Mexico City, Lagos, Karachi, Manila, Los Angeles y New York.

\(\textbf{Area}\): Esta variable va de 21.4 a 1680, y tiene media de 409.4

``` r
summary(ciudades$Area)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    21.4    64.0   216.0   409.4   630.0  1680.0

\(\textbf{Pop 1980}\): Se refiere a la población en millones de las ciudades en 1980. La media se encuentra en 9150.

``` r
summary(ciudades$Pop.1980)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    3290    5985    8789    9150    9990   21850

\(\textbf{Pop 1990}\): Se refiere a la población en millones de las ciudades en 1990. La media se encuentra en 11584.

``` r
summary(ciudades$Pop.1990)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    6578    8882   10870   11580   12220   25010

\(\textbf{Pop 2000}\): Se refiere a la población en millones de las ciudades en 2000. La media se encuentra en 14544.

``` r
summary(ciudades$Pop.2000)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   10600   12160   12950   14540   16190   27960

\(\textbf{Growth}\): El crecimiento de las ciudades analizadas va de .3 a 7.2 y tiene media 2.838

``` r
summary(ciudades$Growth)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.300   1.400   2.300   2.838   4.100   7.200

\(\textbf{Food}\)

``` r
summary(ciudades$Growth)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.300   1.400   2.300   2.838   4.100   7.200

\(\textbf{PersRoom}\): Es el número de personas que en promedio comparten habitación.

``` r
summary(ciudades$PersRoom)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.500   0.900   1.900   2.071   3.000   5.800

\(\textbf{Water}\): Es el porcentaje de personas con acceso a agua potable en cada ciudad.

``` r
summary(ciudades$Water)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   47.00   75.00   89.00   82.29   95.00  100.00

\(\textbf{Elec}\): Es el porcentaje de personas con acceso a luz eléctrica en cada ciudad.

``` r
summary(ciudades$Elec)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   53.00   84.00   94.00   89.62   98.00  100.00

\(\textbf{Phones}\)

``` r
summary(ciudades$Phones)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.00    3.00    5.00   13.62   16.00   56.00

\(\textbf{Vehicles}\)

``` r
summary(ciudades$Vehicles)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     148     510    1000    1939    2660    8000

Análisis estadístico
====================

Después de múltiples intentos se decidió prescindir de la variable que registra la población de las ciudades. Uno de los modelo adecuados de análisis factorial para el conjunto de datos con el que estamos trabajando es el siguiente, utilizamos la transformación rígida varimax sobre los factores:

``` r
library(psych)
R <- cor(ciudades[,3:12])

fa(R, nfactors = 3, rotate = 'varimax')
```

    ## Factor Analysis using method =  minres
    ## Call: fa(r = R, nfactors = 3, rotate = "varimax")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##            MR3   MR1   MR2   h2     u2 com
    ## Pop.1980  0.33  0.79 -0.50 0.99 0.0089 2.1
    ## Pop.1990  0.30  0.92 -0.26 0.99 0.0055 1.4
    ## Pop.2000  0.11  0.99  0.05 0.98 0.0159 1.0
    ## Growth   -0.34 -0.18  0.87 0.91 0.0885 1.4
    ## Food     -0.76 -0.08  0.40 0.74 0.2605 1.5
    ## PersRoom -0.72 -0.14  0.43 0.73 0.2720 1.7
    ## Water     0.73  0.37 -0.18 0.70 0.2975 1.6
    ## Elec      0.88  0.15 -0.13 0.82 0.1803 1.1
    ## Phones    0.64  0.32 -0.39 0.66 0.3355 2.2
    ## Vehicles  0.69  0.19 -0.11 0.52 0.4814 1.2
    ## 
    ##                        MR3  MR1  MR2
    ## SS loadings           3.62 2.80 1.64
    ## Proportion Var        0.36 0.28 0.16
    ## Cumulative Var        0.36 0.64 0.81
    ## Proportion Explained  0.45 0.35 0.20
    ## Cumulative Proportion 0.45 0.80 1.00
    ## 
    ## Mean item complexity =  1.5
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  15.74
    ## The degrees of freedom for the model are 18  and the objective function was  3.26 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.08 
    ## The df corrected root mean square of the residuals is  0.12 
    ## 
    ## Fit based upon off diagonal values = 0.98
    ## Measures of factor score adequacy             
    ##                                                 MR3  MR1  MR2
    ## Correlation of scores with factors             0.97 1.00 0.97
    ## Multiple R square of scores with factors       0.95 0.99 0.95
    ## Minimum correlation of possible factor scores  0.89 0.98 0.89

De las estimaciones de la matriz factorial, al factor MR3 lo interpretamos como el factor de confort de la ciudad pues refleja de manera positiva las variables de los servicios como agua, luz , teléfono y vehículos mientras que se relaciona negativamente con las variables que pueden ocasionar escasez de recursos como el crecimiento y el número de personas por habitación. El factor MR1 mide el factor de la población considerando el registro de los tres años y además los pesos de ellos en este factor son menores conforme más se aleja en el tiempo el registro. Y el factor MR2 mide directamente el crecimiento de la población.

Por otro lado la unicidad de las variables con respecto a los factores es baja excepto para las variables que reflejan el servicio de teléfono y vehículos por lo que consideramos adecuado el modelo de factores (a pesar del tercer factor que cuyos pesos son poco heterogéneos )

Conclusiones
============

En contraposición de los ejemplos en la literatura en donde se suelen definir factores políticos (derecha-izquierda) en este ejercicio se planteó un factor que considera la población y principalmente los servicios con los que cuenta una ciudad para poder definir un índice de desarrollo que integre diversos agentes de la vida urbana.

Bibliografía
============

T. Hastie, R. Tibshirani and J. Friedman. Elements of Statistical Learning. Springer, second Edition 2012.
