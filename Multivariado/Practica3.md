



















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
\textbf{Aspectos de valor en una evaluación para un puesto de trabajo} \\
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

Determinar los aspectos de interés que se contemplan en una evaluación para un puesto de trabajo a partir de variables cuantificables utilizando análisis factorial.

Problemática
============

El ejercicio de establecer aspectos de personalidad de un individuo, los cuales no son directamente observables y medibles, a partir de características que sí se pueden contemplar es importante históricamente dentro del desarrollo de la teoría de análisis factorial pues esa fue la situación en la que Fisher planteo el modelo.

Evaluar a un candidato a un puesto de trabajo puede ser determinando su posible desmpeño en distintos aspectos de su persona. Algunos de los aspectos mas importantes en un proceso de selección suelen ser, según reclutadores, los siguientes:

-   Pasión. Entusiasmo y las expresiones al hablar sobre trabajo previo desarrollado en el área de trabajo pueden dar una buena impresión de la pasión o interés de la persona en el rubro en el que se desempeña. Suele ser el aspecto estrella ya que un candidato apasionado por su trabajo dará lo mejor de sí y buscará un crecimiento profesional beneficioso para la empresa y el individuo.

-   Actitud. La actitud se muestra a través de la forma en la que una persona se conduce. Es importante notar conductas nocivas para un equipo de trabajo, ya que a pesar de que un miembro pueder ser valioso por su conocimiento o desempeño, puede ser nocivo por su actitud.

Es importante notar que la experiencia no es un aspecto determinante para conseguir un empleo, al igual que su currículum y su solicitud, pero lo es para conseguir una entrevista y ganar unos puntos extra en la evaluación.

Otro aspecto es el nivel de salario que solicite el candidato, aunque un candidato idóneo para un puesto de trabajo pedirá mas o menos lo justo para el trabajo a desempeñar.

De manera análoga en esta práctica partimos de las observaciones registradas de 48 individuos y 14 variables que de alguna manera miden aspectos de las personas que se pueden reflejar ya sea en un exmanen escrito (como la habilidad numérica) o bien por factores físicos (por ejemplo la apariencia y confianza).

\newpage
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

Análisis descriptivo
====================

``` r
calif <- read.csv("test_trabajo.csv")
```

Análisis descriptivo de las variables
=====================================

Para el Análisis Factorial contamos con 14 variables, las cuales corresponden a la calificaci?n de algunas actitudes/aptitudes de 48 personas. La calificación que se puede obtener en cada uno de estos aspectos va de 0 a 10.

\(\textbf{Solicitud}\): Se refiere a la calificación que se le otorga a la presentación de la solicitud del candidato, la media de esta variable se encuentra en 6.

``` r
summary(calif$SOLICITUD)
```

    ##   ,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     2     4     2     4     9     2     9     5     2     9

\(\textbf{Apariencia}\)

``` r
summary(calif$APARIENC)
```

    ## 10,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     3     5     2     1     6    11    11     9

\(\textbf{Habilidad}\): Capacidad de una persona para hacer una cosa correctamente y con facilidad. Los datos recabados tienen media 7.083

``` r
summary(calif$HABILIDAD)
```

    ## 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     7     1     1     3     6     5    11    10     4

\(\textbf{Veracidad}\)

``` r
summary(calif$VERACIDAD)
```

    ##   ,00  1,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     3     1     5     1     4     5     3     5     7    10     4

\(\textbf{Confianza}\): Se busca medir la confianza que transmiten los candidatos, en la muestra que se tiene se obtuvo una media de 6.938

``` r
summary(calif$CONFIANZA)
```

    ##  1,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     1     5     2     3     2     6     3     5    11    10

\(\textbf{Lucidez}\): En la claridad y rapidez mental para exponer o comprender algo se obtuvo una media de 6.312

``` r
summary(calif$LUCIDEZ)
```

    ##   ,00  1,00 10,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     4     2     6     3     7     3     1     3     9    10

\(\textbf{Honestidad}\): La mayor?a de las personas obtuvo una calificación alta en este aspecto, por lo que la media de la variable es de 8.042

``` r
summary(calif$HONESTID)
```

    ##   ,00 10,00  2,00  3,00  5,00  6,00  7,00  8,00  9,00 
    ##     2    18     1     1     3     1     3    12     7

\(\textbf{Ventas}\): La mayoría de las personas obtuvo un puntaje bajo en las habilidades de venta, a pesar de que algunos obtuvieron 10 la media se mantuvo en 4.792

``` r
summary(calif$VENTAS)
```

    ##   ,00  1,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     7     1     7     7     7     3     5     2     1     4     4

\(\textbf{Experiencia}\): Se obtuvo una media baja debido a que muchos participantes tenían experiencia casi nula.

``` r
summary(calif$EXPERIEN)
```

    ##   ,00  1,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     4     7     6     8     8     3     3     2     1     4     2

\(\textbf{Manejo}\)

``` r
summary(calif$MANEJO)
```

    ##   ,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     2     4     9     5     5     7     3     1     6     6

\(\textbf{Ambición}\)

``` r
summary(calif$AMBICION)
```

    ##   ,00  1,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     2     1     7     3     6     3     7     4     5     4     6

\(\textbf{Solidez}\)

``` r
summary(calif$SOLIDEZ)
```

    ##   ,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     5     6     1     5     2     1     6     6    12     4

\(\textbf{Potencia}\)

``` r
summary(calif$POTENCIA)
```

    ##   ,00  1,00 10,00  2,00  3,00  4,00  5,00  6,00  7,00  8,00  9,00 
    ##     6     1     4     3     2     3     5     6     3     9     6

\(\textbf{Gana}\)

``` r
summary(calif$GANA)
```

    ##   ,00 10,00  2,00  3,00  4,00   ,50  5,00  6,00  7,00  8,00  9,00 
    ##     4     5     1     4     4     1    11     7     4     6     1
