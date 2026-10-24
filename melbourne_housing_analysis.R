# =====================================================================
# Melbourne Housing Market Analysis & Price Prediction Pipeline
# Coautores del Proyecto: Andrés Peña - Maximiliano Gallardo - Patricio Frez - Alejandro Martínez
# Language: R
# Dataset: Melbourne Housing Snapshot (melb_data.csv)
# Description: Data cleaning, MICE imputation, PCA, and multiple linear
#              regression modeling for housing price estimation.
# =====================================================================

library(tidyverse)             # Manipulación y gráficos
library(dlookr)                # Diagnóstico del dataset
library(qqplotr)               # Q-Q plot
library(moments)               # Asimetría y curtosis
library(mice)                  # Imputación MICE / PMM
library(corrplot)              # Matriz de correlación
library(PerformanceAnalytics)  # Panel de correlaciones
library(factoextra)            # PCA
library(broom)                 # Resultados ordenados de modelos
library(car)                   # VIF
library(fitdistrplus)          # Ajuste de distribuciones
library(Amelia)                # Mapa de valores faltantes
library(knitr)                 # Tablas
library(nortest)               # Lilliefors

select <- dplyr::select

options(digits = 4)

alpha <- 0.05


# Se recomienda trabajar con melb_data.csv en la misma carpeta del script.
archivo <- if (file.exists("data/melb_data.csv")) {
  "data/melb_data.csv"
} else if (file.exists("melb_data.csv")) {
  "melb_data.csv"
} else {
  stop("No se encontró 'melb_data.csv' en el directorio ni en la carpeta 'data/'.")
}

melb <- read.csv(
  archivo,
  stringsAsFactors = FALSE
)

# Copia intacta de los datos originales.
melb_original <- melb


# 2. DIAGNÓSTICO INICIAL

dim(melb)
head(melb)
str(melb)
summary(melb)

print(
  dlookr::diagnose(melb),
  n = 21
)


# Niveles de medición utilizados en el informe

# Razón:
# Price, Rooms, Distance, Bedroom2, Bathroom, Car,
# Landsize, BuildingArea, Propertycount
#
# Intervalo:
# YearBuilt, Lattitude, Longtitude
#
# Nominal:
# Suburb, Address, Type, Method, SellerG,
# Postcode, CouncilArea, Regionname
#
# Date:
# variable temporal


# 3. ANÁLISIS DESCRIPTIVO


# 3.1 Variable categórica: Type

tabla_tipo <- melb %>%
  count(Type) %>%
  mutate(
    Porcentaje = n / sum(n) * 100
  )

cat("\nFrecuencia por tipo de propiedad:\n")
print(tabla_tipo)


grafico_tipo <- ggplot(
  tabla_tipo,
  aes(
    x = Type,
    y = Porcentaje
  )
) +
  geom_col(
    fill = "steelblue",
    color = "black"
  ) +
  labs(
    title = "Distribución de propiedades por tipo",
    x = "Tipo de propiedad",
    y = "Porcentaje (%)"
  ) +
  theme_grey(base_size = 14)

print(grafico_tipo)


# 3.2 Variables numéricas

variables_numericas <- c(
  "Price",
  "Rooms",
  "Distance",
  "Bedroom2",
  "Bathroom",
  "Car",
  "Landsize",
  "BuildingArea",
  "YearBuilt",
  "Propertycount"
)


# Función para coeficiente de variación
coef_variacion <- function(x) {
  
  x <- x[is.finite(x)]
  
  if (length(x) < 2 || mean(x) == 0) {
    return(NA_real_)
  }
  
  sd(x) / mean(x) * 100
}


# Tabla descriptiva
resumen_descriptivo <- melb %>%
  select(all_of(variables_numericas)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  group_by(Variable) %>%
  summarise(
    
    n = sum(!is.na(Valor)),
    
    media = mean(
      Valor,
      na.rm = TRUE
    ),
    
    mediana = median(
      Valor,
      na.rm = TRUE
    ),
    
    desviacion = sd(
      Valor,
      na.rm = TRUE
    ),
    
    varianza = var(
      Valor,
      na.rm = TRUE
    ),
    
    Q1 = quantile(
      Valor,
      0.25,
      na.rm = TRUE
    ),
    
    Q3 = quantile(
      Valor,
      0.75,
      na.rm = TRUE
    ),
    
    IQR = IQR(
      Valor,
      na.rm = TRUE
    ),
    
    asimetria = moments::skewness(
      Valor,
      na.rm = TRUE
    ),
    
    curtosis = moments::kurtosis(
      Valor,
      na.rm = TRUE
    ),
    
    exceso_curtosis =
      moments::kurtosis(
        Valor,
        na.rm = TRUE
      ) - 3,
    
    CV_porcentaje =
      coef_variacion(Valor),
    
    .groups = "drop"
  )


cat("\nResumen descriptivo de variables numéricas:\n")
print(resumen_descriptivo)



# 4. DISTRIBUCIÓN Y NORMALIDAD DE PRICE


# 4.1 Histograma + normal de referencia

grafico_hist <- ggplot(
  melb,
  aes(x = Price)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    color = "black",
    fill = "lightgreen",
    alpha = 0.7
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(
        melb$Price,
        na.rm = TRUE
      ),
      sd = sd(
        melb$Price,
        na.rm = TRUE
      )
    ),
    linewidth = 1
  ) +
  geom_vline(
    xintercept = mean(
      melb$Price,
      na.rm = TRUE
    ),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  labs(
    title = "Distribución del precio de venta",
    subtitle = "Histograma y curva normal de referencia",
    x = "Precio (AUD)",
    y = "Densidad"
  ) +
  theme_grey(base_size = 14)

print(grafico_hist)


# ---------------------------------------------------------------------
# 4.2 Gráfico Q-Q
# ---------------------------------------------------------------------

grafico_qq <- ggplot(
  melb,
  aes(sample = Price)
) +
  qqplotr::stat_qq_band() +
  stat_qq_line() +
  stat_qq_point(
    size = 1
  ) +
  labs(
    title = "Gráfico Q-Q del precio",
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  ) +
  theme_grey(base_size = 14)

print(grafico_qq)



# 4.3 Prueba de normalidad Lilliefors

# Se utiliza la corrección de Lilliefors porque los parámetros
# de la normal se estiman a partir de la misma muestra.

prueba_normalidad <- nortest::lillie.test(
  melb$Price
)

cat("\nPrueba de normalidad Lilliefors para Price:\n")
print(prueba_normalidad)

decision_normalidad <- ifelse(
  prueba_normalidad$p.value < alpha,
  "Se rechaza H0: evidencia de no normalidad",
  "No se rechaza H0: normalidad plausible"
)

print(decision_normalidad)


# 5. VALORES ATÍPICOS




# 5.1 Boxplot de Price

boxplot_atip <- ggplot(
  melb,
  aes(y = Price)
) +
  geom_boxplot(
    fill = "lightgreen",
    outlier.color = "red",
    width = 0.55,
    outlier.size = 2
  ) +
  labs(
    title = "Detección de valores atípicos en Price",
    y = "Price (AUD)",
    x = NULL
  ) +
  theme_grey(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

print(boxplot_atip)



# 5.2 Boxplot comparativo Price vs Type

boxplot_comp <- ggplot(
  melb,
  aes(
    x = Type,
    y = Price
  )
) +
  geom_boxplot(
    fill = "lightyellow",
    outlier.color = "darkred",
    width = 0.6,
    outlier.size = 1.5
  ) +
  labs(
    title = "Precio de venta según tipo de propiedad",
    x = "Tipo de propiedad",
    y = "Price (AUD)"
  ) +
  theme_grey(base_size = 14)

print(boxplot_comp)


# 5.3 Límites IQR de Price

Q1_price <- quantile(
  melb$Price,
  0.25,
  na.rm = TRUE
)

Q3_price <- quantile(
  melb$Price,
  0.75,
  na.rm = TRUE
)

IQR_price <- IQR(
  melb$Price,
  na.rm = TRUE
)

LI_price <- Q1_price - 1.5 * IQR_price
LS_price <- Q3_price + 1.5 * IQR_price

cantidad_atipicos_price <- sum(
  melb$Price < LI_price |
    melb$Price > LS_price,
  na.rm = TRUE
)

tabla_iqr_price <- data.frame(
  Q1 = Q1_price,
  Q3 = Q3_price,
  IQR = IQR_price,
  Limite_inferior = LI_price,
  Limite_superior = LS_price,
  Cantidad_candidatos_atipicos =
    cantidad_atipicos_price
)

cat("\nDiagnóstico IQR de Price:\n")
print(tabla_iqr_price)



# 6. CORRELACIONES

# 6.1 Predictoras evaluadas contra Price


variables_correlacion <- c(
  "Rooms",
  "Distance",
  "Bedroom2",
  "Bathroom",
  "Car",
  "Landsize",
  "BuildingArea",
  "YearBuilt",
  "Propertycount"
)


# 6.2 Tabla Pearson, Spearman y Kendall
#     Cada par utiliza sus propias observaciones completas.

tabla_correlaciones <- map_dfr(
  variables_correlacion,
  function(variable) {
    
    datos_par <- melb %>%
      select(
        Price,
        all_of(variable)
      ) %>%
      drop_na()
    
    x <- datos_par[[variable]]
    y_price <- datos_par$Price
    
    prueba_spearman <- suppressWarnings(
      cor.test(
        y_price,
        x,
        method = "spearman",
        exact = FALSE
      )
    )
    
    tibble(
      Predictora = variable,
      n = nrow(datos_par),
      
      Pearson = cor(
        y_price,
        x,
        method = "pearson"
      ),
      
      Spearman = cor(
        y_price,
        x,
        method = "spearman"
      ),
      
      Kendall = cor(
        y_price,
        x,
        method = "kendall"
      ),
      
      p_Spearman =
        prueba_spearman$p.value
    )
  }
)


tabla_correlaciones_mostrar <-
  tabla_correlaciones %>%
  mutate(
    Pearson = round(
      Pearson,
      3
    ),
    Spearman = round(
      Spearman,
      3
    ),
    Kendall = round(
      Kendall,
      3
    )
  )


cat(
  "\nAsociaciones bivariadas de las predictoras con Price:\n"
)

print(
  tabla_correlaciones_mostrar,
  n = Inf
)


# 6.3 Matriz visual Spearman


datos_corr <- melb %>%
  select(
    Price,
    all_of(variables_correlacion)
  )

matriz_spearman <- cor(
  datos_corr,
  use = "pairwise.complete.obs",
  method = "spearman"
)

corrplot::corrplot(
  matriz_spearman,
  method = "ellipse",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.cex = 0.8,
  tl.srt = 45,
  number.cex = 0.6,
  mar = c(0, 0, 1, 0)
)



# 6.4 Panel complementario

datos_panel <- melb %>%
  select(
    Price,
    Rooms,
    Distance,
    BuildingArea,
    Landsize
  ) %>%
  drop_na()

PerformanceAnalytics::chart.Correlation(
  datos_panel,
  histogram = TRUE,
  pch = 19,
  method = "spearman"
)



# 6.5 Contraste principal de la hipótesis:
# Price vs BuildingArea


datos_hipotesis <- melb %>%
  select(
    Price,
    BuildingArea
  ) %>%
  drop_na()

prueba_hipotesis <- cor.test(
  datos_hipotesis$Price,
  datos_hipotesis$BuildingArea,
  method = "spearman",
  exact = FALSE
)

cat(
  "\nContraste principal: Price vs BuildingArea (Spearman):\n"
)

print(prueba_hipotesis)



# 6.6 Redundancia Rooms vs Bedroom2

rho_rooms_bedroom2 <- cor(
  melb$Rooms,
  melb$Bedroom2,
  use = "complete.obs",
  method = "spearman"
)

cat(
  "\nCorrelación Spearman Rooms vs Bedroom2:\n"
)

print(rho_rooms_bedroom2)



# 7. MODELO PROBABILÍSTICO

# Price en cientos de miles de AUD.
y <- melb$Price / 100000

stopifnot(
  all(
    y > 0,
    na.rm = TRUE
  )
)


# 7.1 Ajuste Normal

normal <- fitdist(
  y,
  "norm"
)



# 7.2 Ajuste Lognormal

lognormal <- fitdist(
  y,
  "lnorm"
)


cat("\nResumen distribución Normal:\n")
print(summary(normal))

cat("\nResumen distribución Lognormal:\n")
print(summary(lognormal))


# 7.3 Parámetros estimados

parametros_distribuciones <- data.frame(
  
  Modelo = c(
    "Normal",
    "Lognormal"
  ),
  
  Parametro_1 = c(
    normal$estimate["mean"],
    lognormal$estimate["meanlog"]
  ),
  
  Parametro_2 = c(
    normal$estimate["sd"],
    lognormal$estimate["sdlog"]
  )
)

cat(
  "\nParámetros estimados de las distribuciones:\n"
)

print(parametros_distribuciones)



# 7.4 Bondad de ajuste

bondad_ajuste <- gofstat(
  list(
    normal,
    lognormal
  ),
  fitnames = c(
    "Normal",
    "Lognormal"
  )
)

print(bondad_ajuste)


# 7.5 Gráfico de distribuciones ajustadas

denscomp(
  list(
    normal,
    lognormal
  ),
  plotstyle = "ggplot",
  legendtext = c(
    "Normal",
    "Lognormal"
  ),
  xlegend = "topright",
  main = "Ajuste probabilístico: distribución del precio",
  xlab = "Precio (cientos de miles de AUD)",
  ylab = "Densidad"
)


# Mediana teórica de la lognormal
mediana_lognormal <-
  exp(
    lognormal$estimate["meanlog"]
  )

cat(
  "\nMediana estimada bajo Lognormal:\n"
)

print(mediana_lognormal)

cat(
  "\nMediana observada de y:\n"
)

print(
  median(y)
)


# 8. DATOS FALTANTES Y RUIDOSOS

# 8.1 Diagnóstico de NA en los DATOS ORIGINALES


tabla_na <- data.frame(
  
  Variable =
    names(melb_original),
  
  Cantidad_NA =
    colSums(
      is.na(melb_original)
    ),
  
  Porcentaje_NA =
    colMeans(
      is.na(melb_original)
    ) * 100
) %>%
  arrange(
    desc(Porcentaje_NA)
  )


cat(
  "\nDatos faltantes por variable:\n"
)

print(
  tabla_na,
  row.names = FALSE
)



# 8.2 Gráfico de porcentaje de NA

grafico_na <- ggplot(
  tabla_na,
  aes(
    x = reorder(
      Variable,
      Porcentaje_NA
    ),
    y = Porcentaje_NA
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title =
      "Proporción de datos faltantes por variable",
    x = "Variable",
    y = "Datos faltantes (%)"
  ) +
  theme_grey(
    base_size = 14
  )

print(grafico_na)


# 8.3 Mapa de faltantes original


Amelia::missmap(
  melb_original,
  main =
    "Mapa de valores faltantes antes del tratamiento",
  col = c(
    "lightpink",
    "lightblue"
  ),
  legend = TRUE
)


# 8.4 Inconsistencias temporales:

melb_original$Date_parsed <-
  as.Date(
    melb_original$Date,
    format = "%d/%m/%Y"
  )

melb_original$YearSold <-
  as.numeric(
    format(
      melb_original$Date_parsed,
      "%Y"
    )
  )


errores_temporales <-
  melb_original %>%
  filter(
    !is.na(YearBuilt),
    !is.na(YearSold),
    YearBuilt > YearSold
  )


cat(
  "\nRegistros originales con YearBuilt mayor al año de venta:\n"
)

print(
  nrow(
    errores_temporales
  )
)



# 9. PARTICIÓN ENTRENAMIENTO / PRUEBA

set.seed(105)

id <- sample(
  seq_len(
    nrow(melb_original)
  ),
  size =
    floor(
      0.8 *
        nrow(melb_original)
    )
)


# TRUE = fila de prueba
filas_prueba <-
  !seq_len(
    nrow(melb_original)
  ) %in% id


cat(
  "\nTamaño de las particiones:\n"
)

print(
  c(
    Entrenamiento = length(id),
    Prueba =
      sum(filas_prueba)
  )
)



# 10. IMPUTACIÓN MICE / PMM

# Copia de trabajo.
melb_modelo <- melb_original


# Las ocho predictoras utilizadas posteriormente.
variables_modelo <- c(
  "Rooms",
  "Distance",
  "Bathroom",
  "Car",
  "Landsize",
  "BuildingArea",
  "YearBuilt",
  "Propertycount"
)


datos_mice <-
  melb_modelo[
    ,
    variables_modelo
  ]


# Solo Car, BuildingArea y YearBuilt necesitan imputación.
# El resto de las variables puede actuar como predictor dentro de MICE.
metodos_mice <-
  mice::make.method(
    datos_mice
  )

metodos_mice[] <- ""

metodos_mice[
  c(
    "Car",
    "BuildingArea",
    "YearBuilt"
  )
] <- "pmm"




# Las filas de prueba reciben imputación, pero NO participan
# en la estimación de los modelos internos de imputación.

imp <- mice(
  datos_mice,
  m = 1,
  maxit = 10,
  method = metodos_mice,
  ignore = filas_prueba,
  seed = 123,
  printFlag = FALSE
)


datos_completos <-
  complete(
    imp
  )


# Reemplazar las predictoras en la copia de trabajo.
melb_modelo[
  ,
  variables_modelo
] <- datos_completos


cat(
  "\nNA posteriores a la imputación en las predictoras:\n"
)

print(
  colSums(
    is.na(
      melb_modelo[
        ,
        variables_modelo
      ]
    )
  )
)



# ninguna predictora del modelo debe quedar con NA.
stopifnot(
  !anyNA(
    melb_modelo[
      ,
      variables_modelo
    ]
  )
)



# CouncilArea puede continuar mostrando NA porque no fue imputada.
Amelia::missmap(
  melb_modelo,
  main =
    "Mapa de valores faltantes después de imputar las predictoras",
  col = c(
    "lightpink",
    "lightblue"
  ),
  legend = TRUE
)



# 11. PCA

X <-
  melb_modelo[
    ,
    variables_modelo
  ]



stopifnot(
  nrow(X) ==
    nrow(melb_modelo)
)


# PCA ajustado SOLO con entrenamiento.
pca <- prcomp(
  X[id, ],
  center = TRUE,
  scale. = TRUE
)


autovalores <-
  factoextra::get_eigenvalue(
    pca
  )



k <- which(
  autovalores$cumulative.variance.percent >= 80
)[1]


cat(
  "\nAutovalores y varianza explicada:\n"
)

print(
  autovalores
)


cat(
  "\nComponentes necesarios para alcanzar 80%:\n"
)

print(k)


cat(
  "\nCargas de los componentes retenidos:\n"
)

print(
  round(
    pca$rotation[
      ,
      1:k,
      drop = FALSE
    ],
    3
  )
)



# 11.1 Scree plot

g_pca_varianza <-
  fviz_eig(
    pca,
    addlabels = TRUE
  ) +
  labs(
    title =
      "Varianza explicada por componente",
    x = "Componente",
    y = "Varianza explicada (%)"
  )

print(
  g_pca_varianza
)



# 11.2 Variables en PC1 y PC2

g_pca_variables <-
  fviz_pca_var(
    pca,
    repel = TRUE
  ) +
  labs(
    title =
      "Variables en los dos primeros componentes"
  )

print(
  g_pca_variables
)


# 11.3 Varianza acumulada

tabla_varianza_pca <- data.frame(
  
  Componente =
    seq_len(
      nrow(
        autovalores
      )
    ),
  
  Autovalor =
    autovalores$eigenvalue,
  
  Varianza =
    autovalores$variance.percent,
  
  Acumulada =
    autovalores$cumulative.variance.percent
)


g_pca_acumulada <-
  ggplot(
    tabla_varianza_pca,
    aes(
      x = Componente,
      y = Acumulada
    )
  ) +
  geom_line() +
  geom_point() +
  geom_hline(
    yintercept = 80,
    linetype = "dashed"
  ) +
  labs(
    title =
      "Varianza acumulada del PCA",
    x = "Componente",
    y = "Varianza acumulada (%)"
  ) +
  theme_grey(
    base_size = 14
  )

print(
  g_pca_acumulada
)



# 11.4 Tabla PCA

knitr::kable(
  tabla_varianza_pca[
    1:k,
  ],
  digits = 3,
  caption =
    "Autovalores y varianza explicada por los componentes retenidos"
)


tabla_rotacion <-
  as.data.frame(
    pca$rotation[
      ,
      1:k,
      drop = FALSE
    ]
  )


knitr::kable(
  tabla_rotacion,
  digits = 3,
  caption =
    "Matriz de cargas de los componentes retenidos"
)


# 12. PREPARACIÓN PARA MODELADO

# Las medias y desviaciones utilizadas para estandarizar fueron
# estimadas SOLO con entrenamiento mediante prcomp().

entrenamiento <- data.frame(
  
  Price =
    melb_modelo$Price[id],
  
  scale(
    X[id, ],
    center = pca$center,
    scale = pca$scale
  )
)


prueba <- data.frame(
  
  Price =
    melb_modelo$Price[-id],
  
  scale(
    X[-id, ],
    center = pca$center,
    scale = pca$scale
  )
)


cat(
  "\nTamaño final de entrenamiento y prueba:\n"
)

print(
  c(
    Entrenamiento =
      nrow(entrenamiento),
    
    Prueba =
      nrow(prueba)
  )
)



# 13. MODELO 1: REGRESIÓN LINEAL MÚLTIPLE COMPLETA

modelo1 <- lm(
  Price ~ .,
  data = entrenamiento
)


cat(
  "\nFórmula modelo inicial:\n"
)

print(
  formula(modelo1)
)


cat(
  "\nResumen modelo inicial:\n"
)

print(
  summary(modelo1)
)



# 14. MULTICOLINEALIDAD

vif_modelo1 <-
  car::vif(
    modelo1
  )


cat(
  "\nVIF del modelo inicial:\n"
)

print(
  vif_modelo1
)


tabla_vif <- data.frame(
  
  Variable =
    names(
      vif_modelo1
    ),
  
  VIF =
    as.numeric(
      vif_modelo1
    )
)


g_vif <-
  ggplot(
    tabla_vif,
    aes(
      x = reorder(
        Variable,
        VIF
      ),
      y = VIF
    )
  ) +
  geom_col() +
  geom_hline(
    yintercept = 5,
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title =
      "Multicolinealidad del modelo inicial",
    x = NULL,
    y = "VIF"
  ) +
  theme_grey(
    base_size = 14
  )

print(
  g_vif
)



# 15. MODELO 2: SELECCIÓN BACKWARD POR AIC

modelo2 <- step(
  modelo1,
  direction = "backward",
  trace = 0
)


cat(
  "\nFórmula modelo Backward:\n"
)

print(
  formula(modelo2)
)


cat(
  "\nSecuencia de selección Backward:\n"
)

print(
  modelo2$anova
)


cat(
  "\nResumen modelo Backward:\n"
)

print(
  summary(modelo2)
)



# 16. COEFICIENTES E INTERVALOS DE CONFIANZA

coeficientes <- bind_rows(
  
  Inicial =
    broom::tidy(
      modelo1,
      conf.int = TRUE
    ),
  
  Backward =
    broom::tidy(
      modelo2,
      conf.int = TRUE
    ),
  
  .id = "Modelo"
)


cat(
  "\nCoeficientes de ambos modelos:\n"
)

print(
  coeficientes,
  n = Inf
)


g_coeficientes <-
  ggplot(
    filter(
      coeficientes,
      term != "(Intercept)"
    ),
    aes(
      x = term,
      y = estimate / 1000,
      ymin = conf.low / 1000,
      ymax = conf.high / 1000
    )
  ) +
  geom_pointrange() +
  geom_hline(
    yintercept = 0
  ) +
  coord_flip() +
  facet_wrap(
    ~ Modelo
  ) +
  labs(
    title =
      "Coeficientes e intervalos del 95%",
    x = NULL,
    y =
      "Miles de AUD por una desviación estándar de X"
  ) +
  theme_grey(
    base_size = 14
  )

print(
  g_coeficientes
)


# 17. AJUSTE EN ENTRENAMIENTO

modelo0 <- lm(
  Price ~ 1,
  data = entrenamiento
)


ajuste <- data.frame(
  
  Modelo = c(
    "Nulo",
    "Inicial",
    "Backward"
  ),
  
  AIC = c(
    AIC(modelo0),
    AIC(modelo1),
    AIC(modelo2)
  ),
  
  LogLik = c(
    as.numeric(
      logLik(modelo0)
    ),
    as.numeric(
      logLik(modelo1)
    ),
    as.numeric(
      logLik(modelo2)
    )
  ),
  
  Devianza_SSE = c(
    deviance(modelo0),
    deviance(modelo1),
    deviance(modelo2)
  ),
  
  GL_residuales = c(
    df.residual(modelo0),
    df.residual(modelo1),
    df.residual(modelo2)
  )
)


ajuste$R2_entrenamiento <-
  1 -
  ajuste$Devianza_SSE /
  deviance(modelo0)


cat(
  "\nAjuste de modelos en entrenamiento:\n"
)

print(
  ajuste
)



# 17.1 Resumen global de los dos modelos

s1 <- summary(modelo1)
s2 <- summary(modelo2)


p_F_modelo1 <- pf(
  s1$fstatistic[1],
  s1$fstatistic[2],
  s1$fstatistic[3],
  lower.tail = FALSE
)


p_F_modelo2 <- pf(
  s2$fstatistic[1],
  s2$fstatistic[2],
  s2$fstatistic[3],
  lower.tail = FALSE
)


resumen_modelos <- data.frame(
  
  Modelo = c(
    "Inicial",
    "Backward"
  ),
  
  R2 = c(
    s1$r.squared,
    s2$r.squared
  ),
  
  R2_Ajustado = c(
    s1$adj.r.squared,
    s2$adj.r.squared
  ),
  
  Error_residual_AUD = c(
    sigma(modelo1),
    sigma(modelo2)
  ),
  
  F = c(
    unname(
      s1$fstatistic[1]
    ),
    unname(
      s2$fstatistic[1]
    )
  ),
  
  gl1 = c(
    unname(
      s1$fstatistic[2]
    ),
    unname(
      s2$fstatistic[2]
    )
  ),
  
  gl2 = c(
    unname(
      s1$fstatistic[3]
    ),
    unname(
      s2$fstatistic[3]
    )
  ),
  
  p_global = c(
    p_F_modelo1,
    p_F_modelo2
  )
)


cat(
  "\nResumen global de las regresiones:\n"
)

print(
  resumen_modelos
)



# 18. DIAGNÓSTICO DE RESIDUOS

residuos <- bind_rows(
  
  Inicial =
    broom::augment(
      modelo1
    ),
  
  Backward =
    broom::augment(
      modelo2
    ),
  
  .id = "Modelo"
)



# 18.1 Residuos vs ajustados

g_residuos <- ggplot(
  residuos,
  aes(
    x = .fitted / 1000,
    y = .resid / 1000
  )
) +
  geom_point(
    alpha = 0.25
  ) +
  geom_hline(
    yintercept = 0
  ) +
  facet_wrap(
    ~ Modelo
  ) +
  labs(
    title =
      "Residuos frente a valores ajustados",
    x =
      "Valor ajustado (miles de AUD)",
    y =
      "Residuo (miles de AUD)"
  ) +
  theme_grey(
    base_size = 14
  )

print(
  g_residuos
)



# 18.2 Q-Q de residuos

g_qq_residuos <- ggplot(
  residuos,
  aes(
    sample = .std.resid
  )
) +
  stat_qq(
    alpha = 0.3
  ) +
  stat_qq_line() +
  facet_wrap(
    ~ Modelo
  ) +
  labs(
    title =
      "Q-Q de residuos estandarizados",
    x =
      "Cuantiles normales",
    y =
      "Cuantiles de residuos"
  ) +
  theme_grey(
    base_size = 14
  )

print(
  g_qq_residuos
)



# 19. EVALUACIÓN EN EL CONJUNTO DE PRUEBA

# El conjunto de prueba no participa en el ajuste de los modelos.

real <-
  prueba$Price


pred1 <- predict(
  modelo1,
  newdata = prueba
)


pred2 <- predict(
  modelo2,
  newdata = prueba
)


comparacion <- data.frame(
  
  Real = real,
  
  Inicial = pred1,
  
  Backward = pred2
) %>%
  pivot_longer(
    cols =
      -Real,
    names_to =
      "Modelo",
    values_to =
      "Prediccion"
  )


metricas <- comparacion %>%
  group_by(
    Modelo
  ) %>%
  summarise(
    
    R2 =
      1 -
      sum(
        (Real - Prediccion)^2
      ) /
      sum(
        (Real - mean(Real))^2
      ),
    
    RMSE =
      sqrt(
        mean(
          (Real - Prediccion)^2
        )
      ),
    
    MAE =
      mean(
        abs(
          Real - Prediccion
        )
      ),
    
    .groups = "drop"
  )


cat(
  "\nMétricas de desempeño en prueba:\n"
)

print(
  metricas
)



# 20. GRÁFICOS DE EVALUACIÓN


# 20.1 Precio real vs estimado

g_predicciones <- ggplot(
  comparacion,
  aes(
    x = Real / 1000,
    y = Prediccion / 1000
  )
) +
  geom_point(
    alpha = 0.3
  ) +
  geom_abline(
    slope = 1,
    intercept = 0
  ) +
  facet_wrap(
    ~ Modelo
  ) +
  labs(
    title =
      "Predicciones en el conjunto de prueba",
    x =
      "Precio real (miles de AUD)",
    y =
      "Precio estimado (miles de AUD)"
  ) +
  theme_grey(
    base_size = 14
  )

print(
  g_predicciones
)



# 20.2 Comparación R2 / RMSE / MAE

metricas_largas <- metricas %>%
  pivot_longer(
    cols =
      -Modelo,
    names_to =
      "Metrica",
    values_to =
      "Valor"
  )


g_metricas <- ggplot(
  metricas_largas,
  aes(
    x = Modelo,
    y = Valor
  )
) +
  geom_col() +
  geom_text(
    aes(
      label =
        round(
          Valor,
          3
        )
    ),
    vjust = -0.3
  ) +
  facet_wrap(
    ~ Metrica,
    scales = "free_y"
  ) +
  labs(
    title =
      "Comparación de los modelos en prueba",
    x = NULL,
    y = "Valor",
    subtitle =
      "RMSE y MAE: menor es mejor. R2: mayor es mejor."
  ) +
  theme_grey(
    base_size = 14
  )

print(
  g_metricas
)



# 21. SALIDAS CLAVE PARA EL INFORME

cat(
  "\n1. Dimensiones del dataset:\n"
)

print(
  dim(melb_original)
)


cat(
  "\n2. Datos faltantes:\n"
)

print(
  tabla_na,
  row.names = FALSE
)


cat(
  "\n3. Inconsistencias temporales originales:\n"
)

print(
  nrow(
    errores_temporales
  )
)


cat(
  "\n4. Hipótesis Price-BuildingArea:\n"
)

print(
  prueba_hipotesis
)


cat(
  "\n5. Parámetros probabilísticos:\n"
)

print(
  parametros_distribuciones
)


cat(
  "\n6. PCA:\n"
)

print(
  autovalores
)

print(
  paste(
    "Componentes retenidos:",
    k
  )
)


cat(
  "\n7. VIF:\n"
)

print(
  vif_modelo1
)


cat(
  "\n8. Fórmula del modelo inicial:\n"
)

print(
  formula(modelo1)
)


cat(
  "\n9. Fórmula del modelo Backward:\n"
)

print(
  formula(modelo2)
)


cat(
  "\n10. Secuencia Backward:\n"
)

print(
  modelo2$anova
)


cat(
  "\n11. Resumen del modelo inicial:\n"
)

print(
  summary(modelo1)
)


cat(
  "\n12. Resumen del modelo Backward:\n"
)

print(
  summary(modelo2)
)


cat(
  "\n13. Coeficientes e IC95%:\n"
)

print(
  coeficientes,
  n = Inf
)


cat(
  "\n14. Ajuste en entrenamiento:\n"
)

print(
  ajuste
)


cat(
  "\n15. Resumen global:\n"
)

print(
  resumen_modelos
)


cat(
  "\n16. Métricas finales de prueba:\n"
)

print(
  metricas
)
