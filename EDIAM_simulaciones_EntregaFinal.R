#Librerías necesarias
library(parallel)
library(snow)
library(rgenoud)
library(tidyverse)
library(ggplot2)
library(dplyr)
#Parámetros iniciales
Y_ce.Nh <<- 4255.120 # Mtoe
Y_re.Sh <<- 1386.1706 # Mtoe
Y_ce.Sh <<- 7002.889 # Mtoe
Re.Nh <<- 4255.120     # Mtoe
Re.Sh <<- 7002.889     # Mtoe
ReToGDP.Nh <<- 0.06227680
ReToGDP.Sh <<- 0.18765958
#Precios del petróleo
oil.times <<- oil.scenario$Year - 2014
Price.oil.y <<- oil.scenario$Price.Oil
#PIB
GDP.Nh <<- 47034.07
GDP.Sh <<- 25688.192
#Población
pop.times <<- pop.scenario$Year - 2014
L.N.y <<- pop.scenario$AdvancedRegion
L.S.y <<- pop.scenario$EmergingRegion
#Parámetros calibrados
calib.params <<- c(
  epsilon.N = round(2.2694, 4),
  epsilon.S = round(3.1662, 4),
  Gamma_re  = round(0.0271, 4),
  Gamma_ce  = round(0.0068, 4),
  Eta_re.N  = round(0.7614, 4),
  Eta_ce.N  = round(0.9430, 4),
  Eta_re.S  = round(0.0516, 4),
  Eta_ce.S  = round(0.0239, 4),
  val.param.N = round(1.3949, 4),
  val.param.S = round(1.5000, 4),
  lrng.re.N = 0.0,
  lrng.ce.N = 0.0,
  lrng.re.S = 0.0,
  lrng.ce.S = 0.0,
  alfa.N = round(0.3163, 4),
  alfa.S = round(0.2999, 4)
)
#Parámetros climáticos
climate.params <<- c(
  qsi = climate.scenario$qsi,
  Delta.S = climate.scenario$Delta.S,
  Delta.Temp.Disaster = climate.scenario$Delta.Temp.Disaster,
  Beta.Delta.Temp = climate.scenario$Beta.Delta.Temp,
  CO2.base = climate.scenario$CO2.base,
  CO2.Disaster = climate.scenario$CO2.Disaster,
  DeltaTempConstant = climate.scenario$DeltaTempConstant,
  S_0 = climate.scenario$S.0
)
#Políticas a evaluar
policy.params <<- c(
  ce.tax.N = round(x[1], 3),
  Schedule.ce.tax.N = round(x[2], 4),
  Tec.subsidy.N = round(x[3], 3),
  Schedule.Tec.subsidy.N = round(x[4], 4),
  RD.subsidy.N = round(x[5], 3),
  Schedule.RD.subsidy.N = round(x[6], 4),
  ce.tax.S = round(x[7], 3),
  Schedule.ce.tax.S = round(x[8], 4),
  Tec.subsidy.S = round(x[9], 3),
  Schedule.Tec.subsidy.S = round(x[10], 4),
  RD.subsidy.S = round(x[11], 3),
  Schedule.RD.subsidy.S = round(x[12], 4)
)
#Ejecución de simulación según FLAG de verbosidad
if (verbose == FALSE) {
  Objective.Function.Value <- ediamMain(calib.params, verbose = FALSE)
  return(Objective.Function.Value)
} else {
  SimulData <- ediamMain(calib.params, verbose = TRUE)
  return(SimulData)
}
#Optimización genética usando Genoud
set.seed(55555)
nCore <- 40
cl <- makeSOCKcluster(rep("localhost", nCore))
global.elements <- list("ediamPolicyEval", "ediamMain", "ediamEmpiricalParameters", 
                        "ediamInitialConditions", "EdiamEquations", "Ediam", 
                        "oil.scenario", "pop.scenario", "climate.scenario")
clusterExport(cl, global.elements, envir = environment()
#Ejecutar optimización
result <- genoud(
  ediamPolicyEval,
  max = TRUE,
  nvars = 12,
  starting.values = c(0.5, 0.02, 0.10, 0.02, 1.0, 0.02, 0.5, 0.02, 0.10, 0.02, 1.0, 0.02),
  pop.size = 1000,
  Domains = matrix(c(0.0, 1.0, 0.0, 0.1, 0.0, 0.9, 0.0, 0.1, 0.0, 4.0, 0.0, 0.1,
                     0.0, 1.0, 0.0, 0.1, 0.0, 0.9, 0.0, 0.1, 0.0, 4.0, 0.0, 0.1),
                   ncol = 2, byrow = TRUE),
  cluster = cl,
  print.level = 1
)
stopCluster(cl)
#Configurando el clúster
nCore <- 40  # Número de núcleos a utilizar
cl <- makeSOCKcluster(rep("localhost", nCore))
#Exportando objetos para el clúster
global.elements <- c("ediamPolicyEval", "ediamMain", "ediamEmpiricalParameters",
                     "ediamInitialConditions", "EdiamEquations", "Ediam",
                     "oil.scenario", "pop.scenario", "climate.scenario")
clusterExport(cl, global.elements, envir = environment())
#Estableciendo semilla para reproducibilidad
set.seed(55555)
#Ejecutando optimización con renoud
result <- genoud(
  fn = ediamPolicyEval,
  max = TRUE,
  nvars = 12,
  starting.values = c(
    # Región avanzada
    0.5, 0.02, 0.10, 0.02, 1.0, 0.02,
    # Región emergente
    0.5, 0.02, 0.10, 0.02, 1.0, 0.02
  ),
  pop.size = 1000,
  Domains = matrix(c(
#Límites inferiores y superiores para cada variable (ordenados por parámetro)

    # ce.tax.N                 Schedule.ce.tax.N
    0.0, 1.0,                 0.0, 0.1,

    # Tec.subsidy.N           Schedule.Tec.subsidy.N
    0.0, 0.9,                 0.0, 0.1,

    # RD.Subsidy.N            Schedule.RD.subsidy.N
    0.0, 4.0,                 0.0, 0.1,

    # ce.tax.S                Schedule.ce.tax.S
    0.0, 1.0,                 0.0, 0.1,

    # Tec.subsidy.S           Schedule.Tec.subsidy.S
    0.0, 0.9,                 0.0, 0.1,

    # RD.Subsidy.S            Schedule.RD.subsidy.S
    0.0, 4.0,                 0.0, 0.1
  ), ncol = 2, byrow = TRUE),
  cluster = cl,
  print.level = 1
)
#Solución óptima para el escenario:
#Clima: "GFDL-ESM2G" | Población: "UN.Median.PI" | Timestep = 1 
#lambda.S = 0.1443 * 4.0
#Valor de la función objetivo: -41.2252
x <- c(
  #Región Avanzada
  0.1759729267,  # ce.tax.N
  0.0083342075,  # Schedule.ce.tax.N
  0.8278753097,  # Tec.subsidy.N
  0.0023607783,  # Schedule.Tec.subsidy.N
  0.2428055360,  # RD.subsidy.N
  0.0748504300,  # Schedule.RD.subsidy.N

  #Región Emergente
  0.1915780205,  # ce.tax.S
 -0.0007484287,  # Schedule.ce.tax.S
  0.6971772584,  # Tec.subsidy.S
  0.0091336108,  # Schedule.Tec.subsidy.S
  0.0002592633,  # RD.subsidy.S
  0.0652135338   # Schedule.RD.subsidy.S
)

#Soluciones óptimas para escenarios alternativos (Timestep = 5)
#Escenario 1: Clima = "GFDL-ESM2G", Población = "UN.Lower95.PI"
#Valor de la función objetivo: -10.92281

x <- c(
  0.260432451,  # ce.tax.N
  0.019417471,  # Schedule.ce.tax.N
  0.833999113,  # Tec.subsidy.N
  0.002035506,  # Schedule.Tec.subsidy.N
  0.540403727,  # RD.subsidy.N
  0.080215080,  # Schedule.RD.subsidy.N
  0.180240385,  # ce.tax.S
  0.004941565,  # Schedule.ce.tax.S
  0.696275779,  # Tec.subsidy.S
  0.001916819,  # Schedule.Tec.subsidy.S
  0.001107793,  # RD.subsidy.S
  0.010012071   # Schedule.RD.subsidy.S
)
#Parámetros óptimos. Escenario 2
#Clima: "GFDL-ESM2G" 
#Población: "UN.Median.PI"
#Timestep = 5 | lambda.S = 0.1443 * 4.0
#Valor de la función objetivo: -10.9294
x <- c(
  #Región Avanzada (N)
  0.1633620908,  # ce.tax.N
  0.0043222848,  # Schedule.ce.tax.N
  0.8239402876,  # Tec.subsidy.N
  0.0015262409,  # Schedule.Tec.subsidy.N
  0.6740002979,  # RD.subsidy.N
  0.0749896069,  # Schedule.RD.subsidy.N

  #Región Emergente (S)
  0.1645699259,  # ce.tax.S
  0.0018567055,  # Schedule.ce.tax.S
  0.7039641752,  # Tec.subsidy.S
  0.0022420558,  # Schedule.Tec.subsidy.S
  0.0003846924,  # RD.subsidy.S
  0.0441517900   # Schedule.RD.subsidy.S
)

#Simulación simple usando un sólo cojunto de parámetro
#Cargando el diseño experimental (muestreo Latin Hypercube)
source(file.path(dir.calib, "LatinHypercube.R"))
#Confirmar que el objeto 'lhs.sample' existe
if (!exists("lhs.sample")) {
  stop("El objeto 'lhs.sample' no fue creado. Verifica el archivo LatinHypercube.R")
}

#Seleccionando el primer conjunto de parámetros del diseño
Parameters <- lhs.sample[1, ]

#Ejecutando la simulación principal con esos parámetros
test.out <- EdiamMain(t = 1, State = NULL, Parameters = Parameters)

#Graficando la producción en el tiempo
plot(
  test.out$Time, test.out$Y,
  type = "l", col = "blue",
  xlab = "Tiempo", ylab = "Producción",
  main = "Producción en el tiempo"
)

#Configurando el clúster paralelo para simulaciones en batch
library(parallel)
cl1 <- makeCluster(detectCores() - 1)

#Exportando variables necesarias al clúster
clusterExport(cl1, c("t", "State", "Parameters", "EdiamMain"))

#Condiciones iniciales del modelo
#Iniciación del modelo

#Definiendo directorio donde están los scripts de calibración
dir.calib <- "C:/Users/52556/Documents/MEK/6TO TRIMESTRE/MACROECONOMÍA/FINAL/Ediam_v2020_02_18/"

#Cargando scripts de calibración y generadores
source(file.path(dir.calib, "LatinHypercubeGenerator.R"))
source(file.path(dir.calib, "LatinHypercubeSampling.R"))
source(file.path(dir.calib, "Calibrator.R"))
source(file.path(dir.calib, "CalibrationScript_new.R"))

#Leyendo diseño experimental y seleccionar una fila de parámetros
design <- read.csv("Exp.design_P0.csv")
Parameters <- design[1, ]  # Puedes cambiar el índice para otro conjunto

#Asignando parámetros como variables globales (si el modelo lo requiere)
for (param in names(Parameters)) {
  assign(param, Parameters[[param]])
}

#Definiendo el vector de tiempo (por ejemplo, bloques de 5 años)
t <- seq(0, 100, by = 5)

#Estableciendo condiciones iniciales para la simulación
State <- ediamInitialConditions(
  t,
  population_data,
  oil.times,
  climate.times,
  pop.scenario,
  climate.scenario,
  oil.scenario
)

#Verificando estructura y dimensiones
summary(Y_re.Nh)
length(Y_re.Nh)
head(Y_re.Nh, 10)

#Graficando producción de bienes renovables
plot(
  1:length(Y_re.Nh), Y_re.Nh,
  type = "l", col = "forestgreen",
  xlab = "Tiempos (en bloques de 5 años)",
  ylab = "Producción de bienes renovables (Y_re)",
  main = "Producción de bienes renovables - País en desarrollo (Nh)"
#Cargando archivos clave
exp_design <- read_csv("Exp.design.csv")
model_runs <- read_csv("model.runs.csv")
prim_data <- read_csv("prim.data.csv")
robust_mapping <- read_csv("robust_mapping.csv")

#Exploración inicial
head(exp_design)
head(model_runs)
glimpse(exp_design)
glimpse(model_runs)
summary(exp_design)
summary(model_runs)

#Verificando nombres de variables
names(exp_design)
names(model_runs)

#Combinando diseño experimental con resultados del modelo
merged_data <- merge(exp_design, model_runs, by = "Run.ID")
head(merged_data)
nrow(merged_data)

#Gráfica 1: Temperatura global por corrida
ggplot(model_runs, aes(x = time, y = Delta.Temp, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "red") +
  labs(title = "Evolución de la temperatura global",
       x = "Año",
       y = "Cambio de temperatura (°C)") +
  theme_minimal()

#Promedio de temperatura global
temp_avg <- model_runs %>%
  group_by(time) %>%
  summarise(mean_temp = mean(Delta.Temp, na.rm = TRUE))

#Gráfica 2: Promedio de temperatura global
ggplot() +
  geom_line(data = model_runs, aes(x = time, y = Delta.Temp, group = Run.ID),
            alpha = 0.05, color = "red") +
  geom_line(data = temp_avg, aes(x = time, y = mean_temp),
            color = "black", size = 1) +
  labs(title = "Temperatura global promedio",
       x = "Año",
       y = "Cambio de temperatura (°C)") +
  theme_minimal()

#Gráfica 3: Evolución del PIB global
ggplot(model_runs, aes(x = time, y = Y, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "blue") +
  labs(title = "Evolución del PIB global",
       x = "Año",
       y = "PIB (trillones de USD constantes)") +
  theme_minimal()

#Gráfica 4: Evolución del bienestar del consumidor
ggplot(model_runs, aes(x = time, y = Utility.Consumer, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "darkgreen") +
  labs(title = "Bienestar del consumidor",
       x = "Año",
       y = "Utilidad") +
  theme_minimal()

#Filtrando valores negativos en bienestar
model_runs_clean <- subset(model_runs, Utility.Consumer > 0)

#Gráfica 5: Bienestar sin outliers
ggplot(model_runs_clean, aes(x = time, y = Utility.Consumer, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "darkgreen") +
  labs(title = "Bienestar del consumidor (valores positivos)",
       x = "Año",
       y = "Utilidad") +
  theme_minimal()

#Promedio de bienestar
utility_avg <- model_runs_clean %>%
  group_by(time) %>%
  summarise(mean_utility = mean(Utility.Consumer, na.rm = TRUE))

#Gráfica 6: Bienestar promedio
ggplot(utility_avg, aes(x = time, y = mean_utility)) +
  geom_line(color = "darkgreen", size = 1.2) +
  labs(title = "Bienestar promedio del consumidor",
       x = "Año",
       y = "Utilidad promedio") +
  theme_minimal()

#Gráfica 7: Concentración de CO₂
ggplot(model_runs, aes(x = time, y = CO2.Concentration, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "brown") +
  labs(title = "Concentración de CO₂",
       x = "Año",
       y = "ppm") +
  theme_minimal()

#Promedio de concentración de CO₂
co2_avg <- model_runs %>%
  group_by(time) %>%
  summarise(mean_co2 = mean(CO2.Concentration, na.rm = TRUE))

#Gráfica 8: Concentración promedio de CO₂
ggplot(co2_avg, aes(x = time, y = mean_co2)) +
  geom_line(color = "brown", size = 1) +
  labs(title = "Concentración promedio de CO₂",
       x = "Año",
       y = "ppm") +
  theme_minimal()

#Gráfica 9: Emisiones globales de CO₂
ggplot(model_runs, aes(x = time, y = Delta.S, group = Run.ID)) +
  geom_line(alpha = 0.05, color = "orange") +
  labs(title = "Emisiones globales de CO₂",
       x = "Año",
       y = "GtCO₂ por año") +
  theme_minimal()

#Gráfica 10: Consumo de hogares
ggplot(model_runs, aes(x = time, y = Consumption, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "purple") +
  labs(title = "Evolución del consumo de hogares",
       x = "Año",
       y = "Billones de USD") +
  theme_minimal()

#Gráfica 11: Costo económico del daño climático
ggplot(model_runs, aes(x = time, y = Cost.S.Damage, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "firebrick") +
  labs(title = "Costo económico del daño climático",
       x = "Año",
       y = "Billones de USD") +
  theme_minimal()

#Promedio del costo climático
damage_avg <- model_runs %>%
  group_by(time) %>%
  summarise(mean_damage = mean(Cost.S.Damage, na.rm = TRUE))

#Gráfica 12: Costo promedio del daño climático
ggplot(damage_avg, aes(x = time, y = mean_damage)) +
  geom_line(color = "darkred", size = 1.2) +
  labs(title = "Costo económico promedio del daño climático",
       x = "Año",
       y = "Billones de USD") +
  theme_minimal()

#Gráfica 13: Precio relativo de energías limpias
ggplot(model_runs, aes(x = time, y = Price.re, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "orange") +
  labs(title = "Precio relativo de energías limpias",
       x = "Año",
       y = "Índice de precio") +
  theme_minimal()

#Promedio del precio relativo
price_avg <- model_runs %>%
  group_by(time) %>%
  summarise(mean_price = mean(Price.re, na.rm = TRUE))

#Gráfica 14: Precio relativo promedio
ggplot(price_avg, aes(x = time, y = mean_price)) +
  geom_line(color = "orange", size = 1) +
  labs(title = "Precio relativo promedio de energías limpias",
       x = "Año",
       y = "Índice de precio") +
  theme_minimal()

#Gráfica 15: Subsidio global a tecnologías limpias
ggplot(model_runs, aes(x = time, y = Tec.subsidy.GF_N, group = Run.ID)) +
  geom_line(alpha = 0.1, color = "forestgreen") +
  labs(title = "Subsidio global a tecnologías limpias",
       x = "Año",
       y = "USD billones/año") +
  theme_minimal()

#Filtrado de escenarios con subsidios activos
model_subsidy <- model_runs %>%
  group_by(Run.ID) %>%
  filter(max(Tec.subsidy.GF_N, na.rm = TRUE) > 0)

#Gráfica 16: Escenarios con subsidios activos
ggplot(model_subsidy, aes(x = time, y = Tec.subsidy.GF_N, group = Run.ID)) +
  geom_line(alpha = 0.2, color = "forestgreen") +
  labs(title = "Subsidio a tecnologías limpias (escenarios activos)",
       x = "Año",
       y = "USD billones/año") +
  theme_minimal()

#Promedio del subsidio a tecnologías limpias
tec_avg <- model_runs %>%
  group_by(time) %>%
  summarise(mean_tec = mean(Tec.subsidy.GF_N, na.rm = TRUE))

#Gráfica 17: Subsidio promedio a tecnologías limpias
ggplot(tec_avg, aes(x = time, y = mean_tec)) +
  geom_line(color = "forestgreen", size = 1) +
  labs(title = "Subsidio promedio global a tecnologías limpias",
       x = "Año",
       y = "USD billones/año") +
  theme_minimal()
