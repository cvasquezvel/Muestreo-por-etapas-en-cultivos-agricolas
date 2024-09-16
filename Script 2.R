### Setup ----

# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Otras opciones
options(scipen = 999)    # Eliminar la notación científica
options(digits = 4)      # Número de decimales

### Load packages ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, tidyverse, varSelRF, BWGS, foreach,
               tidymodels, rsample, parallel, doParallel)
tidymodels_prefer()

### Load data ----

data <- readxl::read_excel(path = "FENOLOGIA 2023_cruda.xlsx",
                           sheet = "Reporte Evaluación de Variable",
                           col_types = c("date",
                                         "numeric",
                                         rep("text",9),
                                         "numeric",
                                         rep("text",3),
                                         rep("numeric",6),
                                         rep("text",2))) %>%
  dplyr::filter(Variable %in% c("Flores",
                                "Flor cerrada",
                                "Flor abierta"))

hist(data$Valor)

library(naniar)

### Process data ----

datos <- data %>%
  tidyr::pivot_wider(names_from = Variable, values_from = Valor) %>%
  as.data.frame() %>%
  # replace_with(is.null, replace = 0)
  # dplyr::mutate(Flores = ifelse(is.null(Flores), 0, Flores),
  #               `Flor cerrada` = ifelse(is.null(`Flor cerrada`), 0, `Flor cerrada`),
  #               `Flor abierta` = ifelse(is.null(`Flor abierta`), 0, `Flor abierta`)) %>%
  dplyr::mutate(Flores = `Flor cerrada` + `Flor abierta`) %>%
  dplyr::mutate(Year = year(FechaRegistro),
                Week = week(FechaRegistro)) %>%
  unnest(Flores) %>%
  dplyr::filter(Year %in% 2023,
                # Week %in% 27,
                Fundo %in% "AGROMORIN",
                Modulo %in% "M 01",
                Turno %in% "T 01"
                # !Flores %in% NULL
                )

# flores = as.numeric(datos$`Flor cerrada`) + as.numeric(datos$`Flor abierta`)
flores = as.numeric(datos$`Flores`)
length(flores)

datos <- datos %>%
  dplyr::mutate(Flores = flores)

### Calculo de tamaño muestral ----

# Agrupar el dataframe por la columna Week
datos_grouped <- datos %>%
  group_by(Week)

library(purrr)
library(dplyr)
library(parallel)

results_list <- list()

# Agrupar el dataframe por la columna Week
datos_grouped <- datos %>%
  group_by(Week)

# Crear un clúster de múltiples núcleos
cl <- makeCluster(detectCores())

# Registrar y exportar la función calculate_bootstrap en los nodos del clúster
clusterEvalQ(cl, {
  library(dplyr)
  calculate_bootstrap <- function(data, iterations, sample_sizes, target_error) {
    results <- data.frame(n_size = rep(sample_sizes, iterations),
                          x_boot = numeric(iterations * length(sample_sizes)),
                          sd_boot = numeric(iterations * length(sample_sizes)),
                          cv_boot = numeric(iterations * length(sample_sizes)),
                          x_pre = numeric(iterations * length(sample_sizes)),
                          error = numeric(iterations * length(sample_sizes)))
    
    index <- 1  # Índice para almacenar los resultados
    
    for (i in 1:length(sample_sizes)) {
      sample_size <- sample_sizes[i]
      
      for (j in 1:iterations) {
        bootstrap_sample <- sample(data, sample_size, replace = TRUE)
        x_boot <- mean(bootstrap_sample, na.rm = T)
        sd_boot <- sd(bootstrap_sample, na.rm = T)
        # cv_boot <- mean(sd(bootstrap_sample)/mean(bootstrap_sample))*100
        cv_boot <- mean(sd_boot / x_boot, na.rm = T) * 100
        x_pre <- mean(data, na.rm = T)
        error <- abs(x_boot - x_pre) / x_pre * 100
        
        results[index, "n_size"] <- sample_size
        results[index, "x_boot"] <- x_boot
        results[index, "sd_boot"] <- sd_boot
        results[index, "cv_boot"] <- cv_boot
        results[index, "x_pre"] <- x_pre
        results[index, "error"] <- error
        
        index <- index + 1  # Incrementar el índice para almacenar los resultados
      }
    }
    
    df <- results %>%
      dplyr::group_by(n_size) %>%
      dplyr::summarize(mean_pre = mean(x_pre, na.rm = T),
                       mean_boot = mean(x_boot, na.rm = T),
                       sd_error = mean(sd_boot, na.rm = T),
                       cv_error = mean(cv_boot, na.rm = T),
                       mean_error = mean(error, na.rm = T),
                       exitos = mean(error <= (target_error * 100), na.rm = T) * 100,
                       sd_error = sd(x_boot)) %>%
      dplyr::ungroup()
    
    return(list(results = results, resume = df))
  }
})

set.seed(2023)
# Aplicar la función calculate_bootstrap a cada grupo de semanas en paralelo
results_list <- parLapply(cl, split(datos_grouped, f = datos_grouped$Week), function(data) {
  calculate_bootstrap(data = data$Flores,
                      iterations = 1000,
                      sample_sizes = c(#2,3,4,
                                       5,10,25,50,100,250,500,1000,2500#,5000,10000
                                       ),
                      target_error = 0.10)
})

# Detener el clúster de múltiples núcleos
stopCluster(cl)

# Combinar los resultados en una lista única
# results_list_c <- do.call(c, results_list)

# Desagrupar el dataframe
datos_grouped %>% ungroup()

# Crear una lista vacía para almacenar los resultados y el resumen
combined_list <- list(results = NULL, resume = NULL)

# Recorrer los elementos de la lista original
for (i in 1:length(results_list)) {
  # Obtener la semana del nombre de la lista
  week <- names(results_list)[i]
  
  # Obtener los resultados y el resumen del elemento actual
  results <- results_list[[i]]$results
  resume <- results_list[[i]]$resume
  
  # Agregar la columna "week" al resultado y al resumen
  results$week <- as.numeric(week)
  resume$week <- as.numeric(week)
  
  # Agregar los resultados y el resumen al nuevo listado
  combined_list$results <- rbind(combined_list$results, results)
  combined_list$resume <- rbind(combined_list$resume, resume)
}

combined_list$resume %>%
  dplyr::filter(!is.na(n_size),
                !is.na(mean_error)) %>%
  ggplot(aes(x = n_size,
             y = mean_error/100)) +
  geom_point() +
  geom_smooth(method = "gam", #"glm.nb"
              formula = y ~ s(log(x)),
              # method.args = list(family = binomial(link = "cloglog")),
              # method.args = list(family = negative.binomial),
              se = TRUE) +
  # geom_smooth(method = "glm.nb", #"glm.nb"
  #             formula = y ~ log(x),
  #             method.args = list(family = negative.binomial(theta = 1)),
  #             # method.args = list(family = negative.binomial),
  #             se = TRUE) +
  # geom_ribbon(aes(ymin = conf.low/100, ymax = conf.high/100), alpha = 0.2) +
  labs(x = "Tamaño de muestra", y = "Error promedio (%)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  facet_wrap(week ~ .)

# library(MASS)

# Ajustar un modelo glm binomial negativo
# model <- glm.nb(mean_error ~ log(n_size), data = combined_list$resume %>%
#                   dplyr::filter(week %in% 16))

# Obtener la lista de semanas únicas
weeks <- unique(combined_list$resume$week)[-c(32,33)]

# Calcular el resultado por cada semana utilizando lapply()
results <- lapply(weeks, function(week) {
  subset <- combined_list$resume[combined_list$resume$week == week, ]
  approx(subset$exitos, subset$n_size, xout = 90)$y
})

# Unir los resultados en un nuevo data frame
result_df <- data.frame(week = weeks, exitos_approx = unlist(results))

costo_jornal <- 20.4
plantas_jornal <- 30
area <-  8.02  #cp #m02 50.69 # m01 55.52
area_total <- 561.57

table_result <- result_df %>%
  dplyr::rename(Semana = week,
                n = exitos_approx) %>%
  dplyr::mutate(n = ceiling(n),
                n_ha = n / area,
                costo_ha = n_ha / plantas_jornal * costo_jornal,
                costo_total = costo_ha*area_total)
table_result

table_result %>% pull(costo_ha) %>% mean() * area_total

hist(combined_list$resume$mean_error/100)
summary(combined_list$resume$mean_error/100)

hist(combined_list$results$error/100)
summary(combined_list$results$error/100)

# Crear un nuevo dataframe con la fórmula y los coeficientes

g1 <- combined_list$resume  %>%
  dplyr::mutate(n_ha = n_size / area) %>%
  ggplot(aes(x = n_ha, y = exitos/100)) +
  geom_point() +
  geom_smooth(method = "glm",
              formula = y ~ log(x),
              method.args = list(family = binomial(link = "cloglog")),
              se = TRUE) +
  labs(x = "Tamaño de muestra / ha", y = "Tasa de éxitos") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  # geom_text(data = table_result %>%
  #             rename(week = Semana),
  #           aes(label = paste0("n = ", n),
  #               x = 5000, y = 0.25), vjust = -0.5,
  #           size = 4, color = "black") +
  geom_text(data = table_result %>%
              rename(week = Semana),
            aes(label = paste0("n / ha = ", round(n_ha,2)),
                x = 175, y = 0.50), vjust = +0.5,
            size = 4, color = "black") +
  geom_text(data = datos_grouped %>%
              dplyr::summarise(Flores = mean(Flores, na.rm = T)) %>%
              ungroup() %>%
              as.data.frame() %>%
              rename(week = Week),
            aes(label = paste0("Flores = ", round(Flores,0)),
                x = 175, y = 0.50), vjust = +2,
            size = 4, color = "black") +
  facet_wrap(week ~ .)
g1

barplot_file <- paste0("img/AGROMORIN/M01/T01/plot_2.png")
ggsave(barplot_file, plot = g1, 
       width = 120,  # Ancho en mm
       height = 90,  # Altura en mm
       units = "mm",
       dpi = 600,
       scale = 2)

data_cor <- datos_grouped %>%
  dplyr::summarise(Flores = mean(Flores, na.rm = T)) %>%
  ungroup() %>%
  as.data.frame() %>%
  rename(week = Week) %>%
  left_join(table_result %>%
              rename(week = Semana),
            by = "week")

g1 <- data_cor %>%
  ggplot(aes(x = Flores, y = n_ha)) +
  geom_point() +
  labs(y = "Tamaño de muestra / ha", x = "Flores") +
  scale_x_continuous(labels = comma) +
  geom_smooth(method = "glm",
              formula = y ~ log(x),
              method.args = list(family = Gamma(link = "log")),
              se = TRUE) +
  # scale_y_continuous(labels = percent) +
  theme_classic() 
g1

barplot_file <- paste0("img/AGROMORIN/M01/T01/plot_3.png")
ggsave(barplot_file, plot = g1, 
       width = 120,  # Ancho en mm
       height = 90,  # Altura en mm
       units = "mm",
       dpi = 600,
       scale = 2)


g2 <- data_cor %>%
  ggplot(aes(x = week, y = n_ha)) +
  geom_point() +
  labs(y = "Tamaño de muestra / ha", x = "Semana") +
  scale_x_continuous(labels = comma) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 32),
              method.args = list(family = Gamma(link = "log")),
              se = TRUE) +
  # scale_y_continuous(labels = percent) +
  theme_classic() 
g2

g3 <- data_cor %>%
  dplyr::filter(!Flores <= 0) %>%
  ggplot(aes(x = week, y = Flores)) +
  geom_point() +
  labs(y = "Flores / planta", x = "Semana") +
  scale_x_continuous(labels = comma) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 20),
              method.args = list(family = Gamma(link = "log")),
              se = TRUE) +
  # scale_y_continuous(labels = percent) +
  theme_classic() 
g3

library(patchwork)

g4 <- (g2 / g3)

barplot_file <- paste0("img/AGROMORIN/M01/T01/plot_6.png")
ggsave(barplot_file, plot = g4, 
       width = 120,  # Ancho en mm
       height = 90,  # Altura en mm
       units = "mm",
       dpi = 600,
       scale = 2)


##### Error promedio con 2.8 plantas por ha

results <- lapply(weeks, function(week) {
  subset <- combined_list$resume[combined_list$resume$week == week, ]
  approx(subset$n_size, subset$mean_error, xout = 2.8*area)$y
})

# Unir los resultados en un nuevo data frame
result_df <- data.frame(week = weeks, error_approx = unlist(results))
result_df %>% pull(error_approx) %>% mean

##### Tasa de éxito promedio con 2.8 plantas por ha

results <- lapply(weeks, function(week) {
  subset <- combined_list$resume[combined_list$resume$week == week, ]
  approx(subset$n_size, subset$exitos, xout = 2.8*area)$y
})

# Unir los resultados en un nuevo data frame
result_df <- data.frame(week = weeks, exitos_approx = unlist(results))
result_df %>% pull(exitos_approx) %>% mean


##### Error promedio con 10 plantas por ha

results <- lapply(weeks, function(week) {
  subset <- combined_list$resume[combined_list$resume$week == week, ]
  approx(subset$n_size, subset$mean_error, xout = 10*area)$y
})

# Unir los resultados en un nuevo data frame
result_df <- data.frame(week = weeks, error_approx = unlist(results))
result_df %>% pull(error_approx) %>% mean

##### Tasa de éxito promedio con 10 plantas por ha

results <- lapply(weeks, function(week) {
  subset <- combined_list$resume[combined_list$resume$week == week, ]
  approx(subset$n_size, subset$exitos, xout = 10*area)$y
})

# Unir los resultados en un nuevo data frame
result_df <- data.frame(week = weeks, exitos_approx = unlist(results))
result_df %>% pull(exitos_approx) %>% mean
