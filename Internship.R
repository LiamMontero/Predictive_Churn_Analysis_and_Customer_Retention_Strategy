library(tidyverse)
library(ggplot2)
library(scales)
library(recipes)
library(tidymodels)
library(yardstick)
library(dplyr)
library(vip)
library(patchwork) 

set.seed(123)

# cargando los datos
path <- "~/Telco_Customer_Churn_Dataset  (3).csv"

datos <- read.csv(path)

str(datos)

# task 1
cat("El set de datos tiene", nrow(datos), "filas.", "\n")

## revisando si el ID de los usuarios es unico, en caso de que un usuario se halla registrado 2 veces porque se fue y regreso
cat("El set de datos tiene", length(unique(datos$customerID)), "unsuarios unicos, por lo que no hay usuuarios repetidos.", "\n")

# revisando si tenemos algun valor NA
cat("La columna customerID tiene algun valor NA?:", "\n", any(is.na(datos$customerID)), "\n")

## analnizando la columna gender
table(datos$gender)

# convirtiendo los generos a valores binarios con Male = 1 y Female = 0
datos$gender <- ifelse(datos$gender == "Male", 1, 0)

## Analizando la colummna SeniorCitizen
cat("La columna SeniorCitizen tiene algun valor NA?:", "\n", any(is.na(datos$SeniorCitizen)), "\n")

# revisando que tipo de valores tiene esa columna
table(datos$SeniorCitizen)


## Analizando la columna Partner
table(datos$Partner)

# conviertiendo a valores binarion con Yes = 1 y No = 0
datos$Partner <- ifelse(datos$Partner == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna Partner tiene algun valor NA?:", "\n", any(is.na(datos$Partner)), "\n")


## Analizando la columna Dependents
table(datos$Dependents)

# conviertiendo a valores binarion con Yes = 1 y No = 0
datos$Dependents <- ifelse(datos$Dependents == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna Dependents tiene algun valor NA?:", "\n", any(is.na(datos$Dependents)), "\n")


## Analizando la columna tenure
cat("La columna tenure tiene algun valor NA?:", "\n", any(is.na(datos$tenure)), "\n")

# rangos
range(datos$tenure)

# viendo cuantos valores hay en 0
cat("La columna tenure tiene", sum(datos$tenure == 0), "clientes que no han firmado contrato con la compañia", "\n")


## Analizando la columna PhoneService
table(datos$PhoneService)

# conviertiendo a valores binarion con Yes = 1 y No = 0
datos$PhoneService <- ifelse(datos$PhoneService == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna PhoneService tiene algun valor NA?:", "\n", any(is.na(datos$PhoneService)), "\n")


## Analizanndo la columna MultipleLines
table(datos$MultipleLines)

# convirtiendo los valores en binarios, donde Yes = 1, No = 0 y No phone service = 0
datos$MultipleLines <- ifelse(datos$MultipleLines == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna MultipleLines tiene algun valor NA?:", "\n", any(is.na(datos$MultipleLines)), "\n")


## Analizando la columna InternetService
table(datos$InternetService)

# mas adelante haremos una codificacion One-Hot para esta columna, para facilitar el entrenamiento del modelo predictivo


## Aalizando la columna OnlineSecurity
table(datos$OnlineSecurity)

# convirtiendo los valores en binarios, donde Yes = 1, No = 0 y No internet service = 0
datos$OnlineSecurity <- ifelse(datos$OnlineSecurity == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna OnlineSecurity tiene algun valor NA?:", "\n", any(is.na(datos$OnlineSecurity)), "\n")


## Analizando la columna OnlineBackup
table(datos$OnlineBackup)

# convirtiendo los valores en binarios, donde Yes = 1, No = 0 y No internet service = 0
datos$OnlineBackup <- ifelse(datos$OnlineBackup == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna OnlineBackup tiene algun valor NA?:", "\n", any(is.na(datos$OnlineBackup)), "\n")


## Analizando la columna DeviceProtection
table(datos$DeviceProtection)

# convirtiendo los valores en binarios, donde Yes = 1, No = 0 y No internet service = 0
datos$DeviceProtection <- ifelse(datos$DeviceProtection == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna DeviceProtection tiene algun valor NA?:", "\n", any(is.na(datos$DeviceProtection)), "\n")


## Analizando la columna TechSupport
table(datos$TechSupport)

# convirtiendo los valores en binarios, donde Yes = 1, No = 0 y No internet service = 0
datos$TechSupport <- ifelse(datos$TechSupport == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna TechSupport tiene algun valor NA?:", "\n", any(is.na(datos$TechSupport)), "\n")


## Analizando la columna StreamingTV
table(datos$StreamingTV)

# convirtiendo los valores en binarios, donde Yes = 1, No = 0 y No internet service = 0
datos$StreamingTV <- ifelse(datos$StreamingTV == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna StreamingTV tiene algun valor NA?:", "\n", any(is.na(datos$StreamingTV)), "\n")


## Analizando la columna StreamingMovies
table(datos$StreamingMovies)

# convirtiendo los valores en binarios, donde Yes = 1, No = 0 y No internet service = 0
datos$StreamingMovies <- ifelse(datos$StreamingMovies == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna StreamingMovies tiene algun valor NA?:", "\n", any(is.na(datos$StreamingMovies)), "\n")


## Analizando la columna Contract
table(datos$Contract)

# revisando si contiene algun valor NA
cat("La columna Contract tiene algun valor NA?:", "\n", any(is.na(datos$Contract)), "\n")

# mas adelante haremos una codificacion One-Hot para esta columna, para facilitar el entrenamiento del modelo predictivo


## Analizando la columna PaperlessBilling
table(datos$PaperlessBilling)

# conviertiendo a valores binarion con Yes = 1 y No = 0
datos$PaperlessBilling <- ifelse(datos$PaperlessBilling == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna PaperlessBilling tiene algun valor NA?:", "\n", any(is.na(datos$PaperlessBilling)), "\n")


# Analizando la columna PaymentMethod
table(datos$PaymentMethod)

# revisando si contiene algun valor NA
cat("La columna PaymentMethod tiene algun valor NA?:", "\n", any(is.na(datos$PaymentMethod)), "\n")

# mas adelante haremos una codificacion One-Hot para esta columna, para facilitar el entrenamiento del modelo predictivo


## Analizando la columna MonthlyCharges
cat("La columna MonthlyCharges tiene algun valor NA?:", "\n", any(is.na(datos$MonthlyCharges)), "\n")


## Analizando la columna TotalCharges
cat("La columna TotalCharges tiene algun valor NA?:", "\n", any(is.na(datos$TotalCharges)), "\n")
cat("La columna TotalCharges tiene", sum(is.na(datos$TotalCharges)), "valores NA:", "\n")

# mas adelante revisaremos si esos NA tienen que ver con los tenure = 0


## Analizando la columna Churn
table(datos$Churn)

# conviertiendo a valores binarion con Yes = 1 y No = 0
datos$Churn <- ifelse(datos$Churn == "Yes", 1, 0)

# revisando si contiene algun valor NA
cat("La columna Churn tiene algun valor NA?:", "\n", any(is.na(datos$Churn)), "\n")




###############################################
# Exploratory Data Analysis (EDA)
###############################################

# revisando si existe coincidencia entre los valores NA de la columna TotalCharges con los valores 0 de la columna tenure
coincidencia <- datos %>% filter(is.na(TotalCharges))

# Como era de esperarce, los valores faltantes de la columna TotalCharges estan relacionados con todos los clientes que 
# tienen asignado un valor de 0 en la columna tenure, pero si observamos, estos clientes tienen contratos de 2 años y 
# montos asignados a pagar cada mes, por lo cual, los datos faltantes en la columna TotalCharges y los valores = 0 en la 
# columna tenure significa que esos clientes ya firmaron contrato con la compañia pero aun no han hecho su primer pago y
# y por ende aun no llevan 1 mes en la compañia

# Basandome en lo anterior, voy a convertir todos los valores 0 en 1 y todos los NA en lo que deberia pagar cada cliente 
# mensaual respectivamente
datos$tenure[datos$tenure == 0] <- 1

# chequeando los ranngos de la columna tenure nuevamente
range(datos$tenure)

# ahora igualaremos cada valor NA de la columna TotalCharges a lo que deberia pagar ese cliente mensualmente dado los datos
# de la columna MonthlyCharges

# Extrallendo indices de los valores NA de la columna TotalCharges
index_NA <- which(is.na(datos$TotalCharges))

# usando los indices para hallar los valores correspondientes a esos valores NA en la columna MonthlyCharges
values <- datos$MonthlyCharges[index_NA]

# igualando los valores
datos$TotalCharges[is.na(datos$TotalCharges)] <- values

# chequeando que todos los valores se hallan remplazado correctamente
identical(datos$TotalCharges[index_NA], values)



# calcularemos la tasa de abandono de los cliente
churn_rate <- round(sum(datos$Churn[datos$Churn == 1]) / nrow(datos), 3)
# Podemos ver que la tasa de abandono es un poco alta, ya que el 26.5% de los clientes terminan abandonando


# Calcular proporciones y convertir a data.frame
tabla_df <- as.data.frame(
  prop.table(table(datos$Churn)) * 100
)
colnames(tabla_df) <- c("Churn", "porcentaje")

# Convertir los valores de Churn: 0 -> No, 1 -> Yes
tabla_df$Churn <- factor(
  tabla_df$Churn,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# Crear etiquetas SOLO con el porcentaje
tabla_df$etiqueta <- paste0(round(tabla_df$porcentaje, 1), "%")

###################### analicis demografico #############################

# Crear gráfico de pastel
ggplot(tabla_df, aes(x = "", y = porcentaje, fill = Churn)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  ) +
  scale_fill_manual(
    values = c("No" = "#00BFC4", "Yes" = "#F8766D")  # rojo y verde
  ) +
  labs(
    title = "Churn Distribution",
    fill = "Churn"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )




datos_para_graficos <- read.csv(path)

# Gráfico 1: Género 
plot_gender <- ggplot(datos_para_graficos, aes(x = gender, 
                                               fill = Churn)) +
  geom_bar(position = "stack") +
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    color = "black", 
    size = 4
  ) +
  labs(
    title = "Por Género",
    x = NULL,
    y = "Número de Clientes",
    fill = "Churn"
  ) +
  scale_fill_manual(values = c("No" = "#00BFC4", "Yes" = "#F8766D")) + 
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Gráfico 2: Dependientes 
plot_dependents <- ggplot(datos_para_graficos, aes(x = Dependents, 
                                                   fill = Churn)) +
  geom_bar(position = "stack") +
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    color = "black", 
    size = 4
  ) +
  labs(
    title = "Por Dependientes",
    x = NULL,
    y = NULL,
    fill = "Churn"
  ) +
  scale_fill_manual(values = c("No" = "#00BFC4", "Yes" = "#F8766D")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Gráfico 3: Pareja 
plot_partner <- ggplot(datos_para_graficos, aes(x = Partner, 
                                                fill = Churn)) +
  geom_bar(position = "stack") +
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    color = "black", 
    size = 4
  ) +
  labs(
    title = "Por Pareja",
    x = NULL,
    y = NULL,
    fill = "Churn"
  ) +
  scale_fill_manual(values = c("No" = "#00BFC4", "Yes" = "#F8766D")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank()
  )

# Combinar los gráficos con patchwor
combined_plot <- plot_gender + plot_dependents + plot_partner

# Añadir un título general y una leyenda compartida
combined_plot + 
  plot_annotation(
    title = 'Análisis Demográfico del Abandono de Clientes',
    caption = 'Distribución de clientes que abandonan (Yes) y se quedan (No) según su perfil.',
    theme = theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))
  ) & 
  plot_layout(guides = 'collect')


######################## analicis de antiguedad #################################

# Crear el box plot de tenure
ggplot(datos, aes(x = factor(Churn, levels = c(0, 1), labels = c("No Abandona", "Sí Abandona")), 
                  y = tenure, 
                  fill = factor(Churn, levels = c(0, 1), labels = c("No Abandona", "Sí Abandona")))) +
  geom_boxplot(alpha = 0.8, outlier.color = "red") +
  geom_jitter(width = 0.2, alpha = 0.15, color = "black") +
  labs(
    title = "Relación entre Antigüedad del Cliente y Abandono",
    subtitle = "Los clientes que abandonan tienen una antigüedad significativamente menor",
    x = "Estatus del Cliente",
    y = "Antigüedad (en meses)"
  ) +
  scale_fill_manual(values = c("No Abandona" = "#00BFC4", "Sí Abandona" = "#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )


######################## analicis de las condiciones de servicio ################

# Paso 1: Calcular los porcentajes para las etiquetas
contract_summary <- datos %>%
  dplyr::count(Contract, Churn) %>%
  group_by(Contract) %>%
  mutate(percentage = n / sum(n))


# Paso 2: Crear el gráfico de barras apiladas al 100%
ggplot(contract_summary, aes(x = Contract, y = percentage, fill = factor(Churn, labels = c("NO", "YES")))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = percent(percentage, accuracy = 1)),
    position = position_stack(vjust = 0.5), 
    color = "white",
    fontface = "bold",
    size = 4.5
  ) +
  labs(
    title = "Tasa de Abandono por Tipo de Contrato",
    subtitle = "El contrato 'Mes a Mes' es el principal factor de riesgo de abandono",
    x = "Tipo de Contrato",
    y = "Porcentaje de Clientes",
    fill = "Abandono (Churn)"
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("NO" = "#00BFC4", "YES" = "#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


########################################################
# task 3 segmentacion de los clientes
########################################################

#==============================================================
#                TAREA 3: SEGMENTACIÓN DE CLIENTES
#==============================================================

# Crear la columna de Segmentos usando case_when() de dplyr
# Usamos las variables originales (antes de convertirlas a numéricas si es necesario)
# para que las reglas sean más legibles.
# NOTA: Tu código ya convirtió 'Churn' a 0/1, pero para las reglas usaré las columnas
# de texto originales como 'Contract' que aún mantienes.

datos <- datos %>%
  mutate(Segmento = case_when(
    # Regla 1: Clientes Nuevos en Riesgo (la más específica primero)
    tenure <= 12 & Contract == "Month-to-month" ~ "Cliente Nuevo en Riesgo",
    # Regla 2: Clientes Leales de Alto Valor
    tenure > 24 & Contract %in% c("One year", "Two year") ~ "Cliente Leal de Alto Valor",
    # Regla 3: Clientes con Contrato a Plazo (seguros, pero no necesariamente "leales" aún)
    Contract %in% c("One year", "Two year") ~ "Cliente con Contrato a Plazo",
    # Regla 4: Clientes Establecidos pero Flexibles
    tenure > 12 & Contract == "Month-to-month" ~ "Cliente Estable Flexible",
    # Regla 5: Categoría para cualquier otro caso que no cumpla las condiciones anteriores
    TRUE ~ "Otro"
  ))

# Verificamos la distribución de los segmentos creados
cat("Distribución de clientes por segmento:\n")
table(datos$Segmento)



# Analizar la tasa de abandono dentro de cada segmento

# Calcular las proporciones de abandono por segmento
segmento_churn_summary <- datos %>%
  group_by(Segmento) %>%
  dplyr::count(Churn) %>%
  mutate(percentage = n / sum(n)) %>%
  ungroup()

# Imprimir la tabla de resumen
cat("\nResumen de la tasa de abandono por segmento:\n")
print(segmento_churn_summary)

# Visualizar la tasa de abandono por segmento
ggplot(segmento_churn_summary, aes(x = Segmento, y = percentage, fill = factor(Churn, labels = c("NO", "YES")))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(
    aes(label = percent(percentage, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  labs(
    title = "Tasa de Abandono por Segmento de Cliente",
    subtitle = "El segmento 'Cliente Nuevo en Riesgo' muestra la mayor vulnerabilidad",
    x = "Segmento de Cliente",
    y = "Porcentaje de Clientes",
    fill = "Abandono (Churn)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("NO" = "#00BFC4", "YES" = "#F8766D")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas para que no se solapen
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


# Cuantificar el valor del segmento más riesgoso

# Filtrar los clientes del segmento de mayor riesgo
clientes_en_riesgo <- datos %>%
  filter(Segmento == "Cliente Nuevo en Riesgo")

# Calcular métricas clave para este segmento
ingresos_mensuales_riesgo <- sum(clientes_en_riesgo$MonthlyCharges) * mean(clientes_en_riesgo$Churn)
s

# Presentar el análisis
cat("\n--- Análisis del Segmento 'Cliente Nuevo en Riesgo' ---\n")
cat(paste("Número total de clientes en este segmento:", nrow(clientes_en_riesgo), "\n"))
cat(paste("Tasa de abandono específica para este segmento:", round(churn_rate_riesgo, 1), "%\n"))
cat(paste("Ingresos mensuales totales en riesgo (de este segmento): $", format(round(ingresos_mensuales_riesgo, 2), nsmall = 2, big.mark = ","), "\n"))
cat("Este es el grupo de clientes de más alta prioridad para las acciones de retención.\n")


##############################################
# Creacion del modelo predictivo
##############################################


# 1. DIVISIÓN DE DATOS Y PREPROCESAMIENTO
# Nota: La variable objetivo 'Churn' debe ser un factor para que 'recipes' la maneje bien
datos$Churn <- factor(datos$Churn, levels = c(0, 1), labels = c("No", "Yes"))

idx <- sample(seq_len(nrow(datos)), 0.8 * nrow(datos))
train_data <- datos[idx, ]
test_data  <- datos[-idx, ]

# Receta de preprocesamiento
rec <- recipe(Churn ~ ., data = train_data) %>%
  update_role(customerID, Segmento, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())


#========================================================================
#             TAREA 4: MODELO DE LÍNEA BASE - REGRESIÓN LOGÍSTICA
#========================================================================

# 1. DEFINICIÓN DEL MODELO
# Especificamos que queremos un modelo de Regresión Logística
# El motor (engine) será 'glm', que es el estándar en R para modelos lineales generalizados
log_reg_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

cat("--- Especificación del Modelo de Regresión Logística ---\n")
print(log_reg_spec)


# 2. CREACIÓN DEL FLUJO DE TRABAJO
# Un workflow empaqueta el preprocesamiento (nuestra 'rec' de recipes) y el modelo.
# Esto hace que el proceso sea increíblemente limpio y evita errores.
log_reg_workflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(log_reg_spec)

# 3. ENTRENAMIENTO DEL MODELO
log_reg_fit <- fit(log_reg_workflow, data = train_data)



# 4. PREDICCIÓN Y EVALUACIÓN EN EL CONJUNTO DE PRUEBA
predictions_lr <- predict(log_reg_fit, new_data = test_data, type = "prob") %>%
  # También obtenemos la clase predicha para la matriz de confusión
  bind_cols(predict(log_reg_fit, new_data = test_data)) %>%
  # Añadimos los datos reales para la comparación
  bind_cols(test_data %>% select(Churn))

# Renombramos las columnas para que sean más claras
colnames(predictions_lr) <- c("prob_No", "prob_Yes", "clase_predicha", "clase_real")


# 5. ANÁLISIS DE RESULTADOS

# Matriz de Confusión
cat("\nMatriz de Confusión (Regresión Logística):\n")
conf_mat(predictions_lr, truth = clase_real, estimate = clase_predicha)

# Métricas de rendimiento clave (Accuracy, AUC, Precision, Recall, F1)
metrics_lr <- metrics(predictions_lr, truth = clase_real, estimate = clase_predicha, prob_Yes) %>%
  filter(.metric %in% c("accuracy", "roc_auc", "precision", "recall", "f_meas"))

cat("\nMétricas de Rendimiento (Regresión Logística):\n")
print(metrics_lr)

# Curva ROC
roc_curve_lr <- roc_curve(predictions_lr, truth = clase_real, prob_Yes)

autoplot(roc_curve_lr) +
  labs(
    title = "Curva ROC - Modelo de Regresión Logística",
    subtitle = paste("AUC =", round(metrics_lr$`.estimate`[metrics_lr$`.metric` == "roc_auc"], 3))
  ) +
  theme_minimal()



#==============================================================
#             CORRECCIÓN DEFINITIVA DE LA EVALUACIÓN
#==============================================================

# Crear un "conjunto de métricas" para las que dependen de la clase (Accuracy, Precision, etc.)
class_metrics <- metric_set(accuracy, precision, recall, f_meas)

# Calcular estas métricas, especificando la clase positiva con event_level.
results_class <- predictions_lr %>% 
  class_metrics(truth = clase_real, 
                estimate = clase_predicha, 
                event_level = "second")

# Calcular por separado la métrica que depende de la probabilidad (AUC).
# Hacemos esto para asegurar que no haya ambigüedad.
results_prob <- predictions_lr %>% 
  roc_auc(truth = clase_real, 
          prob_Yes,
          event_level = "second")

cat("\n--- Métrica Basada en Probabilidad (AUC) ---\n")
print(results_prob)


# Unir los dos resultados en una tabla final.
final_metrics_lr <- bind_rows(results_class, results_prob)

cat("\n--- Tabla Final de Métricas ---\n")
print(final_metrics_lr)


# Generar la curva ROC
roc_curve_lr_corregido <- roc_curve(
  data = predictions_lr, 
  truth = clase_real, 
  prob_Yes, 
  event_level = "second"
)

# Extraer el valor del AUC de nuestra tabla final para el título
auc_corregido <- final_metrics_lr %>%
  filter(.metric == "roc_auc") %>%
  pull(.estimate)

# Dibujar el gráfico final
autoplot(roc_curve_lr_corregido) +
  labs(
    title = "Curva ROC Corregida - Modelo de Regresión Logística",
    subtitle = paste("AUC =", round(auc_corregido, 3))
  ) +
  theme_minimal()


#========================================================================
#             MEJORA DEL MODELO GLM: FASE 1 - RECETA AVANZADA
#========================================================================

# NOTA: Partimos de nuestro dataframe original 'datos' y los datos de entrenamiento 'train_data'

# 1. INGENIERÍA DE CARACTERÍSTICAS
# Creamos una nueva receta que añade características más complejas.
rec_avanzada <- recipe(Churn ~ ., data = train_data) %>%
  update_role(customerID, Segmento, new_role = "ID") %>%
  step_mutate(valor_promedio_mes = TotalCharges / (tenure + 1)) %>%
  step_interact(~ tenure:starts_with("Contract_")) %>%
  step_poly(MonthlyCharges, degree = 2) %>%
  step_poly(tenure, degree = 2) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.8)



#========================================================================
#             MEJORA DEL MODELO GLM: FASE 2 - RE-ENTRENAMIENTO
#========================================================================


# 1. CREAR UN NUEVO WORKFLOW CON LA RECETA AVANZADA
glm_workflow_avanzado <- workflow() %>%
  add_recipe(rec_avanzada) %>%
  add_model(log_reg_spec)

# 2. ENTRENAR EL NUEVO MODELO
glm_fit_avanzado <- fit(glm_workflow_avanzado, data = train_data)

# 3. EVALUAR EL NUEVO MODELO EN EL CONJUNTO DE PRUEBA
cat("\n--- Evaluando el modelo GLM avanzado... ---\n")
predictions_glm_avanzado <- predict(glm_fit_avanzado, new_data = test_data, type = "prob") %>%
  bind_cols(predict(glm_fit_avanzado, new_data = test_data)) %>%
  bind_cols(test_data %>% select(Churn))

colnames(predictions_glm_avanzado) <- c("prob_No", "prob_Yes", "clase_predicha", "clase_real")

# Calcular métricas (usando el umbral por defecto de 0.5 por ahora)
# (Reutilizamos el código de evaluación que ya teníamos)
class_metrics_avanzado <- predictions_glm_avanzado %>% class_metrics(truth = clase_real, estimate = clase_predicha, event_level = "second")
prob_metrics_avanzado <- predictions_glm_avanzado %>% roc_auc(truth = clase_real, prob_Yes, event_level = "second")
final_metrics_glm_avanzado <- bind_rows(class_metrics_avanzado, prob_metrics_avanzado)

cat("\n--- Tabla de Métricas (GLM Avanzado, umbral 0.5) ---\n")
print(final_metrics_glm_avanzado)

# Extraer el AUC para comparar
auc_avanzado <- final_metrics_glm_avanzado %>% filter(.metric == "roc_auc") %>% pull(.estimate)
cat("\nAUC Modelo Base:", round(auc_corregido, 4)) # auc_corregido es el AUC del primer GLM
cat("\nAUC Modelo Avanzado:", round(auc_avanzado, 4))

#========================================================================
#     MEJORA DEL MODELO GLM: FASE 3 - AJUSTE DEL UMBRAL
#========================================================================

# 1. CALCULAR LA CURVA ROC PARA EL MODELO AVANZADO
roc_curve_glm_avanzado <- roc_curve(
  data = predictions_glm_avanzado,
  truth = clase_real,
  prob_Yes,
  event_level = "second"
)

# 2. ENCONTRAR EL UMBRAL ÓPTIMO
umbral_optimo_j <- roc_curve_glm_avanzado %>%
  mutate(j_index = sensitivity + specificity - 1) %>%
  top_n(1, j_index)

cat("\nUmbral óptimo según J-Index:", umbral_optimo_j$.threshold)

# Opción B: Fijar un Recall mínimo (Ej: "Quiero capturar al menos al 80% de los que se van")
umbral_para_recall_80 <- roc_curve_glm_avanzado %>%
  filter(sensitivity >= 0.80) %>%
  top_n(1, specificity)

cat("\nUmbral para lograr ~80% de Recall:", umbral_para_recall_80$.threshold)


# 3. RE-EVALUAR MÉTRICAS CON EL UMBRAL ELEGIDO (ej. el de J-Index)
umbral_final <- umbral_optimo_j$.threshold

# Recalcular las clases predichas con el nuevo umbral
predictions_con_umbral <- predictions_glm_avanzado %>%
  mutate(clase_predicha_optima = factor(
    ifelse(prob_Yes > umbral_final, "Yes", "No"),
    levels = c("No", "Yes")
  ))

metricas_clase_optimas <- predictions_con_umbral %>%
  class_metrics(truth = clase_real, estimate = clase_predicha_optima, event_level = "second")

# Extraer la fila del AUC que ya teníamos del modelo avanzado
auc_row_avanzado <- final_metrics_glm_avanzado %>% 
  filter(.metric == "roc_auc")

# Unimos las métricas de clase con la fila del AUC para crear una tabla completa
metricas_finales_optimas_completas <- bind_rows(metricas_clase_optimas, auc_row_avanzado)

# Imprimimos la comparación lado a lado para el informe
cat("\n--- Comparación de Métricas: Umbral por Defecto vs. Umbral Optimizado ---\n\n")

cat("Con Umbral por Defecto (0.5):\n")
print(final_metrics_glm_avanzado)

cat("\nCon Umbral Optimizado (", round(umbral_final, 4), "):\n")
print(metricas_finales_optimas_completas) # Usamos la nueva tabla completa


#========================================================================
#     MODELO FINAL: RE-ENTRENAMIENTO CON TODOS LOS DATOS
#========================================================================


modelo_final_produccion <- fit(glm_workflow_avanzado, data = datos)


#========================================================================
#             DEFINICIÓN DEL UMBRAL DE DECISIÓN FINAL
#========================================================================

umbral_final_produccion <- umbral_optimo_j$.threshold

cat(paste("\nEl umbral de decisión final seleccionado para el despliegue es:", round(umbral_final_produccion, 4), "\n"))


#========================================================================
#             FIN DE LA TAREA 4: MODELADO
#========================================================================

# Guardamos los objetos clave para usarlos en las siguientes tareas.
# Extraer la receta final "preparada"
receta_final_preparada <- extract_recipe(modelo_final_produccion)

# Extraer el modelo glm final "ajustado"
modelo_glm_final_ajustado <- extract_fit_parsnip(modelo_final_produccion)

# Guardar los objetos para futuras sesiones
ruta_simple <- "C:/R_Projects/Internship"

if (!dir.exists(ruta_simple)) {
  dir.create(ruta_simple, recursive = TRUE)
}

objetos_a_guardar <- list(
  modelo = modelo_final_produccion,
  umbral = umbral_final_produccion
)

ruta_archivo_rds <- file.path(ruta_simple, "modelo_churn_final.rds")

saveRDS(objetos_a_guardar, file = ruta_archivo_rds)





########################
# Task 5
########################


# Cargamos el modelo final guardado
modelo_guardado <- readRDS("C:/R_Projects/Internship/modelo_churn_final.rds")
modelo_final_produccion <- modelo_guardado$modelo

# Extraemos el modelo ajustado
glm_fit_final <- extract_fit_parsnip(modelo_final_produccion)

# Gráfico de Importancia de Variables con vip
# Mostramos el signo para saber si el impacto es positivo o negativo
vip(glm_fit_final, num_features = 20, geom = "col", mapping = aes(fill = Sign)) +
  scale_fill_manual(values = c("POS" = "#F8766D", "NEG" = "#00BFC4"), name = "Impacto en el Churn") +
  labs(
    title = "Top 20 Factores Determinantes del Abandono de Clientes",
    subtitle = "Basado en la magnitud y signo de los coeficientes del modelo GLM",
    y = "Variable del Modelo",
    x = "Magnitud del Impacto (Importancia)"
  ) +
  theme_minimal(base_size = 14)
