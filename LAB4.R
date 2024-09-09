# Librerías necesarias
library(dplyr)
library(ggplot2)
library(caret)
library(forecast)

# Cargar los datos
data <- read_csv("C:/Users/barri/Documents/UFM/2024/Segundo Semestre/Data Wrangling/Analisis de Flujo de Caja y Gestión/tabla_completa.csv", locale = locale(encoding = "latin1"))



# --------------------------
# 1. Análisis General: Viajes Mensuales
# --------------------------

# Resumir los viajes por mes
viajes_mes <- data %>%
  group_by(MES) %>%
  summarise(viajes_totales = n_distinct(COD_VIAJE))

# Gráfico de viajes por mes
ggplot(viajes_mes, aes(x = MES, y = viajes_totales)) +
  geom_bar(stat = "identity") +
  labs(title = "Viajes por Mes", x = "Mes", y = "Número de Viajes") +
  theme_minimal()

# --------------------------
# 2. Impacto en el Flujo de Caja por Ajuste en Días de Crédito
# --------------------------

# Resumir la información por cliente: días de crédito promedio y total de ingresos
clientes_credito <- data %>%
  group_by(CLIENTE) %>%
  summarise(dias_credito_promedio = mean(CREDITO, na.rm = TRUE),  # Días de crédito promedio
            ingreso_total = sum(Q, na.rm = TRUE))  # Ingreso total generado

# Nueva política de días de crédito: reducir 15 días
nueva_politica_credito <- 15

# Calcular el impacto en el flujo de efectivo al reducir los días de crédito
clientes_credito <- clientes_credito %>%
  mutate(diferencia_dias_credito = dias_credito_promedio - nueva_politica_credito,
         mejora_flujo_efectivo = (diferencia_dias_credito / 365) * ingreso_total)

# Visualizar el impacto en el flujo de efectivo por cliente
ggplot(clientes_credito, aes(x = reorder(CLIENTE, -mejora_flujo_efectivo), y = mejora_flujo_efectivo)) +
  geom_bar(stat = "identity") +
  labs(title = "Impacto de la Reducción de Días de Crédito en el Flujo de Efectivo", 
       x = "Cliente", y = "Mejora en Flujo de Efectivo (Q)") +
  theme_minimal() +
  coord_flip()

# --------------------------
# 3. Desempeño por Vehículo: Ingreso y Cantidad Transportada
# --------------------------

# Resumir información de ingresos y cantidad transportada por tipo de vehículo
vehiculos_ingreso <- data %>%
  group_by(UNIDAD, MES) %>%
  summarise(ingreso_total = sum(Q), cantidad_transportada = sum(CANTIDAD))

# Gráfico: Ingreso por vehículo
ggplot(vehiculos_ingreso, aes(x = MES, y = ingreso_total, fill = UNIDAD)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ingresos por Vehículo", x = "Mes", y = "Ingreso Total (Q)") +
  theme_minimal()

# Gráfico: Cantidad transportada por vehículo
ggplot(vehiculos_ingreso, aes(x = MES, y = cantidad_transportada, fill = UNIDAD)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cantidad Transportada por Vehículo", x = "Mes", y = "Cantidad Transportada") +
  theme_minimal()

# --------------------------
# 4. Clientes Más Importantes (80-20)
# --------------------------

# Calcular ingresos acumulados y porcentaje acumulado (80-20)
cliente_ingreso <- data %>%
  group_by(CLIENTE) %>%
  summarise(ingreso_total = sum(Q)) %>%
  arrange(desc(ingreso_total)) %>%
  mutate(porcentaje_acumulado = cumsum(ingreso_total) / sum(ingreso_total) * 100)

# Filtrar clientes más importantes (los que generan el 80% de los ingresos)
clientes_80_20 <- cliente_ingreso %>%
  filter(porcentaje_acumulado <= 80)

# Gráfico: Clientes más importantes (80-20)
ggplot(clientes_80_20, aes(x = reorder(CLIENTE, -ingreso_total), y = ingreso_total)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Clientes más Importantes (80-20)", x = "Cliente", y = "Ingreso Total (Q)") +
  theme_minimal()

# --------------------------
# 5. Relación entre Crédito Total e Ingreso Generado
# --------------------------

# Relación entre crédito total otorgado e ingreso generado por cliente
credito_ingreso_cliente <- data %>%
  group_by(CLIENTE) %>%
  summarise(credito_total = sum(CREDITO), ingreso_total = sum(Q)) %>%
  arrange(desc(ingreso_total))

# Gráfico: Relación entre crédito otorgado e ingreso
ggplot(credito_ingreso_cliente, aes(x = credito_total, y = ingreso_total)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relación entre Crédito Total e Ingreso Generado", x = "Crédito Total (Q)", y = "Ingreso Total (Q)") +
  theme_minimal()

# --------------------------
# 6. Clientes con Mayor Riesgo de Crédito
# --------------------------

# Cálculo de la relación entre crédito total otorgado y el ingreso generado
clientes_riesgo <- data %>%
  group_by(CLIENTE) %>%
  summarise(dias_credito_promedio = mean(CREDITO, na.rm = TRUE),  # Días de crédito promedio
            ingreso_total = sum(Q, na.rm = TRUE),  # Ingreso total generado
            credito_total = sum(CREDITO, na.rm = TRUE))  # Crédito total otorgado

# Añadir una columna con la relación crédito/ingreso
clientes_riesgo <- clientes_riesgo %>%
  mutate(riesgo_credito_ingreso = credito_total / ingreso_total)

# Filtrar clientes de mayor riesgo (días de crédito > 60 o relación crédito/ingreso > 0.5)
clientes_riesgo_filtrados <- clientes_riesgo %>%
  filter(dias_credito_promedio > 60 | riesgo_credito_ingreso > 0.5)

# Gráfico: Clientes con mayor riesgo de crédito
ggplot(clientes_riesgo_filtrados, aes(x = reorder(CLIENTE, -riesgo_credito_ingreso), y = riesgo_credito_ingreso)) +
  geom_bar(stat = "identity") +
  labs(title = "Clientes con Mayor Riesgo de Crédito", x = "Cliente", y = "Relación Crédito/Ingreso") +
  theme_minimal() +
  coord_flip()


# Resumir la información por cliente: días de crédito promedio y total de ingresos
clientes_credito <- data %>%
  group_by(CLIENTE) %>%
  summarise(dias_credito_promedio = mean(CREDITO, na.rm = TRUE),  # Días de crédito promedio
            ingreso_total = sum(Q, na.rm = TRUE))  # Ingreso total generado

# Nueva política de días de crédito: reducir 15 días
nueva_politica_credito <- 15

# Calcular el impacto en el flujo de efectivo al reducir los días de crédito
clientes_credito <- clientes_credito %>%
  mutate(diferencia_dias_credito = dias_credito_promedio - nueva_politica_credito,
         mejora_flujo_efectivo = (diferencia_dias_credito / 365) * ingreso_total)

# Visualizar el impacto en el flujo de efectivo por cliente
ggplot(clientes_credito, aes(x = reorder(CLIENTE, -mejora_flujo_efectivo), y = mejora_flujo_efectivo)) +
  geom_bar(stat = "identity") +
  labs(title = "Impacto de la Reducción de Días de Crédito en el Flujo de Efectivo", 
       x = "Cliente", y = "Mejora en Flujo de Efectivo (Q)") +
  theme_minimal() +
  coord_flip()







