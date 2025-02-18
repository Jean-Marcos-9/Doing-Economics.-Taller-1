                     # TALLER 1. HACIENDO ECONOMÍA. 
                            # PRIMERA PARTE
"""
Febrero de 2025
Jean Marcos Gonzalez Monroy

"""

library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)


# Definir la ruta donde se creará el directorio
ruta_directorio <- "C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs"

if (!dir.exists(ruta_directorio)) {
  dir.create(ruta_directorio, recursive = TRUE)
}
# Verificar que el directorio se creó correctamente
list.dirs("C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs")

"""
Vamos a selecciionar el uso de internet y los medios de pagos para responder la
pregunta de investigación.
"""

#PREGUNTA 411. USO DEL INTERNET-------------------------------------------------
# Cargar librerías necesarias

# Eliminar valores NA
ValBinarios <- na.omit(TenderosFU03_Publica$uso_internet)

# Convertir en factor
ValBinarios <- factor(ValBinarios, levels = c(0, 1), labels = c("No usa Internet", "Usa Internet"))

# Crear tabla de frecuencias como data frame
tabla_frecuencias <- as.data.frame(table(ValBinarios))

# Generar un Gráfico
ggplot(tabla_frecuencias, aes(x = ValBinarios, y = Freq, fill = ValBinarios)) +
  geom_bar(stat = "identity", width = 0.4) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3) +  # Añadir etiquetas con los valores
  scale_fill_manual(values = c("blue", "red")) +  # Definir colores
  labs(title = "Distribución del Uso de Internet en Tiendas de Barrio",
       x = "Uso de Internet",
       y = "Frecuencia") +
  theme_minimal()
ruta_directorio <- "C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs\\grafico_uso_internet.png"
ggsave(ruta_directorio, width = 8, height = 6, dpi = 300)


  #PREGUNTA 805. TIPO DE MEDIOS DE PAGO-------------------------------------------

#Seleccionamos las variables relevantes de toda la base de datos.
Data1 <- TenderosFU03_Publica %>% 
  select(medios_pago__3,medios_pago__2,medios_pago__1,medios_pago__10,medios_pago__8,medios_pago__8,medios_pago__5,medios_pago__4,medios_pago__9,medios_pago_otro)
Data <- Data %>% mutate(across(starts_with("medios_pago__"), ~ as.numeric(unclass(.x))))

# Organizamos los datos para graficar ya que son variables dummies.
frecuencia_pagos <- Data %>%
  summarise(
    Efectivo = sum(medios_pago__3 == 0, na.rm = TRUE),
    `Tarjeta Debito` = sum(medios_pago__2 == 0, na.rm = TRUE),
    `Tarjeta Credito` = sum(medios_pago__1 == 0, na.rm = TRUE),
    `Billeteras Digitales` = sum(medios_pago__10 == 0, na.rm = TRUE),
    `Pagos Link` = sum(medios_pago__8 == 0, na.rm = TRUE),
    `Giros No Bancarios` = sum(medios_pago__5 == 0, na.rm = TRUE),
    Transferencias = sum(medios_pago__4 == 0, na.rm = TRUE),
    Otro = sum(medios_pago__9 == 0, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Medio_Pago", values_to = "Frecuencia") %>%
  mutate(Medio_Pago = rev(Medio_Pago))  

# Crear gráfico de barras. 
ggplot(frecuencia_pagos, aes(x = fct_reorder(Medio_Pago, Frecuencia), y = Frecuencia, fill = Medio_Pago)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Frecuencia de Uso de Medios de Pago en Tiendas de Barrio",
    x = "Medio de Pago",
    y = "Número de Tiendas"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")  # 🔹 Eliminar la leyenda
ruta_directorio <- "C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs\\grafico_Medios de pago.png"
ggsave(ruta_directorio, width = 8, height = 6, dpi = 300)
