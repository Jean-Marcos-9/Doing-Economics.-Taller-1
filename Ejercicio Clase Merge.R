
                       #TALLER 1. HACIENDO ECONOMÍA. 
                              #SEGUNDA PARTE
"""

Febrero de 2025
Jean Marcos Gonzalez Monroy

"""

library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(haven)


#PRIMERO SELECCIONAMOS LAS VARIABLES RELEVANTES DE LA BASE DE DATOS DE LA ENCUESTA



Base1 <- TenderosFU03_Publica %>% 
  select(Municipio, actG1, actG2, actG3, actG4, actG5, actG6, actG7, actG8, actG9, actG10, actG11, uso_internet) %>%
  mutate(uso_internet = as.numeric(zap_labels(uso_internet)))

"""
Vamos a sacar una base de datos con toda la información general pero con las 
tiendas que si tienen internet, para luego sacar una base de datos general con
todas lsa tiendas, para llegar a sacar un porcentaje.

"""

#Base con las tiendas que tienen internet
Filtro = Base1 %>% 
  filter(uso_internet==1)
Internet = Filtro %>% 
  group_by(Municipio) %>% 
  summarise( "Total Tiendas" = sum(actG1),"Total Comida preparada" = sum(actG2),
             "Total Peluquería y belleza" = sum(actG3),"Total Ropa" = sum(actG4),"Total Variedades"
             = sum(actG5),"Total Papeleria y comunicaciones" = sum(actG6), "Total Vida nocturna" = sum(actG7), "Total Productos de bajo inventario" = sum(actG8),
             "Total Salud" = sum(actG9),"Total Servicios" = sum(actG10), "Total Ferreteria y afines"
             = sum(actG11), "Uso internet" = sum(uso_internet))

# Bases con todas las tiendas (con y sin internet).
tiendasMun = Base1 %>% 
  group_by(Municipio) %>% 
  summarise( "Total Tiendas" = sum(actG1),"Total Comida preparada" = sum(actG2),
             "Total Peluquería y belleza" = sum(actG3),"Total Ropa" = sum(actG4),"Total Variedades"
             = sum(actG5),"Total Papeleria y comunicaciones" = sum(actG6), "Total Vida nocturna" = sum(actG7), "Total Productos de bajo inventario" = sum(actG8),
             "Total Salud" = sum(actG9),"Total Servicios" = sum(actG10), "Total Ferreteria y afines"
             = sum(actG11), "Uso internet" = sum(uso_internet))

#UNIMOS LAS BASES DE DATOS SIN INTERNET Y LA QUE TIENE DATOS COMPLETOS.
# x para la cantidad con internet. y para la cantidad con y sin internet.
Base_temp <- merge(Internet, tiendasMun, by = "Municipio", all = TRUE)  

# SACAMOS EL PORCENTAJE DE LAS TIENDAS CON INTERNET, DIVIDIENDO LAS QUE SI 
#CON LAS QUE TIENEN EL TOTAL.
Base_total <- Base_temp %>% 
  mutate(
    pct_tiendas = `Total Tiendas.x` / `Total Tiendas.y`,
    pct_comida = `Total Comida preparada.x` / `Total Comida preparada.y`,
    pct_belleza = `Total Peluquería y belleza.x` / `Total Peluquería y belleza.y`,
    pct_ropa = `Total Ropa.x` / `Total Ropa.y`,
    pct_variedades = `Total Variedades.x` / `Total Variedades.y`,
    pct_papeleria = `Total Papeleria y comunicaciones.x` / `Total Papeleria y comunicaciones.y`,
    pct_nocturna = `Total Vida nocturna.x` / `Total Vida nocturna.y`,
    pct_inventario = `Total Productos de bajo inventario.x` / `Total Productos de bajo inventario.y`,
    pct_salud = `Total Salud.x` / `Total Salud.y`,
    pct_servicios = `Total Servicios.x` / `Total Servicios.y`,
    pct_ferreteria = `Total Ferreteria y afines.x` / `Total Ferreteria y afines.y`
  )

# SACAMOS UNA GRÁFICA CON LOS PORCENTAJES.
ggplot(Base_total, aes(x = Municipio, y = pct_tiendas)) +
  geom_bar(stat = "identity", fill = "gray") +  
  geom_text(aes(label = scales::percent(pct_tiendas, accuracy = 0.1)),  
            vjust = -0.5, size = 2, color = "black") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  labs(title = "Porcentaje de tiendas con internet por municipio",
       y = "Porcentaje",
       x = "Municipio") +
  scale_y_continuous(labels = scales::percent) 
ruta_directorio <- "C:\\Users\\Marcos Gonzalez\\OneDrive\\Escritorio\\UR\\Trabajos\\Sexto Semestre\\Haciendo Economía\\Taller 1\\Outputs\\Gráfica porcentajes.png"
ggsave(ruta_directorio, width = 8, height = 6, dpi = 300)

#Para usar la base de datos de TerriData necesitamos extraer los documentos de 
#una carpeta zip.
ruta <- "C:/Users/Marcos Gonzalez/OneDrive/Escritorio/UR/Trabajos/Sexto Semestre/Haciendo Economía/Taller 1/Datos/TerriData_Dim2.xlsx.zip"
unzip(ruta, exdir = "C:/Users/Marcos Gonzalez/OneDrive/Escritorio/UR/Trabajos/Sexto Semestre/Haciendo Economía/Taller 1/Datos")

# Renombramos la variable de los municipios para que el merge se logre hacer.
TerriData_Dim2_Sub3. <- TerriData_Dim2_Sub3 %>% rename(Municipio = Entidad)

"""
Hacemos flitros con la información relevante, seleccionamos las columnas que 
necesitamos y modificamos la columna de Unidad de medida para sacar el total de
la población.

"""
poblacion_maso <- TerriData_Dim2_Sub3. %>% 
  filter(Año == 2022, 
         `Unidad de Medida` %in% c("Hombres", "Mujeres"), 
         Municipio %in% c("Bello", "Barranquilla", "Bogotá", "Soacha", 
                          "Girardot", "Zipaquirá", "Neiva", "Pereira", 
                          "Bucaramanga", "Ibagué")) %>% 
  select(`Código Departamento`, `Código Entidad`, Municipio, `Dato Numérico`, 
         Año, `Unidad de Medida`) %>%
  pivot_wider(names_from = `Unidad de Medida`, values_from = `Dato Numérico`) %>%
  mutate(
    Hombres = parse_number(Hombres),  
    Mujeres = parse_number(Mujeres),  
    Total_Poblacion = Hombres + Mujeres
  )

# Por último hacemos el merge de la base de datos de la población con la de tiendas.
Base_total <- merge(Base_temp, poblacion_maso, by = "Municipio", all = TRUE) 


                   # Guardar y sacar mi base de datos nueva

install.packages("writexl")
library(writexl)
write_xlsx(Base_total, "C:/Users/Marcos Gonzalez/OneDrive/Escritorio/UR/Trabajos/Sexto Semestre/Haciendo Economía/Taller 1/Outputs/Mi_Base_total.xlsx")
