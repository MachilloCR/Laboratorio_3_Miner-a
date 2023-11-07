##################################################
#-------      Esteban Alvarez Esquivel    -------#
##################################################

install.packages(c("readxl", "dplyr"))
library(dplyr)
library(readxl)
library(ggplot2)
# Especifica la ruta de tu archivo Excel
ruta_del_archivo <- "files/estadsticaspoliciales2023.xlsx"

# Leer el archivo Excel
datos_excel <- read_excel(ruta_del_archivo)
datos_excel_sinmod <- read_excel(ruta_del_archivo)

#------- Exploración Inicial de los Datos -------#
##################################################
View(datos_excel)
head(datos_excel)
tail(datos_excel)

# ver datos detallados
str(datos_excel)
summary(datos_excel)

# nombres de columnas
nombres_columnas <- names(datos_excel)

#-------  Limpieza y Tratamiento de Datos -------#
##################################################

valores_nulos <- sapply(datos_excel, is.na)
print(valores_nulos)
# cambiar datos nulos
datos_excel[is.na(datos_excel)] <- 0
# cambia los datos tipo chr nulos por desconocido de las columnas
columnas_chr <- c("Edad","Delito", "SubDelito", "Victima", "SubVictima", "Genero", "Nacionalidad", "Provincia", "Canton", "Distrito")
datos_excel[columnas_chr][is.na(datos_excel[columnas_chr])] <- "Desconocido"
# cambia los datos nulos de eventos por 0
columnas_num <- c("Eventos")
datos_excel[columnas_num][is.na(datos_excel[columnas_num])] <- 0



frecuencia <- table(datos_excel$Delito)
# Contar la frecuencia de los delitos por tipo
delitos_por_tipo <- datos_excel %>% 
  group_by(Delito) %>% 
  summarize(Cantidad = n())



# Ordenar la tabla por frecuencia descendente
delitos_por_tipo <- delitos_por_tipo %>% arrange(desc(Cantidad))

# grafico de delitos por tipo
ggplot(data = delitos_por_tipo, aes(x = Cantidad, y = Delito)) +
  geom_bar(stat = "identity") +
  labs(x = "Frecuencia", y = "Tipo de Delito") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

boxplot(delitos_por_tipo$Cantidad, main="Eventos")

# 
#-------    visualización de datos       -------#
##################################################
# Contar la frecuencia de los delitos por genero
delitos_por_genero <- datos_excel %>%
  group_by(Genero) %>%
  summarize(Cantidad = n())
# gráfico de delitos por genero
dpg_plot<- barplot(delitos_por_genero$Cantidad, beside = TRUE, 
                   col = c("blue", "red"), 
                   names.arg = delitos_por_genero$Genero, 
                   xlab = "Género", 
                   ylab = "Cantidad de Delitos", 
                   border = "black", 
                   main = "Cantidad de Delitos por Género")
text(x = dpg_plot, 
     y = delitos_por_genero$Cantidad + 1, 
     labels = delitos_por_genero$Cantidad, 
     col = "black")

