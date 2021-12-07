# M?ster -> Detecci?n de anomal?as
# Juan Carlos Cubero. Universidad de Granada

install.packages("devtools")
library(devtools)
install.packages("ggplot2")

# ggbiplot:
install_github("vqv/ggbiplot")  # Si pregunta, seleccione la opci?n 3.

# El directorio en el que se instale no debe tener caracteres como acentos, ?, etc.
# 
# Si no funciona lo anterior, probar alguna de las siguientes:
# devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")
# install_github("ggbiplot", "vqv")
# install_github("vqv/ggbiplot", ref = "experimental")

install.packages("fitdistrplus")  # Ajuste de una distribuci?n
install.packages("reshape")       # melt
install.packages("tidyverse")
install.packages("outliers")      # Grubbs
install.packages("CerioliOutlierDetection")  #MCD Hardin Rocke estimaci?n robusta de la matriz de covarianzas
install.packages("mvoutlier")     # corr.plot , MCD ChiC 
install.packages("mvnormtest")    # mvn. Test Normalidad multivariante
install.packages("MVN")           
install.packages("DDoutlier")     # lof
install.packages("cluster")       # pam
