#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Calculadora de rendimientos financieros #
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Definición de varables ######################################################

int_bru <- .0003252572 # interes bruto
año <- 365    # años o unidad temporal
cap <- 60044.79   # capital inicial
infl <- .00    # inflación
imp <- .00     # impuestos
admin <- .00   # comisiones por administración

# Otras variable
cap_inicial <- cap
int <- int_bru - (infl + imp + admin) #interés real

# Cálculos de capital #########################################################

# Cálculo de capital + diferencia parcial
i <- 0
dif <- 0
dif_par <- c()
capital <- c()

while (i <= año){
  
  dif_par <- c(dif_par, dif)
  dif <- (cap * int)
  
  capital <- c(capital, cap)
  cap <- cap + (cap * int)
  
  i <- i + 1
}

print(capital[año+1])

# Diferencia total + rendimiento acumulado
dif_total <- c()
dif <- 0
rend <- 0
rend_cum <- c()

for (val in capital) {
  
  dif <- val - cap_inicial
  dif_total <- c(dif_total, dif)
  
  rend <- dif / cap_inicial
  rend_cum <- c(rend_cum, rend)
}

print(rend)

# Tablas ######################################################################
años <- c(0:año)
data.frame(años, capital, dif_par, dif_total, rend_cum)
print(int)
print(capital[año+1])
print(rend)
