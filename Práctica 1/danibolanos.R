# Alumno: Daniel Bolaños Martínez
# Asignatura: Estadística Computacional

#Ejercicios propuestos 1
  
DosDados = function(Mostrar=F, Maximo=1000, Total=1)
  {
    ###############################################################
    # 
    # Experimentamos con el numero de lanzamientos necesarios
    # para obtener un dos con dos dados
    #
    # Pone el contador de aciertos a 0
    # Hacemos 2 lanzamientos y sumamos el resultado
    # Incrementamos el numero de lanzamientos
    # Si no es un 2 vuelve al principio del bucle
    # Si es un 2 se incrementa el contador de aciertos
    # Si conseguimos el total de aciertos requeridos salimos del bucle
    #
    ###############################################################
    
    Lanzamientos = 0
    Resultado = 1:Maximo
    Aciertos = 0 
    repeat{
      Lanzamientos = Lanzamientos + 1
      if (Maximo <= Lanzamientos ){
        if(Mostrar){
          cat("No he podido obtener ", Total , "acierto(s) en ", Lanzamientos, "lanzamientos. \n")
        }
        return(list(E = NA, R = Resultado, Conseguido=F))
      }
      TiraPrimerDado = sample(6, 1, T)
      TiraSegundoDado = sample(6, 1, T)
      
      Resultado[Lanzamientos] = TiraPrimerDado + TiraSegundoDado
      if(Resultado[Lanzamientos] != 2){next}
      
      Aciertos = Aciertos + 1
      
      if(Aciertos == Total){break}
    }
    length(Resultado) = Lanzamientos
    
    if(Mostrar){
      cat("He necesitado", Lanzamientos, "lanzamientos para obtener", Total, "acierto(s) en el experimento\n")
    }
    
    return(list(E = Lanzamientos, R = Resultado, Conseguido=T))
  }

LanzaMoneda = function(Mostrar=F, Maximo=10, Total=1)
{
  
  ###############################################################
  # 
  # Experimentamos con el numero de caras seguidas
  # obtenidas en el lanzamiento de una moneda
  #
  # Pone el contador de aciertos a 0
  # Hacemos 1 lanzamiento 0 cruz 1 cara
  # Incrementamos el numero de lanzamientos
  # Si sale 1 cara se incrementa el contador de aciertos
  # Si sale 1 cruz los aciertos pasan a 0
  # Si conseguimos el total de aciertos seguidos salimos del bucle
  #
  ###############################################################
  
  Lanzamientos = 0
  Resultado = 1:Maximo
  Aciertos = 0 
  repeat{
    Lanzamientos = Lanzamientos + 1
    if (Maximo <= Lanzamientos){
      if(Mostrar){
        cat("No he podido obtener", Total , "caras en", Lanzamientos, "lanzamientos. \n")
      }
      return(list(E = NA, R = Resultado, Conseguido=F))
    }
    LanzaMoneda = sample(c(0,1), 1, T)
    
    Resultado[Lanzamientos] = LanzaMoneda
    if(Resultado[Lanzamientos] == 1){
      Aciertos = Aciertos + 1
    }
    
    else{
      Aciertos = 0
    }
    
    if(Aciertos == Total){break}
  }
  length(Resultado) = Lanzamientos
  
  if(Mostrar){
    cat("He necesitado", Lanzamientos, "lanzamientos para obtener", Total, "caras seguidas\n")
  }
  
  return(list(E = Lanzamientos, R = Resultado, Conseguido=T))
}


DistriDados = function(n=10, Maximo=100, Total=1)
{
  Lanzo = vector(length=n)
  for(i in 1:n)
    Lanzo[i] = DosDados(F,Maximo,Total)$E
  Lanzo
}

DistriMoneda = function(n=10, Maximo=20, Total=2)
{
  Lanzo = vector(length=n)
  for(i in 1:n)
    Lanzo[i] = LanzaMoneda(F,Maximo,Total)$E
  Lanzo
}

Distribucion1 = DistriDados()

summary(Distribucion1)

hist(Distribucion1)

Distribucion2 = DistriMoneda()

summary(Distribucion2)

hist(Distribucion2)

#Ejercicios propuestos 2
  
datos1 = read.csv("http://www.ugr.es/local/andresgc/Datos.txt",header=T, sep=",")

datos2 = read.table("http://www.ugr.es/local/andresgc/Datos2.txt",header=T)

summary(datos1)

summary(datos2)

hist(datos1$Altura, main="Altura datos 1")

hist(datos2$Peso, main="Peso datos 2")

barplot(datos1$Peso)

plot(datos1)

boxplot(datos1)

barplot(datos2$Altura)

plot(datos2)

boxplot(datos2)

#Ejercicios propuestos 3
  
library()

library("parallel")

search()

library(help="parallel")

help("mclapply")

detach(pos=2)

search()
