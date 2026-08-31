#Primeiro eu criei um arquivo de texto pelo bloco de notas com as casas decimais de pi.
#Em sequiga deparei termo a termo e a transformei em uma lista numérica.

numeros_pi <- readLines("C:/Users/lcex.CCE-313629/Documents/numeros_pi.txt")
pedacos <- strsplit(numeros_pi,split='')
numero <- as.numeric(unlist(pedacos))

#Separei a lista de valores em uma lista de frequências para identificar a frequenciade cada termo.
#Eu sei que a forma que eu fiz está horrível, tive um problema com o laço for, 
#aparentemente tinha alguma outra função neste comptador com o mesmo nome 
#do for e estva dando erro qunado eu usava, demorei descobrr a causa e fritei meu cerébro tentando resolver.


f_0 <- 0
f_1 <- 0
f_2 <- 0
f_3 <- 0
f_4 <- 0
f_5 <- 0
f_6 <- 0
f_7 <- 0
f_8 <- 0
f_9 <- 0
  
for(i in 1:1000000){
   if(numero[i]==0){
     f_0 <- f_0+1
   }
  if(numero[i]==1){
    f_1 <- f_1+1
  }
  if(numero[i]==2){
    f_2 <- f_2+1
  }
  if(numero[i]==3){
    f_3 <- f_3+1
  }
  if(numero[i]==4){
    f_4 <- f_4+1
  }
  if(numero[i]==5){
    f_5 <- f_5+1
  }
  if(numero[i]==6){
    f_6 <- f_6+1
  }
  if(numero[i]==7){
    f_7 <- f_7+1
  }
  if(numero[i]==8){
    f_8 <- f_8+1
  }
  if(numero[i]==9){
    f_9 <- f_9+1
  }

}

vetor_f <- c(f_0,f_1,f_2,f_3,f_4,f_5,f_6,f_7,f_8,f_9)
vetor_f

#Após gerar o vetor com as frequencias, gerei uma matriz disto.

matriz_f<- matrix(vetor_f,nrow = 1,ncol=10)
matriz_f

mean(numero)








