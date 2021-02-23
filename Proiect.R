
#1
constanta <- function(f) 
{
  temp <- cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")
  if(temp$integral==0)
    stop("Constanta nu exista")
  return (1/temp$integral)
}

constanta(f)

#2
e_densitate <- function(f)
{
  temp <- cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")
  if(!(temp$integral+temp$error>=1 && temp$integral-temp$error<=1)) #Daca integrala nu e aproximativ 1
    return (F)
  
  for (i in seq(-10000,10000,0.003))
    if(f(i)<0)
      return(F)
    
  return (T)
}

e_densitate(f)


#3 Are sens doar impreuna cu alte cerinte (vezi sa fie restul de functii in env cand compilezi)
check_VC <- function(object)
{ errors <- character()
  g <- object@f
  temp <- cubintegrate(g, lower = -Inf, upper = Inf, method = "pcubature")
  if(!(temp$integral+0.001>=1 && temp$integral-0.001<=1)) #Daca integrala nu e aproximativ 1
  {
    msg <- paste("Valoarea integralei este aproximativ ", temp$integral, ".  Ar trebui sa fie 1.", sep = "")
    errors <- c(errors, msg)
    return (errors)
  }
  
  for (i in seq(-1000,1000,0.003))
    if(g(i)<0)
    {
      msg <- paste("Densitatea nu poate lua valori negative")
      errors <- c(errors, msg)
      return (errors)
    }
  
  return (TRUE)
}


setClass("VC",representation(f="function"),validity = check_VC)

X <-new("VC",f=function(x){x})


setGeneric("constanta", valueClass = "numeric", function(object){
  standardGeneric("constanta")
})
setMethod("constanta",signature(object="VC"),function(object)
{
  constanta(object@f)
}
  )

setGeneric("medie", valueClass = "numeric", function(object){
  standardGeneric("medie")
})
setMethod("medie",signature(object="VC"),function(object)
{
  medie(object@f)
}
)

setGeneric("moment", valueClass = "numeric", function(object,rang){
  standardGeneric("moment")
})
setMethod("moment",signature(object="VC"),function(object,rang)
{
  moment_centrat(object@f,rang)
}
)

setGeneric("dispersie", valueClass = "numeric", function(object){
  standardGeneric("dispersie")
})
setMethod("dispersie",signature(object="VC"),function(object)
{
  moment_centrat(object@f,2)
}
)

setGeneric("moment_initial", valueClass = "numeric", function(object){
  standardGeneric("moment_initial")
})
setMethod("moment_initial",signature(object="VC"),function(object)
{
  moment_initial(object@f)
}
)

setGeneric("moment_centrat4", valueClass = "numeric", function(object){
  standardGeneric("moment_centrat4")
})
setMethod("moment_centrat4",signature(object="VC"),function(object)
{
  moment_centrat4(object@f)
}
)

setGeneric("medie2", valueClass = "numeric", function(object){
  standardGeneric("medie2")
})
setMethod("medie2",signature(object="VC"),function(object)
{
  medie2(object@f)
}
)

setGeneric("dispersie2", valueClass = "numeric", function(object){
  standardGeneric("dispersie2")
})
setMethod("dispersie2",signature(object="VC"),function(object)
{
  dispersie2(object@f)
}
)

plot2 <- function(f)
{
  interval <- seq(-1000,1000,0.1)
  plot(interval, lapply(interval,f))
}

setGeneric("plotting", valueClass = "numeric", function(object){
  standardGeneric("plotting")
})
setMethod("plotting",signature(object="VC"),function(object)
{
  plot2(object@f)
}
)

test <- function(x)
{
  dnorm(x)
}

f1a <- function(x)
{
  if(x>=0 && x<=pi)
    return(sin(x)/2)
  return(0)
}

f1b <- function(x)
{
  if(x>=0 && x<=pi/2)
    return(cos(x))
  return(0)
}


X<- new("VC",f=f1a)
X<- new("VC",f=f1b)
showMethods(class="VC")
constant(X)

#4 && 8
# REPARTITIA EXPONENTIALA

d_exp <- function (x,lambda)
{
  
  lambda * exp (-lambda*x)
}

r_exp <- function (x,lambda)
{
  1-((exp)(-lambda*x))
}

media_exp <- function(x,lambda)
{
  1/lambda
}

mediana_exp <- function (x,lambda)
{
  log(2)/lambda
}

var_exp <- function (x,lambda)
{
  1/(lambda^2)
}

#REPREZENTAREA GRAFICA

info_exp <- function()
{
  x<-seq(1,10,by=0.01) 
  plot(x,d_exp(x,0.5),type="l",ylab="PDF exp")
  plot(x,r_exp(x,1.0),type="l",ylab="CDF exp")
  
  print("Repartitia exponentiala, notata exponential(lambda) sau exp(lambda), este distributia de probabilitate a timpului dintre evenimente intr-un proces de tip punct Poisson(proces in care evenimentele se petrec intr-un mod continuu si independent la o rata medie constanta). Este un caz particular al repartitiei Gamma.Parametrul lambda (numar pozitiv) se numeste parametru de scala si ne arata cat de rapid se petrece scaderea functiei exponentiale. Domeniul de valori al acestei repartitii este [0,oo). O proprietate importanta a repartitiei exponentiale este legata de probabilitatile conditionate si anume, lipsa de memorie.Spre exemplu, presupunem ca probabilitatea ca un taxi sa soseasca in primele 5 minute este p. Daca astept 5 minute si e fapt nu soseste niciun taxi, atunci probabilitatea ca un taxi sa soseasca in urmatoarele 5 minute este tot p.Repartitia exponentiala este folosita in problemele practice in care suntem interesati de timpul scurs pana la aparitia unui eveniment. Exemple: timpul pana la sosirea primului autobuz in statie, timpul pana la primul cutremur, timpul pana la sosirea primului client intr-un magazin etc.")
  
  
}

#REPARTITIA UNIFORMA

# dunif(x,a,b) ne ofera PDF pentru repartitia uniforma, unde x reprezinta vectorul de quantile, iar a si b parametrii repartitiei, a<b

media_unif <- function(x,a,b)
{
  (a+b)/2
}

mediana_unif <- function(x,a,b)
{
  (a+b)/2
}

var_unif <- function (x,a,b)
{
  ((b-a)^2)/12
}



info_uniform <- function()
{
  x1 <- seq(0,100,by=1)
  y1 <- dunif(x1,10,50)
  plot(y1,type="l",xlab="x",ylab="density")
  
  
  x2<- seq(-1,2,by=0.001)
  y2 <- punif(x2,1,3)
  plot(x2,y2, type="l",ylab="distribution")
  print("Repartitia uniforma, notata uniform(a,b) sau U(a,b), reprezinta o familie de distributii de probabilitate simetrice. Acest tip de repartitie descrie un experiment unde exista un rezultat arbitrar care se afla intre anumite limite. Aceste limite sunt reprezentate de parametrii a si b, ce indica valoarea minima, respectiv valoarea maxima. Diferen??a dintre limite define??te lungimea intervalului; toate intervalele de aceea??i lungime pe suportul distribu??iei sunt la fel de probabile. Repartitia uniforma se ocupa cu evenimentele care au aceeasi probabilitate de a se petrece. Prin urmare, exista numeroase aplicatii in care acest tip de repartitie poate fi folosita: situatii de testare a ipotezelor, cazuri de esantionare aleatorie, finante etc. Cateva exemple de probleme: rezultatul jocului la o ruleta care are n sloturi, generarea de numere pseudo-aleatoare, probabilitatea ca un bebelus sa zambeasca mai mult de 12 secunde, stiind ca acesta zambeste mai mult de 8 secunde etc.")
  
  
}


#REPARTITIA NORMALA (REPARTITIA GAUSSIANA)
#dnorm -  densitatea, pnorm - distributia
#dnorm(miu, sigma^2)

d_norm <- function(x,miu, sigma_p)
{
  sigma <- sqrt(sigma_p)
  (1/(sigma*(sqrt(2*pi)))) * exp((-(x-miu)^2)/(2*sigma_p))
}

media_norm <- function (x,miu,sigma_p)
{
  miu
}

mediana_norm <- function (x,miu,sigma_p)
{
  miu
}

var_norm <- function (x,miu,sigma_p)
{
  sigma_p
}



#nu avem o formula exacta pentru CDF_norm, se calculeaza cu pnorm (aceasta este folosita si pentru aproximare)



info_norm <- function()
{
  x3 <- seq (-10,10,by=0.1)
  val <- d_norm(x3,2.5,0.5)
  plot(x3,val,type="l",ylab="PDF norm")
  y4 <- pnorm(x3,mean=2.5,sd=0.5)
  plot(x3,y4,type="l",ylab="CDF norm")
  print("Repartitia normala sau repartitia Gaussiana, notata normal(miu,sigma^2) sau N(miu,sigma^2), este un tip de distributie continua de probabilitate pentru variabile aleatoare cu valori reale.
      Parametrul miu, numar real, reprezinta media repartitiei (de asemenea si mediana), iar sigma^2 (sigma - parametru real pozitiv) reprezinta dispersia. Sigma se mai numeste si abaterea medie patratica.
      Repartitia normala este folosita in stiintele naturale si sociale pentru a reprezenta variabile aleatoare cu valori reale ale caror distributii nu sunt cunoscute. Importanta acestei repartitii este oferita, in mare, de teorema limita centrala
      ce afirma ca, în anumite conditii, media mai multor esantioane ale unei variabile aleatoare cu medie finita si dispersie este ea insasi o variabila aleatoare a carei distributie converge la o distributie normala pe masura ce numarul esantioanelor creste.
      Exemple de probleme: masurarea erorii, inteligentei/abilitatii, inaltimii, mediilor loturilor de date, propagarea incertitudinii, ajustarea parametrilor celor mai mici patrate etc.")
}

#REPARTITIA PARETO


dpareto <- function(x,m,alpha)
{
  (alpha*m^alpha)/(x^(alpha+1))
  
}




rpareto <- function (x,m,alpha)
{
  1-(m^alpha/x^alpha)
}


media_pareto <- function (x,m,alpha)
{
  if (alpha<=1)
    Inf
  else
    return (alpha*m)/(alpha-1)
}


mediana_pareto <- function (x,m,alpha)
{
  m*((2)^(1/alpha))
}

var_pareto <- function (x,m,alpha)
{
  if(alpha<=2)
    Inf
  else
    ((m^2)*alpha)/((alpha-1)^2*(alpha-2))
}

info_Pareto <- function()
{
  x <- seq(1, 10, 0.01)
  plot(x, dpareto(x, 1,2), xlab="x", ylab="PDF", type="l")
  plot(x,rpareto(x,1,2),xlab="x",ylab="CDF",type="l")
  print("Reparitia Pareto, numita astfel dupa inginerul,economistul si sociologul italian Vilfredo Pareto este o repartitie de probabilitate a legii puterii, folosita pentru a descrie fenomene sociale, stiintifice, geofizice etc. 
      Initial, a fost folosita pentru a descrie distributia averilor intr-o societate, sustinand ideea ca o mare parte din bogatii este detinuta de o mica fractiune.(Regula 80-20). Are 2 parametri: m (parametrul de scala) si alfa (parametrul de forma). Domeniul de valori este [m,oo].
      Acesti tip de repartitie modeleaza o lege de putere, unde probabilitatea ca un eveniment sa aiba loc variaza ca o putere a unui atribut al evenimentului. Cateva aplicatii practice in care este folosita distributia Pareto: evaluarea angajatilor, business management, marimea meteoritilor, nivelurile populatiei in orase etc.")
}



#5 
medie <- function(f)
{
  g<-function(x)
  {
    x*f(x)
  }
  return (cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")$integral)
}

medie(f)

moment_centrat<- function(f,k) #Functie, rang
{
  med <- medie(f)
  g<-function(x)
  {
    (x-med)^k*f(x)
  }
  return (cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")$integral)
}

moment_centrat(f)

moment_initial  <- function(f)
{
 
  g<-function(x)
  {
    x^k*f(x)
  }
  return (cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")$integral)
}

moment_initial(f)


moment_centrat4 <- function(f)
{
  adev <- e_densitate(f) 
  for (r in seq(1,4))
  {
    if(adev == T)
    {
      cat("Momentul centrat de ordin ", r, " este ", moment_centrat(f,r))
      #print("Momentul centrat de ordin " ++ r ++ " este "  ++ moment_centrat(f,r))
    }
    else
      cat("Nu exista moment centrat de ordin", r, "\n")
      #print("Nu exista moment centrat de ordin " ++ r)
  }
}

moment_centrat4(f)

dispersie <- function(f)
{ moment_centrat(f,2)}

dispersie(f)


#6 
#medie  
install.packages("cubature")

library(cubature)

multiply = function(a,b)
{
  force(a)
  force(b)
  function(x){a(x)*b(x)}
}

medie2 <- function(f,g)
{
  h = multiply(g,f)
  cubintegrate(h, lower = -Inf, upper = Inf, method = "pcubature")
  
}

#dispersie 
dispersie2 <- function(f,g)
{
  h = multiply(g,f)
 dispersie(h)
  
}

f<-function(x){
  return(x+2)
}

g<-function(x){
  return(x^2)
}

dispersie2(f,g)

medie2(f,g)

#7 --


#8 



#9 
genereaza <- function(n,f)
{
  sample(seq(-1000, 1000, by = 0.01), size=n, prob=lapply(seq(-1000, 1000, by = 0.01), f), replace=TRUE)
  
}

fs <- function(x)
{
  if((x+2) < 0)
      {
        return(0)
      }
  return(x+2)
}

sample(seq(-1000, 1000, by = 0.01), size=1000, prob=c(fs(seq(-1000, 1000, by = 0.01))), replace=TRUE)

seq(0, 10, by = 0.01)

lapply(seq(10,20),fs)

sample(seq(-1000, 1000, by = 0.01), size=6, prob=lapply(seq(-1000, 1000, by = 0.01), fs), replace=TRUE)



# 10 - covarianta

cov <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{
  #verificare intervale
  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }
  
  #determinam mediile necesare formulei de covariatie
  
  temp_func_x <- function(x, y) x*joint_pdf(x, y)
  
  Ex <- integrate(function(x) { 
    sapply(x, function(x) {
      integrate(function(y) temp_func_x(x, y), interval_Y_lower, interval_Y_higher)$value
    })
  }, interval_X_lower, interval_X_higher)$value
  
  temp_func_y <- function(x, y) y*joint_pdf(x, y)
  
  Ey <- integrate(function(x) { 
    sapply(x, function(x) {
      integrate(function(y) temp_func_y(x, y), interval_Y_lower, interval_Y_higher)$value
    })
  }, interval_X_lower, interval_X_higher)$value
  
  temp_func_xy <- function(x, y) x*y*joint_pdf(x, y)
  
  Exy <- integrate(function(x) { 
    sapply(x, function(x) {
      integrate(function(y) temp_func_xy(x, y), interval_Y_lower, interval_Y_higher)$value
    })
  }, interval_X_lower, interval_X_higher)$value
  
  return(Exy - Ex*Ey)
  
}


#variabile test
jpdf <- function(x,y) 4*x*y
xl <- 0
xh <- 1
yl <- 1
yh <- 2

cov(jpdf, xl, xh, yl, yh)


#10 - coeficientul de corelatie

coef <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{
  #verificare intervale
  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }
  
  #determinam variatia fiecarei variabile apoi dispersia si in final raportul care ne da coeficientul de corelatie
  
  temp_func_x <- function(x, y) x*joint_pdf(x, y)
  
  Ex <- integrate(function(x) { 
    sapply(x, function(x) {
      integrate(function(y) temp_func_x(x, y), interval_Y_lower, interval_Y_higher)$value
    })
  }, interval_X_lower, interval_X_higher)$value
  
  temp_func_x2 <- function(x, y) x*x*joint_pdf(x, y)
  
  Ex2 <- integrate(function(x) { 
    sapply(x, function(x) {
      integrate(function(y) temp_func_x2(x, y), interval_Y_lower, interval_Y_higher)$value
    })
  }, interval_X_lower, interval_X_higher)$value
  
  Var_x = Ex2 - Ex*Ex
  
  #acum pt Y
  
  temp_func_y <- function(x, y) y*joint_pdf(x, y)
  
  Ey <- integrate(function(x) { 
    sapply(x, function(x) {
      integrate(function(y) temp_func_y(x, y), interval_Y_lower, interval_Y_higher)$value
    })
  }, interval_X_lower, interval_X_higher)$value
  
  temp_func_y2 <- function(x, y) y*y*joint_pdf(x, y)
  
  Ey2 <- integrate(function(x) { 
    sapply(x, function(x) {
      integrate(function(y) temp_func_y2(x, y), interval_Y_lower, interval_Y_higher)$value
    })
  }, interval_X_lower, interval_X_higher)$value
  
  Var_y = Ey2 - Ey*Ey
  
  #dispersiile
  
  if(Var_x * Var_y < 0){
    stop("Produsul variatiilor negativ, nu putem aplica radical.")
  }
  
  numitor <- sqrt(Var_x * Var_y)
  
  #raportul
  
  #conditia de existenta (numitorul diferit de 0)
  
  
  if(numitor <= 0){
    stop("Numitor egal cu 0, coeficientul nu exista.")
  }
  
  return(cov(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher) / numitor)
  
}

coef(jpdf, xl, xh, yl, yh)

#11

#11 - densitatile marginale

marginale <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{
  #verificare intervale
  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }
  
  #pentru a obtine marginala pentru X, integram dupa y pdf comuna si viceversa.
  
  pdf_x <- function(x) integrate(function(y) joint_pdf(x, y), interval_Y_lower, interval_Y_higher)
  
  pdf_y <- function(y) integrate(function(x) joint_pdf(x, y), interval_Y_lower, interval_Y_higher)
  
  pdf_list <- list("Fx" = pdf_x, "Fy" = pdf_y)
  
  return(pdf_list)
}

marginale(jpdf, xl, xh, yl, yh)$Fx(1)

marginale(jpdf, xl, xh, yl, yh)$Fy(1)

#11 - densitatile conditionate

conditionate <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{
  #verificare intervale
  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }
  
  #obtinem marginalele pentru calculul conditionatelor
  
  marginale_list <- marginale(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher) 
  
  marginala_x <- marginale_list$Fx
  
  marginala_y <- marginale_list$Fy
  
  #definim conditionatele si cu un if pentru criteriul de existenta
  
  conditionata_x <- function(x, y) {
    
    if(marginala_x(x)$value <= 0)
      stop("Densitatea marginala este mai mica sau egala cu 0.")
    
    return(joint_pdf(x, y)/marginala_x(x)$value)
    
  }
  
  conditionata_y <- function(x, y) {
    
    if(marginala_y(y)$value <= 0)
      stop("Densitatea marginala este mai mica sau egala cu 0.")
    
    return(joint_pdf(x, y)/marginala_y(y)$value)
    
  }
  
  conditionate_list <- list("Fy" = conditionata_x, "Fx" = conditionata_y)
  
  return(conditionate_list)
}

Fx <- conditionate(jpdf, xl, xh, yl, yh)$Fx
Fy <- conditionate(jpdf, xl, xh, yl, yh)$Fy

Fx(1, 1)
Fy(1, 2)

