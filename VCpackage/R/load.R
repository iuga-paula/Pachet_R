#' Constanta de normalizare k
#'
#' Pt o functie f, intodusa de utlizator se determina o constanta de normalizare k
#'
#' @usage constanta(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f o repartitie de v.a. continua
#' @return number - constanta de normalizare daca exista
#' @export
constanta <- function(f)
{
  temp <- cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")
  if(temp$integral==0)
    stop("Constanta nu exista")
  return (1/temp$integral)
}


#' Densitate de probabilitate
#'
#' Pt o functie f, intodusa de utlizator se determina daca aceasta este densitate de probabilitate
#'
#' @usage e_densitate(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f o repartitie de v.a. continua
#' @return Bool
#' @export
e_densitate <- function(f)
{
  temp <- cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")
  if(!(temp$integral+temp$error>=1 && temp$integral-temp$error<=1))
    return (F)

  for (i in seq(-10000,10000,0.003))
    if(f(i)<0)
      return(F)

  return (T)
}

#' Densitate de probabilitate pentru un obiect de tip v.a.continua
#'
#' Pt un obiect  se determina daca acesta este densitate de probabilitate
#'
#' @usage e_densitate(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f o repartitie de v.a. continua
#' @return Bool
check_VC <- function(object)
{ errors <- character()
g <- object@f
temp <- cubintegrate(g, lower = -Inf, upper = Inf, method = "pcubature")
if(!(temp$integral+0.001>=1 && temp$integral-0.001<=1))
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


#' @slot f function
setClass("VC",slots = list(f= "function"),validity = check_VC)


setGeneric("constanta", valueClass = "numeric", function(object){
  standardGeneric("constanta")
})

#' Constanta pentru obiect de v.a. continua
#'
#' Wrapper pt functia contanta a unui obiect de v.a. continua
#' Pt o functie f, intodusa de utlizator se determina o constanta de normalizare k
#'
#' @usage constanta(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
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

#' Grafic pentru repartitia uniforma a unei v.a. continue
#'
#' Deseneaza graficul
#'
#' @usage plot2(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f o repartitie de v.a. continua
#' @return empty
#' @export
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

#' Densitate de probabilitate(Rep. Exponentiala)
#'
#' Calculeaza densitatea de probabilitate in x a unei v.a. continue in repartitia Exponentiala - functie folosita in repartizarea grafica
#'
#' @usage d_exp(x, lambda)
#' @param x,lamda : x - number si lambda
#' @return number -  Densitate de probabilitate in Rep. Exponentiala
#' @export
d_exp <- function (x,lambda)
{

  lambda * exp (-lambda*x)
}


#' Repartitia (Rep. Exponentiala)
#'
#' Calculeaza functia de repatitie in x a unei v.a. continue in repartitia Exponentiala - functie folosita in repartizarea grafica
#'
#' @usage r_exp(x, lambda)
#' @param x,lamda : x - number si lambda
#' @return number - Repartitia in Rep. Exponentiala
#' @export
r_exp <- function (x,lambda)
{
  1-((exp)(-lambda*x))
}

#' Media (Rep. Exponentiala)
#'
#' Calculeaza functia de medie in x a unei v.a. continue in repartitia Exponentiala - functie folosita in repartizarea grafica
#'
#' @usage media_exp(x,lambda)
#' @param x,lamda : x - number si lambda
#' @return number - Media in Rep. Exponentiala
#' @export
media_exp <- function(x,lambda)
{
  1/lambda
}

#' Mediana (Rep. Exponentiala)
#'
#' Calculeaza mediana unei v.a. continue in repartitia Exponentiala - functie folosita in repartizarea grafica
#'
#' @usage mediana_exp(x,lambda)
#' @param x,lamda : x - number si lambda
#' @return number - Mediana in Rep. Exponentiala
#' @export
mediana_exp <- function (x,lambda)
{
  log(2)/lambda
}



#' Variatia (Rep. Exponentiala)
#'
#' Calculeaza variatia unei v.a. continue in repartitia Exponentiala - functie folosita in repartizarea grafica
#'
#' @usage val_exp(x,lambda)
#' @param x,lamda : x - number si lambda
#' @return number - Variatia in Rep. Exponentiala
#' @export
var_exp <- function (x,lambda)
{
  1/(lambda^2)
}

#' Fisa de sinteza (Rep. Exponentiala)
#'
#' Afisarea unei fise de sinteza pentru o v.a. continua in repartitia Exponentiala
#'
#' @usage info_exp()
#' @param null
#' @return empty - afiseaza fisa de sinteza
#' @export
info_exp <- function()
{
  x<-seq(1,10,by=0.01)
  plot(x,d_exp(x,0.5),type="l",ylab="PDF exp")
  plot(x,r_exp(x,1.0),type="l",ylab="CDF exp")

  print("Repartitia exponentiala, notata exponential(lambda) sau exp(lambda), este distributia de probabilitate a timpului dintre evenimente intr-un proces de tip punct Poisson(proces in care evenimentele se petrec intr-un mod continuu si independent la o rata medie constanta). Este un caz particular al repartitiei Gamma.Parametrul lambda (numar pozitiv) se numeste parametru de scala si ne arata cat de rapid se petrece scaderea functiei exponentiale. Domeniul de valori al acestei repartitii este [0,oo). O proprietate importanta a repartitiei exponentiale este legata de probabilitatile conditionate si anume, lipsa de memorie.Spre exemplu, presupunem ca probabilitatea ca un taxi sa soseasca in primele 5 minute este p. Daca astept 5 minute si e fapt nu soseste niciun taxi, atunci probabilitatea ca un taxi sa soseasca in urmatoarele 5 minute este tot p.Repartitia exponentiala este folosita in problemele practice in care suntem interesati de timpul scurs pana la aparitia unui eveniment. Exemple: timpul pana la sosirea primului autobuz in statie, timpul pana la primul cutremur, timpul pana la sosirea primului client intr-un magazin etc.")


}


#' Media (Rep. Uniforma)
#'
#' Calculeaza functia de medie in x a unei v.a. continue in repartitia Unfiorma - functie folosita in repartizarea grafica
#'
#' @usage media_unif(x,a,b)
#' @param x,a,b : x - number, [a,b] capetele intevalului de definitie pt X
#' @return number - Media in Rep. Unfiorma
#' @export
media_unif <- function(x,a,b)
{
  (a+b)/2
}


#' Mediana (Rep. Uniforma)
#'
#' Calculeaza mediana unei v.a. continue in repartitia Uniforma - functie folosita in repartizarea grafica
#'
#' @usage mediana_unif(x,a,b)
#' @param x,a,b : x - number, [a,b] capetele intevalului de definitie pt X
#' @return number - Mediana in Rep. Uniforma
#' @export
mediana_unif <- function(x,a,b)
{
  (a+b)/2
}




#' Variatia (Rep. Uniforma)
#'
#' Calculeaza variatia unei v.a. continue in repartitia Unifroma - functie folosita in repartizarea grafica
#'
#' @usage val_unif(x,a,b)
#' @param x,a,b : x - number, [a,b] capetele intevalului de definitie pt X
#' @return number - Variatia in Rep. Uniforma
#' @export
var_unif <- function (x,a,b)
{
  ((b-a)^2)/12
}



#' Fisa de sinteza (Rep. Uniforma)
#'
#' Afisarea unei fise de sinteza pentru o v.a. continua in repartitia Uniforma
#'
#' @usage info_unifrom()
#' @param null
#' @return empty - afiseaza fisa de sinteza
#' @export
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



#' Densitate de probabilitate(Rep.Normala)
#'
#' Calculeaza densitatea de probabilitate in x a unei v.a. continue in repartitia Normala - functie folosita in repartizarea grafica
#'
#' @usage d_norm(x, miu, sigma)
#' @param x,miu,sigma_p : x - number si miu, sigma
#' @return number -  Densitate de probabilitate in Rep. Normala
#' @export
d_norm <- function(x,miu, sigma_p)
{
  sigma <- sqrt(sigma_p)
  (1/(sigma*(sqrt(2*pi)))) * exp((-(x-miu)^2)/(2*sigma_p))
}


#' Media (Rep. Normala)
#'
#' Calculeaza media unei v.a. continue in repartitia Normala - functie folosita in repartizarea grafica
#'
#' @usage media_exp(x,miu, sigma)
#' @param x,miu,sigma_p : x - number si miu, sigma
#' @return number - Media in Rep. Normala
#' @export
media_norm <- function (x,miu,sigma_p)
{
  miu
}



#' Mediana (Rep. Normala)
#'
#' Calculeaza mediana unei v.a. continue in repartitia Normala - functie folosita in repartizarea grafica
#'
#' @usage mediana_norm(x,lambda, miu, sigma)
#' @param x,miu,sigma_p : x - number si miu, sigma
#' @return number - Mediana in Rep. Normala
#' @export
mediana_norm <- function (x,miu,sigma_p)
{
  miu
}



#' Variatia (Rep. Normala)
#'
#' Calculeaza variatia unei v.a. continue in repartitia Normala - functie folosita in repartizarea grafica
#'
#' @usage val_norm(x,miu,sigma_p)
#' @param x,miu,sigma_p : x - number si miu, sigma
#' @return number - Variatia in Rep. Normala
#' @export
var_norm <- function (x,miu,sigma_p)
{
  sigma_p
}



#' Fisa de sinteza (Rep. Normala)
#'
#' Afisarea unei fise de sinteza pentru o v.a. continua in repartitia Normala
#'
#' @usage info_norm()
#' @param null
#' @return empty - afiseaza fisa de sinteza
#' @export
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
      ce afirma ca, Ã®n anumite conditii, media mai multor esantioane ale unei variabile aleatoare cu medie finita si dispersie este ea insasi o variabila aleatoare a carei distributie converge la o distributie normala pe masura ce numarul esantioanelor creste.
      Exemple de probleme: masurarea erorii, inteligentei/abilitatii, inaltimii, mediilor loturilor de date, propagarea incertitudinii, ajustarea parametrilor celor mai mici patrate etc.")
}



#' Densitate de probabilitate(Rep.Pareto)
#'
#' Calculeaza densitatea de probabilitate in x a unei v.a. continue in repartitia Pareto - functie folosita in repartizarea grafica
#'
#' @usage dpareto(x,m,alpha)
#' @param x,m,alpha : x - number si m, alpha
#' @return number -  Densitate de probabilitate in Rep. Pareto
#' @export
dpareto <- function(x,m,alpha)
{
  (alpha*m^alpha)/(x^(alpha+1))

}



#' Repartitia (Rep. Pareto)
#'
#' Calculeaza functia de repatitie in x a unei v.a. continue in repartitia Pareto - functie folosita in repartizarea grafica
#'
#' @usage rpareto(x,m,alpha)
#' @param x,m,alpha : x - number si m,alpha
#' @return number - Repartitia in Rep. Pareto
#' @export
rpareto <- function (x,m,alpha)
{
  1-(m^alpha/x^alpha)
}



#' Media (Rep. Pareto)
#'
#' Calculeaza media unei v.a. continue in repartitia Pareto - functie folosita in repartizarea grafica
#'
#' @usage media_exp(x,m,alpha)
#' @param x,m,alpha : x - number si m,alpha
#' @return number - Media in Rep. Pareto
#' @export
media_pareto <- function (x,m,alpha)
{
  if (alpha<=1)
    Inf
  else
    return (alpha*m)/(alpha-1)
}


#' Mediana (Rep. Pareto)
#'
#' Calculeaza mediana unei v.a. continue in repartitia Pareto - functie folosita in repartizarea grafica
#'
#' @usage mediana_pareto(x,m,alpha)
#' @param x,m,alpha : x - number si m,alpha
#' @return number - Mediana in Rep. Pareto
#' @export
mediana_pareto <- function (x,m,alpha)
{
  m*((2)^(1/alpha))
}



#' Variatia (Rep. Pareto)
#'
#' Calculeaza variatia unei v.a. continue in repartitia Pareto - functie folosita in repartizarea grafica
#'
#' @usage val_pareto(x,m,alpha)
#' @param x,m,alpha : x - number si m,alpha
#' @return number - Variatia in Rep. Pareto
#' @export
var_pareto <- function (x,m,alpha)
{
  if(alpha<=2)
    Inf
  else
    ((m^2)*alpha)/((alpha-1)^2*(alpha-2))
}



#' Fisa de sinteza (Rep. Pareto)
#'
#' Afisarea unei fise de sinteza pentru o v.a. continua in repartitia Pareto
#'
#' @usage info_Pareto()
#' @param null
#' @return empty - afiseaza fisa de sinteza
#' @export
info_Pareto <- function()
{
  x <- seq(1, 10, 0.01)
  plot(x, dpareto(x, 1,2), xlab="x", ylab="PDF", type="l")
  plot(x,rpareto(x,1,2),xlab="x",ylab="CDF",type="l")
  print("Reparitia Pareto, numita astfel dupa inginerul,economistul si sociologul italian Vilfredo Pareto este o repartitie de probabilitate a legii puterii, folosita pentru a descrie fenomene sociale, stiintifice, geofizice etc.
      Initial, a fost folosita pentru a descrie distributia averilor intr-o societate, sustinand ideea ca o mare parte din bogatii este detinuta de o mica fractiune.(Regula 80-20). Are 2 parametri: m (parametrul de scala) si alfa (parametrul de forma). Domeniul de valori este [m,oo].
      Acesti tip de repartitie modeleaza o lege de putere, unde probabilitatea ca un eveniment sa aiba loc variaza ca o putere a unui atribut al evenimentului. Cateva aplicatii practice in care este folosita distributia Pareto: evaluarea angajatilor, business management, marimea meteoritilor, nivelurile populatiei in orase etc.")
}






#' Media unei v.a continue X
#'
#' Calculeaza media unei v.a continue X
#'
#' @usage medie(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f : f-o repartitie de v.a. continua
#' @return number - media
#' @export
medie <- function(f)
{
  g<-function(x)
  {
    x*f(x)
  }
  return (cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")$integral)
}



#' Momente centrat al unei v.a continue X
#'
#' Calculeaza moment centrat de ordin k al unei v.a continue X
#'
#' @usage moment_centrat(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, k)
#' @param f,k : f-o repartitie de v.a. continua, k - ordinul momentului centrat
#' @return number - moment centrat
#' @export
moment_centrat<- function(f,k) #Functie, rang
{
  med <- medie(f)
  g<-function(x)
  {
    (x-med)^k*f(x)
  }
  return (cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")$integral)
}



#' Moment initial al unei v.a continue X
#'
#' Calculeaza momentul initial al unei v.a continue X
#'
#' @usage moment_initial (f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f: f-o repartitie de v.a. continua
#' @return number - moment_initial
#' @export
moment_initial  <- function(f)
{

  g<-function(x)
  {
    x^k*f(x)
  }
  return (cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")$integral)
}




#' Momente centrate ale unei v.a continue X
#'
#' Calculeaza momentele centrale pana la ordin 4 ale unei v.a continue X pe baza functiei moment_centrat
#'
#' @usage moment_centrat4(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f: f-o repartitie de v.a. continua
#' @return empty - afiseaza momentele centrale daca exista, daca nu afiseaza un mesaj corespunzator
#' @export
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




#' Dispersia unei v.a continue X
#'
#' Calculeaza dispersia unei v.a continue X
#'
#' @usage dispersie(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param f: f-o repartitie de v.a. continua
#' @return number - dispersia
#' @export
dispersie <- function(f)
{ moment_centrat(f,2)}



#' Produsul a 2 functii
#'
#' Calculeaza produsul pentru 2 functii
#'
#' @usage multiply(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, g(x){return (x+2)})
#' @param f,g : f-o repartitie de v.a. continua, g functie continua
#' @return function
#' @export
multiply = function(a,b)
{
  force(a)
  force(b)
  function(x){a(x)*b(x)}
}


#' Media unei v.a continue X trecuta print-o functie G
#'
#' Calculeaza media unei v.a continue X trecuta print-o functie G continua
#'
#' @usage medie2(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, g(x){return (x+2)})
#' @param f,g : f-o repartitie de v.a. continua, g functie continua
#' @return number - media
#' @export
medie2 <- function(f,g)
{
  h = multiply(g,f)
  cubintegrate(h, lower = -Inf, upper = Inf, method = "pcubature")

}


#' Dispersia unei v.a continue X trecuta print-o functie G
#'
#' Calculeaza dispersia unei v.a continue X trecuta print-o functie G continua
#'
#' @usage dispersie2(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, g(x){return (x+2)})
#' @param f,g : f-o repartitie de v.a. continua, g functie continua
#' @return number - dispersia
#' @export
dispersie2 <- function(f,g)
{
  h = multiply(g,f)
  dispersie(h)

}


#' Genereaza n valori
#'
#' Calculeaza n valori dintr-o repartitie de v.a continua
#'
#' @usage genereaza(n,f(x){if (x > a && x< b) return(1/(b-a)) else return(0)})
#' @param n,f : f-o repartitie de v.a. continua, n cate valori sa genereze
#' @return empty - afiseaza valorile generate
#' @export
genereaza <- function(n,f)
{
  sample(seq(-1000, 1000, by = 0.01), size=n, prob=lapply(seq(-1000, 1000, by = 0.01), f), replace=TRUE)

}


#' Covalenta pentru 2 v.a. continue
#'
#' Realizeaza denistatea comuna a 2 v.a. continue, apoi calculeaza Covalenta pe baza ei
#'
#' @usage conditionate(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, a, b, c, d)
#' @param f,a,b,c,d : f-o repartitie de v.a. continua, [a,b], [c,d] - intevalele de definitie ale celor 2 v.a continue
#' @return number - covalenta
#' @export
cov <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{

  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }



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




#' Coefincientul de corelatie pentru 2 v.a. continue
#'
#' Realizeaza denistatea comuna a 2 v.a. continue, apoi calculeaza Coefincientul de corelatie pe baza ei
#'
#' @usage coef(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, a, b, c, d)
#' @param f,a,b,c,d : f-o repartitie de v.a. continua, [a,b], [c,d] - intevalele de definitie ale celor 2 v.a continue
#' @return number - coeficientul de corelatie, daca exista
#' @export
coef <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{

  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }



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



  if(Var_x * Var_y < 0){
    stop("Produsul variatiilor negativ, nu putem aplica radical.")
  }

  numitor <- sqrt(Var_x * Var_y)



  if(numitor <= 0){
    stop("Numitor egal cu 0, coeficientul nu exista.")
  }

  return(cov(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher) / numitor)

}


#' Densitatea marginala pentru 2 v.a. continue
#'
#' Realizeaza denistatea comuna a 2 v.a. continue, apoi calculeaza densitatea marginala pe baza ei
#'
#' @usage marginale(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, a, b, c, d)
#' @param f,a,b,c,d : f-o repartitie de v.a. continua, [a,b], [c,d] - intevalele de definitie ale celor 2 v.a continue
#' @return number - denistatea marginala
#' @export
marginale <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{

  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }


  pdf_x <- function(x) integrate(function(y) joint_pdf(x, y), interval_Y_lower, interval_Y_higher)

  pdf_y <- function(y) integrate(function(x) joint_pdf(x, y), interval_Y_lower, interval_Y_higher)

  pdf_list <- list("Fx" = pdf_x, "Fy" = pdf_y)

  return(pdf_list)
}


#' Densitatea conditionala pentru 2 v.a. continue
#'
#' Realizeaza denistatea comuna a 2 v.a. continue, apoi calculeaza densitatea contionala pe baza ei
#'
#' @usage conditionate(f(x){if (x > a && x< b) return(1/(b-a)) else return(0)}, a, b, c, d)
#' @param f,a,b,c,d : f-o repartitie de v.a. continua, [a,b], [c,d] - intevalele de definitie ale celor 2 v.a continue
#' @return number - denistatea conditionala
#' @export
conditionate <- function(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)
{

  if(interval_X_lower > interval_X_higher | interval_Y_lower > interval_Y_higher){
    stop("Unul dintre intervalele specificate este invalid.")
  }



  marginale_list <- marginale(joint_pdf, interval_X_lower, interval_X_higher, interval_Y_lower, interval_Y_higher)

  marginala_x <- marginale_list$Fx

  marginala_y <- marginale_list$Fy



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
