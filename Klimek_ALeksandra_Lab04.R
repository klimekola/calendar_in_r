"Zadanie 02-04"
s<-function(x) {
  k=1
  suma=k^2/(k+2)
  while (suma<x) {
    k=k+1
    suma=suma+(k^2)/(k+2)}
  if (k-1>=1) {print(k-1)} 
  else {print('B³¹d')}}
s(8)
s(4)
s(-8)

"Zadanie 02-05"
A <- matrix(c(2, 1, 5, 4, 8, 9, 4,2, 3), nrow=3, ncol=3, byrow=TRUE)
B <- matrix(c(3, 3, 3, 4, 0, 8, 9, 7, 3), nrow=3, ncol=3, byrow=TRUE)
C <- matrix(c(11, 15, 29, 40, 15, 9, 7, 13, 4), nrow=3, ncol=3, byrow=TRUE)

macierzx <- function(a, b, c) {
  # x = (t(c) - (b %*% t(a))) %*% (solve(t(a)))
  aT = t(a)
  cT = t(c)
  
  if (ncol(b) != nrow(aT)) {
    return ("Macierze B oraz A^T nie mog¹ zostaæ pomno¿one.")
  }
  
  first_step = b %*% aT
  if (ncol(cT) != ncol(first_step) | nrow(cT) != nrow(first_step)) {
    return ("Macierze C^T oraz B*A^T nie mog¹ zostaæ pomno¿one.")
  }
  
  second_step = cT - first_step
  if (det(aT) == 0) {
    return ("Wyznacznik macierzy A^T jest równy 0.")
  }
  
  inv_t_a = solve(aT)
  x <- second_step %*% inv_t_a
  return (x)
}

macierzx(A, B, C)

"Zadanie 03-01A"

file.create('Zadanie-02-01A.tex',showWarnings = TRUE)
q <- 1:29
dni<-c("Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela")
dni
for (i in q)
      {cat(paste(i, dni[1+(i+1)%%7], '\n'))
      write(paste(i, dni[1+(i+1)%%7]), 'Zadanie-02-01A.tex', append = TRUE)}
write('\\end{document}','Zadanie-02-01A.tex', append=TRUE)
file.copy('Podstawowa preambu³a.tex', 
          'Zadanie-02-01A.tex',overwrite=TRUE)
file.show('Zadanie-02-01A.tex')

"Zadanie 03-01B"

file.create('Zadanie-02-01B.tex',showWarnings = TRUE)
text = "Kalendarz rok 2020 + 'moj nr'8 = 2028"
write(text, 'Zadanie-02-01B.tex', append = TRUE)
dni<-c("Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela")

luty <- 1:29
styczen = marzec = maj = lipiec = sierpien = pazdziernik = grudzien <- 1:31
kwiecien = czerwiec = wrzesien = listopad<- 1:30


write('STYCZEN', 'Zadanie-02-01B.tex', append = TRUE)
for (i in styczen)
{  write(paste(i, dni[1+(i+4)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write("LUTY", 'Zadanie-02-01B.tex', append = TRUE)
for (i in luty)
{  write(paste(i, dni[1+(i)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('MARZEC', 'Zadanie-02-01B.tex', append = TRUE)
for (i in marzec)
{  write(paste(i, dni[1+(i+1)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('KWIECIEN', 'Zadanie-02-01B.tex', append = TRUE)
for (i in kwiecien)
{  write(paste(i, dni[1+(i+4)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('MAJ', 'Zadanie-02-01B.tex', append = TRUE)
for (i in maj)
{  write(paste(i, dni[1+(i+6)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('CZERWIEC', 'Zadanie-02-01B.tex', append = TRUE)
for (i in czerwiec)
{  write(paste(i, dni[1+(i+2)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('LIPIEC', 'Zadanie-02-01B.tex', append = TRUE)
for (i in lipiec)
{  write(paste(i, dni[1+(i+4)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('SIERPIEN', 'Zadanie-02-01B.tex', append = TRUE)
for (i in sierpien)
{  write(paste(i, dni[1+(i)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('WRZESIEN', 'Zadanie-02-01B.tex', append = TRUE)
for (i in wrzesien)
{  write(paste(i, dni[1+(i+3)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('PAZDZIERNIK', 'Zadanie-02-01B.tex', append = TRUE)
for (i in pazdziernik)
{  write(paste(i, dni[1+(i+5)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('LISTOPAD', 'Zadanie-02-01B.tex', append = TRUE)
for (i in listopad)
{  write(paste(i, dni[1+(i+1)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('GRUDZIEN', 'Zadanie-02-01B.tex', append = TRUE)
for (i in grudzien)
{  write(paste(i, dni[1+(i+3)%%7]), 'Zadanie-02-01B.tex', append = TRUE)}
write('\\end{document}','Zadanie-02-01B.tex', append=TRUE)
file.show('Zadanie-02-01B.tex')

'Zadanie 03-02'
file.create( 'Zadanie03-02.tex', showWarnings = TRUE)

MAC = matrix(c(2,-8,3))
if( nrow(MAC) != 3 || ncol(MAC) != 1) write("Blad wymiaru macierzy",'Zadanie03-02.tex')

a<-MAC[1]
b<-MAC[2]
c<-MAC[3]
d<-(b^2-4*a*c)

if (a==0) {
  write('Wspolczynnik a musi byc rozny od zera.','Zadanie03-02.tex')} else {
  
    x0=(-b)/(2*a)
    x1=((-b)-sqrt(d))/(2*a)
    x2=((-b)+sqrt(d))/(2*a)

    if (d<0) {text1 = 'ujemna'; text2 = 'nie ma pierwiastka'}
    if (d==0) {text1 = 'rowna zero'; text2 = paste("ma jedne pierwiastek zerowy rowwny",x0)}
    if (d>0) {text1 = 'dodatnia'; text2 = paste('ma dwa pierwiastki rowne x1=',x1,'x2=',x2)}

    write(paste("Trojmian kwadratowy o wsp. a,b,c ma delte: ", text1, "zatem", text2), 'Zadanie03-02.tex')
}
write('\\end{document}','Zadanie03-02.tex', append=TRUE)
file.show('Zadanie03-02.tex')

