install.packages(ggplot2)

library(ggplot2)

A = matrix(c(2,2,3,3,5,9,5,6,7), nrow= 3, byrow = T)
A.inv = solve(A) #solve gives the inverse of a matrix

check = A%*%A.inv
format(check, scientific = F)
round(check)
floor(A) #round  to the lowest integer
ceiling(A)#round  to the highest integer

det(A) # solvable because the determinant is non zero

#x1 +3y = 7
#2x + 4y = 10

A = matrix(c(1,2,3,4), ncol = 2)
det(A)
b = c(7,10)

x = solve(A, b) #inverts and solves the sys, but will only give the solution to a well-determined problem (there are as many equations as there are unknowns or square matrices)

#eigenvalues
e = eigen(A)
e$values
e$vectors

install.packages("imager")
library(imager)

file = system.file('extdata/parrots.png', package = 'imager')

im = load.image(file)
plot(im)



## image with large grains
lg = 'https://www.dropbox.com/s/rtrzedg0e5fw7a2/IMG_0202.JPG?dl=0'

download.file(lg, 'largegrains.JPG', method='auto')

## image with small grains
sg = 'https://www.dropbox.com/s/j1skinhwe69gpnh/IMG_0249.JPG?dl=0'

download.file(sg, 'smallgrains.JPG', method='auto')

## image with medium grains
sg = 'https://www.dropbox.com/s/dao092gr5dptjxs/IMG_0229.JPG?dl=0'

download.file(sg, 'mediumgrains.JPG', method='auto')

## image with medium large grains
mlg = 'https://www.dropbox.com/s/82udxnw8oezrgxj/IMG_0213.JPG?dl=0'

download.file(mlg, 'medlargegrains.JPG', method='auto')

getwd()
setwd("//cefnsshares/Homes/NAU-STUDENTS/sha59/Desktop/ENV599/Lab05-sarcusa/in-class-notes")

lg = load.image("largegrains.JPG")
lg.g = grayscale(lg)

mg = load.image("mediumgrains.JPG")
mg.g = grayscale(mg)

mlg = load.image("mediumlargegrains.JPG")
mlg.g = grayscale(mlg)

sg = load.image("smallgrains.JPG")
sg.g = grayscale(sg)

ug = load.image("unknowngrains.JPG")
ug.g = grayscale(ug)

r = 50 #row number 50
imrow(lg.g, r) %>% plot(main = "Large grains along row", type = "l")

r = 50
imrow(sg.g, r) %>% plot(main = "Small grains along row", type = "l")

#autocorrelation of the trace

#gives a sequence of autocorrelation values for increasing lag
sg.acf = acf(sg.g[r,], plot = F)
mg.acf = acf(mg.g[r,], plot = F)
mlg.acf = acf(mlg.g[r,], plot = F)
lg.acf = acf(lg.g[r,], plot = F)
ug.acf = acf(ug.g[r,], plot = F)

library(ggplot2)

a = data.frame(
  "lag" = lg.acf$lag[c(seq(30))],
  "acf_lg" = lg.acf$acf[c(seq(30))],
  "acf_mg" = mg.acf$acf[c(seq(30))],
  "acf_mlg" = mlg.acf$acf[c(seq(30))],
  "acf_sg" = sg.acf$acf[c(seq(30))],
  "acf_ug" = ug.acf$acf[c(seq(30))]
)

ggplot(a, aes(lag))+
  geom_line(aes(y = acf_lg,  colour = 'Large'))+
  geom_line(aes(y = acf_mg,  colour = 'Medium'))+
  geom_line(aes(y = acf_mlg, colour = 'Medium large'))+
  geom_line(aes(y = acf_sg,  colour = 'Small'))

ggplot(a, aes(lag))+
  geom_line(aes(y = acf_lg,  colour = '0.8mm'))+
  geom_line(aes(y = acf_mg,  colour = '0.2mm'))+
  geom_line(aes(y = acf_mlg, colour = '0.6mm'))+
  geom_line(aes(y = acf_sg,  colour = '0.1mm'))+
  geom_line(aes(y = acf_ug,  colour = 'Unknown'))
  
plot(ug.g)

#Ax = b

#well determined prob (square A)

A = matrix (cbind(a$acf_sg[c(seq(2,5))],
                  a$acf_mg[c(seq(2,5))],
                  a$acf_mlg[c(seq(2,5))],
                  a$acf_lg[c(seq(2,5))]),
                  nrow = 4)

#but there is no info in the first row, this row was removed

b = a$acf_ug[c(seq(2,5))]
x = solve(A, b) #solution to the problem, i.e. the proportion for every lag

s = c(.1, .2, .6, .8) #weights on a weighted average

sum(x * s) #mean grain size for the unknown grainsize

#solve the same prob, using all the info available in the correlogram (over determined prob because more equations than unknows)

#Ax = b, using A^-1 = analytical solution (only one solution)

#use a numerical solution/method i.e. QR decomposition

A = matrix (cbind(a$acf_sg,
                  a$acf_mg,
                  a$acf_mlg,
                  a$acf_lg),
            nrow = 30)

b = a$acf_ug

x = qr.solve(A, b)

sum(x * s)

#nnls #optimization (non nega least squares)
#Ax = b, x >= 0 (not a physical result to have a negative proportion)

install.packages("nnls")
library(nnls)

x = nnls(A, b)
x$X

sum(x$x * s)





