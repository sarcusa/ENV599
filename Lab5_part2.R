install.packages("imager")
install.packages("ggplot2")
install.packages("expm")
install.packages("nnls")
install.packages("scatterplot3d")

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

# [x,y,z] location of satellites
# in units of 1000 m
x = c(3,1,5,1,7,1)
y = c(2,3,7,7,6,4)
z = c(3,1,4,3,7,9)

library(scatterplot3d)
scatterplot3d(x,y,z, color='blue', pch='S')

#Recorded delay (ms) between accurate transmission time and the receive time
delay = c(10010.00692286, 10013.34256381, 10016.67820476, 
          10020.01384571, 10023.34948666, 10030.02076857)

# radio waves move at 300,000 km / sec
#
c_m_ns = 0.299792458 #speed of radio wave, in 1000 km / millisecond

# A = matrix(c(-(-x[seq(1,length(x)-1)] - - x[length(x)])*2,
#     -(-y[seq(1,length(x)-1)] - - y[length(x)])*2,
#     -(-z[seq(1,length(x)-1)] - - z[length(x)])*2,
#     2 * c_m_ns^2 *(delay[6] - delay[seq(1,length(x)-1)] )
#             ), nrow=5)

# A

A = matrix(c(4,0,8,0,12,
             -4,-2,6,6,4,
             -12,-16,-10,-12,-4,
             3.59751, 2.99792, 2.39834, 1.79875, 1.19917),
           nrow=5)

A

b = c(35971.1, 29957.2, 24031.4, 17993.5, 12059.7)

xyzt = solve(A[1:4,1:4], b[1:4])
round(xyzt)

xyzt = qr.solve(A, b)
round(xyzt)

# let's add our location
s3d = scatterplot3d(x,y,z, color='blue', pch='S')
s3d$points3d(5,3,1,col='red', pch='R')

# let's make a 4 state MC model 
T = matrix(c(.7, .2, .1, .1, .8, .1, .3, .3, .4), nrow=3, byrow=T)

rownames(T) = c("D","R","I")
colnames(T) = rownames(T)
T

rowSums(T)

library(expm)

pivec = (T %^% 15)[1,] # the higher the power the matrix is raised to, the more it converges
pivec

# notice what happens when you do this
pivec %*% T

round(det(T - diag(3)))

# 2016 voter distribution
P_0 = c(.482, .461, .057)

T %*% P_0

##cbind(T - diag(3), c(1))

#eigenvalues
e = abs(eigen(T)$values)

format(e,digits=3, scientific=F)

#left eigenvectors
lv = eigen(t(T))$vectors

format(lv,digits=3, scientific=F)

lv[,1] / sum(lv[,1])

Mx = c(0,0.5,1, 1.5,2)
My = c(0,1,0,1,0)

plot(c(0,3), c(0,3), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)

#translate
x = 1.1
y = 1.2
translate = matrix(c(1,0,x,0,1,y,0,0,1), nrow=3, byrow=T)

Mt = translate %*% rbind(Mx, My, c(1,1,1,1,1))

lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#scale
W = 1.5
H = 1.5
scale = matrix(c(W,0,0,0,H,0,0,0,1), nrow=3, byrow=T)

Mt = scale %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(0,3), c(0,3), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#rotate
theta = pi/2
rotate = matrix(c(cos(theta),sin(theta),0,-sin(theta),cos(theta),0,0,0,1), nrow=3, byrow=T)

Mt = rotate %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(-2,2), c(-2,2), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#reflect origin
reflect = matrix(c(-1,0,0,0,-1,0,0,0,1), nrow=3, byrow=T)

Mt = reflect %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(-2,2), c(-2,2), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#reflect x
reflect = matrix(c(1,0,0,0,-1,0,0,0,1), nrow=3, byrow=T)

Mt = reflect %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(-2,2), c(-2,2), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#shear x
theta = .5
shear = matrix(c(1,tan(theta),0,0,1,0,0,0,1), nrow=3, byrow=T)

Mt = shear %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(0,2), c(0,2), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#shear y
theta = .6
shear = matrix(c(1,0,0,tan(theta),1,0,0,0,1), nrow=3, byrow=T)

Mt = shear %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(0,2), c(0,2), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#rotate and scale
theta = 1.25
W = 3
H = 3
rotate = matrix(c(W*cos(theta),sin(theta),0,-sin(theta),H*cos(theta),0,0,0,1), nrow=3, byrow=T)

Mt = rotate %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(-2,3), c(-2,2), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

#rotate and scale and shear in x and translate in x and y
omega = .75
theta = -1.25
W = 2.5
H = 3.5
tx = 2
ty = 1
rotate = matrix(c(W*cos(theta),tan(omega)*sin(theta),tx,-sin(theta),H*cos(theta),ty,0,0,1), nrow=3, byrow=T)

Mt = rotate %*% rbind(Mx, My, c(1,1,1,1,1))

plot(c(0,4), c(0,4), type="n", xlab="x", ylab="y")
lines(Mx, My, col="red", lwd=2)
lines(Mt[1,], Mt[2,], col="blue", lwd=2)

source('H_from_points.R')

H_from_points <- function(fp,tp) {
  # Find homography H, such that fp is mapped to tp using the linear DLT method.
  
  #  The points are conditioned by normalizing so that they have zero mean and unit
  # standard deviation. This is very important for numerical reasons since the stability
  # of the algorithm is dependent of the coordinate representation. 
  
  # 'from' points
  
  m = apply(fp,1,mean)
  m = m[c(1,2)]
  
  maxstd = max(apply(fp, 1, function(fp) sd(fp) * sqrt((length(fp) - 1) / length(fp)) )) + 1e-9
  
  C1 = diag(c(1/maxstd, 1/maxstd, 1) )
  C1[1, 3] = -m[1]/maxstd
  C1[2, 3] = -m[2]/maxstd   
  
  fp = C1 %*% fp 
  
  # 'to' points    
  m = apply(tp,1,mean)
  m = m[c(1,2)]
  
  maxstd = max(apply(tp, 1, function(tp) sd(tp) * sqrt((length(tp) - 1) / length(tp)) )) + 1e-9
  
  C2 = diag(c(1/maxstd, 1/maxstd, 1) )
  C2[1, 3] = -m[1]/maxstd
  C2[2, 3] = -m[2]/maxstd       
  tp = C2 %*% tp    
  
  # Then the matrix A is created using the point correspondences. 
  
  nbr_correspondences = dim(fp)[2]
  
  A = matrix(0,2*nbr_correspondences,9)    
  
  for (i in seq(nbr_correspondences)){    
    A[2*i - 1,] = c(-fp[1,i],-fp[2,i],-1,0,0,0, tp[1,i]*fp[1,i],tp[1,i]*fp[2,i],tp[1,i])
    A[2*i,] = c(0,0,0,-fp[1,i],-fp[2,i],-1,tp[2,i]*fp[1,i],tp[2,i]*fp[2,i],tp[2,i])
  }
  
  # The least squares solution is found as the
  # last row of the matrix V of the SVD.     
  # carry out the singular value decomposition on matrix A    
  s = svd(A, nu = 9, nv = 9)  
  
  S = s$d
  U = s$u
  V = s$v
  
  #The row is reshaped to create H .        
  # the homography    
  H = matrix(V[,9],nrow=3,byrow=T)    
  
  # This matrix is then de-conditioned and normalized before returned. 
  #decondition and normalize   
  H = solve(C2) %*% (H %*% C1) 
  #H = H/H[3,3]
  
  return(H / H[3,3])
}

# 'from' points
fp = t(cbind(Mx,My,c(1)))

# 'to' points
tp = t(cbind(Mt[1,], Mt[2,],c(1)))


H = H_from_points(fp,tp)
H 

rotate

format(rotate, scientific=F)

# transform the M back.

# inv(H) * "to"
Mback = solve(H) %*% tp

plot(c(0,4), c(0,4), type="n", xlab="x", ylab="y")
lines(Mt[1,], Mt[2,], col="red", lwd=2)
lines(Mback[1,], Mback[2,], col="blue", lwd=2)

# forward transform

# H * "from"
Mback = H %*% fp

lines(Mback[1,], Mback[2,], 'p', col="black", pch=16)

## download image 
aerial = 'https://www.dropbox.com/s/wux4mcz3gdau0eb/flag1.jpg?dl=0'

download.file(aerial, 'flag1.jpg', method='auto') 
setwd("//cefnsshares/Homes/NAU-STUDENTS/sha59/Desktop/ENV599/in-class-notes")

# if this doesn't work, it is in the class_notes repo on github

library(imager)

im = load.image('flag1.jpg')
im = grayscale(im)
plot(im)

# counter-clockwise, top left to top right
# big lake, intersection, small lake
u = c(0,0, 2436, 2436, 770, 1350, 285)
v = c(0,2488, 2488, 0, 285, 1100, 2070)

lines(u, v, 'p', col="blue", pch=16)

im2 = imrotate(im,30)

# counter-clockwise, top left to top right
# big lake, intersection, small lake

uprime = c(1250,0, 2100, 3353,  1750, 1870, 465)
vprime = c(0,2150,  3372, 1230, 620, 1650, 1950)

plot(im2) 
lines(uprime, vprime, 'p', col="red", pch=16)

# 'from' points
fp = t(cbind(u,v,c(1)))

# 'to' points
tp = t(cbind(uprime,vprime,c(1)))

H = H_from_points(fp,tp)

format(H, scientific=F)

theta = 30 * (pi/180)

R = matrix(c(cos(theta),-sin(theta),0,sin(theta),cos(theta),0,0,0,1), nrow=3, byrow=T)

R

# we can check the homography is correct by multiplying H by the 'from' coordinates
tp_sim = round(H %*% fp)

# and you can see that the homography is a fairly good solution (with a little error)
tp_sim

tp

# allows us to apply affine transformation to the image
library("EBImage")

rows = dim(im2)[1]
cols = dim(im2)[2]

useH = H[1:3,1:2]

useH

img_affine = affine(im2, useH, filter = "bilinear")#, output.dim = c(cols, rows))

img_affine = as.cimg(img_affine)

#img_affine = as.cimg(flip(img_affine))

img_affine = img_affine[rowSums(img_affine != 0) != 0, ] 
img_affine = img_affine[, colSums(img_affine != 0) != 0 ] 

img_affine = as.cimg(img_affine)

par(mfrow=c(2,3))
plot(im)
plot(im2)
plot(img_affine, ylim=c(0, 2000))


