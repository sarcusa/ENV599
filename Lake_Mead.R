#Author : Stephanie Arcusa
#Deterministic model

#Replication of the study

decrease <- c(0.1, 0.2, 0.3)
n <- length(decrease)
v0 <- 25.7
r0 <- 15
w <- r0
end <- 50

level <- function(v0, r0, w, decrease, end){
 
  v = matrix(data = NA, nrow = 50, ncol = 3)
  v[1,] = v0
  
for(i in 2:50){
    v[i,] <- r0*(1-decrease*(i/end)) - w + v[i-1,]
    }
  
  return(v)

}

reservoir <- level(v0, r0, w, decrease, end = 50)
t = seq(2007, length.out = 2057, by = 1)
reservoir <- cbind(reservoir, t)
colnames(reservoir) <- c("10%", "20%", "30%", "Year")
reservoir <- as.data.frame(reservoir)

library(reshape2)

melted <- melt(reservoir, id = "Year")

Lake.Mead <- ggplot(data = melted, aes(x = Year, y = value, colour = variable))+
  geom_line()+
  ylim(-2, 30)+
  labs(y = "Volume (maf)", title = "Lake Mead Projection")

Lake.Mead