"name: Han Xiao
assignment1, MA415"

#3 try R by itself
b <- scan()
length(b)
sum(b)
mean(b)
c <- scan("read_this_1.txt")
write.table(c, file = "read_this_1.csv",row.names=FALSE,col.names = FALSE)
d <- scan("read_this_1.csv")

#basic R exercise1
#1
a <- 1:20
y <- 20:1
z <- append(a,19:1)
tmp <- c(4,6,3)
tmp1 <- rep(tmp, 10)
tmp2 <- rep(tmp, times = 10, len = 31)
tmp3 <- rep(tmp, c(10,20,30))

#2
x <- seq(3,6,by=.1)
vectorcos <- exp(x)*cos(x)

#3
(.1^seq(3,36,by=3))*(0.2^seq(1,34,by=3))
(2^seq(1,25))/(1:25)

#4
e <- seq(10,100)
sum(e^3+(4*(e^2)))

f <- seq(1,25)
sum(((2^f)/f)+((3^f)/(f^2)))

#5
labs <- paste(c("label "), 1:30, sep="")

labs2 <- paste(c("fn"), 1:30, sep="")

#6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)

#(a)
yVec[2:250]/xVec[1:249]
#(b)
sin(yVec[1:249])/cos(xVec[2:250])
#(c)
xVec[1:248]+(2*xVec[2:249])-xVec[3:250]
#(d)
sum(exp(-xVec[2:250])/(xVec[1:249]+10))

#7
#(a)
vec1 <- yVec[yVec>600]
#(b)
yindex <- match(vec1,yVec)
#(c)
xvalue <- xVec[yindex]
#(d)
xMean = mean(xVec)
xVec2 = abs(xVec-xMean)^.5
#(e)
sortedY <- sort(yVec,decreasing = TRUE)
maxY <- sortedY[1]
minY <- sortedY[200]
length(xVec[maxY>xVec & xVec>minY])
#(f)
length(xVec[xVec%%2 == 0])
#(g) ???
#(h)
indexPos <- seq(1,250,by=3)
yVec[indexPos]

#8
sum(cumprod(seq(2,38,by=2)/seq(3,39,by=2)))+1
