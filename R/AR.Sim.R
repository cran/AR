AR.Sim <-
function(n, f_X, Y.dist, Y.dist.par, xlim=c(0,1), S_X=xlim, Rej.Num=TRUE, Rej.Rate=TRUE, Acc.Rate=TRUE ){
options(warn=-1)
x = c()
k = 0  # Countur
f_Y <- function(x) pdf(T.dist=Y.dist, T.dist.par=Y.dist.par, x)  # pdf is a function from DISTRIB Package
c = optimize(f=function(x) f_X(x)/f_Y(x), interval=S_X, maximum=T)$objective
c.max = optimize(f=function(x) f_X(x)/f_Y(x), interval=S_X, maximum=T)$maximum
max.f_X = optimize(f_X, interval=S_X, maximum=T)$objective  #Is need for ylim in figures
max.f_Y = optimize(f_Y, interval=S_X, maximum=T)$objective  #Is need for ylim in figures
cat("Optimal c =", round(c,3), fill=TRUE )

par(mfrow=c(3,1), oma=c(1.5, 4, 1.3, 0), mar=rep(1.2, 4), cex=.9, las=1)
curve(f_X(x), col=1, lwd=2, lty=1, xlim=xlim, ylim=c(0, .1+ max(c, max.f_X, max.f_Y)))
curve(f_Y(x), col="coral1", lwd=2, lty=1, add=T)
curve(f_X(x)/f_Y(x), lwd=2, lty=1, col=4, add=T)
abline(h=c, v=c.max, col=4, lty=3)

curve(dunif(x) , xlim=xlim, ylim=c(0,1.1), lwd=2)
curve(f_X(x)/(c*f_Y(x)) , xlim=xlim, col=4, lty=2, lwd=2, add=T)

Y <- U <- c()  #For drow Fig 3

while( length(x) < n ) {
  k = k + 1
  u = runif(1)
  y = rd(1, T.dist=Y.dist, T.dist.par=Y.dist.par)  # rd is a function from DISTRIB Package
  Y = c(Y,y)  #Only for drow Fig 3
  U = c(U,u)  #Only for drow Fig 3
  if( u <= f_X(y)/(c*f_Y(y)) ) 
    { x = c(x,y) 
    points(y, u , type="p", pch=20, col=3, add=T)
    }
   else
     { 
     points(y, u , type="p", pch=20, col=2, add=T)
     }
  }

curve(c * f_Y(x) , col="coral1", xlim=xlim, ylim=c(0, .02+ max(max.f_X, c*max.f_Y)), lty=2, lwd=2)
curve( f_X(x) , xlim=xlim, col=1, lty=2, lwd=2, add=T)
points(Y, U*c*f_Y(Y) , type="p", pch=20, add=T,  
       col = 3*(U <= f_X(Y)/(c*f_Y(Y))) + 2*(U > f_X(Y)/(c*f_Y(Y)))  )
title("Graphical Presentation to Acceptance-Rejection Method          ", outer=TRUE)
par(mfcol=c(1,1))    #Reset display

#mtext("y", side=1, padj=3, outer=F)  #For xlab

mtext("f_X(x)       ", font=4, col=1, side=2, padj=-19, outer=T)  #For ylab of Fig 1
mtext("f_Y(x)       ", font=4, col="coral1", side=2, padj=-17, outer=T)  #For ylab of Fig 1
mtext("f_X(x)       ", font=4, col=4, side=2, padj=-14, outer=T)  #For ylab of Fig 1
mtext("---------       ", font=4, col=4, side=2, padj=-13, outer=T)  #For ylab of Fig 1
mtext("f_Y(x)       ", font=4, col=4, side=2, padj=-12, outer=T)  #For ylab of Fig 1

mtext("f_U(y)       ", font=4, col=1, side=2, padj=-2, outer=T)  #For ylab of Fig 2
mtext("f_X(y)       ", font=4, col=4, side=2, padj=1, outer=T)  #For ylab of Fig 2
mtext("----------      ", font=4, col=4, side=2, padj=2, outer=T)  #For ylab of Fig 1
mtext("c. f_Y(y)   ", font=4, col=4, side=2, padj=3, outer=T)  #For ylab of Fig 1

mtext("c. f_Y(y)    ", font=4, col="coral1", side=2, padj=14, outer=T)  #For ylab of Fig 3
mtext("f_X(y)       ", font=4, col=1, side=2, padj=16, outer=T)  #For ylab of Fig 3

mtext("Points  (y ,  u. c. f_Y(y))                     Points  (y , u)               Computing the optimum c", 4, 0, outer=F, las=0)  #For ylab in right side

if(Rej.Num != "FALSE")  
   cat( "The numbers of Rejections =", k-n, fill=TRUE )
if(Rej.Rate != "FALSE") 
   cat( "Ratio of Rejections =", round((k-n)/k, 3), fill=TRUE )
if(Acc.Rate != "FALSE")  
   cat( "Ratio of Acceptance =", 1-round((k-n)/k, 3), fill=TRUE )

Sim.data <- x
return(Sim.data)
}
