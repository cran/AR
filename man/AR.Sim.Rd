\name{AR.Sim}
\alias{AR.Sim}
\title{
Graphical Visualization for Accept-Reject Method
}
\description{

Package \code{AR} provides a graphical presentation for Accept-Reject method by drawing three figures which their explanations are as follow:


Explanation of Figure 1:  

Moreover, even when the Rejection Accept-Reject method is applied, it is always hard to optimize the constant \eqn{c} for the likelihood ratio. Although, the algorithm works with a bigger constant \eqn{c} (with respect to optimal/minimum possible \eqn{c}), but increasing \eqn{c} cause high rejection rate and the algorithm can be very in-efficient. 
The first figure show three curves \eqn{f_X(x)}, \eqn{f_Y(x)} and \eqn{\frac{f_X(x)}{f_Y(x)}}. Moreover, the optimum \eqn{c}  (minimum possible \eqn{c}, such that \eqn{\frac{f_X(x)}{f_Y(x)} \leq c}) calculated as the maximum height of the curve \eqn{\frac{f_X(x)}{f_Y(x)}}, which is also shown on the first figure.


Explanation of Figure 2:  

To visualize the motivation behind the \code{Acceptance-Rejection method}, imagine graphing curve \eqn{\frac{f_X(y)}{c \ f_Y(y)}} onto a large rectangular board and throwing darts at it. Assume that the \eqn{x}-positions of these darts/points are uniformly distributed around the board and the distribution of \eqn{y}-positions of them are based on \eqn{Y} distribution. Now, remove all of the darts/points that are outside the area under the curve \eqn{\frac{f_X(y)}{c \ f_Y(y)}}. 
The \eqn{x}-positions of the remaining darts will be distributed according to the random variable's density of \eqn{X} within the area under the curve. Since, it can be prove that \deqn{ P\left[ Y\leq y \ | \ U \leq \frac{f_X(Y)}{c \ f_Y(Y)} \right] = P\left( X \leq x \right) . } 


Explanation of Figure 3:  

For another graphical presentation of the motivation behind the \code{Acceptance-Rejection method}, assumes that the considered board (which is presented in explanation of Figure 2) is not necessarily rectangular but is shaped according to some distribution that we know how to generate sample from it (\eqn{c . f_Y(y)}).
Therefore, if \eqn{y}-positions of random points/darts be equal to \eqn{u.c.f_Y(y)}, then all darts/points will be land under the curve \eqn{c.f_Y(y)}. 
The acceptance condition in the \code{Acceptance-Rejection method} is \deqn{u \leq \frac{f_X(y)}{c \ f_Y(y)},} or equivalently  \deqn{u.c.f_Y(y) \leq f_X(y),} 
and it means that after omitting the extra/red random darts/points from the board (which are not satisfy in the acceptance condition), the \eqn{x}-positions of the remaining darts/points will be distributed according to the distribution of \eqn{X}.
}

\usage{
AR.Sim(n, f_X, Y.dist, Y.dist.par, xlim = c(0, 1), S_X = xlim, Rej.Num = TRUE,
       Rej.Rate = TRUE, Acc.Rate = TRUE)
}
\arguments{
  \item{n}{
The number/length of data which must be generated/simulated from \eqn{f_X} density.
}
  \item{f_X}{
The density \eqn{f_X} of interest for simulation (called the target density)
}
  \item{Y.dist}{
The distribution name of the random variable \eqn{Y}, which used to generate the random data from \eqn{f_Y}. Precisely, \code{Y.dist} is the name of \eqn{f_Y} density which is match with \code{DISTRIB} Package. For example, use \code{ Y.dist = "norm"}, when \eqn{ Y \sim N(\mu, \sigma^2) }. 
}
  \item{Y.dist.par}{
A vector of \eqn{Y} distribution parameters with considered ordering in \code{stats} package and also is match with \code{DISTRIB} Package. For example, use \code{ Y.dist.par = }
 \code{c(2,3)}, when \eqn{ Y \sim N(\mu=2, \sigma^2=9) }. 
}
  \item{xlim}{
\code{NULL} or a numeric vector of length 2; if non-NULL it provides the defaults for c(from, to) and, unless \code{add=TRUE}, selects the \eqn{x}-limits of the available plot. Its default is \code{ xlim=c(0,1)}.
}
  \item{S_X}{
The support of \eqn{X} with default \code{ S_X = xlim }, which is needed for calculating the optimum value of constant \eqn{c}.
}
  \item{Rej.Num}{
A logical argument with default \code{TRUE} for calculate the number of rejections in \code{Accept-Reject method}. If \code{Rej.Num = FALSE}, then the number of rejections is not reported.
}
  \item{Rej.Rate}{
A logical argument with default \code{TRUE} for calculate the ratio of rejections in \code{Accept-Reject method} (i.e. \code{Rej.Num / n}). If \code{Rej.Rate = FALSE}, then the ratio of rejections is not reported.
}
  \item{Acc.Rate}{
A logical argument with default \code{TRUE} for calculate the ratio of acceptances in \code{Accept-Reject method} (i.e. \code{ 1 - Rej.Rate }). If \code{Acc.Rate = FALSE}, then the ratio of acceptances is not reported.
}
}

\value{
A vector of generated/simulated data from random variable \eqn{X} with length \eqn{n}.

Optimum value for \eqn{c}, i.e. the minimum possible value for \eqn{c}.
}
\references{
Robert, C.P., Casella, G., Introducing Monte Carlo Methods with R, New York: Springer (2010).

Wikipedia, the free encyclopedia, Rejection sampling, 
https://en.wikipedia.org/wiki/Rejection_sampling
}


\examples{
# Example 1: 
data = AR.Sim( n = 150, 
         f_X = function(y){dbeta(y,2.7,6.3)},
         Y.dist = "unif", Y.dist.par = c(0,1),
         Rej.Num = TRUE,
         Rej.Rate = TRUE,
         Acc.Rate = FALSE
         ) 

# QQ-plot
q <- qbeta(ppoints(100), 2.7, 6.3)
qqplot(q, data, cex=0.6, xlab="Quantiles of Beta(2.7,6.3)", 
       ylab="Empirical Quantiles of simulated data")
abline(0, 1, col=2)


# ------------------------------------------------------
# Example 2: From Page 54 of (Robert and Casella, 2009)
f_X = function(x) dbeta(x,2.7,6.3)
Simulation1 <- AR.Sim(n=300, f_X, Y.dist = "unif", Y.dist.par = c(0,1)) 
Simulation2 <- AR.Sim(n=2000, f_X, Y.dist="beta", Y.dist.par=c(2,6) ) 
Simulation3 <- AR.Sim(n=1000, f_X, Y.dist="beta", Y.dist.par=c(1.5,3.7) ) 
Simulation4 <- AR.Sim(n=250, f_X, Y.dist="norm", Y.dist.par=c(.5,.2) ) 
Simulation5 <- AR.Sim(n=200,  f_X, Y.dist="exp", Y.dist.par=3 ) 
Simulation6 <- AR.Sim( 400 ,  f_X, Y.dist="gamma", Y.dist.par=c(2,5) ) 

hist(Simulation1, prob=TRUE)#, col="gray20")
hist(Simulation2, prob=TRUE, add=TRUE, col="gray35")
hist(Simulation3, prob=TRUE, add=TRUE, col="gray60")
hist(Simulation4, prob=TRUE, add=TRUE, col="gray75")
hist(Simulation5, prob=TRUE, add=TRUE, col="gray85")
hist(Simulation6, prob=TRUE, add=TRUE, col="gray100")
curve(f_X(x), add=TRUE, col=2, lty=2, lwd=3)

#compare empirical and theoretical percentiles:
p <- seq(.1, .9, .1)
Qhat1 <- quantile(Simulation1, p) #Empirical quantiles of simulated sample 
Qhat2 <- quantile(Simulation2, p) #Empirical quantiles of simulated sample 
Qhat3 <- quantile(Simulation3, p) #Empirical quantiles of simulated sample 
Qhat4 <- quantile(Simulation4, p) #Empirical quantiles of simulated sample 
Qhat5 <- quantile(Simulation5, p) #Empirical quantiles of simulated sample 
Qhat6 <- quantile(Simulation6, p) #Empirical quantiles of simulated sample 
Q <- qbeta(p, 2.7, 6.3)    #Theoretical quantiles of Be(2.7,6.3)
round( rbind(Q, Qhat1, Qhat2, Qhat3, Qhat4, Qhat5, Qhat6), 3)

# Compute p-value of Kolmogorov-Smirnov test:
ks.test(Simulation1, "pbeta", 2.7, 6.3)$p.value
ks.test(Simulation2, "pbeta", 2.7, 6.3)$p.value
ks.test(Simulation3, "pbeta", 2.7, 6.3)$p.value
ks.test(Simulation4, "pbeta", 2.7, 6.3)$p.value
ks.test(Simulation5, "pbeta", 2.7, 6.3)$p.value
ks.test(Simulation6, "pbeta", 2.7, 6.3)$p.value


# ------------------------------------------------------
# Example 3: Simulate Truncated N(5,2^2) at l=3 and r=14 in left and rigth sides, respectively. 

mu = 5
sigma = 2
l = 3
r = 14
n = 400
f_X = function(x)  dnorm(x,mu,sigma) * 
                   as.integer(l<x & x<r) / (pnorm(r,mu,sigma)-pnorm(l,mu,sigma)) 

Sim1 <- AR.Sim(n, f_X, S_X=c(l,r), Y.dist="norm", Y.dist.par=c(5,4), xlim=c(l-1,r+1) ) 
head(Sim1, 15)
hist(Sim1, prob=TRUE, col="lightgreen")
curve(f_X(x), add=TRUE, col=2, lty=2, lwd=3) # Truncated pdf of N(5,2^2) at l and r
c2 = 1 / (pnorm(r,mu,sigma)-pnorm(l,mu,sigma)) ; c2 #See page 15 jozve

}

\keyword{ DISTRIB }
\keyword{ AR }
\keyword{ AR.Sim }
\keyword{ Simulation }
\keyword{ Accept-Reject method }
\keyword{ optimization }
