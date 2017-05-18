\name{AR-package}
\alias{AR-package}
\alias{AR}
\docType{package}
\title{
\packageTitle{AR}
}
\description{
There are many distributions for which the inverse transform method and even
general transformations will fail to be able to generate the required random
variables. For these cases, we must turn to indirect methods; that is, methods
in which we generate a candidate random variable and only accept it subject
to passing a test. This class of methods is extremely powerful
and will allow us to simulate from virtually any distribution; see (Robert and Casella, 2010) for more detailes.
These so-called \code{Accept-Reject methods} only require us to know the functional
form of the density \eqn{f_X(.)} of interest (called the target density) up to a
multiplicative constant. We use a simpler (to simulate) density \eqn{f_Y}, called the
instrumental or candidate density, to generate the random variable for which
the simulation is actually done. The constraints we impose on this candidate
density \eqn{f_Y} are that:

(i) \eqn{Y} be simulate-able (the data simulation from \eqn{Y} be actually possible).

(ii) There is a constant \eqn{c} with \eqn{ \frac{f_X(x)}{f_Y(x)} \leq c } for all \eqn{ x \in S_X=\lbrace x: f_X(x)>0 \rbrace}.

(iii) \eqn{f_X} and \eqn{f_Y} have compatible supports (i.e., \eqn{S_X \subseteq S_Y}).

In this case, \eqn{X} can be simulated as follows by Accept-Reject method. First, we generate \eqn{y} from \eqn{Y \sim f_Y} and,
independently, we generate \eqn{u} from  \eqn{U \sim U(0,1)}. If  \deqn{u \leq \frac{f_X(y)}{c \ f_Y(y)},} 
then we set  \eqn{x=y}. If the inequality is not satisfied, we then discard/reject \eqn{y} and
\eqn{u} and start again (Robert and Casella, 2010).
}
\details{
Package \code{AR} provides a useful tool for teaching students to understand the theoritical idea behind the \code{Accept-Reject method}. This package works with only one function, i.e. function \code{AR.Sim} which can generate random sample/vector on the basis of the  \code{Accept-Reject method}.
}
\author{
Abbas Parchami

Maintainer: \packageMaintainer{AR}
}

\references{
Iacus, S.M., Simulation and Inference for Stochastic Differential Equations: With R Examples, Springer, New York (2008).

Jones, O., Maillardet, R, Robinson, A., Introduction to Scientific Programming and Simulation Using R, Chapman & Hall/CRC, Boca Raton (2009).

Robert, C.P., Casella, G., Introducing Monte Carlo Methods with R, New York: Springer (2010).

Vasishth, S., Broe, M., The Foundations of Statistics: A Simulation-based Approach, Springer (2010).

Wikipedia, the free encyclopedia, Rejection sampling, 
https://en.wikipedia.org/wiki/Rejection_sampling
}

\keyword{ DISTRIB }
\keyword{ AR }
\keyword{ AR.Sim }
\keyword{ Simulation }
\keyword{ Accept-Reject method }
\keyword{ optimization }