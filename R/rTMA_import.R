#    Copyright (C) 2010-2017 The University of Edinburgh and 2018-2021 Queens
#    University Belfast
#
#    This file is part of rTMA. The functions in this script have been included
#    with permission from Ian Overton as rTMA is not publically avaialable at
#    time of writing.
#
#    rTMA is free software: you can redistribute it and/or modify it under the
#    terms of the GNU General Public License as published by the Free Software
#    Foundation, either version 3 of the License, or (at your option) any later
#    version.
#
#    rTMA is distributed in the hope that it will be useful, but WITHOUT ANY
#    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#    FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
#    details.
#
#    You should have received a copy of the GNU General Public License along
#    with rTMA.  If not, see <http://www.gnu.org/licenses/>.


##' Calculates entropy given a vector of probabilities (which should sum to one)
##'
##' @title Calculate entropy
##' @param p.vec A vector (or matrix) of discrete probability values, summing to
##'   one
##' @noRd
##' @return The entropy of the vector in bits
##' @keywords internal
##' @author Alexander Lyulph Robert Lubbock, Ian Overton

entropy_from_discrete_probs <- function(p.vec) {
  -sum(p.vec*log2(p.vec),na.rm=T)
}

##' Compute mutual information between all rows of a matrix containing discrete
##' outcomes.
##'
##' Note that only the lower triangle of the matrix is populated for speed, as
##' the result is symmetric. Takes a matrix as input.
##'
##' @title Calculate mutual information of a matrix of discrete values
##' @param mat A matrix of discrete values
##' @param progress.bar Outputs status to terminal when set to 'text', or no
##'   status updates are output when set to \code{FALSE}.
##' @return A lower triangular matrix where element [i,j] contains the mutual
##'   information in bits between row i and row j of the input matrix
##' @keywords networks
##' @author Alexander Lyulph Robert Lubbock, Ian Overton

discrete.mi <- function(mat,progress.bar=FALSE) {
  # compute the marginal entropy from the marginal probability for each row
  marginal.entropy <- apply(mat,1,function(x){entropy_from_discrete_probs(table(x)/length(x))})

  result <- matrix(NA,nrow(mat),nrow(mat),dimnames=list(rownames(mat),rownames(mat)))
  for(i in 2:nrow(result)) {
    for(j in 1:(i-1)) {
      # computes the joint probabilities of rows i and j
      joint.prob.ij <- stats::xtabs(data=t(mat[c(i,j),])) / ncol(mat)
      # the entropy fn works for joint entropy too
      joint.entropy.ij <- entropy_from_discrete_probs(joint.prob.ij)
      # utilise I(X;Y) = H(X)+H(Y)-H(X,Y)
      result[i,j] <- marginal.entropy[i] + marginal.entropy[j] - joint.entropy.ij
    }
  }
  result
}

##' Calculates KDE for a set of points exactly, rather than an approximation as
##' per the density() core function.
##'
##' Only tractable for around 10,000 data points or less - otherwise consider
##' using the density() core function for a close approximation.
##'
##' The density() core function approximation is normally a very good approximation, but
##' some small values close to zero may become zero rather than just very small.
##' This makes it less suitable for mutual information estimation.
##'
##' @title Exact kernel density estimation
##' @param x A numeric vector of values
##' @param bw The bandwidth to use - either a single value, or a vector of
##'   values the same length as \code{x} if using adaptive bandwidth estimation
##'   (with each value giving the bandwidth at the corresponding data point).
##' @param output.domain The domain of values over which to estimate the
##'   density. Defaults to \code{x}. To use the same domain of \code{x} values
##'   as \R's \code{density}, set to \code{NULL}.
##' @param na.rm Remove missing values if \code{TRUE}
##' @return The exact kernel density estimate as a \code{density} object,
##'   compatible with \R's \code{density} function.
##' @importFrom stats bw.nrd0 dnorm na.omit
##' @author Alexander Lyulph Robert Lubbock, Ian Overton
##' @examples
##' \dontrun{
##' x <- c(rnorm(100,sd=2),runif(50),4*rnorm(75,mean=2,sd=4))
##' plot(exact.kde(x,bw.nrd0(x),output.domain=NULL),xlab="x",
##'      col="black",lty=2,main="Exact vs approximate KDE")
##' lines(density(x),col="red",lty=1)
##' legend("topright",c("Exact KDE","Approx KDE"),lty=2:1,col=c("black","red"))
##' }
exact.kde <- function(x,bw,output.domain=x,na.rm=FALSE) {
  dens <- list(call=match.call(),data.name=deparse(substitute(x)),
               has.na=any(is.na(x)),bw=bw)
  if(na.rm) {
    x <- na.omit(x)
    output.domain <- na.omit(output.domain)
  }
  if(is.null(output.domain)) output.domain <- seq(min(x)-(mean(bw)*3),max(x)+(mean(bw)*3),length.out=512)
  if(length(bw)==1) bw <- rep(bw,length(output.domain))
  dens$x <- output.domain[order(output.domain)]
  dens$y <- sapply(output.domain,function(point) sum(dnorm(point, mean=x, sd=bw)))/length(output.domain)
  dens$y <- dens$y[order(output.domain)]
  dens$n <- length(x)
  class(dens) <- "density"
  dens
}

##' Calculates the geometric mean of a vector. Used for variable bandwidth
##' kernel density estimation.
##'
##' @title Geometric mean
##' @param x A numeric vector
##' @return The geometric mean of x
##' @keywords internal
##' @author Alexander Lyulph Robert Lubbock, Ian Overton
geometric.mean <- function(x) exp(mean(log(x)))


##' Calculates variable bandwidth KDE using Abramson's two stage estimator.
##'
##' Bandwidth is first calculated using Silverman's estimator, then refined in a
##' second stage to allow local bandwidth variations in the data based on the
##' initial estimate.
##'
##' @title Variable bandwidth Kernel Density Estimation
##' @param x A numeric vector of values for estimating density
##' @param output.domain The domain of values over which to estimate the
##'   density. Defaults to \code{x}. To use the same domain of \code{x} values
##'   as \R's \code{density}, set to \code{NULL}.
##' @param na.rm Remove missing values if TRUE
##' @param adjust.factor A scaling factor (exponent) applied to the variable
##'   bandwidth calculation. Larger factors result in greater deviation from the
##'   fixed bandwidth (a value of 0 gives the fixed bandwidth case).
##' @return The kernel density estimate as a \code{density} object, compatible
##'   with \R's \code{density} function.
##' @references Abramson, I. S. On Bandwidth Variation in Kernel Estimates-A
##'   Square Root Law. Ann. Statist. 10, 1217-1223 (1982).
##' @author Alexander Lyulph Robert Lubbock, Ian Overton
variable.bw.kde <- function(x,output.domain=x,na.rm=FALSE,adjust.factor=0.5) {
  if(na.rm) {
    x <- na.omit(x)
    output.domain <- na.omit(output.domain)
  }
  base.bw <- bw.nrd0(x)
  ##same x axis as R's density() uses by default
  if(is.null(output.domain)) output.domain <- seq(min(x)-(base.bw*3),max(x)+(base.bw*3),length.out=512)

  d.pilot <- exact.kde(x,base.bw,output.domain=output.domain)
  #prevent divide by zero
  if(any(d.pilot$y < .Machine$double.eps))
    d.pilot$y[d.pilot$y < .Machine$double.eps] <- .Machine$double.eps
  bw.adjust.factor <- (geometric.mean(d.pilot$y)/d.pilot$y)^adjust.factor
  d <- exact.kde(x,base.bw*bw.adjust.factor,output.domain=d.pilot$x)
  d
}
