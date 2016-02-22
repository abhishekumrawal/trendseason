#' Relative Ordering Test for Presence of Trend
#'
#' @param y is vector representing the observed time series with n observations
#' @return results of the relative ordering test
#' @author Abhishek K. Umrawal
#' @details
#' @seealso \code{rank}
#' @export
#' @import from base rank
#' @usage ro.test(y)
#' @examples ro.test(AirPassengers)
ro.test <- function (y = timeseries){
  n<-length(y)
  q<-0
  for(i in 1:(n-1))
  {
    for(j in (i+1):n)
    {
      if(y[i]>y[j])
      {
        q<-q+1
      }
    }
  }
  eq<-n*(n-1)/4
  tau<-1-(4*q/(n*(n-1)))
  var_tau<-(2*(2*n+5))/(9*n*(n-1))
  z<-tau/sqrt(var_tau)
  if(z>0)
  {
    p_value<-1-pnorm(z)
  }
  if(z<0)
  {
    p_value<-pnorm(z)
  }
  cat("            Relative Ordering Test for Presence of Trend \n\n")
  cat("Null Hypothesis: Absence of Trend, and \n")
  cat("Alternative Hypothesis: Presence of Trend. \n\n")
  cat("Test Statistic:",paste(round(z,4)),"\n")
  cat("p_value:", paste(round(p_value,4)),"\n")
  cat("No. of Discordants:",paste(q),"\n")
  cat("Expected No. of Discordants:",paste(eq),"\n")
}

#' Friedman (JASA) Test for Presence of Seasonality
#'
#' @param y is vector representing the observed time series with n observations
#' @param r is a positive integer representing the no. of seasons
#' @return results of the Freidman test
#' @author Abhishek K. Umrawal
#' @details
#' @seealso \code{rank}
#' @export
#' @import from base rank
#' @usage friedman.test(y,r)
#' @examples friedman.test(AirPassengers,12)

friedman.test <- function (y = timeseries, r = seasons){
  n<-length(y)
  c<-n/r
  data<-matrix(y,r,c)
  rank<-matrix(y,r,c)
  for (i in 1:c)
  {
    rank[,i]<-rank(data[,i])
  }
  obsranksums<-rowSums(rank)
  expranksums<-rep(c*(r+1)/2,r)
  sumofsquares<-sum((obsranksums-expranksums)^2)
  chi_square<-sumofsquares/(c*(r+1)/2)
  p_value<-1-pchisq(chi_square,r-1)

  cat("            Freidman (JASA) Test for Presence of Seasonality \n\n")
  cat("Null Hypothesis: Absence of Seasonality, and \n")
  cat("Alternative Hypothesis: Presence of Seasonality. \n\n")
  cat("Test Statistic:",paste(round(chi_square,4)),"(Chi Sqaure with",paste(r-1),"df)","\n")
  cat("p_value:", paste(round(p_value,4)),"\n")
}
