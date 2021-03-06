% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/mass_balance.R
\name{SimpleMassBalance}
\alias{SimpleMassBalance}
\title{Simple MC Water Quality Mass Balance}
\usage{
SimpleMassBalance(us.q, us.f, dis.q, dis.f, shots = 2500, cor = 1L)
}
\arguments{
\item{us.q}{a vector of upstream quality}

\item{us.f}{a vector of upstream flow}

\item{dis.q}{a vector of discharge quality in same units as \code{us.q}}

\item{dis.f}{a vector of discharge flow in same units as \code{us.f}}

\item{shots}{number of shots in Monte Carlo runs. Numeric.}

\item{cor}{integer between 0 and 1 indicating correlation between flow and
       quality}
}
\value{
An S3 object of class "MassBalance" with the following components
        \itemize{
                 \item \code{Downstream.flow} is a vector of downstream flows
                 generated by individual Monte Carlo shots
                 \item \code{Downstream.quality} is a vector of downstream
                 quality generated by individual Monte Carlo shots
                 \item \code{Downstream.summary} vector of \code{mean} and
                 \code{sd} for flow and quality
        }
}
\description{
\code{SimpleMassBalance} returns a "MassBalance" object.
}
\details{
Use SimpleMassBalance() within ofther functions to do the mass
         balance stuff will use other functions to create distributions and
         summary bits and bobs depending on whether user wants to input full
         datasets or just summary stats equivalent of RQP 2.5
}
\examples{
s <- 1000

     us.flow <- rlnorm(s, meanlog = log(us.flow.mean), sdlog = log(us.flow.sd))
     us.qual <- rlnorm(s, meanlog = log(us.qual.mean), sdlog = log(us.qual.sd))
     dis.flow <- rlnorm(s, meanlog = log(dis.flow.mean), sdlog = log(dis.flow.sd))
     dis.qual <- rlnorm(s, meanlog = log(dis.qual.mean), sdlog = log(dis.qual.sd))

     z <- SimpleMassBalance(us.q = us.qual, us.f = us.flow, dis.q = dis.qual,
     dis.f = dis.flow, shots = s, cor = 1L)
}

