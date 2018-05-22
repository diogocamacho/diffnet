\name{diffnet}
\alias{diffnet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Differential network analyses based on correlation.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function infers a co-expression matrix based on expression data based on either Pearson or Spearman correlation, for 2 independent conditions.  It then computes the differences in topology (as measured by differences in edge weights -- correlations) basedon Fisher's correlation test.
}
\usage{
diffnet(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Numerical matrix given as Variable x Observations}
  \item{group1}{IDs for observations for a condition of interest}
  \item{group2}{IDs for observations for a second condition of interest}
  \item{correlation_type}{Pearson or Spearman correlations.  Defaults to Spearman.}
  \item{correlation_thr}{Correlation threshold.  Defaults to 0.8}
  \item{p_thr}{p-value threshold for correlation (FDR-corrected).  Defaults to 0.05}
  \item{variable_names}{Names of variables in the data set.  If absent uses row names (if available) or row numbers.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
A <- replicate(expr = runif(n = 100,min = 2,max = 16),n = 120)
ctr <- sample(x = ncol(A),size = ncol(A)/2,replace = FALSE)
test <- setdiff(seq(1,ncol(A)),ctr)
DN <- diffnet(data = A,
              group1 = ctr,
              group2 = test,
              corType = "spearman",
              pThr = 1,
              corThr = 0,
              varNames = var_names)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
