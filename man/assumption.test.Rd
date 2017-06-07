\name{assumption.test}
\alias{assumption.test}

\title{
Assumption Test
}
\description{
Tests the normality and the homogeneity assumptions.
}
\usage{
assumption.test(aov.object, alpha=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aov.object}{
the ANOVA-Object created with aov()
}
  \item{alpha}{
OPTIONAL the alpha value to reject H0, default: 0.05
}
}
\details{
  The function used the Agostino test to test for normality. If normality holds, 
  it uses the Bartlett test to check for homogeneity. If it does not hold, it
  uses the Levene's test if there are no outliers and otherwise the Brown-Forsyth test.
}
\value{
  The test results
}

\author{
  Benjamin Schlegel, \email{kontakt@benjaminschlegel.ch}
}

\examples{
df = data.frame(Y=c(5,3,3,3,1,2,3,6,2,5,3,5,6,5,6,5,4,5,6,5,6,7,6,7),A=sort(rep(1:4,6)))
object = aov(Y~A, data=df)
assumption.test(object)
}

\keyword{anova, assumption, oneway anova, twoway anova, normality, homogeneity of variance}
