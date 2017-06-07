\name{effect.size}
\alias{effect.size}

\title{
Effect Size
}
\description{
Calculates Cohen's f, Eta^2 and Omega^2
}
\usage{
effect.size(aov.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aov.object}{
the ANOVA-Object created with aov()
}
}
\details{
  The function calculates the Cohen's f, Eta^2 and Omega^2 for interpreting the effect size of the experiment.
}
\value{
  The output is the three effect sizes Cohen's f, Eta^2 and Omega^2.
}

\author{
  Benjamin Schlegel, \email{kontakt@benjaminschlegel.ch}
}

\examples{
df = data.frame(Y=c(5,3,3,3,1,2,3,6,2,5,3,5,6,5,6,5,4,5,6,5,6,7,6,7),A=sort(rep(1:4,6)))
object = aov(Y~A, data=df)
effect.size(object)
}

\keyword{anova, effect size, oneway anova, twoway anova}
