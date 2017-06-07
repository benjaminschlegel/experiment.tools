\name{cohens.f}
\alias{cohens.f}

\title{
Cohen's f
}
\description{
Calculates Cohen's f
}
\usage{
cohens.f(aov.object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aov.object}{
the ANOVA-Object created with aov()
}
}
\details{
  The function calculates the Cohen's f. the function is a wrapper for ES.anova.oneway of the package powerAnalysis.
}
\value{
  The output is the effect sizes Cohen's f.
}

\author{
  Benjamin Schlegel, \email{kontakt@benjaminschlegel.ch}
}

\examples{
df = data.frame(Y=c(5,3,3,3,1,2,3,6,2,5,3,5,6,5,6,5,4,5,6,5,6,7,6,7),A=sort(rep(1:4,6)))
object = aov(Y~A, data=df)
cohens.f(object)
}

\keyword{anova, effect size, oneway anova, twoway anova, cohens f}
