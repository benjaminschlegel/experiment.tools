cohens.f = function(aov.object){
  SSA = summary(aov.object)[[1]][, "Sum Sq"][1]
  SST = sum(summary(aov.object)[[1]][, "Sum Sq"])
  return(powerAnalysis::ES.anova.oneway(data = NULL, sst = SST, ssb = SSA))
}

effect.size = function(aov.object){
  f = cohens.f(aov.object)
  cat("Cohen's f: ",f$f)
  if(f$f<0.25){
    cat(" (small effect size)\n\n")
  }else if(f$f<0.4){
    cat(" (medium effect)\n\n")
  }else{
    cat(" (large effect)\n\n")
  }
  cat("Eta^2:\n")
  etasq = as.data.frame(BaylorEdPsych::EtaSq(aov.object))
  etasq$'' = ifelse(etasq$`Eta^2`<0.06,"(small effect)",ifelse(etasq$`Eta^2`<0.14,"(medium effect)","(large effect)"))
  print(etasq)
  cat("\n\nOmega^2 is", f$f^2/(1-f$f^2))
}
