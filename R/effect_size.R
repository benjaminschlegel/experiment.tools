cohens.f = function(aov.object){
  df = summary(aov.object)[[1]][,"Df"]
  df = df[-length(df)]
  MS = summary(aov.object)[[1]][,"Mean Sq"]
  sigma2 = df/nobs(aov.object) * (MS[1:(length(MS)-1)]-MS[length(MS)])
  
  sigma = sqrt(sigma2)
  sigma.epsilon = sqrt(MS[length(MS)])
  
  output = data.frame(cohens.f=sigma/sigma.epsilon)
  
  rnames = rownames(summary(aov.object)[[1]])
  rnames = rnames[-length(rnames)]
  
  rownames(output) = rnames
  return(output)
}

effect.size = function(aov.object){
  cat("Effect size:\n\n")
  output = cohens.f(aov.object)
  output$coehns.f_effect.size = ifelse(output$cohens.f<0.25,"(small effect size)",ifelse(output$cohens.f<0.4,"(medium effect)","(large effect)"))
  etasq = as.data.frame(BaylorEdPsych::EtaSq(aov.object))
  etasq$'Eta^2 Effect Size' = ifelse(etasq$`Eta^2`<0.06,"(small effect)",ifelse(etasq$`Eta^2`<0.14,"(medium effect)","(large effect)"))
  output = cbind(output,etasq)
  output$'Omega^2' = output$cohens.f^2/(1-output$cohens.f^2)
  colnames(output) = c("Cohen's f","Cohen's f Effect Size","Eta^2","Partial Eta^2","Eta^2 Effect Size","Omega^2")
  return(output)
}
