assumption.test = function(aov.object, alpha=0.05){
  data = eval(aov.object$call$data)
  Y = data[[paste0(aov.object$call$formula[[2]])]]
  A = data[[trimws(strsplit(paste0(aov.object$call$formula[[3]]),"\\*|\\+")[[1]][1])]]
  normality.test = fBasics::dagoTest(Y)
  p.values = normality.test@test$p.value
  normality = (sum(p.values<alpha)==0)
  
  if(normality){
    cat(paste0("D'Agostino Omnibus Normality Test:\n  Y is normal distributed (p-values: ",paste0(round(p.values,digits=3),collapse=", "),")\n\n"))
    homogeneity.test = bartlett.test(eval(aov.object$call$formula), eval(aov.object$call$data))
    test.used = "Barlett test of homogeneity of variance"
  }else{
    cat(paste0("D'Agostino Omnibus Normality Test:\n  Y is not normal distributed (p-values: ",paste0(round(p.values,digits=3),collapse=", "),")\n\n"))
    quartilsabstand = 1.5*(quantile(Y)[4]-quantile(Y)[2])
    upper = quantile(Y)[3]+quartilsabstand
    lower = quantile(Y)[3]-quartilsabstand
    outliers = (sum(Y<lower | Y>upper)>0)
    if(outliers){
      homogeneity.test = lawstat::levene.test(Y, A, location = "mean")
      test.used = "Levene's test of homogeneity of variance"
    }else{
      homogeneity.test = lawstat::levene.test(Y, A, location = "median")
      test.used = "Brown-Forsyth test of homogeneity of variance (found outliers)"
    }
  }
  p.value = homogeneity.test$p.value
  homogeneity = (p.value>alpha)
  cat(paste0(test.used,":\n  "))
  if(homogeneity){
    cat(paste0("Homogeneity of variance holds (p-value: ",round(p.value,digits = 3),")"))
  }else
    cat(paste0("Homogeneity of variance fails (p-value: ",round(p.value,digits = 3),")"))
}