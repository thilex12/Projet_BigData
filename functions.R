lst_abattre <- function(data){
  model <- glm(data$fk_arb_etat != "EN PLACE" ~ data$age + data$fk_stadedev + data$remarquable, family="binomial")
  # summary(model)
  resultat<-predict(model, data.frame(data),type="response")
  # plot(resultat)


  data$abattre <- FALSE
  data$abattre[resultat > 0.6] <- TRUE

  return(data)
}
