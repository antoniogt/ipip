dt <- 0.75
b <- 5
r.b <- 1
mt <- function(n) { ceiling((b-n) / 3) }
r.min <- 0.75
bs <- 47

createSubsets <- function(minClassTrain, mayClassTrain){
  
  #Partición
  np <- round(nrow(minClassTrain)*as.double(r.min),0)
  dfs <- list()
  
  # Seleccionamos todos los pacientes no ingresados que usaremos, y los reordenamos
  id.may <- sample(sample(x = 1:nrow(mayClassTrain), size = bs*np), size = bs*np)
  
  for(k in 1:bs){
    id.min <- sample(x = 1:nrow(minClassTrain), size = np, replace = TRUE)
    dfs[[k]] <- rbind(mayClassTrain[id.may[1:np + np*(k-1)],],minClassTrain[id.min,])
  }
  dfs
}

prediction <- function(ensemble, x, q){
  pred <- data.frame(matrix(nrow=nrow(x),ncol=0))
  for(modelo in ensemble) pred <- cbind(pred, predict(modelo,x))
  pred <- apply(pred, 1, function(x) prop.table(table(x))["NO"])
  ifelse(is.na(pred) | pred<q, "YES", "NO")
}

metricas <- function(data, lev = levels(as.factor(data$obs)), model = NULL){
  c(
    ACCURACY = MLmetrics::Accuracy(data[, "pred"], data[, "obs"]),
    SENS = sensitivity(data[, "pred"],data[, "obs"],positive="YES",negative="NO"),
    SPEC = specificity(data[, "pred"], data[, "obs"],positive="YES",negative="NO"),
    PPV = posPredValue(data[, "pred"], data[, "obs"],positive="YES",negative="NO"),
    NPV = negPredValue(data[, "pred"], data[, "obs"],positive="YES",negative="NO"),
    KAPPA = psych::cohen.kappa(cbind(data[, "obs"],data[, "pred"]))$kappa
  )
}

createEnsemble<- function(metric){
  
  set.seed(12)
    
    for(k in 1:bs){
      Ek <- list() # Ensemble de modelos k-ésimo
      i <- 0 # Contador para el número de intentos de ampliar el ensemble
      # Conjunto de datos perfectamente balanceado:
      
      df <- createSubsets(minClassTrain,maxClassTrain)[[k]]
      while(length(Ek)<=b && i<mt(length(Ek),b)){
        # Seleccionamos muestras para entrenar el modelo de random forest
        mayority <- which(df$classificationCol == "NO")
        minority <- which(df$classificationCol == "YES")
        ind.train <- c(
          sample(mayority, size = round(round(nrow(minClassTrain)*as.double(r.min),0)*as.double(r.b),0), replace = TRUE),
          sample(minority, size = round(round(nrow(minClassTrain)*as.double(r.min),0)*as.double(r.b),0), replace = TRUE)
        )
        
        rf <- train(
          x = df[ind.train,-classificationCol],
          y = df[ind.train,classificationCol],
          method = "ranger",
          metric = metric,
          maximize = T,
          trControl = tC
        )
        
        
        # Evaluamos el ensemble actual (sin el nuevo modelo)
        metricas.ensemble <-
          if (length(Ek)==0){
            u <- -Inf
            names(u) <- metric
            u
          } else metricas(data.frame(
            obs = test.set$classificationCol,
            pred= prediction(Ek, test.set[-classificationCol], q = dt)
          ))
        
        Ek[[length(Ek)+1]] <- rf
        # Evaluamos el ensemble formado al añadir el nuevo modelo
        metricas.ensemble.2 <- metricas(data.frame(
          obs = test.set$classificationCol,
          pred= prediction(Ek, test.set[-classificationCol], q = dt)
        ))
        # Comparamos las metricas
        if(metricas.ensemble.2[metric] <= metricas.ensemble[metric]){ # Si el ensemble no mejora con el nuevo modelo...
          i <- i+1
          Ek[[length(Ek)]] <- NULL
        } else{ # En caso de ampliar el ensemble, reseteamos las oportunidades de cara a una nueva ampliación
          i <- 0
        }
        
      } # Fin del WHILE (hemos terminado de construir el ensemble k-ésimo)
      
      # Guardamos la información del ensemble k-ésimo
      E[[length(E)+1]] <- Ek
      
    } # FIN. Hemos terminado de contruir el ensemble final
    
    E
}

numberModels <- function(ensemble){
  
    print(unlist(lapply(ensemble,length)))
  
}

final.prediction <- function(ensemble, x, q = 0.5, q1){
  # Colocamos en cada fila de un conjunto de datos todas las predicciones para una muestra
  pred <- as.data.frame(lapply(ensemble, function(e) prediction(e,x, q = q1)))
  
  pred <- apply(pred, 1, function(x) prop.table(table(x))["NO"])
  ifelse(is.na(pred) | pred<q, "YES", "NO")
}
