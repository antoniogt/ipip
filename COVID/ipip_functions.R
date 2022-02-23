dt <- 0.75
b <- 5
r.b <- 1
mt <- function(n) { ceiling((b-n) / 3) }
r.min <- 0.75
bs <- 47

createSubsets <- function(minClassTrain, majClassTrain){
  
  # Number of examples of the minority class in each balanced subset
  np <- round(nrow(minClassTrain)*as.double(r.min),0)
  dfs <- list()
  
  # We select all the examples of the majority class we will use, and we reorder them.
  id.maj <- sample(sample(x = 1:nrow(majClassTrain), size = bs*np), size = bs*np)
  
  for(k in 1:bs){
    id.min <- sample(x = 1:nrow(minClassTrain), size = np, replace = TRUE)
    dfs[[k]] <- rbind(majClassTrain[id.maj[1:np + np*(k-1)],],minClassTrain[id.min,])
  }
  dfs
}

prediction <- function(ensemble, x, q){
  pred <- data.frame(matrix(nrow=nrow(x),ncol=0))
  for(modelo in ensemble) pred <- cbind(pred, predict(modelo,x))
  pred <- apply(pred, 1, function(x) prop.table(table(x))["NO"])
  ifelse(is.na(pred) | pred<q, "YES", "NO")
}

metrics <- function(data, lev = levels(as.factor(data$obs)), model = NULL){
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
      Ek <- list() # k-th set of models
      i <- 0 # Counter for the number of attempts to enlarge the ensemble
      
      df <- createSubsets(minClassTrain,maxClassTrain)[[k]]
      while(length(Ek)<=b && i<mt(length(Ek),b)){
        # We select samples to train the basic models
        majority <- which(df$classificationCol == "NO")
        minority <- which(df$classificationCol == "YES")
        ind.train <- c(
          sample(majority, size = round(round(nrow(minClassTrain)*as.double(r.min),0)*as.double(r.b),0), replace = TRUE),
          sample(minority, size = round(round(nrow(minClassTrain)*as.double(r.min),0)*as.double(r.b),0), replace = TRUE)
        )
        
        # Model to train with the previously selected data. Here we can choose the model to train from the 'caret' package.
        model <- train(
          x = df[ind.train,-classificationCol],
          y = df[ind.train,classificationCol],
          method = "ranger",
          metric = metric,
          maximize = T,
          trControl = tC
        )
        
        
        # We evaluate the current ensemble (without including the new basic model).
        ensemble.metrics <-
          if (length(Ek)==0){
            u <- -Inf
            names(u) <- metric
            u
          } else metrics(data.frame(
            obs = test.set$classificationCol,
            pred= prediction(Ek, test.set[-classificationCol], q = dt)
          ))
        
        Ek[[length(Ek)+1]] <- model
        # We evaluate the ensemble formed by adding the new basic model
        ensemble.metrics.2 <- metrics(data.frame(
          obs = test.set$classificationCol,
          pred= prediction(Ek, test.set[-classificationCol], q = dt)
        ))
        # We compare the metric
        if(ensemble.metrics.2[metric] <= ensemble.metrics[metric]){ # If the ensemble does not improve with the new model...
          i <- i+1
          Ek[[length(Ek)]] <- NULL
        } else{ # In case of enlargement of the ensemble, we reset the opportunities for further enlargement.
          i <- 0
        }
        
      } # We have finished building the k-th set of models 
      
      # We add the k-th set to the final ensemble
      E[[length(E)+1]] <- Ek
      
    } # end
    
    E
}

numberModels <- function(ensemble){
  
    print(unlist(lapply(ensemble,length)))
  
}

final.prediction <- function(ensemble, x, q = 0.5, q1){
  # We place in each row of a dataset all the predictions for a sample.
  pred <- as.data.frame(lapply(ensemble, function(e) prediction(e,x, q = q1)))
  
  pred <- apply(pred, 1, function(x) prop.table(table(x))["NO"])
  ifelse(is.na(pred) | pred<q, "YES", "NO")
}
