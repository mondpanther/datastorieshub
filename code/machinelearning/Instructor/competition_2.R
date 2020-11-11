
competition_2 <- function(
  ngroups = 12,
  seed = 100,          # seed for random generation (default = 100)
  first_split = 0.4,
  second_split = 0.5,
  test = FALSE
) {
  
  require(caret)
  require(rpart)
  require(e1071)
  
  # set the seed
  set.seed(seed)
  
  adult <- read.csv("adult_competition.csv")
  
  train_size <- floor(first_split * nrow(adult))
  train_id <- sample(seq_len(nrow(adult)), train_size, replace = FALSE)
  train_h <- adult[train_id,]
  test_and_val_h <- adult[-train_id,]
  
  val_size <- floor(second_split * nrow(test_and_val_h))
  val_id <- sample(seq_len(nrow(test_and_val_h)), val_size, replace = FALSE)
  test_h <- test_and_val_h[-val_id,]
  val_h <- test_and_val_h[val_id,]
  
  train_h$X..50K <- ordered(train_h$X..50K, c(" <=50K"," >50K"))
  val_h$X..50K <- ordered(val_h$X..50K, c(" <=50K"," >50K"))
  test_h$X..50K <- ordered(test_h$X..50K, c(" <=50K"," >50K"))
  
  performance <- data.frame(group = rep(NA, ngroups),
                            model = rep(NA, ngroups),
                            a1 = rep(NA, ngroups),
                            p1 = rep(NA, ngroups),
                            r1 = rep(NA, ngroups),
                            a2 = rep(NA, ngroups),
                            p2 = rep(NA, ngroups),
                            r2 = rep(NA, ngroups),
                            a3 = rep(NA, ngroups),
                            p3 = rep(NA, ngroups),
                            r3 = rep(NA, ngroups),
                            p = rep(NA, ngroups),
                            formula = rep(NA, ngroups))
  
  for(i in 1:ngroups){
    
    source(paste0("template_2_",LETTERS[i],".R"))
    if(test==TRUE){
      model <- sample(c("logit","tree","svm"),1)
      formula <- paste("X..50K ~ ",paste(sample(colnames(adult)[-14],rbinom(1,13,0.5)),collapse=" + "))
      cp <- rexp(1)
      maxdepth <- rbinom(1,20,0.5)
      cost <- rexp(1)
      gamma <- rexp(1)
    }
    
    if(model == "logit"){
      logit.mod <- glm(formula(formula), data = train_h, 
                       family = binomial(logit)) 
      
      fitted.results <- predict(logit.mod, newdata=train_h, type="response") # predict probabilities
      fitted.results <- ifelse(fitted.results > 0.5,1,0) # convert to binary
      fitted.results <- factor(fitted.results,levels = c(0,1), labels = c(" <=50K"," >50K"))
      performance1 <- confusionMatrix(fitted.results,
                                      train_h[,"X..50K"])
      
      fitted.results <- predict(logit.mod, newdata=val_h, type="response") # predict probabilities
      fitted.results <- ifelse(fitted.results > 0.5,1,0) # convert to binary
      fitted.results <- factor(fitted.results,levels = c(0,1), labels = c(" <=50K"," >50K")) # convert to binary
      performance2 <- confusionMatrix(fitted.results,
                                      val_h[,"X..50K"])
      
      fitted.results <- predict(logit.mod, newdata=test_h, type="response") # predict probabilities
      fitted.results <- ifelse(fitted.results > 0.5,1,0) # convert to binary
      fitted.results <- factor(fitted.results,levels = c(0,1), labels = c(" <=50K"," >50K")) # convert to binary
      performance3 <- confusionMatrix(fitted.results,
                                      test_h[,"X..50K"])
      
      performance$group[i] <- LETTERS[i]
      performance$model[i] <- model
      performance$a1[i] <- round(performance1$overall[1],3)
      performance$p1[i] <- round(performance1$byClass[4],3)
      performance$r1[i] <- round(performance1$byClass[2],3)
      performance$a2[i] <- round(performance2$overall[1],3)
      performance$p2[i] <- round(performance2$byClass[4],3)
      performance$r2[i] <- round(performance2$byClass[2],3)
      performance$a3[i] <- round(performance3$overall[1],3)
      performance$p3[i] <- round(performance3$byClass[4],3)
      performance$r3[i] <- round(performance3$byClass[2],3)
      performance$p[i] <- paste0("params = ", length(logit.mod$coefficients) - 1)
      performance$formula[i] <- formula
    }
    
    if(model == "tree"){
      tree.mod <- rpart(formula(formula), data = train_h, maxdepth = maxdepth, cp = cp) 
      
      fitted.results <- predict(tree.mod, newdata=train_h, type="class") # predict probabilities
      performance1 <- confusionMatrix(fitted.results,
                                      train_h[,"X..50K"])
      
      fitted.results <- predict(tree.mod, newdata=val_h, type="class") # predict probabilities
      performance2 <- confusionMatrix(fitted.results,
                                      val_h[,"X..50K"])
      
      fitted.results <- predict(tree.mod, newdata=test_h, type="class") # predict probabilities
      performance3 <- confusionMatrix(ordered(fitted.results, c(" <=50K"," >50K")),
                                      test_h[,"X..50K"])
      
      
      performance$group[i] <- LETTERS[i]
      performance$model[i] <- model
      performance$a1[i] <- round(performance1$overall[1],3)
      performance$p1[i] <- round(performance1$byClass[4],3)
      performance$r1[i] <- round(performance1$byClass[2],3)
      performance$a2[i] <- round(performance2$overall[1],3)
      performance$p2[i] <- round(performance2$byClass[4],3)
      performance$r2[i] <- round(performance2$byClass[2],3)
      performance$a3[i] <- round(performance3$overall[1],3)
      performance$p3[i] <- round(performance3$byClass[4],3)
      performance$r3[i] <- round(performance3$byClass[2],3)
      performance$p[i] <- paste0("maxdepth = ", maxdepth," cp = ", cp)
      performance$formula[i] <- formula
    }
    
    if(model == "svm"){
      svm.mod <- svm(formula(formula), data = train_h, cost = cost, gamma = gamma) 
      
      fitted.results <- predict(svm.mod, newdata=train_h) # predict probabilities
      performance1 <- confusionMatrix(fitted.results,
                                      train_h[,"X..50K"])
      
      fitted.results <- predict(svm.mod, newdata=val_h) # predict probabilities
      performance2 <- confusionMatrix(fitted.results,
                                      val_h[,"X..50K"])
      
      fitted.results <- predict(svm.mod, newdata=test_h) # predict probabilities
      performance3 <- confusionMatrix(fitted.results,
                                      test_h[,"X..50K"])
      
      performance$group[i] <- LETTERS[i]
      performance$model[i] <- model
      performance$a1[i] <- round(performance1$overall[1],3)
      performance$p1[i] <- round(performance1$byClass[4],3)
      performance$r1[i] <- round(performance1$byClass[2],3)
      performance$a2[i] <- round(performance2$overall[1],3)
      performance$p2[i] <- round(performance2$byClass[4],3)
      performance$r2[i] <- round(performance2$byClass[2],3)
      performance$a3[i] <- round(performance3$overall[1],3)
      performance$p3[i] <- round(performance3$byClass[4],3)
      performance$r3[i] <- round(performance3$byClass[2],3)
      performance$p[i] <- paste0("cost = ", cost," gamma = ", gamma)
      performance$formula[i] <- formula
    }
    
    
  }
  
  
  
  colnames(performance) <- c("group",
                             "model",
                             "accuracy in train",
                             "precision in train",
                             "recall in train",
                             "accuracy in validation",
                             "precision in validation",
                             "recall in validation",
                             "accuracy in test",
                             "precision in test",
                             "recall in test",
                             "parameters",
                             "formula"
  )
  
  write.csv(performance,"performance_2.csv")
  
  
}

