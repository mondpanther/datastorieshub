
competition_1 <- function(
  ngroups = 12,
  seed = 100,          # seed for random generation (default = 100)
  test = FALSE
) {
  
  require(caret)
  
  # set the seed
  set.seed(seed)
  
  crime <- read.csv("crime_competition.csv")
  val <- read.csv("crime_validation.csv")
  
  crime$larcPerPop <- factor(crime$larcPerPop,levels = c(0,1), labels = c("0","1"))
  val$larcPerPop <- factor(val$larcPerPop,levels = c(0,1), labels = c("0","1"))
  
  performance <- data.frame(group = rep(NA, ngroups),
                            a1 = rep(NA, ngroups),
                            #p1 = rep(NA, ngroups),
                            #r1 = rep(NA, ngroups),
                            a2 = rep(NA, ngroups),
                            #p2 = rep(NA, ngroups),
                            #r2 = rep(NA, ngroups),
                            p = rep(NA, ngroups),
                            formula = rep(NA, ngroups))
  
  for(i in 1:ngroups){
    source(paste0("template_1_",LETTERS[i],".R"))
    if(test==TRUE){
      formula <- paste("larcPerPop ~ ",paste(sample(colnames(crime)[-140],rbinom(1,130,0.5)),collapse=" + "))
    }
    
    logit.mod <- glm(formula(formula), data = crime, 
                     family = binomial(logit)) 
    
    fitted.results <- predict(logit.mod, newdata=crime, type="response") # predict probabilities
    fitted.results <- ifelse(fitted.results > 0.5,1,0) # convert to binary
    fitted.results <- factor(fitted.results,levels = c(0,1), labels = c("0","1"))
    performance1 <- confusionMatrix(fitted.results,
                                    crime[,"larcPerPop"])
    
    fitted.results <- predict(logit.mod, newdata=val, type="response") # predict probabilities
    fitted.results <- ifelse(fitted.results > 0.5,1,0) # convert to binary
    fitted.results <- factor(fitted.results,levels = c(0,1), labels = c("0","1"))
    performance2 <- confusionMatrix(fitted.results,
                                    val[,"larcPerPop"])
    
    performance$group[i] <- LETTERS[i]
    performance$a1[i] <- round(performance1$overall[1],4)
    #performance$p1[i] <- round(performance1$byClass[4],4)
    #performance$r1[i] <- round(performance1$byClass[2],4)
    performance$a2[i] <- round(performance2$overall[1],4)
    #performance$p2[i] <- round(performance2$byClass[4],4)
    #performance$r2[i] <- round(performance2$byClass[2],4)
    performance$p[i] <- length(logit.mod$coefficients) - 1
    performance$formula[i] <- formula
  }
  
  colnames(performance) <- c("group",
                             "accuracy in train",
                             #"precision in train",
                             #"recall in train",
                             "accuracy in test",
                             #"precision in test",
                             #"recall in test",
                             "complexity",
                             "formula"
                             )
  
  
  write.csv(performance,"performance_1.csv")
  
}

