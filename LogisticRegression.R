heart=read.csv("heart.csv")
str(heart)
heart$CHESTPAIN = factor(heart$CHESTPAIN)
heart$ECG = factor(heart$ECG)
heart$THAL = factor(heart$THAL)
heart$EXERCISE = factor(heart$EXERCISE)
str(heart)

library(caret)
set.seed(987954)
heart_sampling_vector=createDataPartition(heart$OUTPUT, p = 0.85, list = FALSE)
heart_train=heart[heart_sampling_vector,]
heart_train_labels=heart$OUTPUT[heart_sampling_vector]
heart_test=heart[-heart_sampling_vector,]
heart_test_labels=heart$OUTPUT[-heart_sampling_vector]
heart_model=glm(OUTPUT ~ ., data = heart_train, family = binomial("logit"))
summary(heart_model)

log_likelihoods=function(y_labels, y_probs) {
  y_a <- as.numeric(y_labels)
  y_p <- as.numeric(y_probs)
  y_a * log(y_p) + (1 - y_a) * log(1 - y_p)
}

dataset_log_likelihood=function(y_labels, y_probs) {
  	  sum(log_likelihoods(y_labels, y_probs))
}
deviances=function(y_labels, y_probs) {
  	  -2 * log_likelihoods(y_labels, y_probs)
}

dataset_deviance =function(y_labels, y_probs) {
    sum(deviances(y_labels, y_probs))
}
model_deviance=function(model, data, output_column) {
    y_labels = data[[output_column]]
  	y_probs = predict(model, newdata = data, type = "response")
    dataset_deviance(y_labels, y_probs)
}

model_deviance(heart_model, data = heart_train, output_column = "OUTPUT")
null_deviance(data = heart_train, output_column = "OUTPUT")
model_pseudo_r_squared=function(model,data,output_column) {
  	  1-( model_deviance(model,data,output_column)/null_deviance(data,output_column))
}
model_pseudo_r_squared(heart_model,data =heart_train,output_column = "OUTPUT")
model_deviance_residuals = function(model, data, output_column) { 
  	y_labels = data[[output_column]]
  	 y_probs = predict(model, newdata = data, type = "response") 
  	residual_sign = sign(y_labels - y_probs) 
  	residuals = sqrt(deviances(y_labels, y_probs)) 
  	residual_sign * residuals  }
summary(model_deviance_residuals(heart_model, data = heart_train, output_column = "OUTPUT"))
train_predictions=predict(heart_model, newdata = heart_train,type = "response")
train_class_predictions=as.numeric(train_predictions > 0.5)
mean(train_class_predictions == heart_train$OUTPUT)
test_predictions = predict(heart_model,newdata=heart_test,type ="response")
test_class_predictions=as.numeric(test_predictions > 0.5)
mean(test_class_predictions == heart_test$OUTPUT)
(confusion_matrix=table(predicted =train_class_predictions, actual = heart_train$OUTPUT))
(precision= confusion_matrix[2, 2] / sum(confusion_matrix[2,]))
(recall=confusion_matrix[2, 2] / sum(confusion_matrix[,2]))
(f = 2 * precision * recall / (precision + recall))
(specificity= confusion_matrix[1,1]/sum(confusion_matrix[1,]))
library(ROCR)
train_predictions=predict(heart_model, newdata = heart_train,type = "response")
pred=prediction(train_predictions, heart_train$OUTPUT)
perf=performance(pred, measure = "prec", x.measure = "rec")
plot(perf)
thresholds=data.frame(cutoffs = perf@alpha.values[[1]],recall =perf@x.values[[1]], precision = perf@y.values[[1]])
subset (thresholds,(recall > 0.9) & (precision > 0.8))
