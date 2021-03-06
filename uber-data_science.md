---
title: "uber_challenge"
author: "Neeraj"
date: "Sunday, November 15, 2015"
output: html_document
---


### SQL


> 1. Between Oct1,2013 at 10am PDT and Oct 22,2013 at 5pm PDT, what percentage of requests made by unbanned clients eachday were canceled in each city?



```
x :=   SELECT *,
       date_trunc('day', request_at AT TIME ZONE 'PDT') AS day_id
	     FROM trips	  
	     WHERE request_at >= TIMESTAMP WITH TIME ZONE '2013-10-01 10:00:00 PDT' 
	     AND request_at <= TIMESTAMP WITH TIME ZONE '2013-10-22 17:00:00 PDT'

Y :=   SELECT * 
       FROM users 
	     WHERE banned = FALSE
	     AND role = 'client'


Z :=   SELECT * 
       FROM X
	     INNER JOIN Y
	     ON X.client_id = Y.usersid

P :=  SELECT 
  	  SUM(
			 CASE WHEN status='cancelled_by_driver' THEN 1
			     WHEN status='cancelled_by_client' THEN 1
			     ELSE 0
			  END 
		     ) AS cancelled_count,
		  COUNT() AS total_count,
		  city_id,
		  day_id
	    FROM Z
	    GROUP BY city_id, day_id

Q := SELECT 
     city_id,  
	   cancelled_count / total_count::float AS percent_cancelled
	   FROM P;
```


> 2. For city_ids 1,6,and 12, list the top three drivers by number of completed trips for each week between June3,2013 and June24,2013

```
X :=  SELECT 
        *,
	      extract(day from 
	     age(
			 date_trunc('day', request_at),
			 date_trunc('day', '2013-06-03')
		    ))::int / 7 AS weeks_since_id;

	     FROM trips
	     WHERE request_at >= '2013-06-03' 
	     AND request_at < '2013-06-25';

Y :=   SELECT 
  	   driver_id,
		   city_id,
		   weeks_since_id
		   COUNT(*) AS trips_taken
	     FROM X	  
	     WHERE city_id IN (1,6,12)
	     GROUP BY city_id, weeks_since_id;

Z :=   SELECT   
		   driver_id,
		    city_id,
		    weeks_since_id,
		    trips_taken,
		    RANK() OVER (
			 PARTITION BY city_id, weeks_since_id 
			  ORDER BY trips_taken 
			  DESC) AS driver_rank
	      FROM Y


P :=   SELECT 
  	   city_id, 
		   weeks_since_id, 
		   driver_rank, 
		   driver_id,
		    trips_taken
	      FROM Z
	      WHERE driver_rank <= 3
	     ORDER BY city_id, weeks_since_id, driver_rank;
                                                                    
```



### Experiment and metrics design 

> A product manager on the Growth Team has proposed a new feature. Instead of getting a free ride for every successful invite, users will get 1 Surge Protector, which exempts them from Surge pricing on their next surged trip. 

#### What would you choose as the key measure of the success of the feature?

- 1. **Number of "new users" signed up for successful trip with uber** in other words, **number of successful invites per user**, should definitely be the key indicator/measure for the success of the product feature. If the number of new users have increased than prior to the launch of the feature, it should be considered a viable indicator, keeping in mind we are taking into consideration no. of successful trip by this new user to be at least 1.

#### What other metrics would be worth watching in addition to the key indicator?

- 2. Other metrics worth looking in to should be **increased user-retention** that is previous users have maintained trips with uber as they successfully brought new users, **increased number of trips per user** which would also be directly proportional to each new invite they bring,**number of successful invites per user** the previous mentioned metric would probably result to this, **decrease in percentage of surge trip** which is again self explanatory, **decrease in trip cancellation by clients** as they would want to continue to use the feature, **increase in average distance per trip**, this is after each successful invite. These are additional indicators that can be used for consideration of measure of performance of the new feature, with possiblity of several other metrics .

#### Describe an experiment design that you could use to confirm the hypothesis that your chosen key measure is different in the treated group.

- 3. HYPOTHESIS.


We will assume users gets one of these messages when they complete a trip.

•Thanks for using Uber. Please recommend us to a friend.
•Thanks for using Uber. Please recommend us to a friend: we’ll give you a free ride.
•Thanks for using Uber. Please recommend us to a friend: we’ll give you a Surge Protected ride.

We might refer to these, respectively, as the control, and the treatment group with Free trip & the Surge Protected trip

$Rondomization$ should be a primary consideration while following this experiment, in addition we would not consider users that did not get these messages on completion o heir trip, due to error, or any random reason.

We can consider pairwise comparison of each of the trips. Since the metrics are averages over randomized users,distribution would be assumed normal, z-test would prove enough to produce statistical significance and inference from the hypothesis, with p-value on which decisions will be produced. However p-value would become less determinant to us, when no.of tests increase, so we will keep it within the surface, also it would be good idea to test it on small location per se, instead of exposing to entire population at once, that is seelcting over the sample rather.



### Data Analysis (R-codes included)

Uber is interested in predicting rider retention. To help explore this question, we have provided a sample dataset of a cohort of users who signe dup for an Uber account in January 2014. The data was pulled several months later; we consider a user retained if they were “active” (i.e. took a trip) in the preceding 30 days.

We would like you to use this data set to help understand what factors are the best predictors for retention, and offer suggestions to operationalize those insights to help Uber.

See below for a detailed description of the dataset. Please include any code you wrote for the analysis and delete the data when you have finished with the challenge.

dataset_variables | Defintion
-----|-------------
`city`	| city this user signed up in
`phone`	| primary device for this user
`signup_date` |	date of account registration; in the form ‘YYYY-MM-DD’
`last_trip_date` |	the last time this user completed a trip
`avg_dist` |	the average distance (in miles) per trip taken in the first 30 days after signup
`avg_rating_by_driver` |	the rider’s average rating over all of their trips
`avg_rating_of_driver` |	the rider’s average rating of their driers over all of their trips
`surge_pct` |	the percent of trips taken with surge multiplier > 1
`trips_in_first_30_days` |	the number of trips this user took in the first 30 days after signing up
`uber_black_user` |	TRUE if the user took an Uber Black in their first 30 days; FALSE otherwise
`weekday_pct` |	the percent of the user’s trips occurring during a weekday



- Perform any cleaning, exploratory analysis, and/or visualizations to use the provided data for this analysis ( a few sentences/plots describing your approach will suffice). What fraction of the observed users were retained?

- Build a predictive model to help Uber determine whether or not a user will be retained. Discuss why you chose your approach, what alternatives you considered, and any concerns you have. How valid is your model? Include any key indicators or model performance

- Briefly discuss how Uber might leverage the insights gained from the model to improve its rider retention.



## Load the libraries


```{r, echo=TRUE, eval = TRUE}

library(RJSONIO)
library(plyr)
library(data.table)
require(corrplot, quietly=TRUE)
require(fBasics, quietly=TRUE)
library(ROCR)
require(ggplot2, quietly=TRUE)
```



#### Data cleaning & preparation


```{r,echo=TRUE,eval=TRUE}
setwd("~/Data_Science")
uber.data <- ("uber_data_challenge.json")
#dealing with NaN values to allow reading json.
uber <- fromJSON (uber.data, method = "C", nullValue = NA)
#head(uber)
uber.final <- do.call("rbind.fill", lapply(uber, as.data.frame))
#head(uber.final)
#class(uber.final) check the class if it is dataframe now.
str(uber.final)
uber.final$last_trip_date <- as.Date(uber.final$last_trip_date)

#adding the column for "active user", based on whether they have been active (used the ride) in the preceeding 30 days,in our case which is from 1st june to 1st july, (2014-07-01 is the maximum date for the dataset), so we put "1" for active, "0" for inactive users.

uber.final$active[uber.final$last_trip_date >= "2014-06-01"] <- 1
uber.final$active[uber.final$last_trip_date < "2014-06-01"] <- 0
uber.final$active <- as.factor(uber.final$active)
summary(uber.final)
table(uber.final$active)
```



- 18804/50000, that is 188804 users were retained out of total 50000, which is lesser than the users inactive, or not retained. 


- The big missing data issue is the 8122 records that are missing values for avg_rating_of_driver, and a few missing values from phone and avg_rating_by_driver.



### Exploratory analysis, Visualization and further cleaning



```{r, eval=TRUE,echo=TRUE}
# variables noted.
input <- uber.final[c("city", "trips_in_first_30_days", "signup_date", "avg_rating_of_driver",
     "avg_surge", "last_trip_date", "phone", "surge_pct",
     "uber_black_user", "weekday_pct", "avg_dist", "avg_rating_by_driver")]
     
numeric <- uber.final[c("trips_in_first_30_days", "avg_rating_of_driver", "avg_surge", "surge_pct",
     "weekday_pct", "avg_dist", "avg_rating_by_driver")]

categoric <- uber.final[c("city", "signup_date", "last_trip_date", "phone")]
target <- uber.final["active"]

# average distance traveled by user with uber for "Winterfell" and "King'sLanding" cities are alomost similar.
ggplot(validate.final, aes(y=(avg_dist),x=city)) + geom_boxplot()


#correlation plot

uber.cor <- cor(numeric, use="pairwise", method="pearson")
# Order the correlations by their strength.
ord <- order(uber.cor[1,])
uber.cor <- uber.cor[ord, ord]
corrplot(uber.cor, mar=c(0,0,1,0))

```
![correlation](https://cloud.githubusercontent.com/assets/11197322/11510910/00a4260e-9834-11e5-9dca-0e84c8f3578f.png?raw=true "correlation")

#as expected the variables are not correlated to each other except for correlation between "surge_pct" & "avg_surge", we would come back to it later.


```{r}
# Mosaic Plot 

# Generate the table data for plotting.
ds <- table(uber.final$city, uber.final$active)

# Sort the entries.
ord <- order(apply(ds, 1, sum), decreasing=TRUE)

# Plot the data.
mosaicplot(ds[ord,], main="Mosaic of city (sample) by active", color=colorspace::rainbow_hcl(3)[-1], cex=0.7, xlab="city", ylab="active")
```

![mosaicplot](https://cloud.githubusercontent.com/assets/11197322/11510919/0dd1a04a-9834-11e5-928a-92fb88997cb3.png "mosaicplot")

## mosaic plot shows that being from King’s Landing has the highest impact, it shows that being from King’s Landing increases the odds of an active user by approximately 1.5. In short, it appears King’s Landing is a good market for Uber. In contrast, Astapor is not a good market for Uber.


```{r}
## Principal component analysis with SVD method:



#we would like to remove "avg_surge" or "surge_pct", since it is clear they are correlated and could effect PCA process in determining the variables for the components. We would remove "surge_pct" from further consideration the model, and would go ahead and design it again (PCA as well as prediction models were validated/tested with these correlated variables as well, before considering them to remove from analysis.). The choice is random, and is, in addition, also based on the variable importance where both the variables dominated and referred it would be a good practice to remove one of them

numeric <- uber.final[c("trips_in_first_30_days", "avg_rating_of_driver", "avg_surge",
     "weekday_pct", "avg_dist", "avg_rating_by_driver")]
uber.final <- uber.final[-8]



pc <- prcomp(na.omit(numeric), scale=TRUE, center=TRUE, tol=0)
# Show the output of the analysis.
pc
# Summarise the importance of the components found.
summary(pc)

#This shows that for PC1, avg_rating_of_driver and avg_dist are important vairables, because of high rotation values, for PC2  weekday_pct, avg_rating_by_driver, avg_surge, trips_in_first_30_days are important variable, which can further be confirmed by te plot. Summary also shows first 5 principal components are able to explain about 86% of the variability in the dataset.


# Display a plot showing the relative importance of the components.
plot(pc, main="")
title(main="Principal Components Importance")
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)
```

![PCA](https://cloud.githubusercontent.com/assets/11197322/11510923/115b518e-9834-11e5-8a1d-9ea81ce40a44.png "PCA")

```{r}
# Display a plot showing the two most principal components.
biplot(pc, main="")
title(main="Principal Components")
```

![biplot](https://cloud.githubusercontent.com/assets/11197322/11510916/0928924c-9834-11e5-96f4-c1e644832336.png "biplot")





### For Building the training/validate/test datasets.



```{r,echo=TRUE,eval=TRUE}

# dividing dataset into training, validation and testing dataset.


set.seed(1234) 
nobs <- nrow(uber.final)  
## 80% of the sample size
smp_size <- floor(0.80 * nrow(uber.final))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(uber.final)), size = smp_size)
uber_sample<- uber_train <- uber.final[train_ind, ]

#final test dataset
test <- uber.final[-train_ind, ]  #20% of the input dataset

valid <- floor(0.20 * nrow(uber_sample))
set.seed(123)
valid_sample <- sample(seq_len(nrow(uber_sample)), size = valid)

#final validation dataset
validate <-uber_sample[valid_sample, ]   #20% of the pre-training dataset 

#final training dataset
uber.sample <- uber.train <- uber_sample[-valid_sample,] #32000 #64% for final training dataset (80% 0f the 80% of the input dataset)

```




### Building prediction model


```{r, eval=TRUE, echo=TRUE}


#further cleaning the data, we remove missing values as well as "last_trip-date" from our consideration for the reason that our derived varibale "active" which is also the target variable was produced from last_trip-date variable and therefore would result in inefficient and biased result.

# this was again considered after building the model with the "last_trip-date" variable in to it, which for usual reasons made model bias and heavly inefficient.


train.final <- uber.sample[complete.cases(uber.sample), ]
validate.final <- validate[complete.cases(validate), ]
test.final <- test[complete.cases(test), ]
train.final <- train.final[-6]
validate.final <- validate.final[-6]
test.final <- test.final[-6]



#Adaptive boosting algorithm implemented.

require(ada, quietly=TRUE)
library("rpart")
set.seed(12345)
u.ada <- ada(active ~ .,
      data=train.final,
      control=rpart.control(maxdepth=30,
           cp=0.010000,
           minsplit=20,
           xval=10),
      iter=50)

# Print the results of the modelling.

print(u.ada)
round(u.ada$model$errs[u.ada$iter,], 2)
varplot(u.ada)
```

![varplot](https://cloud.githubusercontent.com/assets/11197322/11510927/1386fe4a-9834-11e5-8781-0ae1aaedd9a1.png "varplot")


```{r}

#decision tree
library(rpart)
fit = rpart(active~., method="class", data=train.final )
printcp(fit)
plotcp(fit)
summary(fit)
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=.8)


# logistic regression

u.glm <- glm(active ~ .,
             data=train.final,
             family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(u.glm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(u.glm)[1],
            attr(logLik(u.glm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            u.glm$null.deviance-u.glm$deviance,
            u.glm$df.null-u.glm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(u.glm$null.deviance-u.glm$deviance,
                   u.glm$df.null-u.glm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(u.glm$y, u.glm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(u.glm, test="Chisq"))


# support vector machine

require(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(11)
u.ksvm <- ksvm(as.factor(active) ~ .,
               data=train.final,
               kernel="rbfdot",
               prob.model=TRUE)

# Generate a textual view of the SVM model.
u.ksvm



# MARS- Multivariate Adaptive Regression Splines
library(randomForest)
library(earth) 
m.earth <- earth(active~., data=train.final)
mtype <- "earth"
model <- m.earth
model

```


- train error noted is 0.23 which is decent for consideration.
- Variable importance plot acknowledges the importance of each variable in the model, in descending order, therefore, **avg_dist** is very imporrtant variable, while **uber_balck_user** is least, implementing that avg_dist traveled by the user on average is better indicator, here **the best indicator/metric** to determine whether the user will be retained or not, while uber_black_user hols least importance and therefore, it would not affect the decision making.  



### Evaluate model performance with validation dataset.

- The dataset validation was used to check for outliers $again$, and further perform transformation of data, specific mention to avg_dist for **log_10** scale, avg_surge, trips_in-first_30_days for **scaling with mean**  (normalization process) etc, and even though the visual representation (as shown in code below for example) showed the presence of "so-called" outliers, the analysis with transformed data confirmed the **negligible** effect of such condition which indicates that our choice for Adaptive-Boosting algorithm is very efficient and deals with outliers perfectly,specially because teh data doesn't seem to hold noise in it and therefore, *we did not over fit the model*, and so that produced similar results without manual transformation by us. Hence, we would go ahead using the dataset we originally created for testing after this.



```{r, eval=TRUE,echo=TRUE}
#example to show the almost neglible effect of so-called transformation on our model.
ggplot(validate.final, aes(y=avg_dist,x=active)) + geom_boxplot()
ggplot(validate.final, aes(y=log(avg_dist),x=active)) + geom_boxplot()


validate.final$avg_dist <- log(validate.final$avg_dist)

# Generate an Error Matrix for the Ada Boost model.

apr <- predict(u.ada, newdata=validate.final)

# Generate the confusion matrix showing counts.

table(validate.final$active, apr,
        dnn=c("Actual", "Predicted"))

# Calculate the overall error percentage.
overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(apr, validate.final$active,  
        dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr <- function(x) 
  cat(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum))) 

avgerr(table(apr, validate.final$active,  
        dnn=c("Predicted", "Actual")))



# Generate an ROC Curve for the ada model on validation dataset.

adav <- predict(u.ada, newdata=validate.final, type="prob")[,2]

#making it sure with-- Remove observations with missing target.

no.miss   <- na.omit(validate.final$active)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(adav[-miss.list], no.miss)
} else
{
  pred <- prediction(adav, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Ada Boost for active")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)



# Generate an Error Matrix for the Decision Tree model.

pr <- predict(fit, newdata=validate.final, type="class")

# Generate the confusion matrix showing counts.

table(validate.final$active, pr,
      dnn=c("Actual", "Predicted"))

# Calculate the overall error percentage.

overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
} 
overall(table(pr, validate.final$active,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.

avgerr <- function(x) 
  cat(mean(c(x[1,2], x[2,1]) / apply(x, 1, sum))) 

avgerr(table(pr, validate.final$active,  
             dnn=c("Predicted", "Actual")))


# Generate an Error Matrix for the SVM model.

svmpr <- predict(u.ksvm, newdata=validate.final)

# Generate the confusion matrix showing counts.

table(na.omit(validate.final$active, svmpr,
      dnn=c("Actual", "Predicted")))

# Calculate the overall error percentage.
overall(table(svmpr, validate.final$active,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr(table(svmpr, validate.final$active,  
             dnn=c("Predicted", "Actual")))

# Generate an Error Matrix for the Logistic model.


lpr <- as.vector(ifelse(predict(u.glm, type="response", newdata=validate.final) > 0.5, "1", "0"))

# Generate the confusion matrix showing counts.
table(validate.final$active, lpr,
      dnn=c("Actual", "Predicted"))

# Calculate the overall error percentage.

overall(table(lpr, validate.final$active,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr(table(lpr, validate.final$active,  
             dnn=c("Predicted", "Actual")))


# MARS model evaluation

marspr <- predict(m.earth, validate.final, type="class")
# Generate the confusion matrix showing counts.
table(validate.final$active, marspr,
      dnn=c("Actual", "Predicted"))

# Calculate the overall error percentage.
overall(table(marspr, validate.final$active,  
              dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr(table(marspr, validate.final$active,  
             dnn=c("Predicted", "Actual")))

```

#### error matrix of different models on validation dataset.

Model | Average  | Overall
-----|-------------|----------
`ADA` | 0.2319015 | 0.2312556
`Decision tree` | 0.2688798 | 0.2609154
`Logistic model` | 0.3104115 | 0.2997591
`MARS` | 0.2436861 | 0.2353715
`SVM` | 0.2437124 | 0.2393857



- here,overall and average error percentage are both approximaltely equal to .23 (23%), which is what we got from training set.

- ROC curve is another brilliant performance indicator for the model, and it shows the AUC value around 0.84 which is highly desirable.


> Validation datset was further utilized and was performed against "Decision Tree", "Random Forest", "Adaptive-Boosting(ADA)", "Logistic Regression", "Multivariate Adaptive Regression Splines(MARS)" & Support Vector Machine(SVM) and several times iterated over by removing variables (as described at necessary steps the reason for removl of a particular varibale), PCA and correlation were taken into consideration and dataset was changed over time with the model development. However, in this project, I am only presenting the final overview or description of what the dataset looks-like (final) after alteration. Upon changes and comparion based on confusion matrix/error matrix and ROC etc., **Adaptive-Boosting** was finally selected as the best model with best result produced by it in comparison to other, and therefore, would be implemented on test dataset. 



### Applying final model on test dataset.

```{r, eval=TRUE,echo=TRUE}
# Generate an Error Matrix for the Ada Boost model.

apt <- predict(u.ada, newdata=test.final)

# Generate the confusion matrix showing counts.

table(test.final$active, apt,
        dnn=c("Actual", "Predicted"))

# Calculate the overall error percentage.
overall(table(apt, test.final$active,  
        dnn=c("Predicted", "Actual")))

# Calculate the averaged class error percentage.
avgerr(table(apt, test.final$active,  
        dnn=c("Predicted", "Actual")))

# Generate an ROC Curve for the ada model on validation dataset.

adat <- predict(u.ada, newdata=test.final, type="prob")[,2]

#making it sure with-- Remove observations with missing target.

no.missq   <- na.omit(test.final$active)
miss.listq <- attr(no.missq, "na.action")
attributes(no.missq) <- NULL

if (length(miss.listq))
{
  predq <- prediction(adat[-miss.listq], no.missq)
} else
{
  predq <- prediction(adat, no.missq)
}

pt <- performance(predq, "tpr", "fpr")
auq <- performance(predq, "auc")@y.values[[1]]
pdt <- data.frame(fpr=unlist(pt@x.values), tpr=unlist(pt@y.values))
ps <- ggplot(pdt, aes(x=fpr, y=tpr))
ps <- ps + geom_line(colour="red")
ps <- ps + xlab("False Positive Rate") + ylab("True Positive Rate")
ps <- ps + ggtitle("ROC Curve Ada Boost for active")
ps <- ps + theme(plot.title=element_text(size=10))
ps <- ps + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
ps <- ps + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(auq, 2)))
print(ps)

# Generate a Precision/Recall Plot for the ada model on test.

plot(performance(predq, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Generate a Precision/Recall Plot for the ada model on train.
# In ROCR (1.0-3) plot does not obey the add command.
# Calling the function directly (.plot.performance) does work.

.plot.performance(performance(prediction(adat, test.final$active),"prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("Test","Train"), col=rainbow(2, 1, .8), lty=1:2, title="ada", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot")
grid()
```


- Again, here we see the final implementation of our model on test dataset, and to much of our likeness, it produces the desirable result. The overall as well as average error percentage is around 24%, which is only 0.1 more than training dataset, as well the ROC curve shows the AUC value as .83, again closer to the value obtained on validation dataset, which shows our model produced fair result on all the dataset. The choice of stochastic boosting models for a binary response, that is, AdditiveLogistic Regression (adaptive boosting) was appropriate. The otehr models, produced higher error rate/percentage around ranging between 24-30%, hence this is better predictor.

- Precision/recall plot confirms our understanding of the model on train and test dataset, as they areover lapping, and symmetrical to each other. 



### Key take aways (insights) for uber from this model.

- Variables actually used in model construction:
 `"avg_dist"` |              `"avg_rating_by_driver"` |  `"avg_rating_of_driver"` |  `"avg_surge"`   |          
 `"city"` |                  `"phone"`            |      `"signup_date"`            | `"trips_in_first_30_days"` |
 `"uber_black_user"`    |    `"weekday_pct"`           

- Frequency of variables actually used:

 variable | frequency
-----|-------------
  `city`  |    35      
  `weekday_pct` |   25      
  `uber_black_user`         |  23       
  `phone` | 22  
  `trips_in_first_30_days` | 18
  `avg_surge`    | 13
  `avg_rating_by_driver` |  15          
  `signup_date`           | 13    
  `avg_dist`  | 9
  `avg_rating_of_driver` | 6 
  
```{r, echo=TRUE, eval=TRUE} 
ggplot(validate.final, aes(y=(avg_dist),x=trips_in_first_30_days)) + geom_point() +facet_wrap(~active)
ggplot(validate.final, aes(y=(avg_dist),x=avg_rating_by_driver)) + geom_point() +facet_wrap(~active)
ggplot(validate.final, aes(x=active, y=log(avg_surge)))  +geom_boxplot() 
ggplot(validate.final, aes(y=avg_dist, x=avg_rating_by_driver))  +geom_point() + facet_wrap(~phone) 
```


- Recalling from above table & above graph on importance of variable as well as exploratory analysis, it can be seen that rating by driver if, is around or close to 5, meaning the drier was happy with the customer, would result in higher retention of the users.

- Again, We have seen that being from King’s Landing has the highest impact which increases the odds of an active user. In short, King’s Landing is a good market for Uber. In contrast, Astapor is not a good market for Uber.

- Higher trips in first 30 days is also a good indicator that user will stay longer, and is happier.

- Uber should definitely concentrate on the average distance a cutomer is traveling, combined **in** King's landing city as a preferred marketplace(could expand the model understanding and implement it to other places learning from it), that is the driver is happy with the customer as well (had good conversation, social), and if they took trips in starting 30 days.

- Another unlikely insight is about phone- Android. Our study suggests that the Android experience is far less compelling than the iPhone, with respect to avg_driver_rating on avg_dist. This could require understanding more in detail.

- The last insight is that from our model, which was developed and iterated over 50 trees, it was found that root tree belonged to the average distance traveled by  the user over. From the tree, we examined that if it is more than 1.1 or closer and city is "King's landing", uber has a better chance of retaining the customer, if the city is wetterell but for average distance of more than 2.71 and sign_up date is early, there is extremely fair chance of retaining the user with uber and maing travel longer. In addition, if the average rating by driver is more than 4.05 on average, and phone is iphone, and if the use is traveling King's, user retention is positive.

- There are several other insights that could be obtained from the model, and visualization, however these are prdominantley the important points for coniseration for uber to improve customer retention and business.
