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

- 1. **Number of "new users" signed up for successful trip with uber** in other words, **number of successful invites per user**, should definitely be the key indicator/measure for the success of the product feature. If the number of new users have increased than prior to the launch of the feature, it should be considered a viable indicator, keeping in mind we are taking into consideration no. of successful trip by this new user to be at least 1.

- 2. Other metrics worth looking in to should be **increased user-retention** that is previous users have maintained trips with uber as they successfully brought new users, **increased number of trips per user** which would also be directly proportional to each new invite they bring,**number of successful invites per user** the previous mentioned metric would probably result to this, **decrease in percentage of surge trip** which is again self explanatory, **decrease in trip cancellation by clients** as they would want to continue to use the feature, **increase in average distance per trip**, this is after each successful invite. These are additional indicators that can be used for consideration of measure of performance of the new feature, with possiblity of several other metrics .

- 3. HYPOTHESIS.


We will assume users gets one of these messages when they complete a trip.

•Thanks for using Uber. Please recommend us to a friend.
•Thanks for using Uber. Please recommend us to a friend: we’ll give you a free ride.
•Thanks for using Uber. Please recommend us to a friend: we’ll give you a Surge Protected ride.

We might refer to these, respectively, as the control, and the treatment group with Free trip & the Surge Protected trip

$Rondomization$ should be a primary consideration while following this experiment, in addition we would not consider users that did not get these messages on completion o heir trip, due to error, or any random reason.

We can consider pairwise comparison of each of the trips. Since the metrics are averages over randomized users,distribution would be assumed normal, z-test would prove enough to produce statistical significance and inference from the hypothesis, with p-value on which decisions will be produced. However p-value would become less determinant to us, when no.of tests increase, so we will keep it within the surface, also it would be good idea to test it on small location per se, instead of exposing to entire population at once, that is seelcting over the sample rather.



### Data Analysis (R-codes included)


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



#correlation plot

uber.cor <- cor(numeric, use="pairwise", method="pearson")
# Order the correlations by their strength.
ord <- order(uber.cor[1,])
uber.cor <- uber.cor[ord, ord]
corrplot(uber.cor, mar=c(0,0,1,0))

#as expected the variables are not correlated to each other except for slight correlation between "surge_pct" & "avg_surge"



# Mosaic Plot 

# Generate the table data for plotting.
ds <- table(uber.final$city, uber.final$active)

# Sort the entries.
ord <- order(apply(ds, 1, sum), decreasing=TRUE)

# Plot the data.
mosaicplot(ds[ord,], main="Mosaic of city (sample) by active", color=colorspace::rainbow_hcl(3)[-1], cex=0.7, xlab="city", ylab="active")


## mosaic plot shows that being from King’s Landing has the highest impact, it shows that being from King’s Landing increases the odds of an active user by approximately 1.5. In short, it appears King’s Landing is a good market for Uber. In contrast, Astapor is not a good market for Uber.



## Principal component analysis with SVD method:



#we would like to remove "avg_surge" or "surge_pct", since it is clear they are correlated and could effect PCA process in determining the variables for the components. We would remove "surge_pct" from further consideration the model, and would go ahead and design it again (PCA as well as prediction models were validated/tested with these correlated variables as well, before considering them to remove from analysis.) 

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

# Display a plot showing the two most principal components.
biplot(pc, main="")
title(main="Principal Components")
```




### For Building the training/validate/test datasets.



```{r,echo=TRUE,eval=TRUE}
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
uber.sample <- uber.train <- uber_sample[-valid_sample,] #32000 #64% for final training dataset (805 0f the 80% of the input dataset)

```




### Building prediction model


```{r, eval=TRUE, echo=TRUE}


#further cleaning the data, we remove missing values as well as "last_trip-date" from our consideration for the reason that our derived varibale "active" which is also the target variable was produced from last_trip-date variable and therefore would result in inefficient and biased result.

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
```



> Validation datset was further utilized and was performed against "Decision Tree", "Random Forest", "Adaptive-Boosting(ADA)", "Logistic Regression", "Multivariate Adaptive Regression Splines(MARS)" & Support Vector Machine(SVM) and several times iterated over by removing variables (as described at necessary steps the reason for removl of a particular varibale), PCA and correlation were taken into consideration and dataset was changed over time with the model development. However, in this project, I am only presenting the final overview or description of what the dataset looks-like (final) after alteration. Upon changes and comparion based on confusion matrix/error matrix and ROC etc., **Adaptive-Boosting** was finally selected as the best model with best result produced by it in comparison to other, and therefore, would be implemented on test dataset. For further details on how comparison stood up for each model against each other, and the codes used to generate them, please follow this link.



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

title(main="Precision/Recall Plot  uber.csv ")
grid()
```


# stochastic boosting models for a binary response AdditiveLogistic Regression
