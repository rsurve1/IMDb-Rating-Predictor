library(rpart) #to make CART
library(rpart.plot) #to plot CART
library(rattle) #to plot CART in a prettier fashion!
library(countrycode)
library(SDMTools) #to use accuracy function
library('ROCR')

IMDB = read.csv("IMDB.csv")

str(IMDB)

#trimming variables and resaving file with correct columns
IMDB = IMDB[,-1]
IMDB = IMDB[,-2]
IMDB = IMDB[,-2]
IMDB = IMDB[,-5]
IMDB = IMDB[,-6]
IMDB = IMDB[,-7]
IMDB = IMDB[,-7]
IMDB = IMDB[,-9]
IMDB = IMDB[,-10]
IMDB = IMDB[,-10]
IMDB = IMDB[,-19]
IMDB = IMDB[,-7]
IMDB = IMDB[,-9]

#Replacing the NA in duration
IMDB$duration[867] = 85
IMDB$duration[1009] = 114
IMDB$duration[1169] = 135
IMDB = IMDB[-2396,] 
IMDB$duration[2609] = 153
IMDB = IMDB[-2786,]
IMDB$duration[2921] = 146
IMDB$duration[2922] = 130
IMDB$duration[3992] = 144
IMDB$duration[4199] = 151
IMDB$duration[4681] = 100
IMDB = IMDB[-4738,]
IMDB$duration[4772] = 93

#replacing the NA in Facenumber:
which(is.na(IMDB$facenumber_in_poster))
IMDB$facenumber_in_poster[98] = 1
IMDB$facenumber_in_poster[172] = 6
IMDB$facenumber_in_poster[388] = 4
IMDB$facenumber_in_poster[606] = 4
IMDB$facenumber_in_poster[919] = 4
IMDB$facenumber_in_poster[1363] = 1
IMDB$facenumber_in_poster[2457] = 2
IMDB$facenumber_in_poster[2647] = 2
IMDB$facenumber_in_poster[2767] = 0
IMDB$facenumber_in_poster[3526] = 1
IMDB$facenumber_in_poster[3650] = 0
IMDB$facenumber_in_poster[4042] = 1
IMDB$facenumber_in_poster[4530] = 0

which(is.na(IMDB$director_facebook_likes))
IMDB$director_facebook_likes[3042] = 58000
IMDB$director_facebook_likes[3043] = 58000
IMDB$director_facebook_likes[3044] = 58000
IMDB$director_facebook_likes[3045] = 58000
IMDB$director_facebook_likes[3046] = 58000
IMDB$director_facebook_likes[3047] = 58000
IMDB$director_facebook_likes[4933] = 58000

IMDB$title_year[709] = 2008
IMDB$title_year[1169] = 2015
IMDB$title_year[2107] = 1974
IMDB$title_year[3287] = 2009
IMDB$title_year[4933] = 2006

#save(IMDB, file = "imdb.csv")

#checking NA 624 to 23 to 5
sum(is.na(IMDB))
str(IMDB)

#NA Gender: 0
sum(is.na(IMDB$Gender))

#NA Duration: 0
sum(is.na(IMDB$duration))

#NA Director FB Likes: 0
sum(is.na(IMDB$director_facebook_likes))

#NA Actor 3 FB Likes: 19 to 0
IMDB$actor_3_facebook_likes[is.na(IMDB$actor_3_facebook_likes)] = 
  round(mean(IMDB$actor_3_facebook_likes, na.rm = TRUE))
sum(is.na(IMDB$actor_3_facebook_likes))

#NA Actor 1 FB Likes: 7 to 0
IMDB$actor_1_facebook_likes[is.na(IMDB$actor_1_facebook_likes)] = 
  round(mean(IMDB$actor_1_facebook_likes, na.rm = TRUE))
sum(is.na(IMDB$actor_1_facebook_likes))

#NA Genres: 0
sum(is.na(IMDB$genres))

#NA Facenumber: 0
sum(is.na(IMDB$facenumber_in_poster))

#NA Language: 0
sum(is.na(IMDB$language))

#NA Country: 0
sum(is.na(IMDB$country))

#NA Content: 0
sum(is.na(IMDB$content_rating))

#NA Budget: 32 to 258 to 0
IMDB$budget[is.na(IMDB$budget)] = round(mean(IMDB$budget, na.rm = TRUE))
sum(is.na(IMDB$budget))

#NA Title year 5 to 0
sum(is.na(IMDB$title_year))

#NA Actor 2: 11 to 0
IMDB$actor_2_facebook_likes[is.na(IMDB$actor_2_facebook_likes)] = 
  round(mean(IMDB$actor_2_facebook_likes, na.rm = TRUE))
sum(is.na(IMDB$actor_2_facebook_likes))

#NA IMDB Score: 0
sum(is.na(IMDB$imdb_score))

#NA Aspect Ratio: 306 to 0
IMDB$aspect_ratio[is.na(IMDB$aspect_ratio)] = round(mean(IMDB$aspect_ratio, na.rm = TRUE))
sum(is.na(IMDB$aspect_ratio))

sum(is.na(IMDB))
which(is.na(IMDB$Continent))

#turning the variables in genre into separate yes or no columns
#starting with column for Action: 1134
for (i in 1:4933){
  
  if (grepl("Action",IMDB$genres[i]) == TRUE){
    IMDB$Action[i] = 1
  } else {
    IMDB$Action[i] = 0
  }
  
}

sum(IMDB$Action)

#next doing Comedy column 1835
for (i in 1:4933){
  
  if (grepl("Comedy",IMDB$genres[i]) == TRUE){
    IMDB$Comedy[i] = 1
  } else {
    IMDB$Comedy[i] = 0
  }
  
}

sum(IMDB$Comedy)

#next doing Adventure 910
for (i in 1:4933){
  
  if (grepl("Adventure",IMDB$genres[i]) == TRUE){
    IMDB$Adventure[i] = 1
  } else {
    IMDB$Adventure[i] = 0
  }
  
}

sum(IMDB$Adventure)

#next using Family: 534
for (i in 1:4933){
  
  if (grepl("Family",IMDB$genres[i]) == TRUE){
    IMDB$Family[i] = 1
  } else {
    IMDB$Family[i] = 0
  }
  
}

sum(IMDB$Family)

#next doing Horror: 558 
for (i in 1:4933){
  
  if (grepl("Horror",IMDB$genres[i]) == TRUE){
    IMDB$Horror[i] = 1
  } else {
    IMDB$Horror[i] = 0
  }
  
}

sum(IMDB$Horror)

#next doing Fantasy: 592 
for (i in 1:4933){
  
  if (grepl("Fantasy",IMDB$genres[i]) == TRUE){
    IMDB$Fantasy[i] = 1
  } else {
    IMDB$Fantasy[i] = 0
  }
  
}

sum(IMDB$Fantasy)

#next doing Sci-Fi: 597 
for (i in 1:4933){
  
  if (grepl("Sci-Fi",IMDB$genres[i]) == TRUE){
    IMDB$SciFi[i] = 1
  } else {
    IMDB$SciFi[i] = 0
  }
  
}

sum(IMDB$SciFi)

#next doing Crime: 859 
for (i in 1:4933){
  
  if (grepl("Crime",IMDB$genres[i]) == TRUE){
    IMDB$Crime[i] = 1
  } else {
    IMDB$Crime[i] = 0
  }
  
}

sum(IMDB$Crime)

#next doing Romance: 1087 
for (i in 1:4933){
  
  if (grepl("Romance",IMDB$genres[i]) == TRUE){
    IMDB$Romance[i] = 1
  } else {
    IMDB$Romance[i] = 0
  }
  
}

sum(IMDB$Romance)

#next doing Drama: 2517
for (i in 1:4933){
  
  if (grepl("Drama",IMDB$genres[i]) == TRUE){
    IMDB$Drama[i] = 1
  } else {
    IMDB$Drama[i] = 0
  }
  
}

sum(IMDB$Drama)

#next doing Thriller: 1385 
for (i in 1:4933){
  
  if (grepl("Thriller",IMDB$genres[i]) == TRUE){
    IMDB$Thriller[i] = 1
  } else {
    IMDB$Thriller[i] = 0
  }
  
}

sum(IMDB$Thriller)

IMDB = IMDB[,-6]
IMDB$Action = as.factor(IMDB$Action)
IMDB$Comedy = as.factor(IMDB$Comedy)
IMDB$Adventure = as.factor(IMDB$Adventure)
IMDB$Family = as.factor(IMDB$Family)
IMDB$Horror = as.factor(IMDB$Horror)
IMDB$Fantasy = as.factor(IMDB$Fantasy)
IMDB$SciFi = as.factor(IMDB$SciFi)
IMDB$Crime = as.factor(IMDB$Crime)
IMDB$Romance = as.factor(IMDB$Romance)
IMDB$Drama = as.factor(IMDB$Drama)
IMDB$Thriller = as.factor(IMDB$Thriller)

#Creating a grouping for year so that we can include it within the model
#Year would be able to make an IMDb prediction score for movies that
# have not been rated as of yet. It will also allow the model to weigh movies
#  this would mean movies from different time periods would be ranked differently

#checking levels of title year
levels(IMDB$title_year)
which.min(IMDB$title_year)
IMDB$title_year[830]
which.max(IMDB$title_year)
IMDB$title_year[84]

#assigning groups based on 50 years groupings
#groups up to 1950 but not including
for (i in 1:4933){
  
  if (IMDB$title_year[i]<1950){
    IMDB$up1950[i] = 1
  } else {
    IMDB$up1950[i] = 0
  }
  
}

sum(IMDB$up1950)

#for groups from 1950, including, to 2000, excluding
for (i in 1:4933){
  
  if (IMDB$title_year[i]>1949){
    IMDB$up2000[i] = 1
  } else {
    IMDB$up2000[i] = 0
  }
  
}

sum(IMDB$up2000)

for (i in 1:4933){
  
  if (IMDB$title_year[i]>1999){
    IMDB$up2000[i] = 0
  } else {
    print("No")
  }
  
}

sum(IMDB$up2000)

#last grouping from 2000 onwards inclusive

for (i in 1:4933){
  
  if (IMDB$title_year[i]>1999){
    IMDB$over2000[i] = 1
  } else {
    IMDB$over2000[i] = 0
  }
  
}

sum(IMDB$over2000)

#removing title year
IMDB = IMDB[,-12]

#changing years to be as.factor
IMDB$up1950 = as.factor(IMDB$up1950)
IMDB$up2000 = as.factor(IMDB$up2000)
IMDB$over2000 = as.factor(IMDB$over2000)

#creating variable for English or not 
for (i in 1:4933){
  
  if (grepl("English",IMDB$language[i]) == TRUE){
    IMDB$English[i] = 1
  } else {
    IMDB$English[i] = 0
  }
  
}

sum(IMDB$English)/n

#removing language from dataset
IMDB = IMDB[,-8]

#changing English to be as.factor
IMDB$English = as.factor(IMDB$English)

#installing package countrycode
#install.packages('countrycode')

#creating a new variable for continent
IMDB$Continent = countrycode(IMDB$country, origin = "country.name", destination = "continent")
IMDB$Continent = as.factor(IMDB$Continent)
levels(IMDB$Continent)

#removing country from the dataset
IMDB = IMDB[,-8]

#replacing the continent NAs with correct ones
which(is.na(IMDB$Continent))
IMDB = IMDB[-709,]
IMDB$Continent[868] = "Americas"
IMDB$Continent[1168] = "Americas"
IMDB$Continent[3493] = "Americas"
IMDB$Continent[4170] = "Americas"

sum(is.na(IMDB))

#running several plots to evaluate any multicollinearity
plot(IMDB$duration,IMDB$aspect_ratio)

#################

#################

#################

#################

######CART######
IMDB_cart = IMDB
str(IMDB_cart)

#checking what mean was 6.4 before removing score
#mean(IMDB$imdb_score)

for (i in 1:4933){
  
  if (IMDB_cart$imdb_score[i]>7.5){
    IMDB_cart$Success[i] = 1
  } else {
    IMDB_cart$Success[i] = 0
  }
  
}

str(IMDB_cart)
IMDB_cart$Success = as.factor(IMDB_cart$Success)

IMDB_cart = IMDB_cart[,-11]

#Splitting the dataset intro training and validation
n_cart = nrow(IMDB_cart)
set.seed(12345) #Setting a fix seed to make the results reproducible
Index_cart = sample(n_cart,0.75*n_cart)
train_cart = IMDB_cart[Index_cart,]
valid_cart = IMDB_cart[-Index_cart,]

table(valid_cart$Success)
169/(169+1064)

#build model
cart_model = rpart(Success ~ ., data=train_cart)
prp(cart_model,type=2,extra=1, cex = 1) #a basic plot
#fancyRpartPlot(cart_model, cex = 1)

asRules(cart_model) # display the set of rules

printcp(cart_model) #summary of the results

#build a new model by changing some control paramaters
stoppingRules = rpart.control(cp = 0.001)
unpruned = rpart(Success ~ ., data=train_cart, control=stoppingRules)
prp(unpruned,type=2,extra=1, cex = 0.5)
printcp(unpruned)

# make predictions
# we use "prob" to get the probabilities
pred_model = predict(cart_model, valid_cart, type = "prob")
pred_unpruned = predict(unpruned, valid_cart, type = "prob")

# compare the prediction performance of the models
measures_model = accuracy(valid_cart$Success,pred_model[,2])
measures_unpruned = accuracy(valid_cart$Success,pred_unpruned[,2])

#Compare overall accuracy, sensitivity and specificity
measures_model$prop.correct
measures_unpruned$prop.correct

measures_model$sensitivity
measures_unpruned$sensitivity

measures_model$specificity
measures_unpruned$specificity

#change the seed so that we can get a range of different results
#change the proportion of training to validation
####################

####################

####################

####################

############## k-means Clustering ####################

str(IMDB)

IMDB_km = IMDB[,-1]

str(IMDB_km)

IMDB_km = IMDB_km[,-7]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]
IMDB_km = IMDB_km[,-11]

str(IMDB_km)

# Elbow chart
x = c()
for(i in 2:10){
  model = kmeans(scale(IMDB_km),i)
  x[i] = mean(model$withinss)
}
plot(x,type='b',xlab='Cluster Size',ylab='Average Within-Cluster Sum of Squares')
###based off elbow chart, you should have either 3 or 4 clusters

set.seed(1) # Set the seed if you want to make the results reproducible
km = kmeans(scale(IMDB_km),3)

km$cluster #cluster assignments (i.e., which company belongs to which cluster)
km$size #size of each cluster ###first cluster has 2283, second cluster has 2333, and thrid cluster has 317
km$centers #centroid of each cluster

#Attach cluster membership info to the original dataset and compare 
# variable averages of each group
IMDB_km_info = cbind(IMDB_km,km$cluster)
aggregate(IMDB_km_info, by=list(km$cluster) , mean)
##The best imbd score is cluster 1, correlated with the 2nd highest budget,  
# 2nd highest duration (118 mins), at least one face in the poster, 
# 2nd largest apect ratio, and 2nd largest facebook votes overal. 
# The other factors are binary and not helpful for kmeans.
#However, a majority of the data is in clusters 1&2
#very interesting to see that cluster 1 has the highest IMDB score rating since,
# it is lower on budget and likes!

#################

#################

#################

############## Logistic Regression ####################

#creating dataset for logistic regression
str(IMDB)

IMDB_log = IMDB

for (i in 1:4933){
  
  if (IMDB_log$imdb_score[i]>7.5){
    IMDB_log$Success[i] = 1
  } else {
    IMDB_log$Success[i] = 0
  }
  
}

IMDB_log$Success = as.factor(IMDB_log$Success)
IMDB_log = IMDB_log[,-11]

#We're putting 80% of the data into training and the rest in validation
n_log = nrow(IMDB_log)
set.seed(12345) 
Index_log = sample(n_log,0.75*n_log)
train_log = IMDB_log[Index_log,]
valid_log = IMDB_log[-Index_log,]

table(valid_log$Success)
169/(169+1064) #ratio of 13.7%

#Used gaussian as binomial was not working
score_logistic = glm(Success ~ ., train_log, family="binomial")
summary(score_logistic)

pred_probs_logistic = predict(score_logistic, valid_log, type = "response")
#pred_probs_logistic

confusion.matrix(valid_log$Success,pred_probs_logistic,threshold=0.5)
AccuMeasures_logistic = accuracy(valid_log$Success, pred_probs_logistic, threshold=0.5)

#Extracting specific values from accuracy table
AccuMeasures_logistic$prop.correct
AccuMeasures_logistic$sensitivity
AccuMeasures_logistic$AUC # 'Area Under the Curve': Refers to ROC curve

#Stepwise
#Build a null model to use in 'forward' part
#score_null = glm(Success ~ 1, train_log, family="binomial") 
#backward = step(score_logistic, direction="backward")
#forward = step(score_null, scope=list(upper=score_logistic), direction="forward")
both = step(score_logistic, direction="both")

#Prediction using the stepwise models. 
# As an example, we use the last one, saved as "both"
pred_probs_both = predict(both, valid_log,type="response")
#pred_probs_back = predict(backward, valid_log,type="response")

#measures for both
confusion.matrix(valid_log$Success,pred_probs_both,threshold=0.5)
AccuMeasures_both = accuracy(valid_log$Success, pred_probs_both, threshold=0.5)
#AccuMeasures_back = accuracy(valid_log$Success, pred_probs_back, threshold=0.5)

#Extracting specific values from accuracy table
AccuMeasures_both$prop.correct
AccuMeasures_both$sensitivity
AccuMeasures_both$AUC # 'Area Under the Curve': Refers to ROC curve

#AccuMeasures_back$prop.correct
#AccuMeasures_back$sensitivity

pred_logistic = prediction(pred_probs_logistic,valid_log$Success)
pred_both = prediction(pred_probs_both,valid_log$Success)

acc_logistic = performance(pred_logistic,"acc")
plot(acc_logistic,main = "Accuracy for different cutoffs")

gain_logistic = performance(pred_logistic,"tpr","rpp")
plot(gain_logistic, main = "Gain Chart")
lines(x=c(0,1),y=c(0,1),lty=3)

#Adding the result of the model called "both" to ROC curve for comparison to the full model
roc_logistic = performance(pred_logistic,"tpr","fpr")
roc_both = performance(pred_both,"tpr","fpr")
plot(roc_logistic, main = "ROC Chart")
lines(x=c(0,1),y=c(0,1),lty=3)
plot(roc_both, add=TRUE,col="red")

#####################################################################


