Project 1 - Titanic
================
Nick Tedesco
2022-12-18

## Package and Data Loading

``` r
train <- read.csv('train.csv', na = "")

glimpse(train)
```

    ## Rows: 891
    ## Columns: 12
    ## $ PassengerId <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,…
    ## $ Survived    <int> 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1…
    ## $ Pclass      <int> 3, 1, 3, 1, 3, 3, 1, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 2, 3, 3…
    ## $ Name        <chr> "Braund, Mr. Owen Harris", "Cumings, Mrs. John Bradley (Fl…
    ## $ Sex         <chr> "male", "female", "female", "female", "male", "male", "mal…
    ## $ Age         <dbl> 22, 38, 26, 35, 35, NA, 54, 2, 27, 14, 4, 58, 20, 39, 14, …
    ## $ SibSp       <int> 1, 1, 0, 1, 0, 0, 0, 3, 0, 1, 1, 0, 0, 1, 0, 0, 4, 0, 1, 0…
    ## $ Parch       <int> 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 0, 5, 0, 0, 1, 0, 0, 0…
    ## $ Ticket      <chr> "A/5 21171", "PC 17599", "STON/O2. 3101282", "113803", "37…
    ## $ Fare        <dbl> 7.2500, 71.2833, 7.9250, 53.1000, 8.0500, 8.4583, 51.8625,…
    ## $ Cabin       <chr> NA, "C85", NA, "C123", NA, NA, "E46", NA, NA, NA, "G6", "C…
    ## $ Embarked    <chr> "S", "C", "S", "S", "S", "Q", "S", "S", "S", "C", "S", "S"…

``` r
test <- read.csv('test.csv', na = "")

glimpse(test)
```

    ## Rows: 418
    ## Columns: 11
    ## $ PassengerId <int> 892, 893, 894, 895, 896, 897, 898, 899, 900, 901, 902, 903…
    ## $ Pclass      <int> 3, 3, 2, 3, 3, 3, 3, 2, 3, 3, 3, 1, 1, 2, 1, 2, 2, 3, 3, 3…
    ## $ Name        <chr> "Kelly, Mr. James", "Wilkes, Mrs. James (Ellen Needs)", "M…
    ## $ Sex         <chr> "male", "female", "male", "male", "female", "male", "femal…
    ## $ Age         <dbl> 34.5, 47.0, 62.0, 27.0, 22.0, 14.0, 30.0, 26.0, 18.0, 21.0…
    ## $ SibSp       <int> 0, 1, 0, 0, 1, 0, 0, 1, 0, 2, 0, 0, 1, 1, 1, 1, 0, 0, 1, 0…
    ## $ Parch       <int> 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ Ticket      <chr> "330911", "363272", "240276", "315154", "3101298", "7538",…
    ## $ Fare        <dbl> 7.8292, 7.0000, 9.6875, 8.6625, 12.2875, 9.2250, 7.6292, 2…
    ## $ Cabin       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "B45", NA,…
    ## $ Embarked    <chr> "Q", "S", "Q", "S", "S", "S", "Q", "S", "C", "S", "S", "S"…

Our initial inspection of the data reveals a few interesting points… -
there are a few variables which are seemingly useless for our analysis
(ticket, cabin, name) - however, we may be able to extract an
individual’s title from their name, or perhaps their relative location
on the ship from their cabin - we have to deal with missing values

Let’s start by preprocessing our data. This will include dealing with
missing values, looking for and addressing outliers, and some feature
engineering.

## Preprocessing

We’ll start by joining our training and testing data for joint
preprocessing, and dropping the ticket variable.

``` r
## define train/test identification variable
train$train <- "yes"
test$train <- "no"

## extract outcome variable from training dataset
train.Y <- train$Survived
train <- train %>% select(-Survived)

data <- rbind(train, test)
```

``` r
data <- data %>% select(-Ticket)
```

### Missing Values

Next, let’s take a look at the distribution of missing values.

``` r
colSums(is.na(data)) / nrow(data)
```

    ##  PassengerId       Pclass         Name          Sex          Age        SibSp 
    ## 0.0000000000 0.0000000000 0.0000000000 0.0000000000 0.2009167303 0.0000000000 
    ##        Parch         Fare        Cabin     Embarked        train 
    ## 0.0000000000 0.0007639419 0.7746371276 0.0015278839 0.0000000000

The proportion of missing values for Cabin is too high for imputation,
so we will simply remove this variable.

``` r
data <- data %>% select(-Cabin)
```

Fare and Embarked have extremely low missing value proportions. As long
as they aren’t in the testing dataset, we can simply drop these rows.

``` r
data[is.na(data$Fare) | is.na(data$Embarked), ]
```

    ##      PassengerId Pclass                                      Name    Sex  Age
    ## 62            62      1                       Icard, Miss. Amelie female 38.0
    ## 830          830      1 Stone, Mrs. George Nelson (Martha Evelyn) female 62.0
    ## 1044        1044      3                        Storey, Mr. Thomas   male 60.5
    ##      SibSp Parch Fare Embarked train
    ## 62       0     0   80     <NA>   yes
    ## 830      0     0   80     <NA>   yes
    ## 1044     0     0   NA        S    no

As shown by the “train” indicator variable, the two rows that contain
missing values for Embarked are in the training set. Therefore, we can
drop these rows. In order to drop the entire row, we have to join back
the training set outcome (Survived) before we drop these rows.

``` r
train <- data[data$train == "yes", ]
test <- data[data$train == "no", ]

train$Survived <- train.Y

train <- train[!is.na(train$Embarked), ]

train.Y <- train$Survived

train <- train |> 
  select(-Survived)

data <- rbind(train, test)
```

Unfortunately, the row with a missing value for Fare is in the test
dataset - therefore, we cannot drop it. Instead, we will perform missing
value imputation (here, we’ll use the median to avoid the influence of
outliers). Since we have a ticket class variable (Pclass), it would make
sense to group median fare by ticket class.

``` r
unique_pclass <- unique(data$Pclass)

for(class in unique_pclass){
  data[is.na(data$Fare) & data$Pclass == class, "Fare"] <- median(data[data$Pclass == class, "Fare"], na.rm = TRUE)
}
```

Finally, let’s consider age. Even if all the observations with missing
values for age were within the training set, dropping these rows would
result in losing a large chunk of our data. Therefore, we will perform
missing value imputation for age.

The overall mean or median of age is likely a poor estimate of a given
passenger’s age. We have the Name variable, which seems to contain each
passenger’s title (ex: Master, Mr, Dr). Therefore, it makes sense to
group our age imputation by title. Here, we will use the mean (instead
of the median) because the groupings will likely contain a small number
of rows, which would suggest that the median might be a poor
representation of a given group’s age.

Let’s start by creating a Title variable. We will use a collapsed
version to limit the number of unique titles.

``` r
## use gsub to take advantage of the fact that each title ends with a period
data$Title <- gsub("\\..*|.*, |the ", "", data$Name)

## collapse titles 
data <- data |> 
  mutate(Title = 
           case_when(
             Title %in% c("Major", "Sir", "Col", "Don", "Jonkheer", "Capt") ~ "Mr", 
             Title %in% c("Mlle", "Mme", "Ms") ~ "Miss", 
             Title %in% c("Lady", "Countess", "Dona") ~ "Mrs", 
             TRUE ~ Title
           )
  )
```

Now, let’s perform missing value imputation by title group.

``` r
unique_title <- unique(data$Title)

for(title in unique_title){
  data[is.na(data$Age) & data$Title == title, "Age"] <- mean(data[data$Title == title, "Age"], na.rm = TRUE)
}
```

Finally, let’s check to make sure we’ve addressed all the missing
values.

``` r
colSums(is.na(data)) / nrow(data)
```

    ## PassengerId      Pclass        Name         Sex         Age       SibSp 
    ##           0           0           0           0           0           0 
    ##       Parch        Fare    Embarked       train       Title 
    ##           0           0           0           0           0

### Checking for Outliers

Next, we will check our continuous variables (Age, Fare) for any
potential outliers.

``` r
data |> 
  pivot_longer(cols = c(Age, Fare), names_to = "Column") |> 
  ggplot() + 
  geom_boxplot(aes(y = value)) + 
  facet_wrap(~Column, scales = "free")
```

![](project1_titanic_files/figure-gfm/check%20for%20outliers-1.png)<!-- -->

It looks like we don’t have any crazy outliers for Age, but we do have a
single obvious outlier for Fare that should be removed. In order to drop
the entire row, we have to join back the training set outcome (Survived)
before our filter.

``` r
train <- data[data$train == "yes", ]
test <- data[data$train == "no", ]

train$Survived <- train.Y

train <- train |> 
  filter(Fare < 300)

train.Y <- train$Survived

train <- train |> 
  select(-Survived)

data <- rbind(train, test)
```

### Feature Engineering

Finally, let’s perform some feature engineering. Considering the context
of the Titanic, we know that children were more likely to survive.
Therefore, let’s create an indicator variable for child status.

``` r
data <- data |> 
  mutate(is.child = ifelse(Age < 16, "yes", "no"))
```

We also have some interesting variables in our dataset that could
indicate family size (Parch and SibSp). Let’s create a variable for
family size. We will create a categorical variable to represent family
size as small, medium, or large. We will also create another variable,
is.alone, to indicate whether or not the given passenger is alone (i.e.,
has a family size of 1)

``` r
data <- data |> 
  mutate(
    family.size = Parch + SibSp + 1, 
    family.size.cat = 
      case_when(
        family.size < 4 ~ "small", 
        family.size < 8 ~ "medium", 
        TRUE ~ "large"
      ),
    is.alone = ifelse(family.size == 1, "yes", "no")
  )
```

### Final Preprocessing

Finally, let’s drop the variables we aren’t interested in anymore,
factorize any relevant variables, and split the data back into the
training and testing sets. We also have to make sure that our outcome
variable isn’t numeric.

``` r
## first, drop variables we aren't interested in anymore
data <- data |> 
  select(-c(Name, SibSp, Parch, Title, family.size))

## next, factorize the relevant variables
factor_var <- c("Pclass", "Sex", "Embarked", "is.child", "is.alone", "family.size.cat")

for(var in factor_var) {
  data[, var] <- factor(data[, var])
}

## split data back into training and testing sets
train <- data[data$train == "yes", ] |> 
  select(-train) |> 
  cbind(train.Y) |> 
  rename(Survived = train.Y)
test <- data[data$train == "no", ] |> 
  select(-train)

## make sure outcome variable isn't numeric
train <- train |> 
  mutate(Survived = ifelse(Survived == 1, "yes", "no"))
```

Now we’re ready to move onto modeling.

## Statistical Modeling

First, let’s define our resampling scheme.

``` r
my_trControl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
```

We will begin by fitting a few different logistic regression models.

### Logistic Regression

``` r
set.seed(15213)

log_mod1 <- train(Survived ~ ., 
                  data = train %>% select(-PassengerId), 
                  preProcess = c("center", "scale"),
                  trControl = my_trControl, 
                  method = "glm", 
                  family = binomial(link = "logit"),
                  metric = "Accuracy")

summary(log_mod1)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5021  -0.6322  -0.3858   0.5737   2.4203  
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)            -0.875922   8.634101  -0.101  0.91919    
    ## Pclass2                -0.392754   0.127088  -3.090  0.00200 ** 
    ## Pclass3                -1.084673   0.160354  -6.764 1.34e-11 ***
    ## Sexmale                -1.296796   0.098614 -13.150  < 2e-16 ***
    ## Age                    -0.331347   0.122860  -2.697  0.00700 ** 
    ## Fare                    0.018736   0.132305   0.142  0.88739    
    ## EmbarkedQ               0.008654   0.109517   0.079  0.93702    
    ## EmbarkedS              -0.151624   0.108614  -1.396  0.16272    
    ## is.childyes             0.373241   0.125542   2.973  0.00295 ** 
    ## family.size.catmedium   4.181125 166.819633   0.025  0.98000    
    ## family.size.catsmall    4.900495 178.730396   0.027  0.97813    
    ## is.aloneyes            -0.027641   0.108097  -0.256  0.79818    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1177.03  on 885  degrees of freedom
    ## Residual deviance:  761.22  on 874  degrees of freedom
    ## AIC: 785.22
    ## 
    ## Number of Fisher Scoring iterations: 15

``` r
log_mod1
```

    ## Generalized Linear Model 
    ## 
    ## 886 samples
    ##   8 predictor
    ##   2 classes: 'no', 'yes' 
    ## 
    ## Pre-processing: centered (11), scaled (11) 
    ## Resampling: Cross-Validated (5 fold, repeated 2 times) 
    ## Summary of sample sizes: 708, 710, 709, 708, 709, 710, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.8064951  0.5837674

``` r
set.seed(15213)

log_mod2 <- train(Survived ~ Pclass + Sex + Age + is.child, 
                  data = train %>% select(-PassengerId), 
                  trControl = my_trControl, 
                  method = "glm", 
                  family = binomial(link = "logit"),
                  metric = "Accuracy")

summary(log_mod2)
```

    ## 
    ## Call:
    ## NULL
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7722  -0.6973  -0.3906   0.6350   2.4074  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.151552   0.412707   7.636 2.24e-14 ***
    ## Pclass2     -1.068113   0.259148  -4.122 3.76e-05 ***
    ## Pclass3     -2.299159   0.245505  -9.365  < 2e-16 ***
    ## Sexmale     -2.585989   0.188041 -13.752  < 2e-16 ***
    ## Age         -0.024611   0.008919  -2.759  0.00579 ** 
    ## is.childyes  0.718620   0.357010   2.013  0.04413 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1177.03  on 885  degrees of freedom
    ## Residual deviance:  794.14  on 880  degrees of freedom
    ## AIC: 806.14
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
log_mod2
```

    ## Generalized Linear Model 
    ## 
    ## 886 samples
    ##   4 predictor
    ##   2 classes: 'no', 'yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 2 times) 
    ## Summary of sample sizes: 708, 710, 709, 708, 709, 710, ... 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.7991471  0.5661425

Now, let’s try using elastic net regression.

### Elastic Net

``` r
set.seed(15213)

enet_mod1 <- train(Survived ~ ., 
                   data = train %>% select(-PassengerId), 
                   trControl = my_trControl, 
                   method = "glmnet", 
                   metric = "Accuracy")
```

Now, let’s try using neural networks.

### Neural Networks

``` r
set.seed(15213)

nnet_mod1 <- train(Survived ~ ., 
                   data = train %>% select(-PassengerId), 
                   preProcess = c("center", "scale"),
                   trControl = my_trControl, 
                   method = "nnet", 
                   metric = "Accuracy", 
                   trace = FALSE)
```

Next, let’s try xgBoost.

### xgBoost

``` r
set.seed(15213)

xgbTree_mod1 <- train(Survived ~ ., 
                      data = train %>% select(-PassengerId), 
                      preProcess = c("center", "scale"),
                      trControl = my_trControl, 
                      method = "xgbTree", 
                      metric = "Accuracy", 
                      verbosity = 0)
```

Finally, let’s compare all of our models and choose the best one for
predictions on the test set.

``` r
model_comparison <- resamples(list(full_logistic = log_mod1, 
                                   partial_logistic = log_mod2, 
                                   elastic_net = enet_mod1,
                                   neural_network = nnet_mod1, 
                                   xgboost_tree = xgbTree_mod1))
                                   
  
dotplot(model_comparison, metric = 'Accuracy')
```

![](project1_titanic_files/figure-gfm/model%20comparison-1.png)<!-- -->

The xgbTree model performed best, as according to cross validated
accuracy.

NOTE: I submitted predictions for all of the model types, and the
partial logistic model ended up performing best!

### Predictions

``` r
pred_log_mod2 <- predict(log_mod2, newdata = test, type = "prob")

log_output <- 
  tibble(
    PassengerId = test$PassengerId,
    Survived = ifelse(pred_log_mod2$yes < 0.5, 0, 1)
)
```

``` r
pred_nnet_mod1 <- predict(nnet_mod1, newdata = test, type = "prob")

nnet_output <- 
  tibble(
    PassengerId = test$PassengerId,
    Survived = ifelse(pred_nnet_mod1$yes < 0.5, 0, 1)
)
```

``` r
pred_xgbTree_mod1 <- predict(xgbTree_mod1, newdata = test, type = "prob")

xgbTree_output <- 
  tibble(
    PassengerId = test$PassengerId,
    Survived = ifelse(pred_xgbTree_mod1$yes < 0.5, 0, 1)
)
```

``` r
write.csv(log_output, file = "titanic_predictions.csv", row.names = FALSE)
```
