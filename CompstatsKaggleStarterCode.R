# Required packages
require(MASS) #for lm.ridge

# Import training data from csv file 
train_data <- read.csv("cs-training.csv", header=TRUE)
summary(train_data)

count(train_data$NumberOfDependents)
table(unlist(train_data$NumberOfDependents))

v<-train_data$RevolvingUtilizationOfUnsecuredLines
range0_.5 <- sum(v >= 0  & v < .5)
range0_.5 = 108712

range.5_1 <- sum(v >= .5  & v <= 1)
range.5_1 = 37967

range1_10 <- sum(v >= 1  & v < 10)
range1_10 = 3097

range10_inf <- sum(v >= 10)
range10_inf = 241

table(unlist(train_data$SeriousDlqin2yrs))

v<-train_data$DebtRatio
range0_.5 <- sum(v >= 0  & v < .5)
range0_.5

range.5_1 <- sum(v >= .5  & v <= 1)
range.5_1

range1_10 <- sum(v >= 1  & v < 10)
range1_10

range10_100 <- sum(v >= 10  & v < 100)
range10_100

range100_1000 <- sum(v >= 100  & v < 1000)
range100_1000

range1000_inf <- sum(v >= 1000)
range1000_inf

v<-train_data$MonthlyIncome
range0_1000<- sum(v >= 0  & v < 1000, na.rm = TRUE)
range0_1000
range1000_3000 <- sum(v >= 1000  & v < 3000, na.rm = TRUE)
range1000_3000
range3000_6000 <- sum(v >= 3000  & v < 6000, na.rm = TRUE)
range3000_6000
range6000_10000 <- sum(v >= 6000  & v < 10000 , na.rm = TRUE)
range6000_10000
range10000_20000 <- sum(v >= 10000  & v < 20000,na.rm = TRUE)
range10000_20000
range20000_inf<- sum(v >= 20000, na.rm = TRUE )
range20000_inf



# Find columns with NAs
num_nas_per_col <- c()
for (i in 1:dim(train_data)[2]) {
  num_nas_per_col[i] <- sum(is.na(train_data[,i]))
}
colnames(train_data)[which(num_nas_per_col != 0)]
# We can see that Monthly Income and Number of Dependents both have NAs

# Save row numbers which have NAs
na_income <- which(is.na(train_data[,7]))
na_dep <- which(is.na(train_data[,12]))

# Find starting values for Gibbs-like filling in of NAs
income_mean <- mean(train_data[,7], na.rm=TRUE)
dependents_mean <- mean(train_data[,12], na.rm=TRUE)
# Round dependents mean to 0 because this is by far the mode
dependents_mean <- 0
#hist(train_data[,12])

# Create a copy of the dataset to manipulate
train_data_copy <- train_data[,2:12]

# Fill NAs in copy data set with initial estimates calculated above
for (i in 1:length(train_data_copy[,6])) {
  if (is.na(train_data_copy[i,6])) {
    train_data_copy[i,6] <- income_mean
  }
}
# Check
# head(which(is.na(train_data[,7])))
# train_data[7,7]
# train_data_copy[7,6]
for (i in 1:length(train_data_copy[,11])) {
  if (is.na(train_data_copy[i,11])) {
    train_data_copy[i,11] <- dependents_mean
  }
}
# Check
# head(which(is.na(train_data[,12])))
# train_data[9,12]
# train_data_copy[9,11]

# Check to make sure all NAs were filled in
sum(is.na(train_data))
sum(is.na(train_data_copy))

# Initialize vectors to keep track of values we are filling in so that we can 
# see when they "converge"
in_vec <- rep(income_mean, length(na_income))
dep_vec <- rep(dependents_mean, length(na_dep))
in_vec_old <- rep(0, length(na_income))
dep_vec_old <- rep(0, length(na_dep))

# This while loops runs until the values we are filling in "converge."  For our purposes,
# we define convergence to be when the number of dependents value does not change more
# than one between iterations and the monthly income does not change more than 1000 between
# iterations (but given a buffer of 10 values).  Originally, we set tighter constraints on
# the monthly income, however this took forever to run; the given code takes approximately 
# 15 minutes to run and performs approximately 10 iterations.
while (sum(abs(dep_vec_old - dep_vec) > rep(1, length(dep_vec))) > 0 | sum(abs(in_vec_old - in_vec) > rep(1000, length(in_vec))) > 10) {
  # Set "old" vectors for comparison between consecutive iterations for convergence test
  in_vec_old <- in_vec
  dep_vec_old <- dep_vec
  
  # Check:
  # train_data_copy[head(na_dep),11]
  # Run a linear model for the number of dependents on all other variables
  model1 <- lm(NumberOfDependents ~., train_data_copy)
  dep_vec <- c()
  j <- 1
  # Fill in original NA spots with the values predicted by the lm
  for (i in na_dep) {
    train_data_copy[i,11] <- as.numeric(predict.lm(model1, data.frame(train_data_copy[i,-11])))
    dep_vec[j] <- round(train_data_copy[i,11], digits=0)
    j <- j+1
  }
  # Check:
  #train_data_copy[head(na_dep),11]
  
  # Check:
  #train_data_copy[head(na_income),6]
  # Run a linear model for the monthly income on all other variables
  model2 <- lm(MonthlyIncome ~., train_data_copy)
  in_vec <- c()
  k <- 1
  # Fill in original NA spots with the values predicted by the lm
  for (l in na_income) {
    train_data_copy[l,6] <- as.numeric(predict.lm(model2, data.frame(train_data_copy[l,-6])))
    in_vec[k] <- round(train_data_copy[l,6],-2)
    k <- k+1
  }
  # Check:
  #train_data_copy[head(na_income),6]
  # Print something so we can see when an iteration happens:
  print("iter")
}







###### EXTRA TEST CODE ######

!isTRUE(all.equal(dep_vec_old,dep_vec)) | !isTRUE(all.equal(in_vec_old,in_vec))

summary(model1)
coefficients <- as.numeric(model1$coef)
model_dependents <- function(x, c) {
  val <- 0
  for (i in 1:length(x)) {
    val <- val + c[i]*x[i]
  }
  return(val)
}

x <- as.numeric(train_data_copy[9,-11])
x <- as.numeric(train_data_copy[2,-11])
model_dependents(x, coefficients)

plot(model1, lambda = seq(0,0.1,0.001))
model1$coef

