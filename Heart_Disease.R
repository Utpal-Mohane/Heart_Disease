# importing the dataset 
df = read.csv("C:\\Users\\utpal\\Desktop\\R PROJECT\\heart.csv")

# changing the column names for the data set
a = c('Age', 'Sex', 'Chest_Pain_Type', 'Resting_Blood_Pressure', 
      'Serum_Cholestrol(mg/dl)', 'Fasting_Blood_Sugar',
      'Resting_Electrocardiographic_Results', 'Maximum_Heart_Rate_Achieved', 
      'Exercise_Induced_Angina', 'Oldpeak', 'Slope_Of_The_Peak', 
      'Number_Of_Major_Vessels', 'Thal', 'Target')

for (i in c(1: 14)){
  counter = 1
  for (f in a){
    if (counter == 1){
      colnames(df)[i] = a[i]
      counter = 0
    }
  }
}

# checking to see what age is most vulnerable to heart disease
hist(df$Age, xlab = 'Age', main = 'Histogram of Age', col = 'yellow')
# people b/w age 50 - 60 are most affected

# checking the type of chest pain that is accompanied with a heart
# disease most 

b = table(df$Chest_Pain_Type)
b = c(b)
barplot(b, xlab = 'chest pain type', ylab = 'frequency', 
        main = 'Barplot for the most common type of chest pain', 
        col = 'red')
pie(b, labels = c('Type 0', 'Type 1', 'Type 2', 'Type 3'))

# Average of resting blood pressure
d = mean(df$Resting_Blood_Pressure)
d

# Average serum cholestrol
e = mean(df$`Serum_Cholestrol(mg/dl)`)
e

# median age 
median(df$Age) # 55

# shuffling data
df2 = sample(nrow(df))
df2 = df[df2, ]

# performing logistic regression

# splitting the data into train and test
0.8* nrow(df2)
train = df2[1: 242, ]
test = df2[243: nrow(df2), ]
colnames(df2)[14] = 'Target'
model = glm(Target~., data = train, family = 'binomial')

summary(model)
pred = predict(model, test, response = 'predict')

p = ifelse(pred>0.5, 1, 0)
t = table(p, test$Target)
confusion_matrix = sum(diag(t))/ sum(t) * 100
confusion_matrix

# we observe that only 'Sex', 'Chest_Pain_Type', 'Excercised_Induced_Angina', 
# 'Number_Of_Major_Vessels' and 'Thal' are related to Target

# Now calculating Logistic Regression with only above mentioned 
# related columns
df2 = df2[, c(2, 3, 9, 12, 13, 14)]

# splitting the data
train = df2[1: 242, ]
test = df2[243: nrow(df2), ]

colnames(df2)[6] = 'Target'
# creating model
model = glm(Target~., data = train, family = 'binomial')
pred = predict(model, test, response = 'predict')

p = ifelse(pred<0.5, 0, 1)
t = table(p, test$Target)
acc = sum(diag(t))/ sum(t) * 100
acc # 85%


# proportion of people having heart disease
t = c(table(df2$Target))
pie(t, labels = c('undiseased', 'diseased'))





