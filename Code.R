########## Badis Zammouri & Mohamed Chandoul - Project: Plant Responses to Drought

#(all conclusions  are in the slides)


################ Data loading & preprocessing 
library(readr)
database<-read_csv("plant responses to drought.csv")
View(database)

#cleaning
database$Replicate<-NULL #because the later code can read it as num.

#structure verification
str(database)
summary(database)

#checkingg for missing values
colSums(is.na(database))

bartlett.test(y ~ group, data = database)

################## ID of variables
#list of variables
names(database)

Qualitative_variables<-c("Watering", "Phase", "Variety")

Quantitative_variables<- setdiff(names(database), Qualitative_variables)
#Quantitative_variables = all variables - Qualitative_variables

Qualitative_variables
Quantitative_variables

#numerical variables
num_valures<-database[, sapply(database, is.numeric)]#needed for later


################## Descriptive Stats 
lapply(num_valures, summary) #lapply summarizes all quantitative variables at once

#variance
lapply(num_valures, var)
#standard deviation
lapply(num_valures, sd)
#range
lapply(num_valures, range)
#interquantile range IQR
lapply(num_valures, IQR)


##################### Data visualization
#boxplots
boxplot(`Water Use Efficiency` ~ Watering, data=database,
      main="Water Use Efficiency & watering")

#histograms
lapply(names(database)[sapply(database, is.numeric)], function(x) {
  hist(database[[x]], main=paste("Histogram of", x), col="yellow")
})

#pie chart
pie(table(database$Watering),
    main= "Distribution of Watering Conditions",
    col=rainbow(length(unique(database$Watering))))

#scatterplot
plot(database$Photosynthesis, database$"Stomatal Conductance",
     main="Photosynthesis vs Conductance",
     xlab="Photosynthesis", ylab="Conductance")


###################### Distribution analysis
#distribution shape for all variables
f<-function(x){
  library(e1071)
  if(shapiro.test(x)$p.value>0.05) a="normal" 
  else if (skewness(x)>0) a="positive-skewed"
  else a="negative-skewed"
  print(a)
}
lapply(num_valures, f)

#density
x=database$`Leaf Water Potential`
hist(x, probability=TRUE, main="Leaf Water Potential", col="red")
curve(dnorm(x, mean=mean(x), sd=sd(x)), add=TRUE, col="purple")
#we use dnorm only because we are modeling the shape of the distribution, not probabilities


#################### Outlier detection 
#boxplot
boxplot(`total leaf area (cm2)` ~ Phase, data=database,
        main = "Leaf Area by Watering")

#outlier detection
d<-database$`total leaf area (cm2)`[database$Phase=="drought"]
r<- database$`total leaf area (cm2)`[database$Phase=="recovery"]
length(d)-length(r) #to verify same sample size
n=length(d)

#whiskers
low_d <- quantile(d, 0.25)- 1.5 * IQR(r)
up_d<- quantile(d, 0.75)+ 1.5 * IQR(r)

low_r <- quantile(r, 0.25)- 1.5 * IQR(r)
up_r<- quantile(r, 0.75)+ 1.5 * IQR(r)

#outliers
d[d<low_d | d>up_d]
r[r <low_r | r> up_r]

#notches
(notch_d_low <- median(d)- 1.58 * IQR(d) / sqrt(n))
(notch_d_up  <- median(d) + 1.58 * IQR(d)/ sqrt(n))

(notch_r_low <- median(d) - 1.58 * IQR(r) / sqrt(n))
(notch_r_up  <- median(d) + 1.58 * IQR(r) / sqrt(n))


######################### Confidence intervals
#a function that builds t-based CI for group means
g <- function(x){
  result <- aggregate(x, list(database$Phase), function(v){
    n<-length(v)
    m<-mean(v)
    se<-sd(v)/sqrt(n)
    tval<-qt(0.975, df = n - 1)
    
    c(lower = m - tval*se, mean=m, upper = m + tval*se)
  })
  names(result)<-c("Phase", "CI")
  result
}
#CIs for all variables
lapply(num_valures, g)


###################### Correlation matrix
correlation_matrix<- cor(num_valures)
correlation_matrix

#heatmap for correlation
library(corrplot) 
corrplot(correlation_matrix,
         type = "upper",
         tl.cex = 0.45,
         tl.srt = 70,
         method = "color")#this version of code gives the best heatmap visually


################## Correlation significance
#correlation test for Water Use Efficiency & Coarse Roots Mass
test<-cor.test(database$`Water Use Efficiency`, database$`Coarse Roots Mass`)
test

#we created a function to interpret the results of cor.test
interpretation<-function(test) {
  r <- test$estimate #strength
  p <- test$p.value #significance
  ci <- test$conf.int #reliability of estimation
  
  if (p<0.05) {
    if (r>0) {
      direction<-"positive"
    }else{
      direction<-"negative"
    }
    if(abs(r)>= 0.7) {
      strength<-"strong"
    }else if (abs(r)>=0.4) {
      strength<-"moderate"
    }else {
      strength<-"weak"
    }
    paste("Significant", strength, direction, "relationship") #paste=concatenation
  } else {
    "No significant relationship (variables are independent in linear sense)"
  }
}
interpretation(test)

mean(database$Photosynthesis)

