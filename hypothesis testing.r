####HYPOTHESIS TEST#######

#### A simple t-test in R ####
#Installing ISwR package and calling its library
install.packages("ISwR")
library(ISwR)

#Viewing Dataset juul2 in this(ISwR) Package
View(juul2)

#Conducting T-test to evaluate Hypothesis that the population's mean weight is 45
t.test(juul2$weight,mu=45)

#Result:
#For significance level of 5%, we Fail to reject Null hypothesis that mean of population weight is 45

##############

####Now let me show how to build a test for hypothesis. Technically I can name it after me :P (You can do as well ;) ) 

#Read the file Health.csv. There is a variable called ofp which indicates the number of office visits. 
#Assuming this number of office visits follows a poisson distribution (Count), 
#Let's test the hypothesis that the mean number of office visits is 5.7


#Reading csv file "Health.csv" from the local, please replace this location with your file location
Health <- read.csv("<file path>/Health.csv")

#sample data looks like this:
# ofp	ofnp	opp	opn age	emr	hosp  exclhlth   poorhlth	 medicaid
																
# 5 	  0      0 	0   6.9  0 	 1	0		0		1
# 1	  0	 2	0   7.4  2	 0  	0		0		1
# 13	  0	 0 	0   6.6  3	 3  	0		1		0
# 16	  0	 5 	0   7.6  1 	 1  	0		1		1
# 3	  0	 0	0   7.9	 0 	 0  	0		0		1
# 17	  0	 0	0   6.6	 0	 0  	0		1		0



#Test statistic is Mean of office 
tstat=mean(Health$ofp)
tstat

#This function generates synthetic samples and returns sample means based on our assumed distribution & hypothesis 
f1=function(){
  x=rpois(nrow(Health),lambda =5.7) #Building assumed poisson distribution with our hypothesis
 return(mean(x))
}

#Running the function 10000 times to collect numerous samples
sdist=replicate(10000,f1())

#Plotting the distribution of synthetic sample means 
plot(density(sdist))

#Calculating difference (or deviance, say gap here) between mean of the synthetic sample's mean and hypothesized mean
gap=abs(mean(sdist)-tstat)

#Calculating the p value
a=mean(sdist)-gap####MEAN ONLY
b=mean(sdist)+gap####MEAN ONLY
abline(v=a,col="red")
abline(v=b,col="red")
s1=sdist[sdist<a | sdist>b]
p=length(s1)/length(sdist)
round(p,4)


#Since p value is 0.04, we reject the hypothesis that mean office visits are 5.5 at a significance level of 5%

##################################

##Lets build test for hypothesis for another scenario in the same data

#Read the file Health.csv. There is a variable called age. 
#We would like to conduct a nonparametric test for the hypothesis that the 75% percentile value of age is 7.7.

#Reading the Health.csv file from the local system. Please use your location of the file
Health <- read.csv("<File Pathh>/Health.csv")

#sample data looks like this:
# ofp	ofnp	opp	opn age	emr	hosp  exclhlth   poorhlth	 medicaid
																
# 5 	  0      0 	0   6.9  0 	 1	0		0		1
# 1	  0	 2	0   7.4  2	 0  	0		0		1
# 13	  0	 0 	0   6.6  3	 3  	0		1		0
# 16	  0	 5 	0   7.6  1 	 1  	0		1		1
# 3	  0	 0	0   7.9	 0 	 0  	0		0		1
# 17	  0	 0	0   6.6	 0	 0  	0		1		0


#Finding 75% of the age in the data
tst=quantile(Health$age,probs=0.75)


#Replacing the age less than hypothesized value with 0's and others with 1's
#Counting number of 1's or the number of people woth age greater than hypothesized value(7.7)
tstat1=sum(ifelse(Health$age<7.7,0,1))

#this function creates a sample data with bootstrapping, 
#with 75 percentile of 0s and 25 percentile of 1's and returns the sum of 1's

f2= function(){
  v=c(0,1)
  p=c(0.75,0.25)
  x=sample(x=v,size=nrow(Health),prob=p,replace=T)
  return(sum(x))
}

#creating a distribution of sample means and plotting its density plot
sdist1=replicate(10000,f2())
plot(density(sdist1))

#Calculating difference or distance of calculated mean and mean from samples
gap1=abs(mean(sdist1)-tstat1)
a1=mean(sdist1)-gap####MEAN ONLY
b1=mean(sdist1)+gap####MEAN ONLY
abline(v=a1,col="red")
abline(v=b1,col="red")

#calculating p value or the probability of the claim that 75th percentile of value of age is 7.7
s11=sdist1[sdist1<a1 |sdist1>b1]
p1=length(s11)/length(sdist1)
round(p1,4)

#Since p value is 0, we reject the hypothesis that 75th percentile of value of age is 7.7 

#################################################################################
#########################################################################

#Now let us have a look at another real time problem (with two sample test):

#A customer complained to the owner (of an independent fast-food restaurant) that the
#restaurant is discriminating against the elderly. 
#The customer claims that 60 years old and older are given fewer french fries
#than people under 60. 

###Step 1: Data Collection (Random Sampling)
#The owner responds by gathering data, collected without the 
#knowledge of the employees so as not to affect their behavior.
#Here are data on the weight of french fries (grams) for the two groups of customers:
#Age less than 60: 75 77 80 69 73 76 78 74 75 81 75 80 79 80
#Age greater than 60: 68 74 77 71 73 75 80 77 78 72 69 71 75 78


#Let us conduct a nonparametric two sample test to evaluate the complaint.



##Loading data into vectors
young=c(75,77,80,69,73,76,78,74,75,81,75,80,79,80)
old=c(68,74,77,71,73,75,80,77,78,72,69,71,75,78)

#calculating difference in mean ages of young and old
tstat2=mean(young)-mean(old)
tstat2

#this function samples the young and old age data together and selects two groups. And finds the difference between 
#those two groups.

f3=function(){
  x=c(young,old)
  y=sample(x) #juggling the data
  return(mean(y[1:14])-mean(y[15:28]))
}

#creating a distribution of sample means and plotting its density plot
sdist2=replicate(10000,f3())
plot(density(sdist2))

#Calculating difference or distance of calculated mean and mean from samples
gap2=abs(mean(sdist2)-tstat2)
a2=mean(sdist2)-gap2####MEAN ONLY
b2=mean(sdist2)+gap2####MEAN ONLY
abline(v=a2,col="red")
abline(v=b2,col="red")

#calculating p value or the probability of the claim that there is discrimination among age groups
s12=sdist[sdist2<a2 |sdist2>b2]
p2=length(s12)/length(sdist2)
round(p2,4)

#Since the p value is greater than 0.05(assumed significance value),
#we can say that there is enough evidence that claim of age discrimination may be true.
#Therefore we fail to reject customers hypothesis that "discrimination shown by employees in restaurent among the
#customer age groups"

#####################################################################################################
#End of Script!#
