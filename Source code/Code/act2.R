df = read.csv('student.csv')
#choose 3 variables Parent, Preparation, Average score
df <- df[,c('ParentLevel', 'TestPreparation','AverageScore')]
head(df)
dim(df)
#data transformation
df[df == "completed"] <- 0
df[df == "none"] <- 1
df[df == "bachelor's degree"] <- 0
df[df == "master's degree"] <- 0
df[df == "associate's degree"] <- 0
df[df == "high school"] <- 1
df[df == "some college"] <- 1
df[df == "some high school"] <- 1
df$ParentLevel[df$ParentLevel == 0] <- "High"
df$ParentLevel[df$ParentLevel == 1] <- "Low"
df$TestPreparation[df$TestPreparation == 0] <- "Prepared"
df$TestPreparation[df$TestPreparation == 1] <- "Not-Prepared"
df
#data visualization
#parent level
barplot(table(df$ParentLevel), main="Levels of parent", names.arg = c("High","Low"))
#preparation
barplot(table(df$TestPreparation), main="Status of Preparation", names.arg = c("Prepared","Not-Prepared"))
#plot against different combinations
#re-transform data to binary value
df$ParentLevel[df$ParentLevel == "High"] <- 0
df$ParentLevel[df$ParentLevel == "Low"] <- 1
df$TestPreparation[df$TestPreparation == "Prepared"] <- 0
df$TestPreparation[df$TestPreparation == "Not-Prepared"] <- 1
#plot
boxplot(df$AverageScore[df$ParentLevel == 0][df$TestPreparation == 0], df$AverageScore[df$ParentLevel == 1][df$TestPreparation == 0], df$AverageScore[df$ParentLevel == 0][df$TestPreparation == 1], df$AverageScore[df$ParentLevel == 1][df$TestPreparation == 1], ylab = "Average Score", main="Average Score for each parent's level and student's preparation", names = c("High-Prepared", "Low-Prepared", "High-NotPrepared", "Low-NotPrepared"))
#model of variance analysis
summary(aov(AverageScore ~ ParentLevel*TestPreparation, data = df))
#model adequacy checking
#1. homogeneity of variances assumption
ANOVA <- aov(AverageScore ~ ParentLevel*TestPreparation, data = df)
plot(ANOVA,1)
library(car)
leveneTest(AverageScore ~ as.factor(ParentLevel)*as.factor(TestPreparation), data = df)
#2. normality assumption
plot(ANOVA,2)
