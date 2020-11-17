#Final Group Project
#Group: Sarah Au, Johana Chazaro Cortes, Spencer Reschly
# Question 1: Correlation between Fitness and Recovery Rate Post-Operation 

rehab.table = read.csv(file.choose(), header = T)
rehab.table$Fitness <- factor(rehab.table$Fitness)

head(rehab.table)
attach(rehab.table)
boxplot(Time ~ Fitness, col=c('blue 4', 'darkgray', 'yellow', 'green4','red4' ))
# What does the boxplot show?
#   > it demonstrates which factors overlap: 
#   > 1 & 2 overlap; 4 & 5 overlap; region 3 is completely isolated meaning...
rehab.lm <- lm(Time ~ Fitness)
summary(rehab.lm)


print(predict(rehab.lm, list(Fitness=factor(c(1,2,3,4,5)))))

c(mean(Time[Fitness == 1]), mean(Time[Fitness == 2]),mean(Time[Fitness == 3]), mean(Time[Fitness == 4]), mean(Time[Fitness == 5]))

# Obtaining the design model matrix
model.matrix((rehab.lm))
# Note: We can see in the matrix table that it has not produced a full factorial design as it does not compare 

# ANOVA table
anova(rehab.lm)

#Also represented by:
F_val = 177.930/18.283
pval = 1 - pf(F_val, 4, 33)
print(data.frame(F_val, pval))
