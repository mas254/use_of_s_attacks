Recoding number of suicide attacks as a binary variable (committed a suicide attack yes/no):
  d$ST <- ifelse(d$SuicideTotal == 0, 0, 1)
  
  Recoding ‘-99’ values to ‘NA’:
    d$GrpSizeOrdinal[d$GrpSizeOrdinal==-99] <- NA
  d$GroupSize[d$GroupSize==-99] <- NA
  
  Recoding number of allies into categories representing number of allies:
    d$Allies <- ifelse(d$NumberAllies == 0, 0,
                       ifelse(d$NumberAllies == 1 | d$NumberAllies == 2 | d$NumberAllies == 3, 1, 
                              ifelse(d$NumberAllies == 4 | d$NumberAllies == 5 | d$NumberAllies==6, 2,
                                     ifelse(d$NumberAllies==7|d$NumberAllies==8|d$NumberAllies==9, 3,
                                            ifelse(d$NumberAllies > 9, 4, NA)))))
  
  
  Figure 2:
    F.2 <- barplot(table(d$ST, d$GrpSizeOrdinal), main = "Figure 2", sub = "Number of Groups that have committed Suicide Attacks by Size Category", ylab = "Number of Groups", xlab = "Group Size Category", legend = c("Did Not", “Did"), args.legend = list(title = "Suicide Attacks", x = "topleft", cex = 0.8), col = c("navy blue", “darkred"), ylim = c(0, 50))

text(F.2, table(d$ST, d$GrpSizeOrdinal)[1,] - .8, table(d$ST, d$GrpSizeOrdinal)[2,], cex = 1, pos = 3, col = "white")

text(F.2, table(d$ST, d$GrpSizeOrdinal)[1,] - 2/2, table(d$ST, d$GrpSizeOrdinal)[1,], cex = 1, pos = 3, col = "white")


Residuals vs Leverage Plot for group size and number of suicide attacks:
plot(lm(SuicideTotal ~ GroupSize, d))

Correlation Test for group size and number of suicide attacks:
  cor.test(d$SuicideTotal[which(d$GroupSize <= 50000)], d$GroupSize[which(d$GroupSize <= 50000)])


Figure 3:
  plot(d$GroupSize[which(d$GroupSize <= 50000)], d$SuicideTotal[which(d$GroupSize <= 50000)], main = "Figure 3", sub = "Number of Suicide Attacks by Group Size", ylab = "Number of Suicide Attacks", xlab = "Group Size", ylim = c(0, 600)) + abline(lm(d$SuicideTotal[which(d$GroupSize <= 50000)] ~ d$GroupSize[which(d$GroupSize <= 50000)])) + text(x = 40000, y = 100, 'r = 0.260')


Residuals vs Leverage Plot for number of allies and number of suicide attacks:
plot(lm(SuicideTotal ~ NumberAllies, d))

Correlation Test for number of allies and number of suicide attacks:
  cor.test(d$SuicideTotal[which(d$NumberAllies < 14)], d$NumberAllies[which(d$NumberAllies < 14)])


Figure 4:
  plot(d$NumberAllies[which(d$NumberAllies < 36)], d$SuicideTotal[which(d$NumberAllies < 36)], main = "Figure 4”, sub = "Number of Suicide Attacks by Number of Allies", ylab = "Number of Suicide Attacks", xlab = "Number of Allies", ylim = c(0, 600)) + abline(lm(d$SuicideTotal[which(d$NumberAllies < 14)] ~ d$NumberAllies[which(d$NumberAllies < 14)])) + text(x = 12, y = 70, 'r = 0.140’)


Figure 5:
F.5 <- barplot(table(d$ST, d$Allies), main = "Figure 5", sub = "Number of Groups that have committed Suicide Attacks by Number of Allies Category", ylab = "Number of Groups", xlab = "Number of Allies Category”, legend = c("Did Not", “Did"), args.legend = list(title = "Suicide Attacks", x = "topright", cex = 0.8), col = c("navy blue", "darkred"), ylim = c(0, 60))

text(F.5, table(d$ST, d$Allies)[1,] - .8, table(d$ST, d$Allies)[2,], cex = 1, pos = 3, col = "white")

text(F.5, table(d$ST, d$Allies)[1,] - 2.3, table(d$ST, d$Allies)[1,], cex = 1, pos = 3, col = “white")
       
       
       Creating a multiple logistic regression model:
         model <- glm(factor(ST) ~ factor(Relig) + factor(Sepa) + (GroupSize) + (NumberAllies), data = d, family = binomial(link = “logit”))
       
       
       Calculating values for Figure 6:
         summary(model)
       
       
       Converting log odds to odds ratio:
         exp(1.832e+00)
       exp(2.649e-01)
       exp(3.714e-06)
       exp(3.790e-01)
       
       
       Predicting the percentage of religious groups that are likely to commit suicide attacks:
         predict(model, data.frame(Relig = c(0, 1), Sepa = c(0, 0), GroupSize = (mean(d$GroupSize, na.rm = T)), NumberAllies = c(mean(d$NumberAllies, na.rm = T))))
       
       
       Figure 7:
         F.7 <- barplot(invlogit(predict(model, data.frame(Relig = c(0,1), Sepa = c(0,0), GroupSize = (mean(d$GroupSize, na.rm = T)), NumberAllies = c(mean(d$NumberAllies, na.rm = T)))))*100, ylim = c(0, 100), main = 'Figure 7', sub = 'Percentage of Groups Predicted to Commit Suicide Attacks if Religious (No/Yes)', ylab = 'Propoeriton of Groups', xlab = 'Religious (No/Yes)', legend = c("Non-Religious: 20.7%", "Religious: 62.0%"), args.legend = list(title = "Percentage of Groups", x = "topleft", cex = 0.8), col = c("navy blue", "darkred"))
text(F.7, labels = c(20.7, '62.0'), cex = 1, 5, col = "white")


Figure 8:
plot(d2$Year, d2$ReligAll, main = "Figure 8", sub = "Number of Religious Groups over Time, Pre- and Post- 9/11", xlab = "Year", ylab = "Number of Religious Groups", ylim = c(25, 55)) + text(x = 1998.2, y = 31.5, '31') + text(x = 2012.2, y = 52.5, '53') + abline(v = 2001, col = 'red') + text(x = 2002.2, y = 47.5, 'Year of 9/11')
                        
                        
                        Figure 9:
                          plot(d2$Year, d2$SepaAll, main = "Figure 9", sub = "Number of Separatist Groups over Time, Pre- and Post- 9/11", xlab = "Year", ylab = "Number of Separatist Groups”, ylim = c(25, 55)) + text(x = 1998.2, y = 29.5, '29') + text(x = 2012.2, y = 49.5, '50') + abline(v = 2001, col = 'red') + text(x = 2002.2, y = 45.5, 'Year of 9/11')
