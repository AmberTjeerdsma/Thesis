library('lme4')
library('ggplot2')

#Transforming to right format (scaling, removing NA's, making country factor)
df <- subset(final_data, select = c("country", "year", "CO2", "member"))%>%
  transform(year=year-1989) %>%
  na.omit()
df$country <- factor(df$country)

plot(df$year, df$CO2)

#Base model (random intercept model)
m.base <- lmer(CO2 ~ year + (1 | country), data=df, REML=F)

#Adding fixed effect of being member
m.0 <- lmer(CO2 ~ year + member + (1 | country), data=df, REML=F)

#Adding the FE on slope (Effects of being member on the rate of growth of CO2 emissions):
m.1 <- lmer(CO2 ~ year * member + (1 | country), data=df, REML=F)


#Statistically evaluate model: model comparison of goodness of fit
anova(m.base, m.0, m.1)

##No significant differences between the models

#Add linear effect on time to capture individual differences in growth rate (year on country)
m.t1 <- lmer(CO2 ~ year * member + (1 + year | country), data=df, REML=F)

anova(m.1, m.t1)
coef(summary(m.t1))

coefs <- data.frame(coef(summary(m.t1))) 
coefs$p <- format.pval(2*(1-pnorm(abs(coefs$t.value))), digits=2, eps=0.0001)
coefs

##This significantly improved model fit


#Create orthogonal polynomials, determine which order gives best model fit
#Create polynominals
t <- poly((unique(df$year)), 8)

#Create orthogonal polynomial time terms
df[,paste("ot", 1:8, sep="")] <- t[df$year, 1:8]

## SECOND ORDER
m.full2 <- lmer(CO2 ~ (ot1+ot2)*member + 
                 (ot1 + ot2 | country) + 
                 (ot1+ot2 | country:member), 
               control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
               data=df, REML=F)

## THIRD ORDER
m.full3 <- lmer(CO2 ~ (ot1+ot2+ot3)*member + 
                 (ot1+ot2+ot3 | country) + 
                 (ot1+ot2 | country:member), 
               control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
               data=df, REML=F)

## FOURTH ORDER
m.full4 <- lmer(CO2 ~ (ot1+ot2+ot3+ot4)*member + 
                  (ot1+ot2+ot3+ot4 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df, REML=F)

##FIFTH ORDER
m.full5 <- lmer(CO2 ~ (ot1+ot2+ot3+ot4+ot5)*member + 
                  (ot1+ot2+ot3+ot4+ot5 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df, REML=F)

##SIXTH ORDER
m.full6 <- lmer(CO2 ~ (ot1+ot2+ot3+ot4+ot5+ot6)*member + 
                  (ot1+ot2+ot3+ot4+ot5+ot6 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df, REML=F)


##SEVENTH ORDER
m.full7 <- lmer(CO2 ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7)*member + 
                  (ot1+ot2+ot3+ot4+ot5+ot6+ot7 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df, REML=F)

##EIGTH ORDER
m.full8 <- lmer(CO2 ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8)*member + 
                  (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8 | country) + 
                  (ot1+ot2 | country:member), 
                control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                data=df, REML=F)


#compare model fit
anova(m.t1, m.full2, m.full3, m.full4, m.full5)
anova(m.full5, m.full6, m.full7, m.full8)
#Assume normal distribution to report p-values
coefs <- data.frame(coef(summary(m.full2))) 
coefs$p <- format.pval(2*(1-pnorm(abs(coefs$t.value))), digits=2, eps=0.0001)
coefs




#-------------------------------------------------------------------------------
# Plot GCA model

df$year <- df$year + 1989
 
plot_CO2 <- ggplot(df, aes(year, CO2, color=member)) +
  stat_summary(fun.data=mean_se, geom="pointrange", alpha =0.5, size = 0.8) + 
  labs(y="Aggregate CO2 emissions", x="Year", color = "") +
  scale_color_manual(labels = c("Non-Members", "Members"), values = c("blue", "red")) +
  stat_summary(aes(y=fitted(m.full2)), fun.y=mean, geom="line", size = 0.8)
