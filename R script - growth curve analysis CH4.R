#Transforming to right format (scaling, removing NA's, making country factor)
df_CH4 <- subset(final_data, select = c("country", "year", "CH4", "member"))%>%
  transform(year=year-1989) %>%
  na.omit()
df_CH4$country <- factor(df_CH4$country)

#Base model (random intercept model)
m.base.CH4 <- lmer(CH4 ~ year + (1 | country), data=df_CH4, REML=F)

#Adding fixed effect of being member
m.0.CH4 <- lmer(CH4 ~ year + member + (1 | country), data=df_CH4, REML=F)

#Adding the FE on slope (Effects of being member on the rate of growth of CO2 emissions):
m.1.CH4 <- lmer(CH4 ~ year * member + (1 | country), data=df_CH4, REML=F)
m.1.CH4

#Statistically evaluate model: model comparison of goodness of fit
anova(m.base.CH4, m.0.CH4, m.1.CH4)

##No significant differences between the models

#Add linear effect on time to capture individual differences in growth rate (year on country)
m.t1.CH4 <- lmer(CH4 ~ year * member + (1 + year | country), data=df_CH4, REML=F)

anova(m.t1.CH4, m.t1.CH4)
coef(summary(m.t1.CH4))

coefs_CH4 <- data.frame(coef(summary(m.t1.CH4))) 
coefs_CH4$p <- format.pval(2*(1-pnorm(abs(coefs_CH4$t.value))), digits=2, eps=0.0001)
coefs_CH4

##This significantly improved model fit


#Create orthogonal polynomials, determine which order gives best model fit
#Create polynominals
t <- poly((unique(df_CH4$year)), 8)

#Create orthogonal polynomial time terms
df_CH4[,paste("ot", 1:8, sep="")] <- t[df_CH4$year, 1:8]

## SECOND ORDER
m.full2.CH4 <- lmer(CH4 ~ (ot1+ot2)*member + 
                      (ot1 + ot2 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_CH4, REML=F)

## THIRD ORDER
m.full3.CH4 <- lmer(CH4 ~ (ot1+ot2+ot3)*member + 
                      (ot1+ot2+ot3 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_CH4, REML=F)

## FOURTH ORDER
m.full4.CH4 <- lmer(CH4 ~ (ot1+ot2+ot3+ot4)*member + 
                      (ot1+ot2+ot3+ot4 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_CH4, REML=F)

##FIFTH ORDER
m.full5.CH4 <- lmer(CH4 ~ (ot1+ot2+ot3+ot4+ot5)*member + 
                      (ot1+ot2+ot3+ot4+ot5 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_CH4, REML=F)

##SIXTH ORDER
m.full6.CH4 <- lmer(CH4 ~ (ot1+ot2+ot3+ot4+ot5+ot6)*member + 
                      (ot1+ot2+ot3+ot4+ot5+ot6 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_CH4, REML=F)


##SEVENTH ORDER
m.full7.CH4 <- lmer(CH4 ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7)*member + 
                      (ot1+ot2+ot3+ot4+ot5+ot6+ot7 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_CH4, REML=F)

##EIGTH ORDER
m.full8.CH4 <- lmer(CH4 ~ (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8)*member + 
                      (ot1+ot2+ot3+ot4+ot5+ot6+ot7+ot8 | country) + 
                      (ot1+ot2 | country:member), 
                    control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),
                    data=df_CH4, REML=F)


#compare model fit
anova(m.full2.CH4, m.full3, m.full4.CH4, m.full5.CH4, m.full6.CH4, m.full7.CH4)
#Assume normal distribution to report p-values
coefs <- data.frame(coef(summary(m.full2.CH4))) 
coefs$p <- format.pval(2*(1-pnorm(abs(coefs$t.value))), digits=2, eps=0.0001)
coefs




#-------------------------------------------------------------------------------
# Plot GCA model

df_CH4$year <- df_CH4$year + 1989

plot_CH4 <- ggplot(df_CH4, aes(year, CH4, color=member)) +
  stat_summary(fun.data=mean_se, geom="pointrange", alpha =0.5, size = 0.8) + 
  labs(y="Aggregate CH4 emissions", x="Year", color = "") +
  scale_color_manual(labels = c("Non-Members", "Members"), values = c("blue", "red")) +
  stat_summary(aes(y=fitted(m.full2.CH4)), fun.y=mean, geom="line", size = 0.8)

library(grid)

grid.newpage()
grid.draw(rbind(ggplotGrob(plot_N2O), ggplotGrob(plot_CH4), size = "last"))

