install.packages("multispatialCCM")
library(multispatialCCM)

#REMOVE NAs AND NON-ENTERED COUNTRIES ------------------------------------------

df_entered <- subset(final_data, member == 1)
df_entered <- df_entered[!is.na(df_entered$CO2),]

#FIND OPTIMAL E ----------------------------------------------------------------

for (country1 in unique(final_data$country)) {
      if(length(which(final_data$country == country1)) >=9) {
          
          #Subset dataset to entered countries
          A <- subset(df_entered, country == country1)
          
          #Calculate optimal E
          maxE<-3 #Maximum E to test
          #Matrix for storing output
          Emat<-matrix(nrow=maxE-1, ncol=1); colnames(Emat)<-c("A")
          #Loop over potential E values and calculate predictive ability
          #of each process for its own dynamics
          for(E in 2:maxE) {
            #Uses defaults of looking forward one prediction step (predstep)
            #And using time lag intervals of one time step (tau)
            Emat[E-1,"A"]<-SSR_pred_boot(A=A$CO2, E=E, tau = 1, predstep = 1)$rho
          }
          E_A <- which(Emat[,"A"] == max(Emat[,"A"]))

#TEST FOR SUFFICIENCY OF INFO --------------------------------------------------
          signal_out<-SSR_check_signal(A=A$CO2, E=E_A, tau=1,predsteplist=1:3)
          
          mypath <- file.path("C:","R","Plots", 
                              paste(country1, ".jpg", sep = ""))
          
          jpeg(file=mypath)
            matplot(signal_out$predatout$predstep, signal_out$predatout$rho, 
                  type="l", col=1:2, lty=1:2,
                  xlab="predstep", ylab="rho", lwd=2)
          dev.off()
      }}

##SELECT COUNTRIES -------------------------------------------------------------

non_countries = c("Bangladesh", "Belgium", "Benin", "Bolivia", "Cote d'Ivoire",
                  "Djibouti", "Eritrea", "Ethiopia", "Georgia", "Iceland", 
                  "Saudi Arabia", "India", "Indonesia", "Iran, Islamic Rep.", 
                  "Japan","Kyrgyz Republic","Maldives", "Malta", 
                  "Marshall Islands", "Mongolia", "Mozambique", 
                  "Russian Federation", "Solomon Islands", "Sudan", "Sweden", 
                  "Switzerland", "Tanzania", "Vietnam", "Uganda", "Tuvalu")

df_entered <- df_entered[! df_entered$country %in% non_countries,]

#TEST FOR OPTIMAL N. OF BOOTSTRAPPING ITERATIONS--------------------------------

# 100 iterations
CCM_boot_A100<-CCM_boot(A$CO2, B$CO2, E_A, tau=1, iterations=100)
CCM_boot_B100<-CCM_boot(B$CO2, A$CO2, E_B, tau=1, iterations=100)

# 200 iterations
CCM_boot_A200<-CCM_boot(A$CO2, B$CO2, E_A, tau=1, iterations=200)
CCM_boot_B200<-CCM_boot(B$CO2, A$CO2, E_B, tau=1, iterations=200)

#1000 iterations
CCM_boot_A1000<-CCM_boot(A$CO2, B$CO2, E_A, tau=1, iterations=1000)
CCM_boot_B1000<-CCM_boot(B$CO2, A$CO2, E_B, tau=1, iterations=1000)

t.test(CCM_boot_A100$rho, CCM_boot_A200$rho)
t.test(CCM_boot_B100$rho, CCM_boot_B200$rho)

t.test(CCM_boot_A100$rho, CCM_boot_A1000$rho)
t.test(CCM_boot_B100$rho, CCM_boot_B1000$rho)


## FINAL ALGORITHM -------------------------------------------------------------
output <- data.frame(c())

for (country1 in unique(df_entered$country)) {
  for (country2 in unique(df_entered$country)) { 
    if (country1 != country2) {
      
      #Subset dataset to countries
      A <- subset(df_entered, country == country1)
      B <- subset(df_entered, country == country2)
      
      #Calculate optimal E
      maxE<-3 #Maximum E to test
      #Matrix for storing output
      Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A", "B")
      #Loop over potential E values and calculate predictive ability
      #of each process for its own dynamics
      for(E in 2:maxE) {
        #Uses defaults of looking forward one prediction step (predstep)
        #And using time lag intervals of one time step (tau)
        Emat[E-1,"A"]<-SSR_pred_boot(A=A$CO2, E=E, predstep=1, tau=1)$rho
        Emat[E-1,"B"]<-SSR_pred_boot(A=B$CO2, E=E, predstep=1, tau=1)$rho
      }
      
      E_A <- which(Emat[,"A"] == max(Emat[,"A"]))
      E_B <- which(Emat[,"B"] == max(Emat[,"B"]))
      
      #Run the CCM test
      #E_A and E_B are the embedding dimensions for A and B.
      #tau is the length of time steps used (default is 1)
      #iterations is the number of bootsrap iterations (default 100)
      # Does A "cause" B?
      #Note - increase iterations to 100 for consistent results
      CCM_boot_A<-CCM_boot(A$CO2, B$CO2, E_A, tau=1, iterations=1000)
      # Does B "cause" A?
      CCM_boot_B<-CCM_boot(B$CO2, A$CO2, E_B, tau=1, iterations=1000)
      
      #Test for significant causal signal
      #See R function for details
      CCM_significance_test<-ccmtest(CCM_boot_A,
                                     CCM_boot_B)
      
      pvalues <- data.frame(CCM_significance_test)
      output[country1, country2] <- pvalues$CCM_significance_test[1]
      
      
      if (pvalues$CCM_significance_test[1] < 0.05) {
        
        ##PLOTTING
        mypath <- file.path("C:","users", "Amber", "Documents",
                            "R", "entered_graphs",
                            paste(country1, country2, ".jpg", sep = " "))
        
        
        jpeg(file=mypath)
        #Plot results
        plotxlimits<-range(c(CCM_boot_A$Lobs))
        #Plot "A causes B"
        plot(CCM_boot_A$Lobs, CCM_boot_A$rho, type="l", col=4, lwd=2, 
             xlim=c(plotxlimits[1], plotxlimits[2]), ylim=c(0,1),
             xlab="Library length (L)", ylab="rho")
        
        #Add +/- 1 standard error
        matlines(CCM_boot_A$Lobs,
                 cbind(CCM_boot_A$rho-CCM_boot_A$sdevrho,
                       CCM_boot_A$rho+CCM_boot_A$sdevrho),
                 lty=3, col=4)
        
        
        legend_A <- paste(country1, "causes", country2, ", p = ", pvalues$CCM_significance_test[1])
        legend("topleft", c(legend_A),
               lty=c(1,2), col=c(4,2), lwd=2, bty="n")
        
        dev.off()
        
      }
      
    }}}

write.csv(output, file = "CCM_entered.csv")
