### power analysis from the data from the first ~ 7 participants. 
### The results suggest that 70 participants are needed for a lower than small effect size 
### to reliably be detected. 

k = 100
results = matrix(nrow = k, ncol = 2)
power = matrix(nrow = 7, ncol = 3)
sizes = c(10,20,30,40,50,60,70)

for(thisSize in 1:nrow(power)){
  
for(thisRun in 1:k){
eng <- rnorm(n = sizes[thisSize], mean = 0.0498, sd = 0.0674)
french <- rnorm(n = sizes[thisSize], mean = 0.0535, sd = 0.103) 


tost <- TOSTER::TOSTpaired(n = sizes[thisSize], m1 = mean(eng), m2 = mean(french), 
                   sd1 = sd(eng), sd2 = sd(french),
                   r12 = .5, low_eqbound_dz = -.5, high_eqbound_dz = .5)

t_test <- t.test(eng, french, paired = TRUE)

tost_result <- pmax(tost$TOST_p1, tost$TOST_p2)
nhst_result <- t_test$p.value

results[thisRun, 1] = tost_result
results[thisRun, 2] = nhst_result

results <- results %>% 
  as.data.frame()

}
power[thisSize, 1] = sizes[thisSize]
power[thisSize, 2] = sum(results$V1 < .05)
power[thisSize, 3] = sum(results$V2 < .05)
}  
  


#### power analysis for comparison group


results2 = matrix(nrow = k)
power2 = matrix(nrow = 7, ncol = 2)
sizes2 = c(10,20,30,40,50,60,70)

for(thisSize2 in 1:nrow(power)){
  
for(thisRun2 in 1:k){
span <- rnorm(n = sizes2[thisSize2], mean = 0.0262, sd = 0.0469)
french <- rnorm(n = sizes2[thisSize2], mean = 0.0535, sd = 0.103) 

t_test <- t.test(span, french)

results2[thisRun2] = t_test$p.value


}
  power2[thisSize2, 1] = sizes[thisSize2]
  power2[thisSize2, 2] = sum(results2 < .05)
}  


