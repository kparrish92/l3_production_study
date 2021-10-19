# a post-hoc power analysis of the effect sizes obtained

# Full dataset PA

l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))
l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
tidy_df <- rbind(l1_df, l2_df)

all_pa_df <- tidy_df %>% 
  group_by(language) %>% 
  summarise(`Relative VOT` = mean(relative_vot), `SD` = sd(relative_vot))

#number of iterations 

k = 100
full_data_eng_sp = matrix(nrow = k)
full_data_fren_sp = matrix(nrow = k)
full_data_eng_fren = matrix(nrow = k)

for(thisRun in 1:k){
    mv1 = rnorm(39, m = all_pa_df$`Relative VOT`[1], 
                sd = all_pa_df$SD[1])
    mv2 = rnorm(39, m = all_pa_df$`Relative VOT`[3], 
                sd = all_pa_df$SD[3])
    eng_span_f = t.test(mv1,mv2)
    full_data_eng_sp[thisRun] = eng_span_f$p.value
}

for(thisRun in 1:k){
  mv1 = rnorm(39, m = all_pa_df$`Relative VOT`[2], 
              sd = all_pa_df$SD[2])
  mv2 = rnorm(39, m = all_pa_df$`Relative VOT`[3], 
              sd = all_pa_df$SD[3])
  fren_span_f = t.test(mv1,mv2)
  full_data_fren_sp[thisRun] = fren_span_f$p.value
}

for(thisRun in 1:k){
  mv1 = rnorm(39, m = all_pa_df$`Relative VOT`[2], 
              sd = all_pa_df$SD[2])
  mv2 = rnorm(39, m = all_pa_df$`Relative VOT`[1], 
              sd = all_pa_df$SD[1])
  fren_eng_f = t.test(mv1,mv2)
  full_data_eng_fren[thisRun] = fren_eng_f$p.value
}

# full eng span power
eng_span_full <- sum(full_data_eng_sp < .05)
fren_span_full <- sum(full_data_fren_sp < .05)
fren_eng_full <- sum(full_data_eng_fren < .05)



# L2 sub PA

l2_pa_df <- l2_df %>% 
  group_by(language) %>% 
  summarise(`Relative VOT` = mean(relative_vot), `SD` = sd(relative_vot))


k = 100
l2_data_eng_sp = matrix(nrow = k)
l2_data_fren_sp = matrix(nrow = k)
l2_data_eng_fren = matrix(nrow = k)

for(thisRun in 1:k){
  mv1 = rnorm(23, m = l2_pa_df$`Relative VOT`[1], 
              sd = l2_pa_df$SD[1])
  mv2 = rnorm(23, m = l2_pa_df$`Relative VOT`[3], 
              sd = l2_pa_df$SD[3])
  eng_span_l2 = t.test(mv1,mv2)
  l2_data_eng_sp[thisRun] = eng_span_l2$p.value
}

for(thisRun in 1:k){
  mv1 = rnorm(39, m = l2_pa_df$`Relative VOT`[2], 
              sd = l2_pa_df$SD[2])
  mv2 = rnorm(39, m = l2_pa_df$`Relative VOT`[3], 
              sd = l2_pa_df$SD[3])
  fren_span_l2 = t.test(mv1,mv2)
  l2_data_fren_sp[thisRun] = fren_span_l2$p.value
}

for(thisRun in 1:k){
  mv1 = rnorm(39, m = l2_pa_df$`Relative VOT`[2], 
              sd = l2_pa_df$SD[2])
  mv2 = rnorm(39, m = l2_pa_df$`Relative VOT`[1], 
              sd = l2_pa_df$SD[1])
  fren_eng_l2 = t.test(mv1,mv2)
  l2_data_eng_fren[thisRun] = fren_eng_l2$p.value
}

# full eng span power
eng_span_l2 <- sum(l2_data_eng_sp < .05)
fren_span_l2 <- sum(l2_data_fren_sp < .05)
fren_eng_l2 <- sum(l2_data_eng_fren < .05)



# l1 sub PA

l1_pa_df <- l1_df %>% 
  group_by(language) %>% 
  summarise(`Relative VOT` = mean(relative_vot), `SD` = sd(relative_vot))


k = 100
l1_data_eng_sp = matrix(nrow = k)
l1_data_fren_sp = matrix(nrow = k)
l1_data_eng_fren = matrix(nrow = k)

for(thisRun in 1:k){
  mv1 = rnorm(23, m = l1_pa_df$`Relative VOT`[1], 
              sd = l1_pa_df$SD[1])
  mv2 = rnorm(23, m = l1_pa_df$`Relative VOT`[3], 
              sd = l1_pa_df$SD[3])
  eng_span_l1 = t.test(mv1,mv2)
  l1_data_eng_sp[thisRun] = eng_span_l1$p.value
}

for(thisRun in 1:k){
  mv1 = rnorm(39, m = l1_pa_df$`Relative VOT`[2], 
              sd = l1_pa_df$SD[2])
  mv2 = rnorm(39, m = l1_pa_df$`Relative VOT`[3], 
              sd = l1_pa_df$SD[3])
  fren_span_l1 = t.test(mv1,mv2)
  l1_data_fren_sp[thisRun] = fren_span_l1$p.value
}

for(thisRun in 1:k){
  mv1 = rnorm(39, m = l1_pa_df$`Relative VOT`[2], 
              sd = l1_pa_df$SD[2])
  mv2 = rnorm(39, m = l1_pa_df$`Relative VOT`[1], 
              sd = l1_pa_df$SD[1])
  fren_eng_l1 = t.test(mv1,mv2)
  l1_data_eng_fren[thisRun] = fren_eng_l1$p.value
}

# full eng span power
eng_span_l1 <- sum(l1_data_eng_sp < .05)
fren_span_l1 <- sum(l1_data_fren_sp < .05)
fren_eng_l1 <- sum(l1_data_eng_fren < .05)


power = c(eng_span_l1, eng_span_l2, eng_span_full, 
fren_span_l1, fren_span_l2, fren_span_full,
fren_eng_l1, fren_eng_l2, fren_eng_full)

subset = c("L1 subset", "L2 subset", "Full subset", 
    "L1 subset", "L2 subset", "Full subset",
    "L1 subset", "L2 subset", "Full subset")

pairing = c("English-Spanish", "English-Spanish", "English-Spanish", 
  "French-Spanish", "French-Spanish", "French-Spanish",
  "French-English", "French-English", "French-English")

power_df <- data.frame(power, subset, pairing)

power_df %>% 
  write.csv(here("data", "tidy", "powerdf.csv"))