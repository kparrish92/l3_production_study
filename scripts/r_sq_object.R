
# this calculates R squared per model and saves it in an object

mod0_r_sq <- MuMIn::r.squaredGLMM(mod0) %>% 
  as.data.frame() %>% 
  mutate(model = "full")

mod1_r_sq <- MuMIn::r.squaredGLMM(model1) %>% 
  as.data.frame() %>% 
  mutate(model = "l2")

mod2_r_sq <- MuMIn::r.squaredGLMM(model2) %>% 
  as.data.frame() %>% 
  mutate(model = "l1")

all_models_r_sq <- rbind(mod0_r_sq, mod1_r_sq, mod2_r_sq)

all_models_r_sq %>% 
  write.csv(here("data", "tidy", "r_sq_df.csv"))

fixef(mod0)

