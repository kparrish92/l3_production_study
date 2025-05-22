
all_df$language = as.factor(all_df$language)

all_df$language = relevel(all_df$language, ref = "french")

model = brm(vot_ms ~ language + (language | participant), data = all_df)

ranef(model)

fixef(model)
r = ranef(model) %>% as.data.frame() %>% 
  rownames_to_column("ppt") 
  # I can use this to adjust the fixed effect and derive a model guess per participant 

adjusted_estimates = data.frame(
  ppt = r$ppt,
  english_effect = fixef(model)[2]+r$participant.Estimate.languageenglish,
  spanish_effect = fixef(model)[3]+r$participant.Estimate.languagespanish
) 

adjusted_estimates$english_upper = adjusted_estimates$english_effect + r$participant.Q97.5.languageenglish
adjusted_estimates$english_lower = adjusted_estimates$english_effect + r$participant.Q2.5.languageenglish
adjusted_estimates$spanish_upper = adjusted_estimates$spanish_effect + r$participant.Q97.5.languagespanish
adjusted_estimates$spanish_lower = adjusted_estimates$spanish_effect + r$participant.Q2.5.languagespanish


adjusted_estimates %>% 
  pivot_longer(cols = 2:7, names_to = "name", values_to = "value") %>% 
  separate(name, into = c("language", "estimate"), sep = "_") %>% 
  pivot_wider(names_from = estimate, values_from = value) %>% 
  ggplot(aes(y = ppt, x = effect, color = language)) + 
  geom_pointrange(aes(xmin = lower, xmax = upper)) +
  geom_vline(xintercept = c(-10,10))

