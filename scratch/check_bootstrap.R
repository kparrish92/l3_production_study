

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
library(MKinfer)
l2_df <- read.csv(here::here("data", "tidy", "l2_subset.csv"))
l1_df <- read.csv(here::here("data", "tidy", "non_l2_subset.csv"))
all_df = rbind(l1_df, l2_df) %>% 
  mutate(sig = ifelse(fr_sp_p < .05, 1, 0))

sig_df = all_df %>% 
  filter(sig == 0) %>% 
  filter(language == "spanish" | language == "french")

ppt_list = unique(sig_df$participant) # fr and spanish are null - can we find more folks are intermediate? 

list = list()

for (i in 1:length(ppt_list))
{
this_df = sig_df %>% 
    filter(participant == ppt_list[i])
  
res = boot.t.test(vot~language, data = this_df)  

list[[i]] = data.frame(boot = res[["boot.p.value"]], reg = res[["p.value"]])
}

changes = do.call(rbind, list) %>% 
  as.data.frame()


