
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------
# after autosegmentation is done, tidy the textgrid data

mono_bio <- read.csv(here("data", "tidy", "completed_mono.csv"))


# load all  all Textgrid files under "participant_uploads"
list_of_files <- list.files(path = here("data", "participant_uploads"), recursive = TRUE,
                            pattern = "\\.TextGrid$", 
                            full.names = TRUE) %>% 
  as.data.frame()



df2 <- character()

for (iteration in 1:nrow(list_of_files)) {
  df <- read_textgrid(list_of_files$.[iteration]) %>% 
    add_words() %>% 
    mutate(file = list_of_files$.[iteration])
  df2 <- rbind(df, df2)
}


english_list <- c("tipping","teller","tacky","penny","pass","parrot","kitten","kennel","cabbage")
spanish_list <- c("tiro","tema","talla","quiso","queja", "cama", "piso","pena","pato")
french_list <- c("tir","terre","tasse","quitte","quelle","pile","pere","patte")


tidy_df <- df2 %>%
  mutate(file = str_remove(file, 
  "/Users/kyleparrish/Documents/GitHub/l3_production_study/data/participant_uploads/")) %>% 
  separate(file, into = c("participant", "other", sep = "/")) %>% 
  mutate(vot = xmax - xmin) %>%
  mutate(relative_vot = vot/duration) %>% 
  dplyr::select(participant, text, tier_name, word, duration, vot, relative_vot) %>% 
  filter(text == "t" | text == "p" | text == "k") %>% 
  mutate(language = case_when(word %in% english_list ~ "english",
                              word %in% spanish_list ~ "spanish", 
                              word %in% french_list ~ "french")) %>% 
  filter(!(text == "p" & word == "tipping")) %>% 
  filter(!(text == "k" & word == "tacky")) %>% 
  filter(!(text == "t" & word == "kitten")) %>% 
  filter(!(text == "t" & word == "parrot")) %>% 
  filter(!(text == "t" & word == "quitte")) %>% 
  filter(!(text == "t" & word == "patte")) %>% 
  filter(!(text == "t" & word == "pato")) %>% 
  mutate(vot_ms = vot*1000)


# segmented participants 

participants <- c(307935, 307937, 307938, 307939, 307940, 
                  307942, 307943, 307944, 307945, 307949, 
                  307953, 307964, 307982, 307983, 307986, 
                  307995, 307996, 307999, 308010, 308018, 
                  308379, 308427, 311348, 313303, 314727, 
                  314744, 314747, 314748, 314752, 314753, 
                  314755, 314759, 314760, 314761, 314763, 
                  314767, 314768, 314772, 314773, 314774, 
                  314778, 314779, 314784, 314785, 314786, 
                  314788, 314789, 314792, 314794, 314795, 
                  314796, 314798, 314799, 314800, 314801, 
                  314803, 314804, 314805, 314806, 314809, 
                  314810, 314812, 314813, 314814, 314815, 
                  314817, 314819, 314820, 314821, 314822, 
                  314825, 314826, 314828, 314829, 314830, 
                  314831, 314833, 314834, 314836, 314837, 
                  314838, 314839, 314841, 314845, 314847, 
                  314848, 314849, 314854, 314860, 314861, 
                  314864)

## load all monolingual participants 

list_of_files_m <- list.files(path = here("data", "mono_uploads"), recursive = TRUE,
                            pattern = "\\.TextGrid$", 
                            full.names = TRUE) %>% 
  as.data.frame()

df3 <- character()

for (iteration in 1:nrow(list_of_files_m)) {
  df <- read_textgrid(list_of_files_m$.[iteration]) %>% 
    add_words() %>% 
    mutate(file = list_of_files_m$.[iteration])
  df3 <- rbind(df, df3)
}

tidy_mono <- df3 %>%
  mutate(file = str_remove(file, 
                           "/Users/kyleparrish/Documents/GitHub/l3_production_study/data/mono_uploads/")) %>% 
  separate(file, into = c("participant", "other", sep = "/")) %>% 
  mutate(vot = xmax - xmin) %>%
  mutate(relative_vot = vot/duration) %>% 
  dplyr::select(participant, text, tier_name, word, duration, vot, relative_vot) %>% 
  filter(text == "t" | text == "p" | text == "k") %>% 
  mutate(language = case_when(word %in% spanish_list ~ "spanish", 
                              word %in% french_list ~ "french")) %>% 
  filter(!(text == "t" & word == "quitte")) %>% 
  filter(!(text == "t" & word == "patte")) %>% 
  filter(!(text == "t" & word == "pato")) %>% 
  mutate(vot_ms = vot*1000)







subset_df <- tidy_df %>% 
  filter(participant %in% participants) %>% 
  filter(!is.na(language))

subset_df %>% 
  write.csv(here("data", "tidy", "subset_df.csv"))
