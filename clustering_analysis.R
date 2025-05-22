

mdf = all_df %>% 
  filter(participant != 307935) %>% 
  select(participant, vot_ms, word) %>% 
  pivot_wider(names_from = word, values_from = vot_ms) 

answers = mdf[,1]  
mdf <- mdf[,-1]  

d <- dist(mdf) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim

x <- fit$points[,1]
y <- fit$points[,2]
data = data.frame(x,y)

data$word = answers


ggplot(data, aes(x, y)) +
  geom_point(size = 1) + stat_ellipse(level = .8) + theme_minimal() + 
  ylab("Dimension 1") +
  xlab("Dimension 2") +
  labs(color = "Language Group") 
return(final_df)

}
