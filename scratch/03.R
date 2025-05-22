data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data


x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")

kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))


set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

library(factoextra)
fviz_cluster(km.res, data = df)


this_ppt_df = l1_df %>% 
  select(word, vot_ms, participant) %>% 
  pivot_wider(names_from = participant, values_from = vot_ms) %>% 
  column_to_rownames(var = "word") 
km.res <- kmeans(this_ppt_df, 3, nstart = 25)
fviz_cluster(km.res, data = this_ppt_df)
