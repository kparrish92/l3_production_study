source(here::here("scripts", "07_small_data.R"))


# look at the histogram 
fdf = all_df %>% 
  filter(language == "french")

all_df %>% 
  filter(language == "french") %>% 
  ggplot(aes(x = vot_ms)) + 
  geom_density() +
  labs(x = "Raw distribution of y", y = "Density")

## fit a simple normal mixture model
mix <- mixture(gaussian, nmix = 2)
prior <- c(
  prior(normal(0, 5), Intercept, nlpar = mu1),
  prior(normal(0, 5), Intercept, nlpar = mu2),
  prior(dirichlet(2, 2), theta)
)

fit1 <- brm(bf(y ~ x), dat, family = mix, chains = 2, init = 0)

summary(fit1)

dat = all_df

## compute the membership probabilities
ppm <- pp_mixture(fit1)
str(ppm)

## extract point estimates for each observation
head(ppm[, 1, ])

## classify every observation according to
## the most likely component
apply(ppm[, 1, ], 1, which.max)
