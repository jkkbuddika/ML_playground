## Unsupervised Machine Learning Training
## This script contain practice exercises to generate distance metrics

# Load required libraries
library(tidyverse)
library(factoextra)

# Let's make a tibble
df <- tibble(Patient_ID=c("P1", "P2", "P3", "P4"),
             ESG=c(11, 13, 2, 1),
             LIN28=c(10, 13, 4, 3),
             TRAL=c(1, 3, 10, 9))
df

# Then let's plot an see any obvious trends
p <- df %>%
  gather(key = Gene, value = Norm_Count, c(ESG:TRAL)) %>%
  ggplot(aes(x = Gene, y = Norm_Count, fill = Patient_ID)) +
  geom_bar(stat = "identity") + facet_wrap(~Patient_ID, ncol = 1)

p

# First step: Distance metrics
# Several methods: Manhattan distance/L1 norm, Euclidean Distance/L2 norm and
# correlation distance

# Data metrics
P1 <- c(11, 10, 1)
P2 <- c(13, 13, 3)
P3 <- c(2, 4, 10)
P4 <- c(1, 3, 9)

df <- rbind(P1, P2, P3, P4)
colnames(df) <- c("ESG", "LIN28", "TRAL")
df

# Let's make the distance metrics using different methods
# Manhattan distance/L1 norm
df %>%
  dist(method = "manhattan") 

# To visualize distance metrics
df %>%
  dist(method = "manhattan") %>%
  get_dist() %>%
  fviz_dist(gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Let's make the distance metrics
# Euclidean Distance/L2 norm
df %>%
  dist(method = "euclidean") %>%
  get_dist() %>%
  fviz_dist(gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Let's make the distance metrics
# Correlation Distance
as.dist(1-cor(t(df)))

# If your data isn't normalized, it's a good idea to do so, before generating
# distance metrics
df %>% 
  # To scale data before generating distance metrics
  scale() %>%
  dist(method = "manhattan")