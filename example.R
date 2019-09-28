library(transitiveR)

#create a partial order
po <- tibble(
   d = c( 1,2,3,5,6,11,7,4,10,12,13,15,8,16,17,19,20,21,9,24,22,23,25,26,27,28,29,30,31,32,33,34,35,36,37)
  ,a = c(NA,1,1,1,2, 3,6,7, 5,10,12,10,6, 3,16,12,12,12,8, 8,34,22,22,22, 8,10, 2, 2, 2,13,13,11,30,30,31)
)

data <- tibble(
  id = po$d
  ,j = as.factor(sample(LETTERS, length(id), replace = T))
  ,i = runif(length(id), 200, 73453)
)

tr <- transitive.closure(po, d, a)
tr.sum <- transitive.summary(tr, d, a, extended = T)

data %>% inner_join(tr, by = c("id" = "d")) %>%
  group_by(a) %>%
  summarise(
    n = n(),
    value = first(i),
    sum = sum(i),
    mean = mean(i),
    letter = paste0(j, collapse = "_")
  ) %>% View




