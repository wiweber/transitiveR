library(dplyr)
library(tidyr)
library(transitiveR)

#create a partial order
tc_po <- tibble(
   d = c(2,3,5,6,11,7,4,10,12,13,15,8,16,17,19,20,21,9,24,22,23,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40)
  ,a = c(1,1,1,2, 3,6,7, 5,10,12,10,6, 3,16,12,12,12,8, 8,34,22,22,22, 8,10, 2, 2,35,13,13,11,30,30,31,37,37,39)
)

tc_data <- tibble(
  id = unique(c(tc_po$d, tc_po$a)) %>% sort()
  ,j = as.factor(sample(LETTERS, length(id), replace = T))
  ,i = runif(length(id), 200, 1000) %>% round(0)
)

tr <- transitive.closure(tc_po, d, a)
tr
tr.sum <- transitive.summary(tr, d, a, extended = T, path_sep = '<-')
tr.sum

# using trasitive.closure result direct
tc_data %>% inner_join(tr, by = c("id" = "d")) %>%
  group_by(a) %>%
  summarise(
    n = n(),
    value = first(i),
    sum = sum(i),
    mean = mean(i)
  )

# using tc on the fly
data <- tc_data %>%
  left_join(tc_po, by = c("id" = "d"))

# using convinient function tc_group_by and do dplyr::summarize after
data %>%
  tc_group_by(id, a) %>%
  summarise(n = n(), s = sum(i))

# using convinient function tc_summarize

data %>%
  tc_summarize(id, a, n = n(), s = sum(i), diff = s / n, )

system.time(
  tc_data %>%
    tc_group_by_(id, a, tr, by = c("id" = "d")) %$%
    data %>%
    summarise(n = n(), s = sum(i))
)
