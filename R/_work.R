load("tc.demo.RData")

tc_po <- ToDataFrameNetwork(tree, direction = "climb") %>%
  mutate(
    from_ = from,
    from = to,
    to = from_,
    from_ =
      NULL
  ) %>% as.tbl()

tc_data <- tibble(
  id = tc_po$from,
  value = runif(nrow(tc_po), 1, 100) %>% round(0)
)

transitive.summary(tr, from, to, extended = T)

system.time(
  tr <- po %>%
      transitive.closure(from, to)
)

system.time(
  tc_po %>% tc_group_by(from, to) %>%
    group_by(to) %>% summarise(n = n())
)

system.time(
  tc_data %>% inner_join(tr, by = c("id" = "from")) %>%
    group_by(to) %>% summarise(n = n(), s = sum(value))
)

x <- 1:20

y <- tibble(
  x = x,
  y = paste0("L",x),
  v = runif(length(x), 1,2)
)

y %>% spread(y, y)

y$y[order(rev(y$x))]

tc_po
