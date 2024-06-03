## Let's do a quick check of the distribution of our data
## First, the raw continuous data:
p1 <- ggplot(mydata, aes(x = eyesize)) +
  geom_histogram(bins = 20, fill = "turquoise4") +
  theme_bw(base_size = 14)
p1 

## And now with it log-tranformed:
p2 <- ggplot(mydata, aes(x = log(eyesize))) +
  geom_histogram(bins = 20, fill = "chartreuse4") +
  theme_bw(base_size = 14)
p2

## _______________________________________________
## Q: Why might we log-transform continuous data?
## _______________________________________________