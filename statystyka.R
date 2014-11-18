
####  STATYSTYKA

## Dystrybuanta empiryczna

tmp <- rnorm(150, 3500, 200)
# Dystrybuanta empiryczna;
plot.ecdf(tmp)
# "jednowymiarowa gęstość"
rug(tmp)

boxplot(tmp, horizontal = TRUE)
summary (tmp)
