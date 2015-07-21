# pakiet z danymi
library(PogromcyDanych)

# wielkość danych i kilka pierwszych wierszy
dim(serialeIMDB)
head(serialeIMDB)

# wartości zmiennych jakościowych
levels(serialeIMDB$serial)

# jak wyznaczyć tabelę liczebności i ją posortować
tabela <- table(serialeIMDB$serial)
sort(tabela, decreasing = TRUE)

# wybieramy tylko dane dotyczące serialu Breaking Bad
BreakingBad <- filter(serialeIMDB, serial == "Breaking Bad")
# o ilu odcinkach dostępna jest informacja dla tego serialu?
dim(BreakingBad)

# różne sposoby oglądania informacji o odcinkach
BreakingBad$ocena
barplot(BreakingBad$ocena)
plot(BreakingBad$ocena)

# sortowanie danych
arrange(BreakingBad, ocena)

# podsumowanie danych o ocenach
summary(BreakingBad$ocena)

# wykres pudełkowy
boxplot(BreakingBad$ocena )

# wykres pudełkowy dla grup
BreakingBad$sezon <- droplevels(BreakingBad$sezon)
boxplot(BreakingBad$ocena ~ BreakingBad$sezon)

# histogram a więc pełna informacja o rozkładzie
hist(BreakingBad$ocena, 10, col="grey")

# kiedy histogram zdradza dodatkowe informacje
TheShield <- filter(serialeIMDB, serial == "The Shield")
hist(TheShield$ocena, 10, col="grey")
boxplot(TheShield$ocena, col="grey", range = 10)


## REGRESJA:

library(PogromcyDanych)
head(galton)

ggplot(galton, aes(x=rodzic, y=syn))+
   geom_point(position = "jitter")

galton %>% group_by(rodzic) %>%
   summarise(srednia = mean(syn)) -> srednie

ggplot(galton, aes(x=rodzic, y=syn))+
   geom_point(position = "jitter") +
   geom_abline(slope =1)+
   geom_line(data = srednie, aes(x=rodzic, y=srednia), size = 2, colour = "blue")+
   geom_point(data = srednie, aes(x=rodzic, y=srednia), size = 6, colour = "blue")+
   # model 3, do danych dopasowujemy trend liniowy
  geom_smooth(method="lm", se=FALSE, size=3, color="red") +
  # model 2, wzost dziecka jest taki jak wzrost rodzica
  geom_abline(size=2, color="gold3")+
  # model 1, wzrost dziecka nei zależy od wzrostu rodzica
  geom_abline(size=2, slope=0, intercept=mean(galton$syn))

galton %>% group_by(rodzic) %>% summarise(avg_diff = mean(rodzic-syn))

model <- lm(syn~rodzic, data = galton)
model$coefficients[2]

galton %>% group_by(rodzic) %>% summarise(true_mean = mean(syn)) %>%
  mutate(est_mean = model$coefficients[1] + model$coefficients[2] * rodzic) %>%
  mutate(diff = 100*abs(true_mean-est_mean)/true_mean)

head(pearson)

model_p <- lm(syn~ojciec, data = pearson)

pearson %>% ggplot(aes(x=ojciec, y=syn)) +
  geom_smooth(method="lm",size = 2, color = "red", se=FALSE) +
  geom_point()
  # geom_point(position = "jitter")
