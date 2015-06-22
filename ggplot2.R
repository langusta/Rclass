#
# ggplot2
#

# install.packages("PogromcyDanych")

library(PogromcyDanych)

# Links:
#     http://docs.ggplot2.org/current/
#     http://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

ggplot(koty_ptaki, aes(x=waga, y=predkosc, shape =  druzyna, label = gatunek)) +
geom_point(size=3) +
geom_text(hjust=0, vjust=-0.4) + xlim(0,350) + ylim(0,200)

ggplot(koty_ptaki, aes(x=waga, y=predkosc, shape =  druzyna, label = gatunek)) +
   geom_point(size=3, aes(color = druzyna)) +
   geom_text(hjust=0, vjust=-0.4) + xlim(0,350) + ylim(0,200)

ggplot(WIG, aes(x=Data, y=Kurs.zamkniecia))+
   geom_line()

ggplot(WIG, aes(x=Data, y=Kurs.zamkniecia))+
   geom_point()

ggplot(WIG, aes(x=Data, ymin=Kurs.otwarcia, ymax=Kurs.zamkniecia))+
   geom_ribbon()

ggplot(WIG, aes(x=Data, ymin=Kurs.minimalny, ymax=Kurs.maksymalny))+
   geom_ribbon()+
   geom_line(aes(x=Data, y=Kurs.zamkniecia, color = "red"))


## Zadanie:
max_z <- max(koty_ptaki$zywotnosc)
min_z <- min(koty_ptaki$zywotnosc)
ggplot(cbind(koty_ptaki, ll= ifelse(koty_ptaki$zywotnosc == max_z, "max", ifelse(koty_ptaki$zywotnosc == min_z, "min", ""))),
   aes(x=zywotnosc, y=waga, label = ll))+
   geom_point(aes(color = druzyna))+
   geom_text(hjust =-0.2) + xlim(0,56)

ggplot(koty_ptaki,aes(x=zywotnosc, y=waga, label = gatunek))+
   geom_point(aes(color = druzyna))+
   geom_text(hjust =-0.2, size = 3) + xlim(0,56)


### AGREGATY:

skody <- auta2012 %>%
              filter(Marka == 'Skoda', Model == 'Octavia') %>%
              select(Marka, Model, Rok.produkcji, Cena.w.PLN, Rodzaj.paliwa)
head(skody)

ggplot(skody, aes(x=Rok.produkcji, y = Cena.w.PLN)) +
  geom_point()

ggplot(skody, aes(x=Rok.produkcji, y = Cena.w.PLN)) +
   geom_point(size = 1) +
  geom_smooth(se=FALSE, size = 1.5)


ggplot(skody, aes(x=Rok.produkcji, y = Cena.w.PLN)) +
   geom_point(size = 1) +
   geom_smooth(se=FALSE, size = 1.5, method = "lm", formula = y~poly(x,2))

ggplot(skody, aes(x=Rok.produkcji, y = Cena.w.PLN)) +
   geom_point(size = 1) +
   geom_smooth(se=FALSE, size = 1.5)+
   coord_trans(y="log10")


skody <- auta2012 %>%
              filter(Marka == 'Skoda', Rok.produkcji==2007) %>%
              select(Marka, Model, Rok.produkcji, Cena.w.PLN, Rodzaj.paliwa)

ggplot(skody, aes(x=Model, y=Cena.w.PLN)) +
   geom_point()
