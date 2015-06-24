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

## Dodanie szczegółów:

ggplot(koty_ptaki, aes(x=waga, y=predkosc, shape=druzyna)) +
  geom_point(size=5) + 
  # większy i czytelny tytuł
  ggtitle("Lżejszym łatwiej szybko biegać!") + theme(plot.title = element_text(size=20)) + 
  # opisy osi
  xlab("Waga [kg]") + ylab("Prędkość [km/h]") + 
  # tytuł w legendzie
  scale_shape_discrete(name="Koty czy Ptaki?") + theme(legend.position="top") + 
  # dodatkowe napisy na wykresie
  geom_text(data=koty_ptaki[c(6,8),], aes(label=gatunek), hjust=-0.2)

ggplot(koty_ptaki, aes(x=waga, y=predkosc, shape=druzyna, color=druzyna)) +
  geom_point(size=5) +
  # określamy kształty poszczególnych grup K/P oraz etykiety w legndzie
  scale_shape_manual(values=c("K", "P"), breaks=c("Kot", "Ptak")) +
  # określamy kolory (ciemnoniebieski i ciemnoczerwony)
  scale_color_manual(values=c("blue3", "red3"), breaks=c("Kot", "Ptak")) +
  # dla osi OY usuwany marginesy poza zakresem 0-200
  scale_y_continuous(limits=c(0,200), expand=c(0,0)) 

ggplot(koty_ptaki, aes(x=waga, y=predkosc, size=zywotnosc, color=zywotnosc)) +
  geom_point() +
  # zmiana zakresu wartości dla wielkosci punktów
  scale_size_continuous(range=c(5,15), limits=c(10,50)) +
  # zmiana skali kolorów (ciągła skala, podaje się dwa skrajne kolory)
  scale_color_gradient(low="gold", high="red4")

ggplot(koty_ptaki, aes(x=waga, y=predkosc, shape=druzyna)) +
  geom_point(size=5) +
  # zmiana napisów w legendzie dotyczących mapowania na kształt
  scale_shape_manual(values=c("K", "P"), 
                     labels=c("Duże Koty", "Wściekłe Ptaki"), 
                     name="Jakie \n to \n zwierze?") +
  # dodanie szarego tła, większego tytułu oraz zmiana położenia legendy
  theme(legend.background = element_rect(color="red", fill="grey95"), 
        legend.title = element_text(size=14), 
        legend.position=c(0.8,0.8))

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

### BOXPLOT

skody$Model <- reorder(skody$Model, skody$Cena.w.PLN, median)
ggplot(skody, aes(x=Model, y=Cena.w.PLN, fill = Model)) +
  geom_boxplot()

### HISTOGRAM

ggplot(skody, aes(x=Cena.w.PLN, fill = Model)) +
  geom_histogram(color = 'white')

### BAR PLOT

counts <- skody %>% group_by(Model) %>% summarise(count = n())

ggplot(skody, aes(x=Model, fill = Rodzaj.paliwa)) +
  geom_bar()+
  geom_text(data = counts, aes(x = Model, y=count, label = count, vjust = -0.2, fill = NULL))



ggplot(skody, aes(x=Model, fill = Rodzaj.paliwa)) +
  geom_bar(position = "fill")

### WIELE WYKRESÓW:

library(grid)
# rysujemy wykres w kwadracie o szerokości i wysokości 0.5 ekranu, 
# w lewym górnym rogu 
pl_waga <- ggplot(koty_ptaki, aes(x=waga, y=predkosc)) +
  geom_point(size=5)

#
# x i y określają położenie środa zarysowanego obszaru!!
#

print(pl_waga + ggtitle("Wykres 1"), 
      vp=viewport(x=0.25, y = 0.75, width=0.5, height=0.5))
# rysujemy wykres w kwadracie o szerokości i wysokości 0.5 ekranu, 
# w lewym dolnym rogu 
print(pl_waga + ggtitle("Wykres 2"), 
      vp=viewport(x=0.25, y = 0.25, width=0.5, height=0.5))
# rysujemy wykres w prostokącie o szerokości 0.5 a wysokości pełnego ekranu, 
# po prawej stronie 
print(pl_waga + ggtitle("Wykres 3"), 
      vp=viewport(x=0.75, y = 0.5, width=0.5, height=1))

## LAYOUT:

pushViewport(viewport(layout = grid.layout(2,2)))
# rysujemy w komórce o współrzędnych 1x1
print(pl_waga + ggtitle("Wykres 1"),
      vp = viewport(layout.pos.row=1, layout.pos.col=1))
# rysujemy w komórce o współrzędnych 2x1
print(pl_waga + ggtitle("Wykres 2"),
      vp = viewport(layout.pos.row=2, layout.pos.col=1))
# rysujemy w komórce o współrzędnych 1:2x2
print(pl_waga + ggtitle("Wykres 3"),
      vp = viewport(layout.pos.row=1:2, layout.pos.col=2))

# install.packages("gridExtra")
library(gridExtra)
grid.arrange( pl_waga + ggtitle("Wykres 1"), 
              pl_waga + ggtitle("Wykres 2"), 
              pl_waga + ggtitle("Wykres 3"),
              ncol=2)

## THEMES:

pl <- ggplot(koty_ptaki, aes(x=waga, y=predkosc, shape=druzyna)) +
  geom_point(size=5) 

# install.packages("ggthemes")
library(ggthemes)

# białe tło i szare linie pomocnicze
pl + theme_bw()
# wykres stylizowany na wykresy z Excela
pl + theme_excel()
# wykres z usuniętymi dodatkowymi elementami
pl + theme_minimal()
# wykres stylizowany na gazecie Economist
pl + theme_economist()

grid.arrange( pl, 
              pl + theme_economist_white(), 
              pl + theme_minimal(),
              pl + theme_economist(),
              ncol=2)
