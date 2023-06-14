install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")
install.packages("e1071")
install.packages("randomForest")
install.packages("cluster")
install.packages("rpart.plot")
install.packages("rpart")
library(rpart)
library(rpart.plot)
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(cluster)

# Wczytaj dane
df <- read.csv2('spotify.csv')

# Sprawdź kompletność danych
cat("Liczba brakujących danych w każdej kolumnie:\n")
print(colSums(is.na(df)))

# Przekształć dane do odpowiedniego formatu czyli data.frame
df <- as.data.frame(df)

#Opisz dane
colnames(df)
length(colnames(df))
#Nasz zbiór danych składa się z 14 kolumn
#title - tytuł utworu
#artist - wykonawca utworu
#top_genre - gatunek utworu
#year - rok wydania utworu
#bpm - tempo utworu
#energy - pokazuje poziom "energii" utworu w skali 0-100 przy czym im wartość jest większa tym utwór jest bardziej energiczny.
#danceability - taneczność utworu w skali 0-100 przy czym im wartość jest wyższa tym piosenka jest bardziej przystępna do tańczenia
#dB - poziom głośności utworu określony w Decybelach
#liveness - określa czy utwór jest nagraniem na żywo w skali 0-100 przy czym im wartość jest większa tym większe prawdopodobieństwo, że utwór jest np. nagraniem z kocnertu.
#valence - określa jak bardzo dany utwór jest "wesoły" w skali 0-100 przy czym im wartość jest większa tym utwór jest bardziej wesoły
#duration - czas trwania utworu określony w sekundach
#acousticness - określa akustyke utworu w skali 0-100 przy czym im wartość jest większa tym utwór jest bardziej akustyczny i ma mniej wokalu.
#speechiness - określa ile śpiewu jest w piosence w skali 0-100 przy czym im wartość jest większa tym utwór zawiera więcej wokalu
#popularity - określa jak bardzo dany utwó jest popularny na platformie Spotify przy czym im wartość jest większa tym utwór jest tam bardziej popularny

# Opisz dane za pomocą charakterystyk statystycznych
stats <- summary(df)
cat("Podstawowe statystyki:\n", stats)

# Stwórz histogram dla kolumny 'popularity'
histogram <- ggplot(df, aes(x=popularity)) + 
  geom_histogram(binwidth=5, fill="aquamarine3", color="azure", alpha=0.9) +
  theme_minimal() +
  ggtitle("Histogram dla 'Popularity'") +
  xlab("Popularity")
histogram


# Stwórz wykres skrzypcowy dla 'energy' w zależności od 'year' (rok wydania)
violin <- ggplot(df, aes(x=year, y=energy)) +
  geom_violin(trim=FALSE, fill="cornflowerblue", color="azure") +
  theme_minimal() +
  ggtitle("Rozkład 'Energy' w zależności od 'Year'") +
  xlab("Year") +
  ylab("Energy")
violin

# Stwórz Wykres rozproszenia dla 'energy' vs 'danceability' z 'popularity' jako kolor
point <-  ggplot(df, aes(x=energy, y=danceability, color=popularity)) +
  geom_point(alpha=0.6) +
  scale_color_gradient(low = "green", high = "red") +
  theme_minimal() +
  ggtitle("Rozproszenie dla 'Energy' vs 'Danceability'") +
  xlab("Energy") +
  ylab("Danceability")
point

# Stwórz wykres skrzypcowy dla 'bpm' w zależności od 'year'
violin2 <- ggplot(df, aes(x=year, y=bpm)) +
  geom_violin(trim=FALSE, fill="darkgreen", color="grey") +
  theme_minimal() +
  ggtitle("Rozkład 'BPM' w zależności od 'Year'") +
  xlab("Year") +
  ylab("BPM")
violin2

#ZADANIE 4 i 5
#Opisz cel badań: Celem badania jest sprawdzenie 5 najpopularniejszych gatunków z badanego okresu. 



#Opisz metody (metodę) użytą do analizy - część teoretyczna

#Do analizy tego celu użyliśmy przede wszystkim analizy eksploracyjnej danych (EDA - Exploratory Data Analysis) oraz metody wizualizacji danych.
#EDA to proces analizy danych, który zwykle obejmuje sumaryczne statystyki, wykresy i inne techniki w celu zrozumienia charakterystyki danych, znalezienia wzorców, relacji lub wykrycia nietypowych obserwacji.
#W przypadku naszego celu, użyliśmy pakiety 'dplyr' i zaczęliśmy od grupowania danych na podstawie roku i gatunku muzycznego, a następnie obliczenia średniej popularności dla każdej grupy (avg_popularity). 

#Wizualizacja danych to bardzo pomocne narzędzie ułatwiające zrozumienie wzorców i trendów w zbiroach danych. 
#Za pomocą pakietu 'ggplot2' stworzyliśmy wykres liniowy (popularity_over_years) z rokiem na osi x i średnią popularnością na osi y dla różnych gatunków muzycznych.

#Wyciągnij wnioski, przeanalizuj osiągnięty cel.
#Wizualizacja przedstawia nam, które gatunki okazały się być najpopularniejsze, co pozwala nam na lepsze zrozumienie trendów w preferencjach muzycznych. 
#Najpopularniejszym gatunkami jest dance pop i pop, zaraz po nim alt hip hop, a na końcu znajduję się alt metal i edm.




# Przetwarzanie danych
df_top_genres <- df %>%
  group_by(top_genre) %>%
  summarise(total_popularity = sum(popularity, na.rm = TRUE)) %>%
  arrange(desc(total_popularity)) %>%
  top_n(5)
df_top_genres

# Wybór tylko pięciu najpopularniejszych gatunków z oryginalnego zestawu danych
df_filtered <- df %>% 
  filter(top_genre %in% df_top_genres$top_genre)
df_filtered

# Wykres
df_filtered %>%
  ggplot(aes(x = top_genre, y = popularity, fill = top_genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Popularność top 5 gatunków na Spotify", x = "Gatunek", y = "Popularność", fill = "Gatunek") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#badanie nr 2


#Cel badań: Głównym celem jest zbadanie i zrozumienie naturalnych grupowań piosenek na podstawie dwóch atrybutów - energii i taneczności.

#Opisz metody (metodę) użytą do analizy - część teoretyczna: użyliśmy techniki grupowania zwanej k-średnich na zestawie danych . K-średnie to metoda, która dzieli dane na k grup (klusterów) na podstawie atrybutów tych danych. W tym przypadku, używamy atrybutów "energy" i "danceability" piosenek.

#Wyciągnij wnioski, przeanalizuj osiągnięty cel.
#Zastosowanie tego kodu pozwaliło nam na identyfikację grup piosenek o podobnych atrybutach energii i taneczności.
#Jesteśmy w stanie zidentyfikować te grupy na podstawie kolorów na wykresie. Każda grupa reprezentuje piosenki o podobnej energii i taneczności.
#Przykładowo zielona grupa reprezentuje piosenki o wysokiej energii co jak widać przekłada się na ich taneczność

# Wybór numerycznych atrybutów
spotify_numeric <- df[, c("energy", "danceability")]

# Grupowanie
k <- 3
set.seed(123)
kmeans_fit <- kmeans(spotify_numeric, centers = k, nstart = 10)

# Stworzenie nowego obiektu z przypisanymi klastrami
spotify_clustered <- data.frame(df, cluster = as.factor(kmeans_fit$cluster))

# Wykres grup
ggplot(spotify_clustered, aes(x = energy, y = danceability, color = cluster)) +
  geom_point() +
  ggtitle("Grupowanie k-średnich dla danych Spotify") +
  xlab("Energia") +
  ylab("Taneczność") +
  theme(plot.title = element_text(hjust = 0.5))

# Macierz błędów (tablica pomyłek)
table(data$popularity, spotify_clustered$cluster)

#ZADANIE 7 p.1
# Stworzenie sztucznej bazy danych o utworach muzycznych
set.seed(123)
df_test <- data.frame(
  track_id = 1:100,
  energy = runif(100, min = 0, max = 1),
  danceability = runif(100, min = 0, max = 1),
  popularity = sample(1:100, 100, replace = TRUE)
)

# Wybór numerycznych atrybutów
spotify_numeric <- df_test[, c("energy", "danceability")]

# Grupowanie
k <- 3
set.seed(123)
kmeans_fit <- kmeans(spotify_numeric, centers = k, nstart = 10)

# Stworzenie nowego obiektu z przypisanymi klastrami
spotify_clustered_1 <- data.frame(df_test, cluster = as.factor(kmeans_fit$cluster))

# Wykres grup
library(ggplot2)
ggplot(spotify_clustered_1, aes(x = energy, y = danceability, color = cluster)) +
  geom_point() +
  ggtitle("Grupowanie k-średnich dla sztucznych danych Spotify") +
  xlab("Energia") +
  ylab("Taneczność") +
  theme(plot.title = element_text(hjust = 0.5))

# Macierz błędów (tablica pomyłek)
table(spotify_clustered$popularity, spotify_clustered$cluster)

#Powyższy kod generuje 100 sztucznych utworów muzycznych, z losowo wybranymi wartościami dla energii, taneczności i popularności. 
#Następnie wykonuje grupowanie k-średnich na atrybutach energii i taneczności. Wyniki grupowania są wizualizowane na wykresie. 
#Macierz błędów pokazuje, jak dobrze klastry przewidują popularność utworu.

#badanie nr 3

#Cel badań: Głównym celem jest zbadanie i zrozumienie, jak energia utworu wpływa na jego popularność. 
#Chcemy zbudować model, który może przewidzieć popularność utworu na podstawie jego energii.

#Opisz metody (metodę) użytą do analizy - część teoretyczna: Użyliśmy techniki modelowania drzewa decyzyjnego do analizy wpływu energii utworu na jego popularność w bazie danych Spotify.

#Wyciągnij wnioski, przeanalizuj osiągnięty cel.
#Na podstawie wyników modelu drzewa decyzyjnego, jesteśmy w stanie wyciągnąć wnioski o zależności między energią utworu a jego popularnością. 
#Na przykład, jeżeli drzewo decyzyjne wskazuje, że utwory o wysokiej energii są bardziej popularne, można to interpretować jako preferencje słuchaczy dla utworów o wysokiej energii.
#Wizualizacja drzewa decyzyjnego również dostarcza ważnych informacji. Każde rozgałęzienie w drzewie reprezentuje decyzję opartą na atrybucie energii, a liście reprezentują prognozowaną popularność.
#Dzięki temu możemy zrozumieć, jak różne poziomy energii mogą wpływać na popularność utworu.
#Analizując wyniki, jesteśmy w stanie wywnioskować, czy energia jest istotnym predyktorem popularności utworów muzycznych, co mogłoby mieć praktyczne zastosowanie, na przykład w rekomendacji utworów o potencjalnie wysokiej popularności.

# Usunięcie potencjalnych braków danych
data <- na.omit(df)

# Przydzielenie danych na objaśniane i objaśniające
response_var <- "popularity"
predictor_vars <- "energy"

# Zbiór testowy i treningowy
set.seed(1)
train_indices <- sample(1:nrow(data), 1 * nrow(data))  
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
regression_tree <- rpart(
  formula = as.formula(paste(response_var, "~", paste(predictor_vars, collapse = "+"))),
  data = train_data,
  method = "anova",
  control = rpart.control(cp = 0.001)
)

cp_values <- regression_tree$cptable[, "CP"]
optimal_cp <- cp_values[which.min(regression_tree$cptable[, "xerror"])]

# Przytnięcie drzewa regresyjnego z optymalnym parametrem złożoności
pruned_tree <- prune(regression_tree, cp = optimal_cp)

# Wizualizacja drzewa regresyjnego
rpart.plot(pruned_tree, box.palette = "Greys", shadow.col = "pink")

#badanie nr 4

#Cel badań: zbadanie zależności między poziomem taneczności ("danceability") a popularnością utworów muzycznych.

#Opisz metody (metodę) użytą do analizy - część teoretyczna: Pierwsza część kodu oblicza średnią popularność dla różnych poziomów taneczności. 
#Druga część kodu przeprowadza test t-studenta, aby porównać średnią popularność między dwoma grupami utworów: tymi o wysokiej taneczności (powyżej 75) i tymi o niskiej taneczności (poniżej 25). 

#Wyciągnij wnioski, przeanalizuj osiągnięty cel.
#Wynik testu t-studenta nie jest istotny (p-value powyżej 0.05), sugeruje to, że nie ma istotnej różnicy między średnią popularnością utworów o wysokiej i niskiej taneczności. 
#To może wskazywać, że inne czynniki mogą mieć większy wpływ na popularność utworu.
#Analiza dostarcyła nam cennych informacji do zrozumienia, jakie cechy utworu muzycznego mogą wpływać na jego popularność.

# Średnia popularność dla różnych poziomów energii
aggregate(popularity ~ danceability, data = data, FUN = mean)

# Testowanie różnicy średniej popularności w dwóch grupach 
# (w naszym przypadku dla utworów o taneczności <25 i >75)
group1 <- data$popularity[data$danceability > 75]
group2 <- data$popularity[data$danceability < 25]
t.test(group1, group2)

#ZADANIE 7 cz.2

# Stworzenie sztucznej bazy danych o utworach muzycznych
set.seed(123)
df_1 <- data.frame(
  track_id = 1:1000,
  danceability = runif(1000, min = 0, max = 100),
  popularity = sample(1:100, 1000, replace = TRUE)
)

# Usunięcie potencjalnych braków danych
data <- na.omit(df_1)

# Średnia popularność dla różnych poziomów taneczności
aggregated_popularity <- aggregate(popularity ~ danceability, data = data, FUN = mean)
print(aggregated_popularity)

# Testowanie różnicy średniej popularności w dwóch grupach 
# (w naszym przypadku dla utworów o taneczności <25 i >75)
group1 <- data$popularity[data$danceability > 75]
group2 <- data$popularity[data$danceability < 25]
t_test_result <- t.test(group1, group2)
print(t_test_result)

#Interpretacja wyniku testowego
#Wynik testu t-studenta nie jest istotny (p-value powyżej 0.05), sugeruje to, że nie ma istotnej różnicy między średnią popularnością utworów o wysokiej i niskiej taneczności. 
#To może wskazywać, że inne czynniki mogą mieć większy wpływ na popularność utworu.















