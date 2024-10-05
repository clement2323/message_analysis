library(ggplot2)
library(dplyr)

# Définir la graine pour la reproductibilité
set.seed(42)

# Fonction pour générer une sinusoïde aléatoire avec des paramètres fixes, un déphasage et une décroissance
random_sine <- function(x) {
  a <- runif(1, 0, 1)  # Amplitude aléatoire entre 0 et 1
  k <- sample(0:5, 1)  # Fréquence aléatoire entre 0 et 5
  phase <- runif(1, 0, 2*pi)  # Déphasage aléatoire entre 0 et 2π
  decay_rate <- runif(1, 0.5, 2)  # Taux de décroissance aléatoire
  function(x) a * sin(k * x + phase) * exp(-decay_rate * x / (2*pi))
}

# Générer les données
x <- seq(0, 2*pi, length.out = 10000)
n_waves <- 100  # Nombre de sinusoïdes à additionner

data <- data.frame(x = x)
for (i in 1:n_waves) {
  wave_function <- random_sine()
  data[paste0("wave", i)] <- sapply(x, wave_function)
}

# Calculer la somme des vagues
data$total <- rowSums(data[, grep("wave", names(data))])

# Préparer les données pour ggplot
plot_data <- data %>%
  tidyr::pivot_longer(cols = starts_with("wave") | matches("total"),
                      names_to = "wave", values_to = "y")

# Grouper par x et calculer la somme pour le graphe global
global_data <- plot_data %>%
  group_by(x) %>%
  summarise(y = sum(y))

# Créer le graphique

# Créer le graphique
p <- ggplot() +
  geom_line(data = plot_data %>% filter(wave != "total"), aes(x, y, color = wave), alpha = 0.3) +
  geom_line(data = global_data, aes(x, y), color = "red", size = 1) +
  labs(title = "La Grande Vague Sinusoïdale Décroissante (100 vagues)",
       x = "Temps",
       y = "Amplitude") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(plot_data$x), by = 0.5))  # Plus de valeurs sur l'axe x

# Sauvegarder le graphique en PNG
ggsave("grande_vague_sinusoidale_decroissante_100.png", plot = p, width = 12, height = 8, dpi = 300)


# plot moi un sinus simple avec une amplitude  2 de frequence 1/2 


# Fonction pour générer un sinus simple
simple_sine <- function(x, amplitude = 1, frequency = 1, phase = 0) {
  amplitude * sin(2 * pi * frequency * x + phase)
}

# Générer les données pour le sinus simple
x_simple <- seq(0, 4*pi, length.out = 1000)
y_simple <- simple_sine(x_simple, amplitude = 2, frequency = 1/2)
y_simple2 <- simple_sine(x_simple, amplitude = 2, frequency = 1/4)
y_simple3 <- y_simple + y_simple2
# Ajouter le quatrième sinus avec un déphasage
d <- pi/4  # Vous pouvez ajuster cette valeur selon le déphasage souhaité
y_simple4 <- simple_sine(x_simple, amplitude = 1.5, frequency = 1/4, phase = d)

# Créer le graphique avec les quatre sinus
p_simple <- ggplot(data.frame(x = x_simple, y = y_simple, y2 = y_simple2, y3 = y_simple3, y4 = y_simple4), aes(x)) +
  #geom_line(aes(y = y), color = "blue", size = 1) +
  #geom_line(aes(y = y2), color = "red", size = 1) +
  geom_line(aes(y = y3), color = "#ff00b7", size = 1) +
  #geom_line(aes(y = y4), color = "#f02050", size = 1) +
  labs(title = "Sinus Simples",
       subtitle = "Bleu: A=2, F=1/2 | Rouge: A=2, F=1/6 | Vert: A=1, F=1/6 | Violet: A=1.5, F=1/4, Phase=π/4",
       x = "Temps",
       y = "Amplitude") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(x_simple), by = pi/2),
                     labels = c("0", "π/2", "π", "3π/2", "2π", "5π/2", "3π", "7π/2", "4π")) +
  coord_cartesian(ylim = c(-2.5, 2.5))

p_simple 
