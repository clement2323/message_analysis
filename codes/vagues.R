library(ggplot2)
library(dplyr)
library(gganimate)
library(plotly)

# Définir la graine pour la reproductibilité
set.seed(42)

# Fonction pour générer une sinusoïde aléatoire avec des paramètres fixes, un déphasage et une décroissance
random_sine <- function(x, storm_intensity) {
  a <- runif(1, 0, 1) * storm_intensity  # Amplitude aléatoire entre 0 et storm_intensity
  k <- sample(0:5, 1)  # Fréquence aléatoire entre 0 et 5
  phase <- runif(1, 0, 2*pi)  # Déphasage aléatoire entre 0 et 2π
  decay_rate <- runif(1, 0.5, 2)  # Taux de décroissance aléatoire
  function(x) a * sin(k * x + phase) * exp(-decay_rate * x / (2*pi))
}

# Définir l'intensité de la tempête
storm_intensity <- 2 # Ajustez cette valeur pour augmenter ou diminuer l'intensité globale

# Générer les données
x <- seq(0, 8*pi, length.out = 3000)
n_waves <- 100  # Nombre de sinusoïdes à additionner

data <- data.frame(x = x)
for (i in 1:n_waves) {
  wave_function <- random_sine(storm_intensity = storm_intensity)
  data[paste0("wave", i)] <- sapply(x, wave_function)
}

# Calculer la somme des vagues
data$total <- rowSums(data[, grep("wave", names(data))])

# Modifier la cration des données pour s'assurer qu'elles ne sont pas vides
plot_data <- data %>%
  tidyr::pivot_longer(cols = starts_with("wave") | matches("total"),
                      names_to = "wave", values_to = "y") %>%
  filter(!is.na(y))  # Ajouter cette ligne pour filtrer les valeurs NA

# Vérifier si les données sont vides
if (nrow(plot_data) == 0) {
  stop("Les données sont vides. Vérifiez la génération des données.")
}

# Grouper par x et calculer la somme pour le graphe global
global_data <- plot_data %>%
  group_by(x) %>%
  summarise(y = sum(y, na.rm = TRUE))  # Ajouter na.rm = TRUE

# Créer le graphique
p <- ggplot() +
  geom_line(data = plot_data %>% filter(wave != "total"), 
            aes(x, y, group = wave, color = wave), 
            alpha = 0.3, show.legend = FALSE) +
  geom_line(data = global_data, aes(x, y), color = "red", size = 1) +
  labs(title = "La Danse Folle des Vagues Déchaînées",
       x = "Distance à l'épicentre de la tempête",
       y = "Amplitude") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(plot_data$x), by = 0.5)) +
  theme(legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )  # Supprimer la légende

# Afficher le graphique
print(p)

# Sauvegarder le graphique en PNG
ggsave("grande_vague_sinusoidale_decroissante_100.png", plot = p, width = 12, height = 8, dpi = 300)



# Fonction pour générer le profil de profondeur
generate_depth_profile <- function(x) {
  # Fonction modifiée pour représenter un fond marin qui monte vers la droite
  return(10 - 8 * (x / max(x))^1.5)
}

# Fonction pour simuler l'effet de la profondeur sur l'amplitude de la vague
wave_amplitude_modifier <- function(x, depth) {
  return(1 / sqrt(depth + 0.5))  # Ajout d'une petite constante pour éviter la division par zéro
}

# Générer les données de profondeur
depth_data <- data.frame(
  x = x,
  depth = generate_depth_profile(x)
)
n_frames <- 20

# Définir wave_data
wave_data <- data.frame(
  x = rep(x, n_frames),
  frame = rep(1:n_frames, each = length(x)),
  depth = rep(generate_depth_profile(x), n_frames)
)

# Simuler le mouvement de la vague
wave_data <- wave_data %>%
  group_by(frame) %>%
  mutate(
    wave = 2 * sin(x - frame/5) * wave_amplitude_modifier(x, depth),
    y = wave + depth,
    breaking = ifelse(abs(wave) > depth * 0., 1, 0)
  ) %>%
  ungroup()



# Générer des points pour représenter l'explosion au point de déferlement
breaking_points <- wave_data %>%
  filter(breaking == 1) %>%
  group_by(x, frame) %>%
  do(data.frame(
    x = .$x + runif(50, -0.2, 0.2),
    y = .$y + runif(50, 0, 1),
    frame = .$frame
  )) %>%
  ungroup()

# Create the base plot
p <- ggplot() +
  geom_line(data = wave_data, aes(x = x, y = y)) +
  geom_line(data = wave_data, aes(x = x, y = depth), color = "blue", linetype = "dashed") +
  geom_point(data = breaking_points, aes(x = x, y = y), color = "red", alpha = 0.5, size = 0.5) +
  labs(title = "Animation de la Vague Déferlante",
       x = "Distance",
       y = "Amplitude") +
  coord_cartesian(ylim = c(0, 12))

# Add animation
animated_plot <- p +
  transition_time(frame) +
  ease_aes('linear') +
  labs(subtitle = "Time: {frame_time}")

# Render the animation
anim <- animate(animated_plot, nframes = 100, fps = 10, renderer = gifski_renderer())

# Save the animation as a GIF
anim_save("animated_plot.gif", animation = anim)

# Fonction pour générer une courbe paramétrique représentant la résultante des vagues
parametric_wave <- function(t, storm_intensity) {
  x <- (1 + 0.2 * sin(5*t)) * cos(t)
  y <- (1 + 0.2 * sin(5*t)) * sin(t)
  amplitude <- storm_intensity * exp(-2*t/pi) * (1 + 0.5 * sin(10*t))
  return(data.frame(x = x * amplitude, y = y * amplitude, t = t))
}

# Générer les données pour la courbe paramétrique
t <- seq(0, 4*pi, length.out = 1000)
storm_intensity <- 2
parametric_data <- parametric_wave(t, storm_intensity)

# Créer le graphique
p <- ggplot() +
  # Fond dégradé du bleu au jaune
  geom_point(data = expand.grid(x = seq(-3, 3, length.out = 100), 
                                  y = seq(-3, 3, length.out = 100)),
             aes(x, y, color = sqrt(x^2 + y^2)), alpha = 0.3) +
  scale_color_gradient(low = "blue", high = "yellow", guide = "none") +
  
  # Courbe paramétrique
  geom_path(data = parametric_data, aes(x, y), color = "white", size = 1)

print(p)
ggsave("vague_parametrique.png", plot = p, width = 12, height = 8, dpi = 300)

# Fonction pour générer une surface paramétrique représentant les vagues
parametric_wave_surface <- function(u, v, storm_intensity) {
  x <- u * cos(v)
  y <- u * sin(v)
  z <- storm_intensity * exp(-u) * (sin(5*v) + 0.5*cos(3*u))
  list(x = x, y = y, z = z)
}


# Générer les données pour la surface paramétrique
u <- seq(0, 2, length.out = 100)  # Distance radiale
v <- seq(0, 2*pi, length.out = 100)  # Angle
storm_intensity <- 2

surface_data <- expand.grid(u = u, v = v)
wave_surface <- with(surface_data,
  parametric_wave_surface(u, v, storm_intensity))

# Créer le graphique en 3D
p <- plot_ly() %>%
  add_surface(x = ~surface_data$u, y = ~surface_data$v, z = ~wave_surface$z, colorscale = "Viridis", opacity = 0.5)

print(p)

# Générer les données
set.seed(123)  # Pour la reproductibilité
data <- expand.grid(x = seq(0, 2*pi, length.out = 200),
                    curve = 1:4) %>%
  mutate(y = sin(x + runif(1, 0, 2*pi)) * runif(1, 0.5, 2))

# Créer le graphique
p <- ggplot(data, aes(x = x, y = y, color = factor(curve))) +
  geom_line(size = 1) +
  scale_color_viridis_d(name = "Courbe") +
  theme_minimal() +
  labs(title = "Quatuor de Sinusoïdes",
       subtitle = "Des sinusoïdes qui dansent!",
       x = "Phase", 
       y = "Amplitude") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 14),
        legend.position = "bottom")

# Afficher le graphique
print(p)

# Sauvegarder le graphique (optionnel)
# ggsave("sinusoides_aleatoires.png", p, width = 10, height = 6, dpi = 300)
