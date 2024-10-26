library(tidyverse)
library(gganimate)
library(progress)

# Définir la classe Voiture
Voiture <- setRefClass("Voiture",
  fields = list(
    id = "numeric",
    rapide = "logical",
    depasseur = "logical",
    resistant = "logical",
    position = "numeric",
    voie = "numeric",
    vitesse = "numeric",
    vitesse_base = "numeric",
    temps_voie_gauche = "numeric",
    etat_depassement = "numeric"  # 0: pas de dépassement, 1: début, 2: milieu, 3: fin
  ),
  methods = list(
    initialize = function(id, rapide, depasseur, resistant, position_initiale, voie_initiale) {
      id <<- id
      rapide <<- rapide
      depasseur <<- depasseur
      resistant <<- resistant
      position <<- position_initiale
      voie <<- voie_initiale
      vitesse_base <<- ifelse(rapide, runif(1, 1.2, 1.5), runif(1, 0.5, 0.8))
      vitesse <<- vitesse_base
      temps_voie_gauche <<- 0
      etat_depassement <<- 0
    },
    deplacer = function(longueur_route) {
      position <<- (position + vitesse) %% longueur_route
      if (voie == 2) temps_voie_gauche <<- temps_voie_gauche + 1
      else temps_voie_gauche <<- 0
    },
    changer_voie = function(nouvelle_voie) {
      voie <<- nouvelle_voie
    },
    ajuster_vitesse = function(vitesse_cible) {
      vitesse <<- min(vitesse_base, vitesse_cible)
    },
    commencer_depassement = function() {
      etat_depassement <<- 1
    },
    continuer_depassement = function() {
      etat_depassement <<- etat_depassement + 1
    },
    terminer_depassement = function() {
      etat_depassement <<- 0
      voie <<- 1  # La voiture est maintenant sur la voie de gauche
    }
  )
)

# Fonction pour simuler le mouvement des voitures
simuler_trafic <- function(n_voitures, n_frames, longueur_route = 100) {
  # Créer les voitures
  #n_voitures =3; n_frames=20; longueur_route = 100
  voitures <- lapply(1:n_voitures, function(i) {
    Voiture$new(
      id = i,
      rapide = sample(c(TRUE, FALSE), 1),
      depasseur = sample(c(TRUE, FALSE), 1),
      resistant = sample(c(TRUE, FALSE), 1),
      position_initiale = runif(1, 0, longueur_route),
      voie_initiale = sample(1:2, 1)
    )
  })
  
  # Créer le dataframe de simulation
  simulation <- tibble(
    frame = rep(1:n_frames, each = n_voitures),
    id = rep(1:n_voitures, times = n_frames)
  ) %>%
    mutate(
      rapide = map_lgl(id, ~voitures[[.x]]$rapide),
      depasseur = map_lgl(id, ~voitures[[.x]]$depasseur),
      resistant = map_lgl(id, ~voitures[[.x]]$resistant),
      position = map_dbl(id, ~voitures[[.x]]$position),
      voie = map_dbl(id, ~voitures[[.x]]$voie),
      vitesse = map_dbl(id, ~voitures[[.x]]$vitesse),
      depassement = FALSE
    )
  
  # Créer une barre de progression
  pb <- progress_bar$new(
    format = "Simulation en cours [:bar] :percent ETA: :eta",
    total = n_frames * n_voitures,  # Modifier le total
    clear = FALSE,
    width = 60
  )
  
  for (f in 2:n_frames) {
    #f <-2
    frame_precedente <- simulation %>% filter(frame == f-1)
    
    for (v in voitures) {
      # Vérification des voitures devant
      voitures_devant <- frame_precedente %>%
        filter(voie == v$voie, 
               position > v$position, 
               position <= v$position + v$vitesse_base)
      
      if (nrow(voitures_devant) > 0) {
        voiture_devant <- voitures_devant %>% 
          arrange(position) %>% 
          slice(1)
        
        if (v$depasseur && v$voie == 2) {
          voitures_gauche <- frame_precedente %>%
            filter(voie == 1, 
                   position > v$position - 5, 
                   position < v$position + 5)
          
          if (nrow(voitures_gauche) == 0) {
            v$changer_voie(1)
          } else {
            v$ajuster_vitesse(voiture_devant$vitesse)
          }
        } else {
          v$ajuster_vitesse(voiture_devant$vitesse)
        }
      } else {
        v$ajuster_vitesse(v$vitesse_base)
      }
      
      if (v$depasseur && v$voie == 2 && v$etat_depassement == 0) {
        voitures_gauche <- frame_precedente %>%
          filter(voie == 1, 
                 position > v$position - 5, 
                 position < v$position + 5)
        
        if (nrow(voitures_gauche) == 0) {
          v$commencer_depassement()
        }
      } else if (v$etat_depassement > 0 && v$etat_depassement < 3) {
        v$continuer_depassement()
      } else if (v$etat_depassement == 3) {
        v$terminer_depassement()
      }

      v$deplacer(longueur_route)
      
      # Ajouter un indicateur de dépassement
      idx <- simulation$frame == f & simulation$id == v$id
      simulation$depassement[idx] <- 
        ifelse(v$voie == 1 && simulation$voie[simulation$frame == f-1 & simulation$id == v$id] == 2, TRUE, FALSE)
      
      # Mettre à jour la barre de progression
      pb$tick()
      
      if (v$voie == 1 && !v$depasseur) {
        voitures_droite <- frame_precedente %>%
          filter(voie == 2, 
                 position > v$position - 5, 
                 position < v$position + 5)
        
        if (nrow(voitures_droite) == 0) {
          v$changer_voie(2)
        }
      }
      
      # Mettre à jour la simulation
      simulation$position[idx] <- v$position
      simulation$voie[idx] <- ifelse(v$etat_depassement == 0, v$voie, 3 - v$etat_depassement)
      simulation$vitesse[idx] <- v$vitesse
      simulation$etat_depassement[idx] <- v$etat_depassement
    }
  }
  
  return(simulation)
}

# Générer les données de simulation
n_voitures <- 50
n_frames <- 200
donnees_simulation <- simuler_trafic(n_voitures, n_frames)

# Créer l'animation
p <- ggplot(donnees_simulation, aes(x = position, y = voie, color = rapide, shape = depasseur, group = id)) +
  geom_point(size = 3) +
  geom_segment(data = donnees_simulation %>% filter(depassement == TRUE & etat_depassement == 1),
               aes(x = position, xend = position, y = 1, yend = 2),
               arrow = arrow(length = unit(0.3, "cm")), color = "green") +
  scale_y_continuous(limits = c(0.5, 2.5), breaks = c(1, 2), labels = c("Droite", "Gauche")) +
  scale_color_manual(values = c("blue", "red"), labels = c("Lent", "Rapide"), name = "Vitesse") +
  scale_shape_manual(values = c(15, 17), labels = c("Non dépasseur", "Dépasseur"), name = "Comportement") +
  theme_minimal() +
  labs(title = "Simulation de trafic", x = "Position", y = "Voie") +
  transition_time(frame) +
  ease_aes('linear')

# Afficher l'animation
anim <- animate(p, nframes = n_frames, fps = 10)

# Sauvegarder l'animation
anim_save("trafic_avec_depassement.gif", animation = anim)
