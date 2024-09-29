# Lire le fichier JSON
data <- fromJSON("input/messages.json")
message_table <- data$messages

message_table$parent_id <- NULL
message_table$indicatrice_previous_message <- sapply(message_table$previous_messages,function(message){
    return((message|> nrow() )>0 )
} )

# Fonction pour extraire le fuseau horaire
extract_timezone <- function(date_string) {
  tz <- str_extract(date_string, "[+-]\\d{4}$")
  return(ifelse(is.na(tz), "Unknown", tz))
}


# Appliquer la fonction pour extraire prénom et nom
message_table <- message_table %>%
  mutate(full_name = sapply(sender, extract_full_name))

message_table$date <- sapply(message_table$date,parse_custom_date)

message_table <- message_table %>%
  separate(date, into = c("annee", "mois", "jour", "heure", "minute", "seconde"), sep = "-", remove = FALSE)

# Ajouter la variable nb_recipients
message_table <- message_table %>%
  mutate(nb_recipients = sapply(recipients, length))

# Create histogram of previous messages
message_table <- message_table %>%
  mutate(prev_msg_count = sapply(previous_messages, nrow)) 


# Vecteur d'emails qui ne semblent pas être des noms et prénoms
non_personne_emails <- c(
  "dg75-c101@insee.fr", "di971-communication-interne@insee.fr", 
  "cei-service-echange-fichiers@insee.fr", "dr54-enquete-logement@insee.fr", 
  "di971-dir@insee.fr", "no-reply@intranet.insee.fr", "guyane@insee.fr", 
  "ssphub-contact@insee.fr", 
  "dg75-ddar-section-internet-et-nouveaux-moyens-de-diffusion@insee.fr", 
  "ne_pas_repondre@siamoi.fr", "di971-formation-saffire-martinique@insee.fr", 
  "dg75-c001@insee.fr", "di971-formation@insee.fr", 
  "dg75-dsi-equipe-datascience@insee.fr", "info@toulouse-space-week.com", 
  "DI971-IGUANE@insee.fr", "DG75-SYND-NAT-FO@insee.fr", 
  "dg75-dsds-infocentre@insee.fr", "dg75-dsds-opale@insee.fr", 
  "contact-utilisateurs-informatique@insee.fr", 
  "ne-pas-repondre@developpement-durable.gouv.fr", "sr972-dir@insee.fr", 
  "DI971-GEST-PERS@insee.fr", "sr971-dir@insee.fr", "gitlab@gitlab.insee.fr", 
  "di971-edir-syndicat-fo@insee.fr", "sr973-giir@insee.fr", 
  "siamoi-reply@insee.fr", "DG75-SYND-NAT-CFDT@insee.fr", 
  "dg75-communication-interne@insee.fr", "nepasrepondre@finances.gouv.fr", 
  "dg75-cgt@insee.fr", "dr69-sern-qualite-traitements@insee.fr", 
  "no-reply@researchgatemail.net", "nepasrepondre_francetransfert@culture.gouv.fr", 
  "dr971-etb971-correspondant-handicap@insee.fr", "DI971-ORG-CONCOURS@insee.fr", 
  "chorus-dt@finances.gouv.fr", "di971-syndicat-cfdt@insee.fr", 
  "DR45-SED-DSU@insee.fr", "dr69-sern-pilotage-collectes@insee.fr", 
  "dr25-gestion-des-deplacements@insee.fr", "info@paris-space-week.com", 
  "dg75-dmcsi-reseau-qualite@insee.fr", "DG75-D001@insee.fr", 
  "DG75-SYND-NAT-SUD@insee.fr", "di971-conseiller-de-prevention@insee.fr", 
  "dr69-sern-repertoires-geographie@insee.fr", "di971-controle-de-gestion@insee.fr", 
  "unite-qualite@insee.fr", "team.sae@globeopremium.fr", 
  "nepasrepondre@globeotravel.fr", "dg75-dsds-administration-capi@insee.fr", 
  "dg75-dmcsi-confidentialite@insee.fr", "SR972-DEM@insee.fr", 
  "dg75-dispositif-de-performance@insee.fr", "dg75-dsds-sabiane@insee.fr", 
  "ESTAT-GISCO-EO@ec.europa.eu", "aeromeet@aeromeet.fr", "SR973-SYND-CGT@insee.fr", 
  "di971-correspondant-compte-individuel-retraite@insee.fr", 
  "dg75-syndicat-national-cfe-cgc@insee.fr", "dr59-prix-mission-nationale@insee.fr", 
  "contact@smart-renting.com", "equipe@info.tchap.beta.gouv.fr", 
  "no-reply@insee.fr", "dr974-ses@insee.fr", "questionnaire-sg@insee.fr", 
  "dr971-etb971-agiire@insee.fr", "copieur.jabrun.etg@insee.fr", 
  "dg75-syndicat-national-unsa@insee.fr", "q2024@leading.pt", "dse-dg@insee.fr", 
  "di971-dirag-syndicat-fo@insee.fr", "SR971-DEM@insee.fr", 
  "dr971-referent-diversite@insee.fr", "info@aeromeet.fr", "DR69-SERN69@insee.fr", 
  "no-reply@zoom.us", "observatoireinterne@ipsossurvey.com", "gitlab@insee.fr", 
  "technews@tech-reporter.com", "info@canadian-space-week.com", 
  "info@sg-lists-email.techtarget.com", "DG75-F601@insee.fr", 
  "MicrosoftExchange329e71ec88ae4615bbc36ab6ce41109e@insee.fr", 
  "SR973-DEM@insee.fr", "cooperation-international@insee.fr", 
  "noreply-esteve@cisirh.finances.gouv.fr", "no-reply-siamoi@insee.fr", 
  "DG75-D101@insee.fr", "di971-sed-activites-editoriales@insee.fr", 
  "will@getouterbounds.co", "EC-NO-REPLY-GRANT-MANAGEMENT@nomail.ec.europa.eu", 
  "siamoi-no-reply@insee.fr", "automated-notifications@nomail.ec.europa.eu", 
  "no-reply@intranet.insee.net", "EC-client-centricity@ec.europa.eu", 
  "missions-internationales@insee.fr", "auriane@infos.bigdataparis.com", 
  "sr971-action-regionale@insee.fr", "teamisd@industryspacedays.com", 
  "dr13-ses@insee.fr", 
  "dg75-insee-gestion-et-suivi-des-deplacements-temporaires@insee.fr", 
  "dr67-etb51-stageSGF@insee.fr", "dr67-etb51-stagesgf@insee.fr", 
  "invitation@airbnb.com", "william@infos.bigdataparis.com", 
  "parisairforum@vi-meet.com", "welcome@overleaf.com", 
  "jessica.wright@itnetworkingsolution.com", "dg75-dmcsi-appui-au-pilotage@insee.fr", 
  "di971-sar@insee.fr", "dg75-l001@insee.fr", "newsletters@demandfocus.io", 
  "no-reply@cnes.fr", "framaforms-609941@framasoft.org", 
  "di971-edir-syndicat-cgt@insee.fr", 
  "dg75-dsds-communication-evolutions-recensement@insee.fr", 
  "fim-aerospace@vi-meet.com", "sgar@guadeloupe.gouv.fr", 
  "nepasrepondregalileo@interne.finances.gouv.fr", "contacts-media-sociaux@insee.fr", 
  "esane-appui-production@insee.fr", "Michael.Davis@mail4technewsdelta.com", 
  "nepasrepondre@developpement-durable.gouv.fr", "mina@leaders-bigboss.biz", 
  "DG33-C990@insee.fr", "technews@itnewszone.com", "fim-defensesecurity@vi-meet.com", 
  "alumni@ens-paris-saclay.fr", "contact@atelier-bambino.fr", 
  "sheryl.davis@netcloudtechnology.com", "info@bigdataparis.com", 
  "dg75-dsds-animation-des-dem-et-sites-prix@insee.fr", "DR34-PCTOURISME@insee.fr", 
  "dr59-sndi-cpie@insee.fr", "dg75-dsds-rem@insee.fr", "dsi-messagerie@insee.fr", 
  "dsi-plateformes-datascience@insee.fr", "dsds-dg@insee.fr", "SR971-AGENTS@insee.fr","DI971-AGENTS-EDIR@insee.fr","sr971-enqueteurs@insee.fr"
)

# Créer la nouvelle variable
message_table <- message_table %>%
  mutate(sender_type = ifelse(sender %in% non_personne_emails, "non_personne", "personne"))


# Filtrer les messages (si nécessaire)
message_table <- message_table %>% 
  filter(!is.na(date), !is.na(full_name))


