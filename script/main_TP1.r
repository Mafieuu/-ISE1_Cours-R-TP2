library(tidyverse);
library(readxl)
library(questionr) # pour la fonction describe()
#library(summarytools) # pour la fonction univari()
#library(gtsummary)

projet <- read_excel("../data/Base_Projet_tp1.xlsx")

head(projet,2) # Petite aperçu de la base de donnée 

paste0("Nombre de ligne  de la base : ",nrow(projet))

paste0("Nombre de colonne de la base : ",ncol(projet))

any(is.na(projet$key)) # y a t-il au moins une valeur manquante ?

# Pas de valeur manquantes dans la variable key

projet<-projet%>%
    rename(region=q1,departement=q2,sexe=q23)

head(projet,1)

class(projet$sexe)  # Un string, donc la casse interviens

projet<-projet%>%
    mutate(sexe_2=as.integer(tolower(sexe)=="femme"))
# tolower pour normaliser la casse
# on se sert de l'egalite TRUE==1 pour la conversion en integer

describe(projet$sexe_2)

# Regardons la maniere dont est ordonne nos variables
names(projet)

# On peut se servir de l'ordre avec q24a_1:q24a_10
# ou alors on peut aussi se servir du fait que ce sont les seuls a commencer par q24a_ 

langues<- projet%>%
    select(key,starts_with("q24a_"))%>%
    as.data.frame()
    # les fonctions de dplyr genere des objet de type tibble convertible en data.frane

head(langues,1)

langue_parle_PME<-langues%>%
    select(-key)# on selectionne uniquement les variables ayant trait a la langue 

langues <-langues%>%
    mutate(parle=rowSums(langue_parle_PME) )# a chaque individus on somme les lignes de la variable langue_parle_PME

head(langues$parle)

langues <-langues%>% 
    select(key,parle)

head(langues,1)

# Les deux data.frames ont la colonne key en commun, on vas maj projet
projet<- merge(projet,langues,by="key")

head(projet[,c("key","parle")],2) # Le merge a fonctionne avec succes

#-------- Repartition des PME suivant le sexe

#  Repartition des PME suivant le sexe
# ---------- ligne des effectifs
tab_effectif <- table(projet$sexe) # Calcul des effectifs
tab_effectif <- rbind(tab_effectif ) # Permet de generer un tableau avec des lignes nommee 
rownames(tab_effectif)[1] <- "Effectifs" # modificiation du nom de la ligne
# ---------- ligne des pourcentages
tab_effectif_percent <- prop.table(tab_effectif) * 100 # calcul des pourcentages
rownames(tab_effectif_percent)<-"Pourcentage" # pas bessoin de rbin on travail deja un tab a ligne nommme (tab_effectif)
# ---------- ligne des valeurs manquantes
tab_sexe <- rbind(tab_effectif, "Valeurs manquantes" = sum(is.na(projet$sexe)))# ajout de la ligne NA
# ---------- tableau repartition des PME suivant le sexe
tab_sexe<-rbind(tab_sexe,tab_effectif_percent)
tab_sexe

# intervertissons les lignes "Valeurs manquantes" et "Pourcentages" 
# -------- tab_sexe est une matrice donc pas de slice()
tab_sexe<-tab_sexe[c("Effectifs","Pourcentage","Valeurs manquantes"),]
tab_sexe

# maintenant nous allons cree une fonction pour automatiser le processus.
# La nouveaute ici c'est que projet$var ne marche pas car var n'est pas en soi une colonne. solution: projet[[var]]

repartition<-function (var,projet_=projet) {# je me laisse la possibilite de modifier projet avant de l'appel de repartition() 
     # ---------- ligne des effectifs
    tab_effectif <- table(projet_[[var]]) # Calcul des effectifs
    tab_effectif <- rbind(tab_effectif ) # Permet de generer un tableau avec des lignes nommee 
    rownames(tab_effectif)[1] <- "Effectifs" # modificiation du nom de la ligne
    # ---------- ligne des pourcentages
    tab_effectif_percent <- prop.table(tab_effectif) * 100 # calcul des pourcentages
    rownames(tab_effectif_percent)<-"Pourcentage" # pas bessoin de rbind on travail deja un tab a ligne nommme (tab_effectif)
    # ---------- ligne des valeurs manquantes
   tab_var <- rbind(tab_effectif, "Valeurs manquantes" = sum(is.na(projet_[[var]])))# ajout de la ligne NA
    # ---------- tableau repartition des PME suivant le sexe
    tab_var<-rbind(tab_var,tab_effectif_percent)
    # intervertissons les lignes "Valeurs manquantes" et "Pourcentages" 
    # -------- tab_var est une matrice donc pas de slice()
    tab_var<-tab_var[c("Effectifs","Pourcentage","Valeurs manquantes"),]
    tab_var<- as.data.frame(tab_var) # conversion en data.frame pour l'elegence du rendu
    # print(tab_var) est moche et tab_var ne marche pas dans la declaration de la fonction donc pas d'affichage ici
    return(tab_var)
  
}

#-------- Repartition des PME suivant le niveau d'instruction

repartition("q25")

#-------- Repartition des PME suivant le statu juridique

repartition("q12")

#-------- Repartition des PME suivant le statu <<proprietaire ou locataire>>

repartition("q81")

#-------- repartition des PME suivant le statu juridique et le sexe

# on cree une nouvelle variable en <<collant>> pour chaque PME<< sexe et statu juridique >>
projet_tmp<-projet%>%
    mutate(var_tmp=paste(sexe,q12,sep=" et "))

repartition("var_tmp",projet_=projet_tmp)

#-------- repartition des PME suivant le niveau d'instruction et le sexe

# analogue au precedent

projet_tmp<-projet%>%
    mutate(var_tmp=paste(q25,q12,sep=" et "))

repartition("var_tmp",projet_=projet_tmp)

# -------- repartition des PME suivant le statu <<proprietaire/locataire>> et le sexe

# Les trois questions ci haut sont analogue,une boucle for semble faire l'affaire mais les tableau generer sont moche et print() n'aide pas

projet_tmp<-projet%>%
    mutate(var_tmp=paste(q81,q12,sep=" et "))

repartition("var_tmp",projet_=projet_tmp)

 # ----------------------------- # ? universe ne fonctionne pas sous jupyterlab !?

#' Analyse univariée d'une variable dans un ensemble de données
#'
#' Cette fonction effectue une analyse univariée d'une variable dans un ensemble de données, y compris le calcul de statistiques descriptives et la création de visualisations.
#'
#' @param var Le nom de la variable à analyser.
#' @param projet_ Le data frame contenant les données à analyser (par défaut, il est attendu que le data frame s'appelle "projet").
#' @param statistiques Un vecteur contenant les statistiques descriptives à calculer (par défaut : c("mean", "median", "mode", "sd", "var", "range", "quantiles")).
#' @param tableau Indique si un tableau des effectifs doit être généré (par défaut : FALSE).
#' @param nb_intervalles Le nombre d'intervalles à utiliser pour le tableau des effectifs (par défaut : NULL).
#' @param histogramme Indique si un histogramme doit être tracé (par défaut : FALSE).
#' @param densite Indique si un graphique de densité doit être tracé (par défaut : FALSE).
#' @param boite_moustache Indique si un diagramme en boîte et moustaches doit être tracé (par défaut : FALSE).
#' @param diagramme_baton Indique si un diagramme à bâtons doit être tracé pour les variables catégorielles (par défaut : FALSE).
#' @param diagramme_circulaire Indique si un diagramme circulaire doit être tracé pour les variables catégorielles (par défaut : FALSE).
#' @return Une liste contenant les résultats des calculs et éventuellement le tableau des effectifs.
#' @examples
#' # Analyse univariée d'une variable numérique
#' univarie("age", projet = my_data_frame, histogramme = TRUE)
#'
#' # Analyse univariée d'une variable catégorielle
#' univarie("genre", projet = my_data_frame, diagramme_baton = TRUE)
#'
#' @export
univarie<-function(var,
                    projet_=projet,statistiques = c("mean", "median", "mode", "sd", "var", "range", "quantiles"),
                    tableau=FALSE,
                    nb_intervalles = NULL,
                    histogramme=FALSE,
                    densite=FALSE,
                    boite_moustache=FALSE,
                   diagramme_baton=FALSE,
                   diagramme_circulaire=FALSE 
                   
                  ){
    resultats<-list()
    tableau_effectifs<-NULL
    # -----------------------------------------------------------------------------------------------------------------------
    # Correction des valeurs abherentes (source de probleme pour la construction de graphiques (intervalles,...),etc)
    if (is.numeric(projet_[[var]])) {
        # Remplacer les valeurs extrêmes par la médiane
        q1 <- quantile(projet_[[var]], 0.25, na.rm = TRUE)
        q3 <- quantile(projet_[[var]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr
        upper_bound <- q3 + 1.5 * iqr
        non_aberrant_values <- projet_[[var]]
        non_aberrant_values[non_aberrant_values < lower_bound | non_aberrant_values > upper_bound] <- median(projet_[[var]], na.rm = TRUE)
        projet_[[var]]<- non_aberrant_values 
    }
    
        #  --- pour les variables categorielle il faut faire une etude cas par cas et utilisant le type factor

    
    
    # ----------------------------------------------------------------------------------------------
    

    if (!(var %in% names(projet_))) {
            stop("La variable spécifiée n'existe pas dans le data.frame.")
    }
    if (is.numeric(projet_[[var]])) {
        # Calculs des ststistiques demandes
        if ("mean" %in% statistiques) {
            resultats$moyenne <- mean(projet_[[var]], na.rm = TRUE) # cas ou la base forunis est autre que projet
          }
        if ("median" %in% statistiques) {
            resultats$mediane <- median(projet_[[var]], na.rm = TRUE)
            }
        if ("mode" %in% statistiques) {
            resultats$mode <- as.numeric(names(sort(table(projet_[[var]]), decreasing = TRUE)[1])) # le mode c'est la plus grande valeur 
            }
        if ("sd" %in% statistiques) {
            resultats$Ecart_type <- sd(projet_[[var]], na.rm = TRUE)
            }
        if ("var" %in% statistiques) {
            resultats$variance <- var(projet_[[var]], na.rm = TRUE)
            }
        if ("range" %in% statistiques) {
            resultats$etendue <- range(projet_[[var]], na.rm = TRUE)
            }
        if ("quantiles" %in% statistiques) {
            resultats$quantiles <- quantile(projet_[[var]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE) #Q1,Q2 et Q3
            }
        if (histogramme){
            hist(projet_[[var]], main = paste0("Repartition de ",var,sep=" "), xlab = var, ylab = "Effectif",col = rainbow(length(unique(projet_[[var]]))))
            }
        if (densite){
            plot(density(projet_[[var]], na.rm = TRUE), main = paste("Repartition de ",var,sep=" "))
            }
         if (boite_moustache){
             boxplot(projet_[[var]], main = paste("Repartition de ",var,sep=" "), ylab = "Effectif",col = rainbow(length(unique(projet_[[var]]))))
            }
        if (tableau){ # je traine avec ces complications (quantiles ...) car avant j'avais des valeurs abherentes donc ...
            if (is.null(nb_intervalles)) {# plus bessoin de nb_intervalle ,les donnee abherentes sont suprimmme
               # Les intervalles serons automatiquement decoupe(recommande)
            # Calcul des quantiles
            quantiles <- quantile(projet_[[var]],probs=seq(0, 1, by = 0.2))
            # Utilisation des quantiles comme points de rupture pour la fonction cut()
            intervalle <- cut(projet_[[var]], breaks=quantiles,include.lowest = TRUE)
            # Tableau des effectifs pour chaque intervalle
            tableau_effectifs <- table(intervalle)
            tableau_effectifs <<-as.data.frame(tableau_effectifs) # rend la variable global ce qui facilite sa visualisation en mode console

          } else {
        intervalle <- cut(projet_[[var]], breaks = nb_intervalles,include.lowest = TRUE)
             tableau_effectifs <- table(intervalle)
                 
            tableau_effectifs <<-as.data.frame(tableau_effectifs) # rend la variable global ce qui facilite sa visualisation en mode console
        
          }
            }
        print("--------------------------------------------------------------------------------------------------------------")
        resultats # a rendre global et a appeler sur la console
        
# ------------------------------------------- Pour les variables categorielles ---------------------------------------------------      
    }else { # si la variable est de type str ou un factor
        if (tableau) {
          # Tableau des effectifs et pourcentages pour les variables catégorielles
          tableau_effectifs <- as.data.frame(table(projet_[[var]]))
          colnames(tableau_effectifs) <- c("Categorie", "Effectif")
          tableau_effectifs$Pourcentage <- tableau_effectifs$Effectif / sum(tableau_effectifs$Effectif) * 100
          tableau_effectifs <<-as.data.frame(tableau_effectifs) # rend la variable global ce qui facilite sa visualisation en mode console
        }
        if (diagramme_baton) {
          barplot(tableau_effectifs$Effectif, names.arg = tableau_effectifs$Categorie, main = paste0("Repartition de ", var), xlab = var, ylab = "Effectif")
        }
        if (diagramme_circulaire) {
            pie(table(projet_[[var]]), main = paste0("Diagramme circulaire de ", var),col = rainbow(length(unique(projet_[[var]]))))
        }
  }

}

        
    



# ------------------------------------------------------------------- Teste pour les variables quantitatifs

univarie("q24",tableau=TRUE,histogramme=TRUE,boite_moustache=TRUE)  # q24 == age

# Le tableau des frequences (si demande)
tableau_effectifs # les intervalles sont du type [a,b]

#-------------------------------------------------------------- Teste pour les variables qualitatives


univarie("sexe",tableau=TRUE,diagramme_circulaire=TRUE,diagramme_baton=TRUE) 

# Le tableau des frequences (si demande)
tableau_effectifs # les intervalles sont du type [a,b]

#' Analyse bivariée entre deux variables dans un ensemble de données
#'
#' Cette fonction effectue une analyse bivariée entre deux variables dans un ensemble de données, y compris la création de visualisations et la réalisation de tests statistiques appropriés.
#'
#' @param var1 Le nom de la première variable à analyser.
#' @param var2 Le nom de la deuxième variable à analyser.
#' @param projet_ Le data frame contenant les données à analyser (par défaut, il est attendu que le data frame s'appelle "projet").
#' @param tableau_croise Indique si un tableau croisé des fréquences doit être généré pour les variables catégorielles (par défaut : TRUE).
#' @param teste_khi2 Indique si le test du khi2 doit être réalisé pour les variables catégorielles (par défaut : FALSE).
#' @param nuage_point Indique si un nuage de points doit être tracé pour les variables numériques (par défaut : FALSE).
#' @param regression_lineaire Indique si une régression linéaire doit être effectuée pour les variables numériques (par défaut : FALSE).
#' @return Un data frame contenant les résultats des analyses bivariées, y compris les tableaux croisés des fréquences et les résultats des tests du khi2, le cas échéant.
#' @examples
#' # Analyse bivariée entre deux variables numériques
#' bivarie("age", "salaire", projet = my_data_frame, nuage_point = TRUE, regression_lineaire = TRUE)
#'
#' # Analyse bivariée entre deux variables catégorielles
#' bivarie("genre", "education", projet = my_data_frame, tableau_croise = TRUE, teste_khi2 = TRUE)
#'
#' @export

bivarie <- function(var1, var2, projet_ = projet,
                   tableau_croise=TRUE,
                   teste_khi2=FALSE,
                   nuage_point=FALSE,
                   regression_lineaire=FALSE) {
  
   # -----------------------------------------------------------------------------------------------------------------------
    # Correction des valeurs abherentes (source de probleme pour la construction de graphiques (intervalles,...),etc)
    if (is.numeric(projet_[[var1]])) {
        # Remplacer les valeurs extrêmes par la médiane
        q1 <- quantile(projet_[[var1]], 0.25, na.rm = TRUE)
        q3 <- quantile(projet_[[var1]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr
        upper_bound <- q3 + 1.5 * iqr
        non_aberrant_values <- projet_[[var1]]
        non_aberrant_values[non_aberrant_values < lower_bound | non_aberrant_values > upper_bound] <- median(projet_[[var1]], na.rm = TRUE)
        projet_[[var1]]<- non_aberrant_values 
    }
     if (is.numeric(projet_[[var2]])) {
        # Remplacer les valeurs extrêmes par la médiane
        q1 <- quantile(projet_[[var2]], 0.25, na.rm = TRUE)
        q3 <- quantile(projet_[[var2]], 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr
        upper_bound <- q3 + 1.5 * iqr
        non_aberrant_values <- projet_[[var2]]
        non_aberrant_values[non_aberrant_values < lower_bound | non_aberrant_values > upper_bound] <- median(projet_[[var2]], na.rm = TRUE)
        projet_[[var2]]<- non_aberrant_values 
    }
    
        #  --- pour les variables categorielle il faut faire une etude cas par cas et utilisant le type factor

    
    
    # ----------------------------------------------------------------------------------------------
    resultats <- list()
      # Vérification de l'existence des variables dans le data.frame
    if (!(var1 %in% names(projet_)) || !(var2 %in% names(projet_))) {
        stop("Une ou plusieurs variables spécifiées n'existent pas dans le data.frame.")
    }
  # Vérification du type des variables
  if (is.numeric(projet_[[var1]]) && is.numeric(projet_[[var2]])) {
    # Les deux variables sont numériques
    
    if (nuage_point) {
      # Nuage de points
      plot(projet_[[var1]], projet_[[var2]], 
           main = paste("Nuage de points entre", var1, "et", var2), 
           xlab = var1, ylab = var2)
    }
    
    if (regression_lineaire) {
      # Régression linéaire
      lm_model <- lm(projet_[[var2]] ~ projet_[[var1]], data = projet_)
      summary(lm_model)
    }
    
  } else if (!is.numeric(projet_[[var1]]) && !is.numeric(projet_[[var2]])) {
    # Les deux variables sont catégorielles
    
    if (tableau_croise) {
      # Tableau croisé
      contingency_table <- table(projet_[[var1]], projet_[[var2]])
      resultats$tableau_croise <- contingency_table
    }
    
    if (teste_khi2) {
      # Test du khi2
      chi2_test <- chisq.test(contingency_table)
      resultats$teste_khi2 <- chi2_test
    }
    
  } else {
    # Les variables sont de types différents (une numérique et une catégorielle)
    stop("Les types des variables spécifiées sont différents. Veuillez fournir deux variables du même type.")
  }
  
  resultats<<- as.data.frame(resultats)
}

bivarie("sexe","region",nuage_point=TRUE)

resultats

bivarie("q24","q26",nuage_point=TRUE)

resultats

# Tous fonctionne bien passons a l'analyse

# on doit convertir filiere_1 en str (definition de bivarie()oblige)
projet$filiere_1 <- as.character(projet$filiere_1)

bivarie("filiere_1","region",nuage_point=TRUE) #pas de nuage de point comme on pouvais s'y attendre

resultats

# on remarque que la region de ziguinchor et saint-louis sont les champions pour la filiere arachide

# regardons la variable today

univarie("today",tableau=TRUE,diagramme_circulaire=TRUE,diagramme_baton=TRUE) 

# la pluspart des donne dates de 2021

# on doit convertir q25 en str (definition de bivarie()oblige)
projet$q25 <- as.character(projet$q25)
bivarie("filiere_1","q25",nuage_point=TRUE)#La population n'est pas representative donc pas de teste du khi-2

resultats

# Dans la filiere arachide ,plus le niveau d'instruction augmente moins on trouve d'effectif

# regardons la filiere mangue

bivarie("filiere_2","q24a_3",nuage_point=TRUE) #q24== age


