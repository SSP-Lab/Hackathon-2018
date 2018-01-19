# Utilisation du moteur de géocodage addok pour recherche de SIRET et/ou adresse

## But

Evaluer les résultats obtenus par l'utilisation du moteur de géocodage "addok" développé par Etalab pour le géocodage d'adresses.

addok est à la base unmoteur de recherche fulltext spécialisé dans les adresses, mais que l'on peut détourner pour rechercher tout type de texte (comme une raison sociale).

3 instances d'addok sont utilisées contenant:
- la base SIRENE géocodée (pour recherche de SIRET)
- la BAN (pour les adresses au numéro)
- la BANO (pour compléter BAN sur les lieux-dits)

## Préparation

Le script 0_preparation.sh remet en forme les fichiers rp_final_xxxx:
- format CSV standard (séparateur virgule, encodage UTF8)
- remise en ordre des colonnes

Il génère 2 fichiers (rp2014.csv et rp2017.csv) exploitables par le script suivant.

## Géocodage

1_geocodage.py appelle le script python geocodage.py pour les deux fichiers rp.

Pour chaque ligne du fichier CSV d'entrée, il effectue le géocodage sur les 3 instances addok en constituant un chaîne de texte à chercher composée de l'adresse seule (pour BAN et BANO) ou complétée par la raison sociale (pour SIRENE).

Les resultats sont ensuite comparés, si le score SIRENE est élevé, il est conservé, sinon on passe sur un géocodage plus classique à l'adresse.

## Fichiers produits

En sortie de script de géocodage, c'est un fichier CSV qui est généré, contenant des colonnes supplémentaires résultat du géocodage.

La colonne '**type**' indique le type d'adresse trouvée: siret (on a trouvé une entreprise), housenumber (adresse au numéro), street (adresse à la voie), municipality (adresse à la commune)

La colonne '**id**' contient l'identifiant de l'adresse, pour le type=siret, il s'agit du SIRET de l'établissement trouvé.

La colonne '**score**' contient le score de matching, plus il est proche de 1 meilleur est le résultat.
