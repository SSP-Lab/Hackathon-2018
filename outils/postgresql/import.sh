# création de la base
createdb hackinsee -O $USER

# extensions utiles... trigrams et fuzzy match (levenstein, etc)
psql hackinsee -c "create extension pg_trgm; create extension fuzzystrmatch; "

# liste des fichiers CSV à importer
for f in *.csv
do
  # liste des champs, tous mis en texte
  champs=$(head -n 1 $f | sed 's/;/ text,/g;s/.$/ text/')
  table=$(echo $f | sed 's/.csv//')
  # creation de la table
  psql hackinsee -c "DROP TABLE IF EXISTS $table ; CREATE TABLE $table ($champs);"
  # import des données
  psql hackinsee -c "\copy $table from $f with (format csv, header true, delimiter ';', encoding 'ISO8859-1')"
done

# création des index
