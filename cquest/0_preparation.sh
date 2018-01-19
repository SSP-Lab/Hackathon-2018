# validation et remise au standard des CSV (UTF8, virgule)
# utilise csvkit (apt install python3-csvkit)
datadir=../données/DonneesTransmisesHackathon/

csvclean $datadir/rp_final_2014.csv -d ';' -e 'ISO8859-1'
csvclean $datadir/rp_final_2017.csv -d ';' -e 'ISO8859-1'

# suppression de colonnes 2014 manquantes en 2017
csvcut $datadir/rp_final_2014_out.csv -C TYPCOL,PROCES_CODAGE_INIT,ARBITRAGE,SIRETQ,I_SIRET_Q,SIRET_ARB,siret_final > $datadir/rp2014.csv

# remise en ordre des colonnes 2017 comme 2014
COLS=$(head -n 1 rp2014.csv)
csvcut $datadir/rp_final_2017_out.csv -c $COLS > $datadir/rp2017.csv
