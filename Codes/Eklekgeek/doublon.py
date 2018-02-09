# -*- coding: utf-8 -*-
"""

On retire les doublons dans le fichier mergé
(Bricolage)

"""

import pandas as pd
df = pd.read_csv("D:/S4LWO8/Mes Documents/hackathon/output files/out_scrapping3.csv", sep=";")

df = df.drop_duplicates(subset=["siret", "cabbi"], keep='first', inplace=False)

df.to_csv('D:/S4LWO8/Mes Documents/hackathon/output files/no doublon.csv', ";")


