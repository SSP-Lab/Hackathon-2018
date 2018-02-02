# ------------------------------------------------------
#           OPEN SIRUS
# ------------------------------------------------------
# 2017-02-01
# pyspark --master local[*]

#Parametrage
from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext
conf = SparkConf().setAppName("sirusOPQ")
sc = SparkContext(conf=conf)
sqlContext = SQLContext(sc)

df1 = sc.textFile("/home/lino/Bureau/4A ENS/Hackathon INSEE/Hackathon/Dossier fourni/Hackathon-2018/données/DonneesTransmisesHackathon/sirus_2017.csv")
df1 = df1.map(lambda l: l.split(";"))

header = df1.first()
df2 = df1.filter(lambda l: l!=header)

df_sirus = sqlContext.createDataFrame(df2,[str(col) for col in header])

# df_sirus.select(df_sirus.columns[:7]).show(2)
# +---------+-----+-----+-----+-----------+----------+----------------------+
# | sirus_id|  nic|  ape| apet|eff_3112_et|eff_etp_et|eff_et_effet_daaaammjj|
# +---------+-----+-----+-----+-----------+----------+----------------------+
# |000325175|00057|3212Z|3212Z|           |          |                      |
# |005420021|00056|4669B|4669B|         12|        11|              20091231|
# +---------+-----+-----+-----+-----------+----------+----------------------+
# only showing top 2 rows
