# ------------------------------------------------------
#           OPEN SIRUS
# ------------------------------------------------------
# Tested successfully on Spark 1.6 with Python 2.7
# pyspark --master local[*] --packages com.databricks:spark-csv_2.10:1.4.0 

from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext
from pyspark.sql.functions import col

#Parametrage
conf = SparkConf().setAppName("sirusOPQ")
sc = SparkContext(conf=conf)
sqlContext = SQLContext(sc)

print("Import dataset SIRUS")

path = "/home/lino/Bureau/4A ENS/Hackathon INSEE/Hackathon/Dossier fourni/Hackathon-2018/données/DonneesTransmisesHackathon/sirus_2017.csv"
df1 = sc.textFile(path)
df1 = df1.map(lambda l: l.split(";"))

header = df1.first()
df2 = df1.filter(lambda l: l!=header)

print("Constitute Dataframe")

df_sirus = sqlContext.createDataFrame(df2,[str(col) for col in header])

# df_sirus.select(df_sirus.columns[:7]).show(2)
# +---------+-----+-----+-----+-----------+----------+----------------------+
# | sirus_id|  nic|  ape| apet|eff_3112_et|eff_etp_et|eff_et_effet_daaaammjj|
# +---------+-----+-----+-----+-----------+----------+----------------------+
# |000325175|00057|3212Z|3212Z|           |          |                      |
# |005420021|00056|4669B|4669B|         12|        11|              20091231|
# +---------+-----+-----+-----+-----------+----------+----------------------+
# only showing top 2 rows

list_right_apet = (["84", "85", "86", "87", "88", "90",
	"91", "92", "93", "94", "95", "96", "97", "98", "99"]
	)

print("Filter dataframe to keep only OPQ")

sirus_opq = (df_sirus
	.withColumn("shortAPET", df_sirus.apet[:2])
	)

sirus_opq = sirus_opq.where(sirus_opq.shortAPET.isin(list_right_apet))

# sirus_opq.select(df_sirus.columns[:7]).show(2)
# +---------+-----+-----+-----+-----------+----------+----------------------+
# | sirus_id|  nic|  ape| apet|eff_3112_et|eff_etp_et|eff_et_effet_daaaammjj|
# +---------+-----+-----+-----+-----------+----------+----------------------+
# |005640602|00032|5630Z|9319Z|           |          |                      |
# |005720164|00028|8610Z|8610Z|        161|       156|              20151231|
# +---------+-----+-----+-----+-----------+----------+----------------------+

#sirus_opq.count()
#1880520

print("Save file")

### METHOD 1: WITH HDFS

# writePath = "hdfs://localhost:54310" + "/sirusOPQ"

# (sirus_opq
# 	.map(lambda x: ";".join([y.encode('utf-8') for y in x]))
# 	.saveAsTextFile(writePath)
# 	)

# # After that do on a shell
# # hdfs dfs -text "hdfs://localhost:54310/sirusOPQ/*" > sirus_o_p_q.csv
# # cd '/home/lino/Bureau/4A ENS/Hackathon INSEE/Hackathon/Dossier fourni/Hackathon-2018/données/DonneesTransmisesHackathon'
# # hdfs dfs -put sirus_o_p_q.csv "/sirus_o_p_q.csv"

### METHOD 2: WITHOUT HDFS


#(sirus_opq
#	.write.format('com.databricks.spark.csv').save(path + "/test.csv")
#	)


print("End of program")




