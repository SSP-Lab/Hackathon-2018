# -*- coding: utf-8 -*-
"""

SCRIPT pour merger les données issues du scrapping
(bricolage rapide)

"""

fout=open("D:/S4LWO8/Mes Documents/hackathon/output files/out_scrapping3.csv","w")
# first file:
for line in open("D:/S4LWO8/Mes Documents/hackathon/output files/api_google100.csv"):
    fout.write(line)
# now the rest:    
for num in range(1,8):
    f = open("D:/S4LWO8/Mes Documents/hackathon/output files/api_2qwant"+str(num*100)+".csv")
    f.readline() # skip the header
    for line in f:
         fout.write(line)
    f.close() # not really needed
    
for num in range(1,7):
    f = open("D:/S4LWO8/Mes Documents/hackathon/output files/api_bing"+str(num*100)+".csv")
    f.readline() # skip the header
    for line in f:
         fout.write(line)
    f.close() # not really needed  
    
for num in range(2,7):
    f = open("D:/S4LWO8/Mes Documents/hackathon/output files/api_google"+str(num*100)+".csv")
    f.readline() # skip the header
    for line in f:
         fout.write(line)
    f.close() # not really needed
    
    
    
f = open("D:/S4LWO8/Mes Documents/hackathon/output files/api_bing667etab_final.csv")
f.readline() # skip the header
for line in f:
     fout.write(line)
f.close() # not really needed  

f = open("D:/S4LWO8/Mes Documents/hackathon/output files/api_google667etab_final.csv")
f.readline() # skip the header
for line in f:
     fout.write(line)
f.close() # not really needed  

f = open("D:/S4LWO8/Mes Documents/hackathon/output files/api_2qwant777etab_final.csv")
f.readline() # skip the header
for line in f:
     fout.write(line)
f.close() # not really needed  
    
    
fout.close()