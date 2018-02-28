# -*- coding: utf-8 -*-

""" Pole Emploi Team
Script made in "Quick and Quick" way during the Insee Hackathon
Siret Scraping with Qwant search engine power: Qwant do the matching for us

Input :
    - manu.csv : csv file with rows in rp with the field
    - output : new_data is a dataframe with the fields :
        - address
        - siret
        - naf
     scrapped from societe.com or/and mappy.fr
"""

import numpy as np
import pandas as pd
import requests
import re
from bs4 import BeautifulSoup
import json

headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36'}

miss_rp = pd.read_csv("manu.csv", sep = ",", index_col=0, dtype="unicode")

new_data = pd.DataFrame(columns=["adr_et_voie_lib", "adr_et_post",
                               "adr_et_com_lib", "enseigne_et1", "siret",
                                 "naf"], index=miss_rp.index)

societe_count = 0
mappy_count = 0
c=0

def is_nan(x):
    """Function that returns True if x is nan value"""
    return (x is np.nan or x != x)


def scrap_societe_site(page):
    """Scrap societe.com site to get siret, address and naf.
    page is the url returned by Qwant query result
    we fill new_data dataframe with the siret, address and naf scraped from societe.com"""

    req = requests.get(page)
    soup = BeautifulSoup(req.content, "html.parser")

    if soup.find("h1", attrs={"id": "identite_deno"}):
        new_data.loc[i, "enseigne_et1"] = soup.find("h1", attrs={"id":"identite_deno"}).get_text().strip()
        if soup.find("table", attrs={"id":"rensjur"}):
            siret_det = soup.find("table", attrs={"id":"rensjur"})
            siret_det = siret_det.get_text().split("\n")
            for j in range(len(siret_det)):
                if "SIRET" in siret_det[j]:
                    new_data.loc[i, "siret"] = siret_det[j+1]
                    break

        if soup.find("table", attrs={"id":"rensjurcomplete"}):
            details = soup.find("table", attrs={"id":"rensjurcomplete"})
            details_list = details.get_text().split("\n")
            for j in range(len(details_list)):
                if "Adresse" in details_list[j]:
                    new_data.loc[i, "adr_et_voie_lib"] = details_list[j+1]
                if "postal" in details_list[j]:
                    new_data.loc[i, "adr_et_post"] = details_list[j+1]
                if "NAF" in details_list[j] and is_nan(new_data.loc[i, "naf"]):
                    new_data.loc[i, "naf"] = details_list[j+1]
                if "Ville" in details_list[j]:
                    new_data.loc[i, "adr_et_com_lib"] = details_list[j+1]

        elif soup.find("table", attrs={"id":"etab"}):
            details = soup.find("table", attrs={"id":"etab"})
            details_list = details.get_text().split("\n")
            for j in range(len(details_list)):
                if "Adresse" in details_list[j]:
                    new_data.loc[i, "adr_et_voie_lib"] = details_list[j+1]
                if "postal" in details_list[j]:
                    new_data.loc[i, "adr_et_post"] = details_list[j+1]
                if "NAF" in details_list[j] and is_nan(new_data.loc[i, "naf"]):
                    new_data.loc[i, "naf"] = details_list[j+1]
                if "Ville" in details_list[j]:
                    new_data.loc[i, "adr_et_com_lib"] = details_list[j+1]


def scrap_mappy_site(page):
    """Scrap mappy site
    Fill new_data with the address found on mappy.fr
    return a new query for Qwant with the new address
    """

    req = requests.get(url)
    soup = BeautifulSoup(req.content, "html.parser")

    if soup.find("p", attrs={"class": "address"}):
        ad = soup.find("p", attrs={"class": "address"}).get_text()
        ad = ad.replace("\n","").strip()

        new_data.loc[i, "adr_et_voie_lib"] = ad.split(",")[0]
        adr = ad.split(",")[1].strip()
        new_data.loc[i, "adr_et_post"] = adr[:5].strip()
        new_data.loc[i, "adr_et_com_lib"] = adr[5:].strip()

    else:
        ad_list_soup = soup.find_all("li", attrs={"class":"geoentity-fulllist"})
        ad_list = []
        for el in ad_list_soup:
            ad_list.append(el.find("p").get_text())
        ad_list = [el.lower() for el in ad_list]

        if len(ad_list) > 0:

            if not is_nan(miss_rp.loc[i, "NOMVOI_X"]):
                rp_numvoi_list = miss_rp.loc[i, "NOMVOI_X"].split(" ")
                rp_numvoi_list = [el.lower() for el in rp_numvoi_list if
                                el.lower() not in ["un","une", "de", "des", "du",
                                                   "le", "la", "les", "rue", "boulevard", "route", "chemin"]]

                ad = 0
                for el in ad_list:
                    el2 = el.split(",")[0]
                    for word in el2.split(" "):
                        if word in rp_numvoi_list:
                            ad = el
                if ad == 0:
                    ad = ad_list[0]
            else:
                ad = ad_list[0]

            new_data.loc[i, "adr_et_voie_lib"] = ad.split(",")[0]
            adr = ad.split(",")[1].strip()
            new_data.loc[i, "adr_et_post"] = adr[:5].strip()
            new_data.loc[i, "adr_et_com_lib"] = adr[5:].strip()

    return "+"+str(new_data.loc[i, "adr_et_voie_lib"])+"+"+str(new_data.loc[i, "adr_et_post"])+"+"+str(new_data.loc[i, "adr_et_com_lib"])+"+societe.com"



def main():
    for i in miss_rp.index:
        c+=1

        # Define query for Qwant api : the query is what people write
        query = ""
        for j in ["RS_X", "ACTET_X", "CLT_X", "DLT_X", "CLT_C_C", "NOMVOI_X"]:
            if not is_nan(miss_rp.loc[i,j]):
                if j != "CLT_C_C":
                    query += "+" + str(miss_rp.loc[i,j])
                elif (j == "CLT_C_C" and isinstance(miss_rp.loc[i,j], float)):
                    query += "+" + str(int(miss_rp.loc[i,j]))
                elif (j == "CLT_C_C" and isinstance(miss_rp.loc[i,j], str)):
                    query += "+" + str(miss_rp.loc[i,j])

        # query on Qwant API
        url = "https://api.qwant.com/egp/search/web?count=10&q="  + query
        req = requests.get(url, headers=headers)

        # Compute links list from req results
        if req.status_code == 200:
            data = req.json()
            links = []

            if "data" in list(data.keys()):
                data = data["data"]["result"]["items"]
                for dico in data:
                    for k, v in dico.items():
                        if k == "url":
                            links.append(v)
                first_five_links = links[:5]

                bool_societe = False
                # Check if societe.com is in the 5 first links
                # If True, scrap societe.com
                for el in first_five_links:
                    if "www.societe.com" in el:
                        bool_societe = True
                if bool_societe:
                    societe_count += 1
                    print("societe")
                    for link in links:
                        if "www.societe.com" in link:
                            url = link
                            scrap_societe_site(url)
                            break
                # If False, scrap mappy.fr to scrap a new address and then scrap
                # societe.com to get the siret as above
                if not bool_societe:
                    for link in links:
                        if "mappy" in link:
                            print("mappy")
                            mappy_count += 1
                            url = link
                            query2 = scrap_mappy_site(url)
                            query2 = str(miss_rp.loc[i,"RS_X"]) + query2

                            url = "https://api.qwant.com/egp/search/web?count=10&q="  + query2
                            req = requests.get(url, headers=headers)
                            if req.status_code == 200:
                                data = req.json()
                                links = []

                                if "data" in list(data.keys()):
                                    data = data["data"]["result"]["items"]
                                    for dico in data:
                                        for k, v in dico.items():
                                            if k == "url":
                                                links.append(v)
                                for link in links:
                                    if "www.societe.com" in link:
                                        url = link
                                        scrap_societe_site(url)
                                        break

        else:
            print("stop API")
            new_data["cabbi"] = miss_rp["CABBI"]
            new_data.to_csv("new_data_score_fin_" +str(i)+ ".csv")
            break

if __name__ == '__main__':
    main()
