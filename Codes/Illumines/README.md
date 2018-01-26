<h1>Les Illuminés</h1>

<p>Description de notre groupe et du contenu des scripts fournis</p>


<p>Contact pour plus d'informations : <a href="mailto:pierre.carrelet@intradef.gouv.fr; mathieu.eury@intradef.gouv.fr" > nous écrire </a></p>

<h2>Notre équipe</h2>

<p>Constituée de trois personnes, deux provennat du ministère des Armées (Pierre Carrelet et Mathieu Eury) et un agent Insee (Marie-Anne Houlbrèque). Nous avions comme objectif de retrouver la raison sociale et le siret des bulletins du recensement pour lesquels la mca avait abouti à un résultat peu satsfaisant (soit rien soit mauvais).</p>
<p>Pour ce faire, nous avons choisi de requêter le site Kompass en utilisant un script R et le package Rvest. Au préalable, il fallait apparier les codes communes pour renseigner le code région (version à 22 régions encore utilisée sous Kompass) et le libellé de commune. </p>

<h2>Les scripts</h2>

<p> Les codes sont dans  :</p>
<p> - prepa_don_com.R : appariement des informations sur le code commune renseigné pour récupérer code région et libellé de commune </p>
<p> - hacka_propre.R : procédure de requêtage du site kompass à partir d'une table filtrée du RP sur les cas problématiques. Extraction des informations souhaitées (Siret, Apet)</p>



 
