-- index trigram sur la raison sociale
create index rp2017_rs_x on rp_final_2017 using gin (rs_x gin_trgm_ops);

-- ajout champ adresse combiné + index trigram
alter table rp_final_2017 add adresse text;
update rp_final_2017 set adresse = trim(regexp_replace(coalesce(numvoi_x,''),'^0*','') || coalesce(bister_x,'') || ' '|| regexp_replace(coalesce(typevoi_x,''),'^R$','RUE') || ' ' || coalesce(nomvoi_x,''));
create index rp2017_adresse on rp_final_2017 using gin (adresse gin_trgm_ops);

-- ajout adresse combinée
alter table sirus_2017 add adr_et text;
update sirus_2017 set adr_et = trim(regexp_replace(coalesce(adr_et_voie_num,''),'^0*','')||coalesce(adr_et_voie_repet,'')||' '||regexp_replace(coalesce(adr_et_voie_type,''),'^R$','RUE')||' '||coalesce(adr_et_voie_lib));

-- ajout colonne DEPCOM_ET
alter table sirus_2017 add depcom_et text;
update sirus_2017 set depcom_et = left(adr_et_loc_geo,5);

-- ajout colonne SIRET
alter table sirus_2017 add siret text;
update sirus_2017 set siret = sirus_id || nic;
create index sirus2017_siret on sirus_2017 (siret);

-- index trigram sur sirus: enseigne, nom commercial, adresse combinée
create index sirus2017_enseigne on sirus_2017 using gin (enseigne gin_trgm_ops);
create index sirus2017_nom on sirus_2017 using gin (nom_comm_et gin_trgm_ops);
create index sirus2017_adr on sirus_2017 using gin (adr_et gin_trgm_ops);
create index sirus2017_depcom on sirus_2017 (depcom_et)
create index sirus2017_denom on sirus_2017 using gin (denom gin_trgm_ops);
create index sirus2017_denom_condense on sirus_2017 using gin (denom_condense gin_trgm_ops);
create index sirus2017_sigle on sirus_2017 using gin (sigle gin_trgm_ops);
create index sirus2017_enseigne2 on sirus_2017 using gin (enseigne gin_trgm_ops);

-- fonction utile pour calculer le minimum entre deux réels
create or replace function minimum(a real, b real) RETURNS real as $$
  begin
    return case when a<b then a else b END;
  END
$$ language plpgsql;

-- vue de rapprochement via les trigram
create or replace view rapproche as select minimum(adresse <-> adr_et,1) + minimum(coalesce(denom <-> rs_x,1),minimum(coalesce(enseigne_et1 <-> rs_x,1),minimum(coalesce(enseigne <-> rs_x,1),coalesce(nom_comm_et <-> rs_x,1)))) as dist, adresse <-> adr_et as dist_adr, minimum(coalesce(denom <-> rs_x,1),minimum(coalesce(enseigne_et1 <-> rs_x,1),minimum(coalesce(enseigne <-> rs_x,1),coalesce(nom_comm_et <-> rs_x,1)))) as dist_rs, depcom_et, sirus_id||s.nic as siret_sirus, siret_dec, cabbi from rp2017 r join sirus_2017 s on (clt_c_c = depcom_et) where adresse <-> adr_et < 0.7 and (enseigne_et1 <-> rs_x <0.7 or nom_comm_et <-> rs_x< 0.7 or rs_x <-> denom < 0.7 or rs_x <-> enseigne < 0.7)
