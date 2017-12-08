*Revision 13;

%LET _SXW0=10;				* Nombre de mots dans marvin;
%LET _SXW1=50000;			* Nombre de lignes dans marvin;
%LET _SXW2=%EVAL(&_SXW0*&_SXW1);
%MACRO _SX(I,J);
  XXmot_[(&I-1)*&_SXW0+&J]
%MEND _SX;

%MACRO _S1(P,BONUS,MALUS);
*** Recherche dans les éléments connus ---------------------------------------------------------------;
  y = y&P; y1 = y&P+c&P._[k]-1;
  do while(y<=y1);
    done = 0; x = .;
	if not(m&P._[y]=:dlt) /* piéger les codes postaux également */
    then do x = xcom, xdep, xreg, xgeo, xngeo to &NMARVIN;
   	  if ((XXtype_[x]='L0') and (k<=k0))
	   or((XXtype_[x]='L1') and (k>=k1))
	   or((XXtype_[x]='L2') and((k<=k0)or(k>=k1)))
       or((XXtype_[x]='NAF')and((eqac=2)and(naf=:trim(XXid_[x]))))
	   or(XXtype_[x] in ('CJ','COM','DEP','REG','GEO','L3','PRE')) 
	  then do;
	    done = 0; m = 1; y0 = y;
	    do while((y<=y1)and(m<=XXn_[x])and not done);
	      if m&P._[y]=%_SX(x,m) then do; y = y+1; m = m+1; end;
		  else done = 1;
	    end;
	    if (m>XXn_[x]) 										/* Tous les mots reconnus */
		or ((y>y1) and (XXtype_[x] in ('COM'))) 			/* toponyme partiel */
		then do; 		* trouvé : on passe au mot suivant;
		  y = y-1;
		  leave;
		end;
		else y = y0;	* pas encore trouvé : on teste vis à vis de la séquence suivante;
	  end;
	end; * do x=;
	if done then do;
%IF &DEBUG %THEN put c_[k] m&P._[y] malus= bonus=;;
      bonus = bonus + &BONUS;
	  malus = malus + &MALUS;
	end;
	else if x ne . 
    then if XXType_[x]='L3'
	  then do;
        bonus = bonus + &BONUS/2;
	    malus = malus + &MALUS/2;
	  end;
	else if XXType_[x]='PRE'
	  then bonus = bonus+10*(&BONUS>0);
	y = y+1;
  end; * do while;
%MEND _S1;

%MACRO S(IN,PA,PB,OUT,DEBUG=0);
%PUT ***;
proc sql noprint;
  select count(*) into :N from &IN;
  select count(*) into :NMARVIN from marvin;
quit;

%PUT *** SCORING en route : &N comparaisons à analyser;
data &OUT; set &IN;
  array c_ c1-c20; array ca_ ca1-ca20; array cb_ cb1-cb20; array va_ va1-va&W;
  array ma_ &PA.mot1-&PA.mot&W;
  array mb_ &PB.mot1-&PB.mot&W;
  length more $70.;
  length less $70.;
  array Stype_ Stype1-Stype&W;

  * Chargement des séquences potentiellement neutres;
  length Xmot1-Xmot&_SXW0 $30.; array Xmot_ Xmot1-Xmot&_SXW0;
  length XXtype1-XXtype&_SXW1 $5.; array XXtype_ XXtype1-XXtype&_SXW1;  retain XXtype1-XXtype&_SXW1; drop XXtype1-XXtype&_SXW1;
  length XXid1-XXid&_SXW1 $5.; array XXid_ XXid1-XXid&_SXW1;  retain XXid1-XXid&_SXW1; drop XXid1-XXid&_SXW1;
  array XXn_ XXn1-XXn&_SXW1; retain XXn1-XXn&_SXW1; drop XXn1-XXn&_SXW1;
  length XXmot1-XXmot&_SXW2 $30.; array XXmot_ XXmot1-XXmot&_SXW2;  retain XXmot1-XXmot&_SXW2; drop XXmot1-XXmot&_SXW2;
  if _n_=1 then do;
    do i = 1 to &NMARVIN;
	  set marvin(drop=texte rename=(id=Xid type=Xtype)) point=i;
	  XXtype_[i] = Xtype;
	  XXid_[i] = Xid;
	  XXn_[i] = Xn;
	  do j = 1 to Xn;
	    %_SX(i,j) = Xmot_[j];
	  end;
	end;
  end;
  drop Xn Xmot1-Xmot&_SXW0 Xid Xtype i j;
  * A mettre en constante;
  xngeo = input(put('974',$XGEO.),5.)+1;
  * A transférer dans la fabrication de c1;
  xdep = input(put(dlt,$XDEP.),5.);
  if '0'<=substr(dclt,3,1)<='9' then xcom = input(put(dclt,$XCOM.),5.); else xcom = xdep;
  xreg = input(put(put(dlt,$DEPREG.),$XREG.),5.);
  if (dlt=:'97') then geo = dlt; else geo = '   ';
  xgeo = input(put(geo,$XGEO.),5.);

  naf = apet;
  if index(nafs,apet)>0 then eqac=2;
  else if (nafs=' ')or index(nafs,'-'!!substr(apet,1,2)) then eqac = 1;
  else eqac = 0;

  * Coincidence de communes;
  if (dclt=Sdc)
   or(put(dclt!!Sdc,$COMALIAS.)='=')
    then if (substr(adr,6)='') then eqdc = 1; else eqdc=2;
  else eqdc = (substr(adr,6)='')and(put(dclt!!Sdc,$COMPROX.)='=');

  if (substr(adr,6,4)=' ')or(substr(Sadr,6,4)=' ') 
  then eqno = 1;
  else eqno = 2*(substr(adr,6,4)=substr(Sadr,6,4));
  if (substr(adr,11,4)=' ')or(substr(Sadr,11,4)=' ')and(substr(adr,11,4) ne substr(Sadr,11,4))
  then eqtp = 1;
  else eqtp = 2*(substr(adr,11,1)=substr(Sadr,11,1));

  _l = min(length(adrmot),length(Sadrmot));		* >3 au moins 4 car./1 erreur;
  _m = 100*floor(1+_l/8);						* 	 au moins 8 car./2 erreurs...;

  if (Sadrmot=' ')or(adrmot=' ')
  then eqdm = 1;
  else eqdm = 2*(compged(adrmot,Sadrmot,_m+1)<=_m);

  * Les substitutions se traduisent en séquences -+
    Calcul d indicateurs fiables de position en début et fin de transformation;
  k0 = 1; k1 = nc;
  if nc>1 then do;
    if (c_[nc-1]='+')and(c_[nc]='-') then k1 = nc-1;
	if (c_[1]='+')and(c_[2]='-') then k0 = 2;
  end;

  * Cas particulier des RS_X à EDUCATION NATIONALE :
    le Levenshtein retourne les LYCEE COLLEGE et ECOLE, il faut choisir sur l adresse et ignorer
    les détails de dénomination dans SIRENE
    Il n y a pas de priorité entre les trois types d établissement.
  ;
  ministere = substr(ma_[1],1,1)>'Z';
  activite = (substr(sid,15,1)='A')and eqac;

  malus = 0; bonus = 0; more = ''; less = '';
  ya = 1; yb = 1;
  do k = nc to 1 by -1;
    select(c_[k]);
	  when('+') do; * --- Insertion ----------------------------------------------------------;
		do y = ya to ya+ca_[k]-1; more = trim(more)!!' '!!ma_[y]; end; more = trim(more)!!'/';
        if not activite 
        then do; %_S1(a,0,100*(length(ma_[y])>1)) end;
	  end; * when +;
	  when('-') do; * --- Omission -----------------------------------------------------------;
		do y = yb to yb+cb_[k]-1; less = trim(less)!!' '!!mb_[y]; end; less = trim(less)!!'/';
		if not ministere
        then do; %_S1(b,0,100*(Stype_[y] ne 1)*(length(mb_[y])>1)) end;
	  end; * when -;
	  when('A','B') do; * Sigle --------------------------------------------------------------;
		bonus = bonus + 10;
		malus = malus + 10;
	  end;
	  when('a') do; * --- B abréviation de A -------------------------------------------------;
		%_S1(a,10+40*(length(mb_[yb+y-ya])>1),10+90*((length(ma_[y])-length(mb_[yb+y-ya]))>3))
	  end; * when =;
	  when('b') do; * --- A abréviation de B -------------------------------------------------;
		%_S1(b,10+40*(length(ma_[ya+y-yb])>1),10+90*((length(mb_[y])-length(ma_[ya+y-yb]))>3))
	  end; * when =;
	  when('x') do; * --- Permutation --------------------------------------------------------;
		%_S1(b,100,10)
	  end;
	  when('<','>') do; * Scission/fusion ----------------------------------------------------;
		%_S1(a,100,0)
	  end;
	  when('s') do; * --- Synonyme -----------------------------------------------------------;
		%_S1(a,100*(length(ma_[y])>1),0)
	  end; * when s;
	  when('=') do; * --- Egalité ou presque -------------------------------------------------;
		%_S1(a,10+(40+50*(va_[y]<=.5))*(Stype_[y-ya+yb] ne 1),10*(va_[y]>.5)*(Stype_[y-ya+yb] ne 1))
	  end; * when =;
	  otherwise;
	end; * select;
    ya = ya+ca_[k]; yb = yb+cb_[k];
  end; * do k=;

  * Comparaison des adresses
    Pas de malus ni de bonus encas :
  		de commune différente mais d adresse identique
    	de type de voie différent
    Tolérance sur les différences de numéro;
  eqad = 1;
  if  (not eqdc and eqno ne 2 and eqtp ne 2 and eqdm ne 2)
   or ((eqdc=2)
   and ((not eqdm and not(eqtp=1))
     or (not eqno and (abs(input(substr(adr,6,4),4.)-input(substr(Sadr,6,4),4.))>2)
          and eqtp=2 and eqdm=2)
       ))
  then do; eqad = 0; malus = malus+200; end;
  if eqdc=2 and eqdm=2 and eqno and (eqtp or (eqno=1))
  then do; eqad = 2; bonus = bonus+100; end;
  nelt = (put(dclt!!Sdc,$COMALIAS.) ne '=') and (dclt ne Sdc);

  if substr(apet,1,3) in ('701') then malus = malus +100;
  else if apet in('6820B','9420Z') then malus = malus+200;

  if not eqac then malus = malus+100;
  if eqac=2 then bonus = bonus+100;
  if eqdc and nelt then malus = malus+100;

  if (bonus>=50)or(malus=0) then output;
  if not mod(_n_,100000) then put _n_ 7.;
  drop k ya yb i y m x;

proc sort; 
  by id descending eqdc malus descending bonus nelt
  descending eqac descending eqdm descending eqtp descending eqno
  descending empl_et descending eff3112_tr_et;

data &OUT.1; set &OUT; by id; 
  if first.id;
  keep id rs_x nom siret siretc siretm i_mca_c actet_c apet adr Sadr
       malus bonus cs more less eqad eqac
       ok;
  if siretm ne '' 
  then if xsiretm then ok = (siret=siretm)+(substr(siret,1,9)=substr(siretm,1,9)); else ok = .;
  else if (siretc ne '')and(i_mca_c='C') 
    then if xsiretc then  ok = (siret=siretc)+(substr(siret,1,9)=substr(siretc,1,9)); else ok=.;
	else ok = .;
run;
%MEND S;

%LET DEP=01;
%LET IN=c2(obs=1000);
%INIT;
proc sql noprint;
  select sum(i_mca_c='C')/count(*)*100 format 5.1 into :MCA from &IN;
quit;
%L(&IN,R,s1,S,x,SELECT=(put(dclt!!Sdc,$COMPROX.)='='));
%S(x,R,S,y);
title &DEP (MCA=&MCA %);
data y2; set y1(where=(bonus>=malus));
 if i_mca_c='C' then mca = 1; else mca = 0;
 ok_naf2 = substr(apet,1,2)=substr(actet_c,1,2);
proc freq; table ok*mca/missing; run;
proc freq; table ok_naf2*mca/missing; run;



/*
data x0; set x;
  if (rs_x='ETF ROLLAND PHILIPPE');
run;
%S(x0,R,S,y0,DEBUG=1);
*/
