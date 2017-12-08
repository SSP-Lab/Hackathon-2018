
%LET W1=%EVAL((&W+1)*(&W+1));
%LET W2=%EVAL(&W*&W);
%MACRO _LMD(C,I,J,COST=1);
  do;
    cost = &COST;
    if %_LD(i-&I,j-&J)+cost<%_LD(i,j)
    then do; 
      %_LD(i,j) = %_LD(i-&I,j-&J)+cost;
      %_LM(&C,i-&I,j-&J);
    end;
  end
%MEND _LMD;
%MACRO _LM(C,I,J);
  do;
    %_LA(e_,i,j) = "&C";
    %_LA(i_,i,j) = &I;
    %_lA(j_,i,j) = &J;
  end
%MEND _LM;
%MACRO _LA(A,I,J);
  &A[&I+(&J)*&W]
%MEND _LA;
%MACRO _LD(I,J);
  d_[1+&I+(&J)*(&W+1)]
%MEND _LD;
%MACRO L(INA,PA,INB,PB,OUT,DEBUG=0,SELECT=,JOKER=0);
proc sql noprint;
  select count(*) into :NA from &INA;
  select count(*) into :NB from &INB;
quit;

** Pré calcul de l appartenance au dictiionnaire;
data _l_a(drop=i); set &INA;
  array f_ _fa1-_fa&W;
  length _na1-_na&W $30; array n_ _na1-_na&W;
  array m_ &PA.mot1-&PA.mot&W;
  do i = 1 to &PA.n; 
    f_[i] = length(m_[i])=1 or not(put(m_[i],$DICT.)=:'=');
    n_[i] = put(m_[i],$CONVERSION.);
  end;
data _l_b(drop=i); set &INB;
  array f_ _fb1-_fb&W;
  length _nb1-_nb&W $30; array n_ _nb1-_nb&W;
  array m_ &PB.mot1-&PB.mot&W;
  do i = 1 to &PB.n; 
    f_[i] = length(m_[i])=1 or not(put(m_[i],$DICT.)=:'='); 
    n_[i] = put(m_[i],$CONVERSION.); 
	* Specifique fab;
	j = index(m_[i],'*');
	if j>1 then do; 
	  n_[i] = substr(n_[i],1,j-1);
	  f_[i] = 1;
	end;
  end;
run;
%PUT *** MATCHING en route : &NA libellés à analyser;
data &OUT; set _l_a;
  length cs $20;
  length nc 3.;							* Nombre de modifications;
  length c1-c20 $1; array c_ c1-c20; 	* Suite (inversée) des modifications;
  array ca_ ca1-ca20;					* Largeur de chaque modification dans A;
  array cb_ cb1-cb20; 					* Largeur de chaque modification dans B;
  array va_ va1-va&W;

  array fa_ _fa1-_fa&W; array na_ _na1-_na&W; drop _fa1-_fa&W _na1-_na&W;
  array fb_ _fb1-_fb&W; length _nb1-_nb&W $30; array nb_ _nb1-_nb&W; drop _fb1-_fb&W _nb1-_nb&W;
  array ma_ &PA.mot1-&PA.mot&W;
  length &PB.mot1-&PB.mot&W $30; array mb_ &PB.mot1-&PB.mot&W;

  array d_ _d1-_d&W1; drop _d1-_d&W1;
  length _e1-_e&W2 $1; array e_ _e1-_e&W2; drop _e1-_e&W2;
  array i_ _i1-_i&W2; drop _i1-_i&W2; array j_ _j1-_j&W2; drop _j1-_j&W2;

  if _n_=1 
  then call compcost("FREPLACE=",100,"REPLACE=",100,"TRUNCATE=",100
    			    ,"DELETE=",100,"INSERT=",100,"DOUBLE=",100,"SINGLE=",100,"SWAP=",100);
  do p = 1 to &NB;
    set _l_b point=p;
	** cf. distance de Levenshtein-Damerau;
	%IF %LENGTH(&SELECT) %THEN if &SELECT then do;; 
	%_LD(0,0) = 0;
	do i = 1 to &PA.n; %_LD(i,0) = i; end;
	do j = 1 to &PB.n; %_LD(0,j) = j; end;
	do i = 1 to &PA.n;
	  do j = 1 to &PB.n; 
		la = length(ma_[i]); lb = length(mb_[j]);
		f = fa_[i] or fb_[j]; * Absence d au moins un des deux mots du dictionnaire;
		_l = min(length(na_[i]),length(nb_[j]));	* >3 au moins 4 car./1 erreur;
		_m = 100*floor(1+_l/8);						* 	 au moins 8 car./2 erreurs...;
        cost = 2;
		k = abs(compare(na_[i],nb_[j]));
        if k=0 then cost = (not(na_[i]=ma_[i])+not(nb_[j]=mb_[j]))*.1;
		if cost=2
        then if f and (_l>3) 
          then do; 
            _d = compged(na_[i],nb_[j],_m+1);
            if _d<=_m then cost = 0.7+_m/10000;					* orthographe approchée;
		  end;
		
        %_LD(i,j) = min(%_LD(i-1,j)+1,  	
		              	%_LD(i,j-1)+1,     
					  	%_LD(i-1,j-1)+cost);
		if %_LD(i,j)=%_LD(i-1,j)+1 		then %_LM(+,i-1,j);		* Insertion;
		else if %_LD(i,j)=%_LD(i-1,j)+1 then %_LM(-,i,j-1);		* Omission;
		else if cost=2 					then %_LM(-,i,j-1);		* Substitutions interdites!;
										else %_LM(=,i-1,j-1);	* Egalité ou presque;

		if fa_[i] and (k=length(na_[i])+1) then %_LMD(b,1,1);	* A abréviation de B;
		if fb_[j] and (k=length(nb_[j])+1) then %_LMD(a,1,1);	* B abréviation de A;

		** Synonymes 1-1 ------------------------------------------------------------------;
		if cost>.5
		then if put(compress(na_[i]!!'='!!nb_[j]),$SYNONYMES.)='='
         then %_LMD(=,1,1,COST=.5);

		 ** Synonymes 1-2 ------------------------------------------------------------------;
		if j>1
		then if put(compress(na_[i]!!'='!!nb_[j-1]!!nb_[j]),$SYNONYMES.)='='
         then %_LMD(s,1,2,COST=.5);

		 ** Synonymes 2-1 ------------------------------------------------------------------;
		if i>1
		then if put(compress(na_[i-1]!!na_[i]!!'='!!nb_[j]),$SYNONYMES.)='='
         then %_LMD(s,2,1,COST=.5);

		 ** Synonymes 2-2 ------------------------------------------------------------------;
		if i>1 and j>1
		then if put(compress(na_[i-1]!!na_[i]!!'='!!nb_[j-1]!!nb_[j]),$SYNONYMES.)='='
         then %_LMD(s,2,2,COST=.5);

		 ** Synonymes 3-1 ------------------------------------------------------------------;
		if i>2
		then if put(compress(na_[i-2]!!na_[i-1]!!na_[i]!!'='!!nb_[j]),$SYNONYMES.)='='
         then %_LMD(s,3,1,COST=.5);

		 ** Synonymes 1-3 ------------------------------------------------------------------;
		if j>2
		then if put(compress(na_[i]!!'='!!nb_[j-2]!!nb_[j-1]!!nb_[j]),$SYNONYMES.)='='
         then %_LMD(s,1,3,COST=.5);

		** Repetitions --------------------------------------------------------------------;
		if i>1
		then if (na_[i]=na_[i-1])and(na_[i-1]=nb_[j]) then %_LMD(d,1,0,COST=.9);

		** Permutations -------------------------------------------------------------------;
		if i>1 and j>1
		then if (na_[i]=nb_[j-1])and(na_[i-1]=nb_[j]) then %_LMD(x,2,2);
		if i>2 and j>2
		then if ((na_[i]=nb_[j-2])and(na_[i-1]=nb_[j-1])and(na_[i-2]=nb_[j]))
			  or((na_[i]=nb_[j-2])and(na_[i-1]=nb_[j])and(na_[i-2]=nb_[j-1]))
			  or((na_[i]=nb_[j-1])and(na_[i-1]=nb_[j-2])and(na_[i-2]=nb_[j]))
          then %_LMD(x,3,3);

		** Concatenations -----------------------------------------------------------------;
		if j>1 and lb<la
        then do;
		  loop = 1; k = 0; x = la+1;
		  do while(loop and k<j and x>1);
		    _l = length(mb_[j-k]); x = x-_l;
			if x>=1 then if substr(ma_[i],x,_l)=mb_[j-k] then k = k+1; else loop = 0;
		  end;
		  if loop and x=1 then %_LMD(>,1,k);
        end;

		** Scissions ----------------------------------------------------------------------;
		if i>1 and la<lb
        then do;
		  loop = 1; k = 0; x = lb+1;
		  do while(loop and k<i and x>1);
		    _l = length(ma_[i-k]); x = x-_l;
			if x>=1 then if substr(mb_[j],x,_l)=ma_[i-k] then k = k+1; else loop = 0;
		  end;
		  if loop and x=1 then %_LMD(<,k,1);
        end;

		** Sigle en A développé en B ------------------------------------------------------;
		if la>1 and j>=la
		then do;
		  loop = 1; k = 0;
		  do while(loop and k<la);
		    if substr(ma_[i],la-k,1)=substr(mb_[j-k],1,1) then k = k+1; else loop = 0;
		  end;
		  if loop then %_LMD(A,1,k);
		end;

		** Sigle en B développé en A ------------------------------------------------------;
		if lb>1 and i>=lb
		then do;
		  loop = 1; k = 0;
		  do while(loop and k<lb);
		    if substr(mb_[j],lb-k,1)=substr(ma_[i-k],1,1) then k = k+1; else loop = 0;
		  end;
		  if loop then %_LMD(B,k,1);
		end;
	  end;
	end;
	** Reconstitution de la liste des opérations;
%IF &DEBUG %THEN %DO;
    do i = 1 to &PA.n; put ma_[i] @; end; put;
    do j = 1 to &PB.n; put mb_[j] @; end; put;
	do j = 1 to &PB.n;
	  do i = 1 to &PA.n;
	    put %_LA(e_,i,j) @;
	  end;
	  put;
	end;
	do j = 1 to &PB.n;
	  do i = 1 to &PA.n;
	    put %_LA(i_,i,j) %_LA(j_,i,j) %_LD(i,j) 2.1 @;
	  end;
	  put;
	end;
%END;
	i = &PA.n; j = &PB.n; f = 0; c0 = ' '; nc = 0;
	do while(i or j);
	  i0 = i; j0 = j; 
	  if i
      then if j 
	    then do; 
          c = %_LA(e_,i,j); i = %_LA(i_,i0,j0); j = %_LA(j_,i0,j0);
		  va_[i0] = %_LD(i0,j0)-%_LD(i,j);
        end;
	    else do; c = '+'; va_[i] = 1; i = i-1; end;
	  else do; c = '-'; j = j-1; end;
	  if (c in ('+','-','=','d'))and(c0=c)
	  then  do; * On cumule;
	    ca_[nc] = ca_[nc]+i0-i; cb_[nc] = cb_[nc]+j0-j;
	  end;
	  else do;
		c0 = c;
	    nc = nc+1; c_[nc] = c; 
        ca_[nc] = i0-i; cb_[nc] = j0-j;
	  end;
	  f = f + not (c in ('+','-'));
	  if i0 then if (c='=')and(va_[i0]=.8) then f = f-1; 
	end;
%IF &DEBUG %THEN %DO;
    do i = nc to 1 by -1; put ca_[i] @; end; put;
    do j = nc to 1 by -1; put cb_[j] @; end; put;
	put cs;
	do i = 1 to &PA.n; put va_[i] @; end; put;
%END;
	cs = '';
	do i = 1 to nc;
	  substr(cs,i,1) = c_[nc-i+1];
	end;
%IF &DEBUG %THEN put cs=;;
    if f then output;
	%IF %LENGTH(&SELECT) %THEN end;;
  end;
  if not mod(_n_,100) then put _n_ 5.;
  drop p i j cost x la lb k _l _m loop i0 j0 c c0 f;
run;
%MEND L;


*** Jeux d essai;

%MACRO LDATA(T,P,L);
data &T;
  length &P.mot1-&P.mot&W $30.;
  array mot_ &P.mot1-&P.mot&W;
%LET I=1;
%DO %WHILE(%LENGTH(%SCAN(&L,&I)));
  mot_[&I] = "%SCAN(&L,&I)";
  %LET I=%EVAL(&I+1);
%END;
  &P.n = %EVAL(&I-1);
run;
%MEND LDATA;


%LDATA(a,a,ETAT);
%LDATA(b,b,ETA);
%L(a,a,b,b,l,DEBUG=1);
