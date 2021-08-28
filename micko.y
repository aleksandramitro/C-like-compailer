%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "defs.h"
  #include "symtab.h"
  #include "codegen.h"

  int yyparse(void);
  int yylex(void);
  int yyerror(char *s);
  void warning(char *s);

  extern int yylineno;
  int out_lin = 0;
  char char_buffer[CHAR_BUFFER_LENGTH];
  int error_count = 0;
  int warning_count = 0;
  int var_num = 0;
  int fun_idx = -1;
  int fcall_idx = -1;
  int vars = 0;
  int var_type = -1;
  int par_num = 0;
  int arg_num = 0;
  int jiro_lit = 0;  //oznacava da se literal javio u jiro naredbi
  int jiro_idx = -1;
  unsigned jiro_tip = 0;  //tip jiro literala
  int lab_num = 0;
  unsigned inc_tip = -1;
  int inc_idx = 0;
  int inc_b = 0;
  int global_num = 0;
  int g_id = 0;
  int inc_prom[20];
  int d_inc = 0;
  int p_num = -1;
  int oiai_num = 0;
  int ret_pojava = -1;
  unsigned tip_f = -1;
  int jiro_num = 0;
  int tranga_num = -1;
  int jiro_id = 0;
  int jiro_prolaz = 0;
  int toerana_p = 0;
  int pp_ind = 0;
  int broj_pp = -1;
  int kond_num = -1;
  int oznaka_op = -1;// 0-if,1-hana,2-kond
  unsigned kond_tip1 = -1;
  unsigned kond_tip2 = -1;
  int pojava_kond1 = 0;
  int pojava_kond2 = 0;
  int kond_i = 0;
  int id_idx = -1;
  int lit_idx = -1;
  int reg1 = -1;
  int reg2 = -1;
  int reg = -1;
  int dec_prom[20];
  int d_dec = 0;
  int dec_b = 0;
  int niz_num = 0;
  int suma_duzina = 0;
  FILE *output;
  
  typedef struct jiro{
    
     long int num[MAX_LENGTH_ARRAY];

  } JIRO_LITERALI;

  typedef struct param{
    
     unsigned fun[MAX_LENGTH_ARRAY];

  } F_PARAMS;

 typedef struct nizovi{
	int redni_broj;
	int duzina;
	int idx;

  } NIZOVI;
 

  JIRO_LITERALI jiro_literali;
  F_PARAMS f_parametri[100];
  int niz_indeksa[MAX_LENGTH_ARRAY];
  NIZOVI nizovi[15];
  
  void init_params(int idx) {
	
	for(int i=0;i<MAX_LENGTH_ARRAY;i++) {
		f_parametri[idx].fun[i] = NO_TYPE;
        }
  }
  
  unsigned get_type_par(int idx,int poz) {
	if(poz < -1 || poz>MAX_LENGTH_ARRAY) {
		return -1;
	} else {
		return f_parametri[idx].fun[poz];
	}

  }
  
  void set_par(int idx,int poz,unsigned tip) {
	if(poz < -1 || poz>MAX_LENGTH_ARRAY) {
		return;
	} else {
	       f_parametri[idx].fun[poz] = tip;
	}

   }

  void inicijalizacija() {
     
	for(int i=0;i<MAX_LENGTH_ARRAY;i++) {
           jiro_literali.num[i] = 0;
        }

   }
   void brisanje() {

      for(int i=0;i<MAX_LENGTH_ARRAY;i++) {
           jiro_literali.num[i] = 0;
        }
   }
   void brisanje_indeksa() {

      for(int i=0;i<MAX_LENGTH_ARRAY;i++) {
           niz_indeksa[i] = 0;
        }
   }
   int set_lit(int idx,char *str) {
      long int n = atol(str);
      int ret = 0;
      for(int i=0;i<MAX_LENGTH_ARRAY;i++) {
	if(jiro_literali.num[i] == n) {
		ret = 1;
         }

      }
      jiro_literali.num[idx] = n;
      return ret;
   }
   
   void inic() {

	for(int i=0;i<10;i++) {
	    inc_prom[i] = 0;
	}
   }
   int duzina_jiro() {
	int n = 0;
	for(int i=0;i<10;i++)  {
	   n++;
        }
   }


%}

%union {
  int i;
  char *s;
}

%token <i> _TIP
%token _IF
%token _ELSE
%token _RETURN
%token <s> _ID
%token <s> _INT
%token <s> _UINT
%token _LZAGRADA
%token _DZAGRADA
%token _LVITICASTA
%token _DVITICASTA
%token _DODELA
%token _TZAREZ
%token <i> _AOP
%token _INC
%token _DEC
%token <i> _ROP
%token _ZAREZ
%token _LUGLASTA
%token _DUGLASTA
%token <i> _LOP
%token _OIAI
%token _HANA
%token _JIRO
%token _TRANGA
%token _FINISH
%token _TOERANA
%token _IMPL
%token _UPITNIK
%token _DTACKA

%type <i> broj_exp exp literal poziv_funkcije argument rel_exp inc_exp if_part prosireni_uslov
%type <i> broj_exp_pp kondicionalni_izraz izraz oiai_naredba jiro_iskaz pp_prosireni_uslov
%type <i> pp_kondicionalni_izraz dec_exp 

%nonassoc ONLY_IF
%nonassoc _ELSE
%nonassoc ONLY_REL_EXP


%%

program
 : lista_dekl
   {
     if(lookup_symbol("main",FUN) == NO_INDEX)
	err("nedefinisana referenca na 'main' funkciju");
     inic();
   }
 ;

lista_dekl
 : deklaracija
 | lista_dekl deklaracija
 ;

deklaracija
 : globalne_promenljive
 | funkcija
 ;

globalne_promenljive
 : _TIP _ID _TZAREZ
   {
      if(lookup_symbol($2,GLOBAL) == NO_INDEX) {
	      insert_symbol($2,GLOBAL, $1, ++global_num, NO_ATR);
	      code("\n%s:", $2);
	      code("\n\t\tWORD\t");
	      code("1");
      } else {
	      err("redefinicija promenljive '%s'", $2);
      }

   }
 ;

funkcija
 : _TIP _ID 
   {
	fun_idx = lookup_symbol($2,FUN);
        if(fun_idx == NO_INDEX)  {
	 
          fun_idx = insert_symbol($2,FUN,$1,NO_ATR,NO_ATR);
	  init_params(fun_idx);

        }
	else
         err("redefinicija funkcije '%s'",$2);

	code("\n%s:", $2);
        code("\n\t\tPUSH\t%%14");
        code("\n\t\tMOV \t%%15,%%14");
	ret_pojava = 0;
	tip_f = $1;
   }

   _LZAGRADA parametri_funkcije _DZAGRADA telo_funkcije
   {
	clear_symbols(fun_idx+1);
        var_num = 0;
        par_num = 0;
	niz_num = 0;
	ret_pojava = 0;
	suma_duzina = 0;

	code("\n@%s_exit:", $2);
        code("\n\t\tMOV \t%%14,%%15");
        code("\n\t\tPOP \t%%14");
        code("\n\t\tRET");

        if(get_name(fun_idx) == "main") {
    		clear_symtab();
		global_num = 0;
        }
   }
 ;

parametri_funkcije
 : /*bez parametara*/  
  {
	set_atr1(fun_idx,0);
   }
 | parametar
   {
	set_atr1(fun_idx,par_num);
   }
 ;

parametar
 : _TIP _ID
   {	
	if($<i>1 == VOID) {
		err("tip parametra ne sme biti void");
	}
         
	 
         par_num++;
	 insert_symbol($2,PAR,$1,par_num,NO_ATR);
	 set_par(fun_idx,par_num,$1);
         
   }
 | parametar _ZAREZ _TIP _ID
  {	
	if($<i>3 == VOID) {
		err("tip parametra ne sme biti void");
	}

	par_num++;
	insert_symbol($4,PAR,$3,par_num,NO_ATR);
	set_par(fun_idx,par_num,$3);
   }
 ;

telo_funkcije
 : _LVITICASTA lista_promenljivih 
   {
     if(var_num > 0 || niz_num > 0) {
         
	code("\n\t\tSUBS\t%%15,$%d,%%15", 4*(var_num + suma_duzina));
     }
     code("\n@%s_body:", get_name(fun_idx));
    
    


     
   }
    lista_naredbi  _DVITICASTA
 ;

lista_promenljivih
 :
 | lista_promenljivih promenljiva
 ;

promenljiva
 : _TIP 
   {
       if($<i>1 == VOID) {
		err("tip promenljive ne sme biti void");
	}
       var_type = $1;
   }
  dekl_prom _TZAREZ
 ;

dekl_prom
 : _ID 
  {
       if(lookup_symbol($1, VAR|PAR|NIZ) == NO_INDEX)  {
	   insert_symbol($1, VAR, var_type, ++var_num, 1);

       }
        else 
           err("redefinicija promenljive '%s'", $1);
  }
   
 | dekl_prom _ZAREZ _ID

  {
       if(lookup_symbol($3, VAR|PAR|NIZ) == NO_INDEX)  {
         
         insert_symbol($3, VAR, var_type, ++var_num, NO_ATR);

       }
        else 
           err("redefinicija promenljive '%s'", $3);
  }
 | _ID _LUGLASTA literal _DUGLASTA 
  {
	char* ime = get_name($3);
	long int num = atol(ime);
	if(lookup_symbol($1, VAR|PAR|NIZ) == NO_INDEX)  {
         
         	insert_symbol($1, NIZ, var_type, ++var_num,num);
		niz_num++;
		suma_duzina+=(num-1);

       }
        else 
           err("redefinicija promenljive '%s'", $1);
	if(num < 0) 
		err("Ne sme biti negativan broj clanova niza");
	int idx = lookup_symbol($1, NIZ);
	nizovi[niz_num].idx = idx;
	nizovi[niz_num].duzina = num;
	
  }
 ;

lista_naredbi
 :
 | lista_naredbi naredba
 ;

naredba
 : blok_naredba
 | naredba_dodele
 | if_naredba
 | inc_naredba
 | pp_oiai_naredba
 | return_naredba
 | pp_jiro_iskaz
 | dec_naredba
 ;

blok_naredba
 : _LVITICASTA lista_naredbi _DVITICASTA
 ;

naredba_dodele
 : _ID _DODELA broj_exp _TZAREZ
   {
	int idx = lookup_symbol($1,VAR|PAR|GLOBAL);
	if(idx == NO_INDEX)  {
           err("nedefinisana leva vrednost '%s' u dodeli",$1);

        }
	else   { 
	  if(get_type(idx) != get_type($3))  {
	     err("nekompatibilne vrednosti u dodeli");
           }

        } 
        d_inc = idx;
	gen_mov($3,idx);
  }
  | _ID _LUGLASTA literal _DUGLASTA _DODELA broj_exp _TZAREZ
  {
	int idx = lookup_symbol($1,NIZ);
	if(idx == NO_INDEX) {
		err("Nedefinisana leva strana '%s' u dodeli", $1);

	}
	if(get_type(idx) != get_type($3)) {
	    err("nekompatibilne vrednosti u dodeli");
		
	} 
        char* ime = get_name($3);
	long int num = atol(ime);
	if(num < 0 || num > get_atr2(idx)) {
		err("indeks niza mora biti u opsegu");
	}
	gen_mov_niz($6,idx,num);
	
  }
  ;  

broj_exp_pp
  : broj_exp
   {
	if(inc_b > 0) {
	   for(int i=1;i<=inc_b;i++) {
                int idx = inc_prom[i];
		if(get_type(idx) == INT) {
			
		                code("\n\t\tADDS\t");
				gen_sym_name(idx);
				code(",");
				code("$1");
				code(",");
				gen_sym_name(idx);
 		} else if(get_type(idx) == UINT) {
 
				code("\n\t\tADDU\t");
				gen_sym_name(idx);
				code(",");
				code("$1");
				code(",");
				gen_sym_name(idx);


		 }  else {
			err("Nepravilna upotreba postinkrement operatora");
		 }
          }
       }
       for(int i=0;i<inc_b;i++) {
	  inc_prom[i] = 0;
       }
       inc_b = 0;
       d_inc = 0;
       if(dec_b > 0) {
	   for(int i=1;i<=dec_b;i++) {
                int idx = dec_prom[i];
		if(get_type(idx) == INT) {
			
		                code("\n\t\tSUBS\t");
				gen_sym_name(idx);
				code(",");
				code("$1");
				code(",");
				gen_sym_name(idx);
 		} else if(get_type(idx) == UINT) {
 
				code("\n\t\tSUBU\t");
				gen_sym_name(idx);
				code(",");
				code("$1");
				code(",");
				gen_sym_name(idx);


		 }  else {
			err("Nepravilna upotreba postdekrement operatora");
		 }
          }
       }
       for(int i=0;i<dec_b;i++) {
	  dec_prom[i] = 0;
       }
       dec_b = 0;
       d_dec = 0;
       
    
   }
  ;
       
  

broj_exp
 : exp
 | broj_exp _AOP exp
   {
	if(get_type($1)!=get_type($3))
	  err("nekompatibilni operandi po tipu : aritmeticka operacija");
	 int t1 = get_type($1);    
        code("\n\t\t%s\t", ar_instructions[$2 + (t1 - 1) * AROP_NUMBER]);
	gen_sym_name($1);
        code(","); 
        gen_sym_name($3);
        code(",");
        free_if_reg($3);
        free_if_reg($1);
        $$ = take_reg();
        gen_sym_name($$);
        set_type($$, t1);
   }
 ;

exp
 : poziv_funkcije
   {
     $$ = take_reg();
     gen_mov(FUN_REG, $$);
   }
 | _LZAGRADA broj_exp _DZAGRADA
   {
	$$ = $2;
   }
 | inc_exp
 | pp_kondicionalni_izraz
 | izraz
 | dec_exp
 ;

literal
 : _INT
   {
	int idx_lit = insert_literal($1,INT);
	$$ = idx_lit;
	if(idx_lit == -1) {
		idx_lit = lookup_symbol($1,LIT);

	}
        if(jiro_lit == 1) {
                int pojava;
		pojava = set_lit(jiro_idx,$1);
		niz_indeksa[jiro_idx] = idx_lit;
		if(jiro_tip != INT) {
		  err("Nekompatibilne vrednosti literala i promenljive");
                }
		if(pojava == 1) {
		  err("Literal se vec pojavio u jiro iskazu");
               }
            jiro_lit = 0;    
        }
	if(pojava_kond1 == 1) {

		kond_tip1 = get_type(idx_lit);
		lit_idx = idx_lit;
		pojava_kond1 = 0;
	}
	 if(pojava_kond2 == 1) {
		kond_tip2 = get_type(idx_lit);
		lit_idx = idx_lit;
		pojava_kond2 = 0;
        }
	
        
   }
 | _UINT
   {
	int idx_lit = insert_literal($1,UINT);
        $$ = idx_lit;
	if(idx_lit == -1) {
		idx_lit = lookup_symbol($1,LIT);

	}
        if(jiro_lit == 1) {
		int pojava;
		pojava = set_lit(jiro_idx,$1);
		niz_indeksa[jiro_idx] = idx_lit;
		if(jiro_tip != UINT) {
		  err("Nekompatibilne vrednosti literala i promenljive");
                }
		if(pojava == 1) {
		  err("Literal se vec pojavio u jiro iskazu");
               }
           jiro_lit = 0;     
        }
	if(pojava_kond1 == 1) {

		kond_tip1 = get_type(idx_lit);
                lit_idx = idx_lit;
		pojava_kond1 = 0;
		
	}
        if(pojava_kond2 == 1) {
		kond_tip2 = get_type(idx_lit);
		lit_idx = idx_lit;
		pojava_kond2 = 0;
        }
	
	
   }
 ;

poziv_funkcije
 : _ID 
  {
	fcall_idx = lookup_symbol($1,FUN);
	if(fcall_idx == NO_INDEX)
	  err("'%s' nije funkcija" , $1);

  }

  _LZAGRADA argument _DZAGRADA
  {
	if(get_atr1(fcall_idx) != $4)
	  err("pogresan broj argumenata u pozivu funkcije '%s'", get_name(fcall_idx));
	set_type(FUN_REG,get_type(fcall_idx));
        $$ = FUN_REG;
        code("\n\t\t\tCALL\t%s", get_name(fcall_idx));
        if($4 > 0)
          code("\n\t\t\tADDS\t%%15,$%d,%%15", $4 * 4);
        set_type(FUN_REG, get_type(fcall_idx));
        $$ = FUN_REG;
	arg_num = 0;
  }
 ;

argument
 :
  {
	$$ = 0;
  }
 | argumenti_funkcije_vise_promenljivih 
  {
	$$ = arg_num;
  }
 ;

argumenti_funkcije_vise_promenljivih
  : broj_exp_pp
    {
	if(get_type_par(fcall_idx,++arg_num) != get_type($1))  {
	  err("nekompatibilne vrednosti argumenata u pozivu funkcije '%s'",get_name(fcall_idx));
        }
        free_if_reg($1);
        code("\n\t\t\tPUSH\t");
        gen_sym_name($1);
   

   }
  | argumenti_funkcije_vise_promenljivih _ZAREZ broj_exp_pp
   {
	if(get_type_par(fcall_idx,++arg_num) != get_type($3))  {
	  err("nekompatibilne vrednosti argumenata u pozivu funkcije '%s'",get_name(fcall_idx));
        }

        free_if_reg($3);
        code("\n\t\t\tPUSH\t");
        gen_sym_name($3);
   }
  ;

if_naredba
 : if_part %prec ONLY_IF
    { code("\n@exit%d:", $1); }
 | if_part _ELSE naredba
    { code("\n@exit%d:", $1); }
 ;

if_part
 : _IF _LZAGRADA 
   {
	oznaka_op = 0;
        $<i>$ = ++lab_num;
        code("\n@if%d:", lab_num);
   }
   pp_prosireni_uslov
   {
       
        code("\n@true%d:", $<i>3);
   }
   _DZAGRADA naredba
   {
        code("\n\t\tJMP \t@exit%d", $<i>3);
        code("\n@false%d:", $<i>3);
        $$ = $<i>3;
   }
 ;

rel_exp
 : broj_exp_pp _ROP broj_exp_pp
  {
	if(get_type($1) != get_type($3))
	   err("nekompatibilni operandi po tipu : relacioni operator");
        $$ = $2 + ((get_type($1) - 1) * RELOP_NUMBER);
	p_num++;
	if(pp_ind == 0) {
		broj_pp++;
	}
        pp_ind++;
	code("\n@provera_uslova%d:", p_num);
        gen_cmp($1, $3);
  }
 ;

pp_prosireni_uslov
 : prosireni_uslov
   { 
     code("\n\t\t%s\t@prosireni_uslov_exit_false%d", opp_jumps[$1], broj_pp);
     code("\n@prosireni_uslov_exit_true%d:", broj_pp);
     if(oznaka_op == 0) {
	code("\n\t\tJMP \t@true%d", lab_num);
     } else if(oznaka_op == 1) {
	code("\n\t\tJMP \t@oiai_naredba%d", oiai_num);
     } else {
	code("\n\t\tJMP \t@kondicionalni_izraz_true%d", ++kond_num);
	
	
     }
     code("\n@prosireni_uslov_exit_false%d:", broj_pp); 
     if(oznaka_op == 0) {
	code("\n\t\tJMP \t@false%d", lab_num);
     } else if(oznaka_op == 1) {
	code("\n\t\tJMP \t@exit_oiai%d", oiai_num);
     } else {
	code("\n\t\tJMP \t@kondicionalni_izraz_false%d", kond_num);
	
     }
     pp_ind = 0;

   }
 ;

prosireni_uslov
 : rel_exp
 | prosireni_uslov _LOP
   {
	if($<i>2 == AND) {

                code("\n\t\t%s\t@prosireni_uslov_exit_false%d", opp_jumps[$1], broj_pp);
			
        } else if($<i>2 == OR) {

	       code("\n\t\t%s\t@prosireni_uslov_exit_true%d", jumps[$1], broj_pp);
        }
        
   }
   rel_exp
 ;

return_naredba
 : _RETURN broj_exp_pp _TZAREZ
  {
	if(get_type(fun_idx) != get_type($2))
	  err("nekompatibilni tip u naredbi return");
	if(get_type(fun_idx) == VOID) {
	  err("ne postoji povratna vrednost iz void funkcije");
	}
	ret_pojava = 1;
        gen_mov($2, FUN_REG);
        code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));
  }
 | _RETURN _TZAREZ
  {
	if(get_type(fun_idx) != VOID) {
	  warning("povratna vrednost iz funkcije mora biti int/unsigned");
	}
	ret_pojava = 1;
        code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));
  }
 | _RETURN _ID _LUGLASTA  literal _DUGLASTA _TZAREZ
  {
	if(get_type(fun_idx) == VOID) {
	  err("ne postoji povratna vrednost iz void funkcije");
	}
	ret_pojava = 1;
	int idx = lookup_symbol($2,NIZ);
	if(get_type(fun_idx) != get_type(idx))
	  err("nekompatibilni tip u naredbi return");
	if(idx == NO_INDEX) {
		err("Nedefinisan niz '%s' u return naredbi", $2);

	}
	char* ime = get_name($4);
	long int num = atol(ime);
	if(num < 0 || num > get_atr2(idx)) {
		err("indeks niza mora biti u opsegu");
	}
	gen_mov_niz(idx, FUN_REG,num);
	code("\n\t\tJMP \t@%s_exit", get_name(fun_idx));

  }
 ;

inc_naredba
 : _ID _INC 
   {   	
        int inc_idx = lookup_symbol($1,VAR|PAR|GLOBAL);
	if(inc_idx == NO_INDEX) 
	  err("nepravilna upotreba postinkrement operatora");
	if(get_type(inc_idx) == INT) {

		code("\n\t\tADDS\t");
                gen_sym_name(inc_idx);
                code(",");
                code("$1");
                code(",");
                gen_sym_name(inc_idx);
         } else if(get_type(inc_idx) == UINT) {
		code("\n\t\tADDU\t");
                gen_sym_name(inc_idx);
                code(",");
                code("$1");
                code(",");
                gen_sym_name(inc_idx);

         }  else {
		err("nepravilna upotreba postinkrement operatora");
         }
 
   }
  _TZAREZ
 | _ID _LUGLASTA literal _DUGLASTA _INC _TZAREZ
 { int idx = lookup_symbol($1,NIZ);
	if(idx == NO_INDEX) {
		err("Nedefinisana leva strana '%s' u dodeli", $1);

	}
	if(get_type(idx) != get_type($3)) {
	    err("nekompatibilne vrednosti u dodeli");
		
	} 
        char* ime = get_name($3);
	long int num = atol(ime);
	if(num < 0 || num > get_atr2(idx)) {
		err("indeks niza mora biti u opsegu");
	}
	if(get_type(idx) == INT) {

		code("\n\t\tADDS\t");
                gen_sym_name_niz(idx,num);
                code(",");
                code("$1");
                code(",");
                gen_sym_name_niz(idx,num);
         } else if(get_type(idx) == UINT) {
		code("\n\t\tADDU\t");
                gen_sym_name_niz(idx,num);
                code(",");
                code("$1");
                code(",");
                gen_sym_name_niz(idx,num);

         }  else {
		err("nepravilna upotreba postinkrement operatora");
         }

   }
	
 ;

inc_exp
  : _ID _INC
   {
        $$ = lookup_symbol($1,VAR|PAR|GLOBAL);
	if($$ == NO_INDEX) 
	  err("nepravilna upotreba postinkrement operatora");
        int idx = lookup_symbol($1,VAR|PAR|GLOBAL);
        inc_b++;
        inc_prom[inc_b] = idx;
   }
  ;

pp_oiai_naredba
 : oiai_naredba
   { code("\n@exit_oiai%d:", $1); }
 ;

oiai_naredba
 :  _OIAI
   {     
        $<i>$ = ++oiai_num;
	oznaka_op = 1;
        code("\n@provera_oiai%d:", oiai_num);
   }
   pp_prosireni_uslov
   {
	code("\n\t\t%s\t@exit_oiai%d", opp_jumps[$3], $<i>2);
        code("\n@oiai_naredba%d:", $<i>2);

   }
   _HANA 
   naredba
   {
        code("\n\t\tJMP \t@provera_oiai%d", $<i>2);
        $$ = $<i>2;

   }
 ;

pp_jiro_iskaz
 : jiro_iskaz
  {  
	code("\n@provere_jiro%d:",$1);
	for(int i=0;i<=jiro_idx;i++) {
		int p = niz_indeksa[i];
		gen_cmp(jiro_id,p);
		code("\n\t\tJEQ \t@tranga_deo%d",i);
        }
	if(toerana_p==1) {
		code("\n\t\tJMP \t@toerana_deo%d", 1);
        } else {
		
		code("\n\t\tJMP \t@exit_jiro%d", jiro_num);
        }
	code("\n@exit_jiro%d:", $1); 
	brisanje();
	brisanje_indeksa();
	jiro_idx = 0;
	toerana_p = 0;
	
  }
 ;

jiro_iskaz
 :  _JIRO
   {
	$<i>$ = ++jiro_num;
	code("\n@jiro%d:", jiro_num);
	code("\n\t\tJMP \t@provere_jiro%d", jiro_num);

    }

    _LZAGRADA _ID
    {
 	 jiro_id = lookup_symbol($4,VAR|PAR);
	 jiro_tip = get_type(jiro_id);
	 if(jiro_id == NO_INDEX)  {
           err("nedefinisano ime promenljive '%s' u jiro naredbi",$4);

        } 
    }
   _DZAGRADA _LVITICASTA tranga_delovi toerana _DVITICASTA
   {	
	$$ = $<i>2;

   }
 ;

tranga_delovi
 : tranga_deo
 | tranga_delovi tranga_deo 
 ;

tranga_deo
 : _TRANGA 
   {
	jiro_lit = 1;
	jiro_idx++;
        code("\n@tranga_deo%d:", ++tranga_num);
   }
    literal  _IMPL naredba
 | tranga_deo _FINISH _TZAREZ
   {	
	code("\n\t\tJMP \t@exit_jiro%d", jiro_num);
   }
 ;

toerana
 : /*prazno*/
 | toerana_deo
 ;

toerana_deo
 : _TOERANA
  { 
      toerana_p++;
      code("\n@toerana_deo%d:", jiro_num); 

   }
   _IMPL naredba
   {
	code("\n\t\tJMP \t@exit_jiro%d", jiro_num);
   }
 ;

pp_kondicionalni_izraz
 : kondicionalni_izraz
   {  
      code("\n@exit_kondicionalni_izraz%d:", kond_num);
      $$ = reg;
      
   }
 ;

kondicionalni_izraz
 :_LZAGRADA  pp_prosireni_uslov 
   {
	code("\n@kondicionalni_izraz%d:", kond_num);
	reg = take_reg();
   }

   _DZAGRADA _UPITNIK
   {
	pojava_kond1 = 1;
	code("\n@kondicionalni_izraz_true%d:", kond_num);

   }
   izraz
   {	
       
	reg1 = $<i>7;
	gen_mov(reg1,reg);
	code("\n\t\tJMP \t@exit_kondicionalni_izraz%d", kond_num);
      
   }
   _DTACKA
   {
	pojava_kond2 = 1;
	code("\n@kondicionalni_izraz_false%d:", kond_num);
   }
   izraz
   {	
	if(kond_tip1 != kond_tip2) {
		err("Izrazi u kondicionalnom izrazu moraju biti istog tipa");
        }
	
	reg2 = $<i>11;
	gen_mov(reg2,reg);
   }
 ;

izraz
 : literal
 | _ID
  {
	$$ = lookup_symbol($1,VAR|PAR|GLOBAL);
	if($$ == NO_INDEX)  {
	  err("'%s' je nedeklarisan", $1);
	} 
	if(pojava_kond1 == 1) {
		kond_tip1 = get_type($$);
		id_idx = $$;
		pojava_kond1 = 0;
	}
	if(pojava_kond2 == 1) {
		kond_tip2 = get_type($$);
		id_idx = $$;
		pojava_kond2 = 0;
	}
	
  }
 ;
dec_naredba
 : _ID _DEC 
   {  	
        int dec_idx = lookup_symbol($1,VAR|PAR|GLOBAL);
	if(dec_idx == NO_INDEX) 
	  err("nepravilna upotreba postdekrement operatora");
	if(get_type(dec_idx) == INT) {

		code("\n\t\tSUBS\t");
                gen_sym_name(dec_idx);
                code(",");
                code("$1");
                code(",");
                gen_sym_name(dec_idx);
         } else if(get_type(dec_idx) == UINT) {
		code("\n\t\tSUBU\t");
                gen_sym_name(dec_idx);
                code(",");
                code("$1");
                code(",");
                gen_sym_name(dec_idx);

         }  else {
		err("nepravilna upotreba postinkrement operatora");
         }
 
   }


  _TZAREZ
 | _ID _LUGLASTA literal _DUGLASTA _DEC _TZAREZ
  {
	int idx = lookup_symbol($1,NIZ);
	if(idx == NO_INDEX) {
		err("Nedefinisana leva strana '%s' u dodeli", $1);

	}
	if(get_type(idx) != get_type($3)) {
	    err("nekompatibilne vrednosti u dodeli");
		
	} 
        char* ime = get_name($3);
	long int num = atol(ime);
	if(num < 0 || num > get_atr2(idx)) {
		err("indeks niza mora biti u opsegu");
	}
	if(get_type(idx) == INT) {

		code("\n\t\tSUBS\t");
                gen_sym_name_niz(idx,num);
                code(",");
                code("$1");
                code(",");
                gen_sym_name_niz(idx,num);
         } else if(get_type(idx) == UINT) {
		code("\n\t\tSUBU\t");
                gen_sym_name_niz(idx,num);
                code(",");
                code("$1");
                code(",");
                gen_sym_name_niz(idx,num);

         }  else {
		err("nepravilna upotreba postinkrement operatora");
         }
	

  }
 ;

dec_exp 
 : _ID _DEC
   {
        $$ = lookup_symbol($1,VAR|PAR|GLOBAL);
	if($$ == NO_INDEX) 
	  err("nepravilna upotreba postinkrement operatora");
        int idx = lookup_symbol($1,VAR|PAR|GLOBAL);
        dec_b++;
        dec_prom[dec_b] = idx;
	
   }
 ;


%%

int yyerror(char *s) {
  fprintf(stderr, "\nline %d: ERROR: %s", yylineno, s);
  error_count++;
  return 0;
}

void warning(char *s) {
  fprintf(stderr, "\nline %d: WARNING: %s", yylineno, s);
  warning_count++;
}

int main() {
  int synerr;
  init_symtab();
  output = fopen("output.asm","w+");

  synerr = yyparse();

  clear_symtab();
  fclose(output);
  
  if(warning_count)
    printf("\n%d warning(s).\n", warning_count);

  if(error_count) {
    remove("output.asm");
    printf("\n%d error(s).\n", error_count);

  }

  if(synerr)
    return -1; //syntax error
  else if(error_count)
    return error_count & 127;
  else if (warning_count)
    return (warning_count & 127) + 127;
  else
    return 0;
}

