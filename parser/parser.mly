%{

open Ast
open Location

%}

%token NODE ROM RAM WHERE CONST
%token LPAREN RPAREN MUX COLON COMMA EQUAL REG OR XOR NAND AND POWER SLASH
%token EOF RBRACKET LBRACKET GREATER LESS NOT SEMICOL PLUS MINUS STAR
%token <string> NAME
%token <int> INT
%token <bool> BOOL

%left OR
%left EQUAL
%right PLUS MINUS
%left NAND XOR AND
%left STAR
%right REG
%right POWER

%start program
%type <Ast.program> program

%%

/** Tools **/
%inline slist(S, x)        : l=separated_list(S, x)                    {l}
%inline snlist(S, x)       : l=separated_nonempty_list(S, x)           {l}
%inline tuple(x)           : LPAREN h=x COMMA t=snlist(COMMA,x) RPAREN { h::t }
%inline option(P,x):
  |/* empty */    { None }
  | P v=x         { Some(v) }

localize(x): y=x { y, (Loc($startpos(y),$endpos(y))) }

program:
  | c=const_decs n=node_decs EOF
      { mk_program c n }

const_decs: c=list(const_dec) {c}
const_dec:
  | CONST n=name EQUAL se=static_exp
      { mk_const_dec ~loc:(Loc($startpos,$endpos)) n se }

name: n=NAME { n }

type_ident: LBRACKET se=static_exp RBRACKET { TBitArray se }

node_decs: ns=list(node_dec) {ns}
node_dec:
  NODE n=name LPAREN args=args RPAREN
  EQUAL LPAREN out=args RPAREN WHERE eqs=equs
      { mk_node n (Loc ($startpos,$endpos)) args out eqs }

args: vl=slist(COMMA, arg) { vl }

arg:
  | n=NAME COLON t=type_ident { mk_var_dec n t }
  | n=NAME { mk_var_dec n TBit }

equs: e=slist(SEMICOL, equ) { e }
equ: p=pat EQUAL e=exp { mk_equation p e }

pat:
  | n=NAME                             { Evarpat n }
  | LPAREN p=snlist(COMMA, NAME) RPAREN { Etuplepat p }

static_exp :
  | i=INT { SConst i }
  | n=NAME { SVar n }
  | se1=static_exp POWER se2=static_exp { SBinOp(SPower, se1, se2) }
  | se1=static_exp PLUS se2=static_exp { SBinOp(SAdd, se1, se2) }
  | se1=static_exp MINUS se2=static_exp { SBinOp(SMinus, se1, se2) }
  | se1=static_exp STAR se2=static_exp { SBinOp(SMult, se1, se2) }

exps: LPAREN e=slist(COMMA, exp) RPAREN {e}

exp: e=_exp { mk_exp e (Loc ($startpos,$endpos)) }
_exp:
  | n=NAME                    { Evar n }
  | LPAREN e=_exp RPAREN      { e }
  | c=const                   { Econst c }
  | op=op a=exps              { Eapp(op, a) }
  | e1=exp op=infix_prim e2=exp { Eapp(OPrim op, [e1; e2])}
  | op=prefix_prim a=exps     { Eapp(OPrim op, a)}

const:
  | b=BOOL { VBit b }

rom_or_ram :
  | ROM { true }
  | RAM { false }

infix_prim:
  | OR { POr }
  | AND { PAnd }
  | XOR { PXor }
  | NAND { PNand }

prefix_prim:
  | NOT { PNot }
  | MUX { PMux }

op:
  | REG { OReg }
  | ro=rom_or_ram LESS addr_size=INT COMMA word_size=INT GREATER
    { OMem(ro, addr_size, word_size) }
  | n=NAME { OCall n }

%%
