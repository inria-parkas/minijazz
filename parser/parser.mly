%{

open Static
open Ast
open Location
open Misc

%}

%token INLINED NODE ROM RAM WHERE CONST PROBING
%token LPAREN RPAREN MUX COLON COMMA EQUAL REG OR XOR NAND AND POWER SLASH
%token EOF RBRACKET LBRACKET GREATER LESS NOT SEMICOL PLUS MINUS STAR
%token IF THEN ELSE LEQ DOT DOTDOT
%token <string> NAME
%token <string> STRING
%token <int> INT
%token <string> BOOL_INT
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

node_decs: ns=list(node_dec) { ns }
node_dec:
  inlined=inlined_status NODE n=name p=params LPAREN args=args RPAREN
  EQUAL LPAREN out=args RPAREN WHERE b=block probes=probe_decls
      { mk_node n (Loc ($startpos,$endpos)) inlined args out p b probes }

inlined_status:
  | INLINED { Inlined }
  | /*empty*/ { NotInlined }

params:
  | /*empty*/ { [] }
  | LESS pl=snlist(COMMA,param) GREATER { pl }

param:
  n=NAME { mk_param n }

args: vl=slist(COMMA, arg) { vl }

arg:
  | n=NAME COLON t=type_ident { mk_var_dec n t }
  | n=NAME { mk_var_dec n TBit }

block:
  | eqs=equs { BEqs (eqs, []) }
  | IF se=static_exp THEN thenb=block ELSE elseb=block { BIf(se, thenb, elseb) }

equs: e=slist(SEMICOL, equ) { e }
equ: p=pat EQUAL e=exp { mk_equation p e }

pat:
  | n=NAME                              { Evarpat n }
  | LPAREN p=snlist(COMMA, NAME) RPAREN { Etuplepat p }

static_exp :
  | i=INT { SInt i }
  | n=NAME { SVar n }
  | LPAREN se=static_exp RPAREN { se }
  /*integer ops*/
  | se1=static_exp POWER se2=static_exp { SBinOp(SPower, se1, se2) }
  | se1=static_exp PLUS se2=static_exp { SBinOp(SAdd, se1, se2) }
  | se1=static_exp MINUS se2=static_exp { SBinOp(SMinus, se1, se2) }
  | se1=static_exp STAR se2=static_exp { SBinOp(SMult, se1, se2) }
  | se1=static_exp SLASH se2=static_exp { SBinOp(SDiv, se1, se2) }
  /*bool ops*/
  | se1=static_exp EQUAL se2=static_exp { SBinOp(SEqual, se1, se2) }
  | se1=static_exp LEQ se2=static_exp { SBinOp(SLeq, se1, se2) }

exps: LPAREN e=slist(COMMA, exp) RPAREN {e}

exp: e=_exp { mk_exp ~loc:(Loc ($startpos,$endpos)) e }
_exp:
  | n=NAME                    { Evar n }
  | LPAREN e=_exp RPAREN      { e }
  | c=const                   { Econst c }
  | op=op a=exps              { Eapp(op, a) }
  | e1=exp op=infix_prim e2=exp { Eapp(OCall (op, []), [e1; e2])}
  | op=prefix_prim a=exp     { Eapp(OCall (op, []), [a])}
  | MUX ce=exp THEN fe=exp ELSE te=exp { Eapp(OCall ("mux", []), [ce; fe; te]) }
  | e1=exp DOT e2=exp               { Eapp(OConcat, [e1; e2]) }
  | e1=exp LBRACKET idx=static_exp RBRACKET { Eapp(OSelect idx, [e1]) }
  | e1=exp LBRACKET low=static_exp DOTDOT high=static_exp RBRACKET
    { Eapp(OSlice(low, high), [e1]) }

const:
  | b=BOOL { VBit b }
  | b=BOOL_INT { VBitArray (bool_array_of_string b) }

rom_or_ram :
  | ROM { MRom }
  | RAM { MRam }

infix_prim:
  | OR { "or" }
  | AND { "and" }
  | XOR { "xor" }
  | NAND { "nand" }

prefix_prim:
  | NOT { "not" }

op:
  | REG { OReg }
  | ro=rom_or_ram LESS addr_size=static_exp
    COMMA word_size=static_exp input_file=option(COMMA, STRING) GREATER
    { OMem(ro, addr_size, word_size, input_file) }
  | n=NAME p=call_params { OCall (n,p) }

call_params:
  | /*empty*/ { [] }
  | LESS pl=snlist(COMMA,static_exp) GREATER { pl }

probe_decls:
  | /*empty*/ { [] }
  | PROBING l=separated_nonempty_list(COMMA, NAME) { l }
%%
