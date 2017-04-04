//Copyright 2017 Andrey S. Ionisyan (anserion@gmail.com)
//
//Licensed under the Apache License, Version 2.0 (the "License");
//you may not use this file except in compliance with the License.
//You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//Unless required by applicable law or agreed to in writing, software
//distributed under the License is distributed on an "AS IS" BASIS,
//WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//See the License for the specific language governing permissions and
//limitations under the License.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    Button_exit: TButton;
    CheckGroupA: TCheckGroup;
    CheckGroup_apb_pow2: TCheckGroup;
    CheckGroup_apb_pow3: TCheckGroup;
    CheckGroup_C: TCheckGroup;
    CheckGroup_b7: TCheckGroup;
    CheckGroup_apb_pow3_minus_b7: TCheckGroup;
    CheckGroup_apb_pow3_minus_b7_plus_a: TCheckGroup;
    CheckGroup_D: TCheckGroup;
    CheckGroupB: TCheckGroup;
    CheckGroup_AplusB: TCheckGroup;
    Memo_help: TMemo;
    procedure Button_exitClick(Sender: TObject);
    procedure CheckGroupAItemClick(Sender: TObject; Index: LongInt);
    procedure CheckGroupBItemClick(Sender: TObject; Index: LongInt);
  private
    { private declarations }
  public
    { public declarations }
    procedure calc_formula;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
uses math;

const sqrt2=1.414213562;
      sqrt3=1.732050808;
      sqrt5=2.236067977;

type
TComplex=record
   re,im:real;
end;

TComplexVector=array of TComplex;
TComplexMatrix=array of TComplexVector;

TIntegerComplex=record
   re,im:integer;
end;
TIntegerComplexVector=array of TIntegerComplex;
TIntegerComplexMatrix=array of TIntegerComplexVector;

type
   tbit=(zero,one);
   tbit_vector=array of tbit;
   tbit_table=array of tbit_vector;
   tqbit=record p0,p1: tcomplex; end;
   tqregister=TComplexVector;

   //====================================================================
   //битовые операции
   function get_bit(n,k:integer):integer;
   begin get_bit:=(n shr k) and 1; end;

   function get_bits(n,k,nn:integer):integer;
   var mask:integer;
   begin mask:=(1 shl nn)-1; get_bits:=(n shr k) and mask; end;

   function set_bit(n,k,value:integer):integer;
   begin set_bit:=(n and (not (1 shl k))) or (value shl k); end;

   function insert_bit(n,k,value:integer):integer;
   var mask:integer;
   begin
      mask:=(1 shl k)-1;
      insert_bit:=((((n shr k)shl 1)or value) shl k) or (n and mask);
   end;

   function insert_bits(n,k,value,n_value:integer):integer;
   var mask:integer;
   begin
      mask:=(1 shl k)-1;
      insert_bits:=((((n shr k)shl n_value)or value) shl k) or (n and mask);
   end;

   function delete_bit(n,k:integer):integer;
   var mask:integer;
   begin
      mask:=(1 shl k)-1;
      delete_bit:=((n shr (k+1))shl k) or (n and mask);
   end;

   //функция вычисляет значение 2^n
   function pow2(n:integer):integer; begin pow2:=1 shl n; end;

   //функция вычисляет ближайшее меньшее целое значение log_2(n)
   function log2(n:integer):integer;
   var res:integer;
   begin
      res:=0;
      while n>1 do begin n:=n>>1; res:=res+1; end;
      log2:=res;
   end;

   //функция выполняет округление числа N вверх до ближайшей степени двойки
   function Power2RoundUp(N:integer):integer;
   var NN:integer;
   begin
     NN:=(1<<log2(N)); if N>NN then NN:=NN<<1;
     Power2RoundUp:=NN;
   end;

   //====================================================================
function c_complex(re,im:real):TComplex;
begin c_complex.re:=re; c_complex.im:=im; end;

function c_vector(n:integer):TComplexVector;
var tmp:TComplexVector; begin setlength(tmp,n); c_vector:=tmp; end;

function c_matrix(n,m:integer):TComplexMatrix;
var i:integer; tmp:TComplexMatrix;
begin
   setlength(tmp,n);
   for i:=0 to n-1 do setlength(tmp[i],m);
   c_matrix:=tmp;
end;

procedure c_matrix_destroy(var M:TComplexMatrix);
var i,n:integer;
begin
   n:=length(M); for i:=0 to n-1 do setlength(M[i],0);
   setlength(M,0);
end;

function c_zero:TComplex; begin c_zero.re:=0; c_zero.im:=0; end;
function c_one:TComplex; begin c_one.re:=1; c_one.im:=0; end;
function c_minus_one:TComplex; begin c_minus_one.re:=-1; c_minus_one.im:=0; end;
function c_i:TComplex; begin c_i.re:=0; c_i.im:=1; end;
function c_minus_i:TComplex; begin c_minus_i.re:=0; c_minus_i.im:=-1; end;
function c_pi:TComplex; begin c_pi.re:=Pi; c_pi.im:=0; end;
function c_pi2:TComplex; begin c_pi2.re:=0.5*Pi; c_pi2.im:=0; end;
function c_pi4:TComplex; begin c_pi4.re:=0.25*Pi; c_pi4.im:=0; end;
function c_pi8:TComplex; begin c_pi8.re:=0.125*Pi; c_pi8.im:=0; end;
function c_sqrt2:TComplex; begin c_sqrt2.re:=sqrt2; c_sqrt2.im:=0; end;
function c_sqrt3:TComplex; begin c_sqrt3.re:=sqrt3; c_sqrt3.im:=0; end;
function c_sqrt5:TComplex; begin c_sqrt5.re:=sqrt5; c_sqrt5.im:=0; end;

function c_root_of_one_CCW(k,n:integer):TComplex;
var phi:real;
begin
     phi:=2*PI*k/n;
     c_root_of_one_CCW.re:=cos(phi);
     c_root_of_one_CCW.im:=sin(phi);
end;

function c_root_of_one_CW(k,n:integer):TComplex;
var phi:real;
begin
     phi:=-2*PI*k/n;
     c_root_of_one_CW.re:=cos(phi);
     c_root_of_one_CW.im:=sin(phi);
end;
//====================================================================

function c_dup(value:TComplex):TComplex;
begin c_dup.re:=value.re; c_dup.im:=value.im; end;

function c_conj(value:TComplex):TComplex;
begin c_conj.re:=value.re; c_conj.im:=-value.im; end;

function c_amp2(a:TComplex):real;
begin c_amp2:=sqr(a.re)+sqr(a.im); end;

function c_amp(a:TComplex):real;
begin c_amp:=sqrt(sqr(a.re)+sqr(a.im)); end;

function c_phi(a:TComplex):real;
var res,amp:real;
begin
     amp:=c_amp(a);
     if amp=0 then res:=0 else res:=arccos(a.re/amp);
     c_phi:=res;
end;

function c_TrigToAlg(amp,phi:real):TComplex;
begin c_TrigToAlg.re:=amp*cos(phi); c_TrigToAlg.im:=amp*sin(phi); end;

function c_amp_cmp(a,b:TComplex):integer;
var amp2_a,amp2_b:real; res:integer;
begin
     res:=0;
     amp2_a:=sqr(a.re)+sqr(a.im);
     amp2_b:=sqr(b.re)+sqr(b.im);
     if amp2_a>amp2_b then res:=1;
     if amp2_a=amp2_b then res:=0;
     if amp2_a<amp2_b then res:=-1;
     c_amp_cmp:=res;
end;

function c_neg(a:TComplex):TComplex;
begin c_neg.re:=-a.re; c_neg.im:=-a.im; end;

function c_inv(a:TComplex):TComplex;
var amp,inv_phi:real; res:TComplex;
begin
   amp:=c_amp(a);
   if amp=0 then res:=c_zero
      else
      begin
         inv_phi:=-arccos(a.re/amp);
         res:=c_TrigToAlg(1.0/amp,inv_phi);
      end;
   c_inv:=res;
end;

function c_add(a,b:TComplex):TComplex;
begin c_add.re:=a.re+b.re; c_add.im:=a.im+b.im; end;

function c_sub(a,b:TComplex):TComplex;
begin c_sub.re:=a.re-b.re; c_sub.im:=a.im-b.im; end;

function c_mul(a,b:TComplex):TComplex;
begin c_mul.re:=a.re*b.re-a.im*b.im; c_mul.im:=a.re*b.im+a.im*b.re; end;

function c_div(a,b:TComplex):TComplex;
begin
     c_div.re:=(a.re*b.re+a.im*b.im)/(b.re*b.re+b.im*b.im);
     c_div.im:=(a.im*b.re-a.re*b.im)/(b.re*b.re+b.im*b.im);
end;

procedure c_AlgToTrig(alg:TComplex; var amp,phi:real);
begin
     amp:=c_amp(alg);
     phi:=c_phi(alg);
end;

function c_sqr(arg:TComplex):TComplex;
begin c_sqr.re:=arg.re*arg.re-arg.im*arg.im; c_sqr.im:=2*arg.re*arg.im; end;

function c_exp_ix(x:real):TComplex;
begin c_exp_ix.re:=cos(x); c_exp_ix.im:=sin(x); end;

procedure c_sqrt(arg:TComplex; var res1,res2:TComplex);
var amp,phi:real;
begin
     amp:=sqrt(c_amp(arg));
     phi:=c_phi(arg);
     res1.re:=amp*cos(phi/2); res1.im:=amp*sin(phi/2);
     res2.re:=amp*cos(phi/2+PI); res2.im:=amp*sin(phi/2+PI);
end;

function c_exp(arg:TComplex):TComplex;
var exp_x:real;
begin
     exp_x:=exp(arg.re);
     c_exp.re:=exp_x*cos(arg.im);
     c_exp.im:=exp_x*sin(arg.im);
end;

function c_ln(arg:TComplex; k:integer):TComplex;
var amp,phi:real;
begin
     amp:=c_amp(arg);
     phi:=c_phi(arg);
     if amp>0 then amp:=ln(amp);
     c_ln.re:=amp;
     c_ln.im:=phi+2*PI*k;
end;

function c_power(arg,pow:TComplex; k:integer):TComplex;
begin
     c_power:=c_exp(c_mul(pow,c_ln(arg,k)));
end;

//-------------------------------------------------------------
procedure c_vector_fill(value:TComplex; var V:TComplexVector);
var i,n:integer;
begin n:=length(V); for i:=0 to n-1 do V[i]:=value; end;

procedure c_vector_copy(var src,dst:TComplexVector);
var i,n:integer;
begin n:=length(src); for i:=0 to n-1 do dst[i]:=src[i]; end;

procedure c_vectors_swap(var V1,V2:TComplexVector);
var i,n:integer; tmp:TComplex;
begin
   n:=length(V1);
   for i:=0 to n-1 do begin tmp:=V1[i]; V1[i]:=V2[i]; V2[i]:=tmp; end;
end;

procedure c_matrix_fill(value:TComplex; var A:TComplexMatrix);
var i,n:integer;
begin
   n:=length(A);
   for i:=0 to n-1 do c_vector_fill(value,A[i]);
end;

procedure c_matrix_copy(var src,dst:TComplexMatrix);
var i,n:integer;
begin n:=length(src); for i:=0 to n-1 do c_vector_copy(src[i],dst[i]); end;

procedure c_subvector_to_vector_put(k:integer; var subvector,V:TComplexVector);
var i,sn,n:integer;
begin
   sn:=length(subvector); n:=length(V);
   if k+sn>n then sn:=n-k;
   for i:=0 to sn-1 do V[i+k]:=subvector[i];
end;

procedure c_subvector_from_vector_get(k,n:integer; var subvector,V:TComplexVector);
var i,nn:integer;
begin
   nn:=length(V);
   if k+n>nn then n:=nn-n;
   for i:=0 to n-1 do subvector[i]:=V[k+i];
end;

procedure c_matrix_raw_put(k:integer; var A:TComplexMatrix; var V:TComplexVector);
var i,n:integer;
begin n:=length(V); for i:=0 to n-1 do A[k,i]:=V[i]; end;

procedure c_matrix_col_put(k:integer; var A:TComplexMatrix; var V:TComplexVector);
var i,m:integer;
begin m:=length(V); for i:=0 to m-1 do A[i,k]:=V[i]; end;

procedure c_matrix_raw_get(k:integer; var A:TComplexMatrix; var V:TComplexVector);
var i,n:integer;
begin n:=length(A[k]); for i:=0 to n-1 do V[i]:=A[k,i]; end;

procedure c_matrix_col_get(k:integer; var A:TComplexMatrix; var V:TComplexVector);
var i,m:integer;
begin m:=length(A); for i:=0 to m-1 do V[i]:=A[i,k]; end;

procedure c_submatrix_to_matrix_put(raw,col:integer; var submatrix,A:TComplexMatrix);
var i,j,n,m:integer;
begin
   n:=length(submatrix);  m:=length(submatrix[0]);
   for i:=0 to n-1 do
      for j:=0 to m-1 do
         A[raw+i,col+j]:=submatrix[i,j];
end;

procedure c_submatrix_from_matrix_get(raw,col,n,m:integer; var submatrix,A:TComplexMatrix);
var i,j:integer;
begin
   for i:=0 to n-1 do
      for j:=0 to m-1 do
         submatrix[i,j]:=A[raw+i,col+j];
end;

procedure c_matrix_transp(var A,res:TComplexMatrix);
var i,n,m:integer; tmp:TComplexVector;
begin
   n:=length(A); m:=length(A[0]); setlength(tmp,m);
   for i:=0 to n-1 do
   begin
      c_matrix_raw_get(i,A,tmp);
      c_matrix_col_put(i,res,tmp);
   end;
end;

procedure c_marix_hermitian(var A,res:TComplexMatrix);
var i,j,n,m:integer;
begin
   c_matrix_transp(A,res);
   n:=length(res); m:=length(res[0]);
   for i:=0 to n-1 do
   for j:=0 to m-1 do
      res[i,j].im:=-res[i,j].im;
end;
//-------------------------------------------------------------

function c_vector_summ(var a:TComplexVector):TComplex;
var tmp:TComplex; i,n:integer;
begin
   n:=length(a); tmp:=c_zero;
   for i:=0 to n-1 do tmp:=c_add(a[i],tmp);
   c_vector_summ:=tmp;
end;

function c_vector_prod(var a:TComplexVector):TComplex;
var tmp:TComplex; i,n:integer;
begin
   n:=length(a); tmp:=c_one;
   for i:=0 to n-1 do tmp:=c_mul(a[i],tmp);
   c_vector_prod:=tmp;
end;

function c_vector_dist2(var a:TComplexVector):TComplex;
var tmp:TComplex; i,n:integer;
begin
   n:=length(a); tmp:=c_zero;
   for i:=0 to n-1 do tmp:=c_add(c_mul(a[i],a[i]),tmp);
   c_vector_dist2:=tmp;
end;

function c_vector_mean(var a:TComplexVector):TComplex;
begin c_vector_mean:=c_div(c_vector_summ(a),c_complex(length(a),0)); end;

function c_vector_dispersion(var a:TComplexVector):TComplex;
var i,n:integer; average,tmp:TComplex;
begin
   n:=length(a);
   average:=c_vector_mean(a);
   tmp:=c_zero;
   for i:=0 to n-1 do tmp:=c_add(tmp,c_sqr(c_sub(a[i],average)));
   c_vector_dispersion:=c_div(tmp,c_complex(n,0));
end;

procedure c_vector_diff(var a,res:TComplexVector);
var i,n:integer; c_two:TComplex;
begin
   n:=length(a); c_two:=c_complex(2,0);
   res[0]:=c_sub(a[1],a[0]); res[n-1]:=c_sub(a[n-1],a[n-2]);
   for i:=1 to n-2 do
      res[i]:=c_div(c_sub(a[i+1],a[i-1]),c_two);
end;

procedure c_func_diff(var x,f,res:TComplexVector);
var i,n:integer;
begin
   n:=length(x);
   res[0]:=c_div(c_sub(f[1],f[0]),c_sub(x[1],x[0]));
   res[n-1]:=c_div(c_sub(f[n-1],f[n-2]),c_sub(x[n-1],x[n-2]));
   for i:=1 to n-2 do
      res[i]:=c_div(c_sub(f[i+1],f[i-1]),c_sub(x[i+1],x[i-1]));
end;

procedure c_vector_neg(var a,neg_a:TComplexVector);
var i,n:integer;
begin n:=length(a); for i:=0 to n-1 do neg_a[i]:=c_neg(a[i]); end;

procedure c_vector_add_vector(var a,b,c:TComplexVector);
var i,n:integer;
begin n:=length(a); for i:=0 to n-1 do c[i]:=c_add(a[i],b[i]); end;

procedure c_vector_sub_vector(var a,b,c:TComplexVector);
var i,n:integer;
begin n:=length(a); for i:=0 to n-1 do c[i]:=c_sub(a[i],b[i]); end;

function c_vectors_scalar_mul(var a,b:TComplexVector):TComplex;
var tmp:TComplex; i,n:integer;
begin
   n:=length(a); tmp:=c_zero;
   for i:=0 to n-1 do tmp:=c_add(c_mul(a[i],b[i]),tmp);
   c_vectors_scalar_mul:=tmp;
end;

function c_vectors_convolution(var a,b:TComplexVector):TComplex;
var tmp:TComplex; i,n:integer;
begin
   n:=length(a); tmp:=c_zero;
   for i:=0 to n-1 do tmp:=c_add(c_mul(a[i],b[n-i-1]),tmp);
   c_vectors_convolution:=tmp;
end;

procedure c_vector_add_scalar(lambda:TComplex; var a,res:TComplexVector);
var i,n:integer;
begin n:=length(a); for i:=0 to n-1 do res[i]:=c_add(lambda,a[i]); end;

procedure c_vector_mul_scalar(lambda:TComplex; var a,res:TComplexVector);
var i,n:integer;
begin n:=length(a); for i:=0 to n-1 do res[i]:=c_mul(lambda,a[i]); end;

procedure c_matrix_mul_scalar(lambda:TComplex; var a,res:TComplexMatrix);
var i,j,n,m:integer;
begin
   n:=length(a); m:=length(a[0]);
   for i:=0 to n-1 do
      for j:=0 to m-1 do
         res[i,j]:=c_mul(lambda,a[i,j]);
end;

procedure c_matrix_add_matrix(var a,b,c:TComplexMatrix);
var i,j,n,m:integer;
begin
   n:=length(a); m:=length(a[0]);
   for i:=0 to n-1 do
      for j:=0 to m-1 do
         c[i,j]:=c_add(a[i,j],b[i,j]);
end;

procedure c_matrix_sub_matrix(var a,b,c:TComplexMatrix);
var i,j,n,m:integer;
begin
   n:=length(a); m:=length(a[0]);
   for i:=0 to n-1 do
      for j:=0 to m-1 do
         c[i,j]:=c_sub(a[i,j],b[i,j]);
end;

procedure c_matrix_mul_vector(var A:TComplexMatrix; var V,res:TComplexVector);
var i,n:integer;
begin
   n:=length(A);
   for i:=0 to n-1 do res[i]:=c_vectors_scalar_mul(A[i],V);
end;

procedure c_vector_mul_matrix(var V:TComplexVector; var A:TComplexMatrix; var res:TComplexVector);
var i,n,m:integer; tmp:TComplexVector;
begin
   m:=length(V); n:=length(A); setlength(tmp,n);
   for i:=0 to m-1 do
   begin
      c_matrix_col_get(i,A,tmp);
      res[i]:=c_vectors_scalar_mul(V,tmp);
   end;
end;

procedure c_matrix_mul_matrix(var A,B,C:TComplexMatrix);
var i,n:integer;
begin
   n:=length(A);
   for i:=0 to n-1 do c_vector_mul_matrix(A[i],B,C[i]);
end;

procedure c_vectorv_kronmul_vectorh(var a,b:TComplexVector; var c:TComplexMatrix);
var i,j,n,m:integer;
begin
   n:=length(a); m:=length(b);
   for i:=0 to n-1 do
      for j:=0 to m-1 do
         c[i,j]:=c_mul(a[i],b[j]);
end;

procedure c_vectorh_kronmul_vectorv(var a,b:TComplexVector; var c:TComplexMatrix);
var i,j,n,m:integer;
begin
   n:=length(b); m:=length(a);
   for i:=0 to n-1 do
      for j:=0 to m-1 do
         c[i,j]:=c_mul(a[j],b[i]);
end;

procedure c_matrix_kronmul_matrix(var a,b,c:TComplexMatrix);
var ia,ja,ib,jb,na,ma,nb,mb:integer;
begin
   na:=length(a); ma:=length(a[0]);
   nb:=length(b); mb:=length(b[0]);
   for ia:=0 to na-1 do
   for ja:=0 to ma-1 do
      for ib:=0 to nb-1 do
      for jb:=0 to mb-1 do
         c[ia*nb+ib,ja*mb+jb]:=c_mul(a[ia,ja],b[ib,jb]);
end;


//====================================================================
//1-qbit gates
function I_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(2,2); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,1]:=c_one;
   I_gate_matrix:=q;
end;

function Hadamard_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(2,2);
   q[0,0]:=c_complex(1.0/sqrt2,0);
   q[0,1]:=c_complex(1.0/sqrt2,0);
   q[1,0]:=c_complex(1.0/sqrt2,0);
   q[1,1]:=c_complex(-1.0/sqrt2,0);
   Hadamard_gate_matrix:=q;
end;

function PauliX_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(2,2); c_matrix_fill(c_zero,q);
   q[0,1]:=c_one;
   q[1,0]:=c_one;
   PauliX_gate_matrix:=q;
end;

function PauliY_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(2,2); c_matrix_fill(c_zero,q);
   q[0,1]:=c_minus_i;
   q[1,0]:=c_i;
   PauliY_gate_matrix:=q;
end;

function PauliZ_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(2,2); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,1]:=c_minus_one;
   PauliZ_gate_matrix:=q;
end;

function PhaseShift_gate_matrix(phi:real):TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(2,2); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,1]:=c_exp_ix(phi);
   PhaseShift_gate_matrix:=q;
end;

function NOT_gate_matrix:TComplexMatrix;
begin NOT_gate_matrix:=PauliX_gate_matrix; end;

//2-qbit gates
function SWAP_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(4,4); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,2]:=c_one;
   q[2,1]:=c_one;
   q[3,3]:=c_one;
   SWAP_gate_matrix:=q;
end;

function SQRT_SWAP_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(4,4); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,1]:=c_complex(0.5,0.5);
   q[1,2]:=c_complex(0.5,-0.5);
   q[2,1]:=c_complex(0.5,-0.5);
   q[2,2]:=c_complex(0.5,0.5);
   q[3,3]:=c_one;
   SQRT_SWAP_gate_matrix:=q;
end;

function CNOT_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(4,4); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,1]:=c_one;
   q[2,3]:=c_one;
   q[3,2]:=c_one;
   CNOT_gate_matrix:=q;
end;

//3-qbit gates
function CCNOT_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(8,8); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,1]:=c_one;
   q[2,2]:=c_one;
   q[3,3]:=c_one;
   q[4,4]:=c_one;
   q[5,5]:=c_one;
   q[6,7]:=c_one;
   q[7,6]:=c_one;
   CCNOT_gate_matrix:=q;
end;

function CSWAP_gate_matrix:TComplexMatrix;
var q:TComplexMatrix;
begin
   q:=c_matrix(8,8); c_matrix_fill(c_zero,q);
   q[0,0]:=c_one;
   q[1,1]:=c_one;
   q[2,2]:=c_one;
   q[3,3]:=c_one;
   q[4,4]:=c_one;
   q[5,6]:=c_one;
   q[6,5]:=c_one;
   q[7,7]:=c_one;
   CSWAP_gate_matrix:=q;
end;


//====================================================================

function qregister_create(n:integer):tqregister;
var tmp:tqregister;
begin
   setlength(tmp,pow2(n));
   c_vector_fill(c_zero,tmp);
   qregister_create:=tmp;
end;

procedure qregister_set_tbits(var qreg:tqregister; value:tbit_vector);
var i,n,idx:integer;
begin
   c_vector_fill(c_zero,qreg);
   n:=length(value);
   if value[n-1]=one then idx:=1 else idx:=0;
   for i:=n-2 downto 0 do
   begin
      idx:=idx*2;
      if value[i]=one then idx:=idx+1;
   end;
   qreg[idx]:=c_one;
end;

procedure qregister_get_tbits(var qreg:tqregister; res:tbit_vector);
var i,n,nn,max_amp2_idx:integer; amp2,max_amp2:real;
begin
   nn:=length(qreg);
   max_amp2_idx:=0; max_amp2:=c_amp2(qreg[0]);
   for i:=1 to nn-1 do
   begin
      amp2:=c_amp2(qreg[i]);
      if amp2>max_amp2 then begin max_amp2:=amp2; max_amp2_idx:=i; end;
   end;
   n:=log2(nn);
   for i:=0 to n-1 do
   begin
      if (max_amp2_idx mod 2)=0 then res[i]:=zero else res[i]:=one;
      max_amp2_idx:=max_amp2_idx div 2;
   end;
end;

procedure qregister_apply_gate(var qreg:tqregister; gate:TComplexMatrix);
var tmp_reg:tqregister;
begin
   tmp_reg:=c_vector(length(qreg));
   c_vector_copy(qreg,tmp_reg);
   c_matrix_mul_vector(gate,qreg,tmp_reg);
   c_vector_copy(tmp_reg,qreg);
   setlength(tmp_reg,0);
end;

function qregister_calc_gate1(k,n:integer; p00,p01,p10,p11:tcomplex):TComplexMatrix;
var i:integer;
   tmp,tmp1,gateP,gateI:TComplexMatrix;
begin
   gateI:=c_matrix(2,2);
   gateI[0,0]:=c_one; gateI[0,1]:=c_zero; gateI[1,0]:=c_zero; gateI[1,1]:=c_one;
   gateP:=c_matrix(2,2);
   gateP[0,0]:=p00; gateP[0,1]:=p01; gateP[1,0]:=p10; gateP[1,1]:=p11;
   tmp:=c_matrix(2,2);
   if k=0 then c_matrix_copy(gateP,tmp) else c_matrix_copy(gateI,tmp);
   for i:=1 to n-1 do
   begin
      c_matrix_destroy(tmp1);
      tmp1:=c_matrix(pow2(i+1),pow2(i+1));
      if i=k then c_matrix_kronmul_matrix(gateP,tmp,tmp1)
             else c_matrix_kronmul_matrix(gateI,tmp,tmp1);
      c_matrix_destroy(tmp);
      tmp:=c_matrix(pow2(i+1),pow2(i+1));
      c_matrix_copy(tmp1,tmp);
   end;
   c_matrix_destroy(tmp1);
   c_matrix_destroy(gateI);
   c_matrix_destroy(gateP);
   qregister_calc_gate1:=tmp;
end;

function qregister_calc_I_gate(n:integer):TComplexMatrix;
var i,nn:integer; tmp:TComplexMatrix;
begin
   nn:=pow2(n);
   tmp:=c_matrix(nn,nn);
   for i:=0 to nn-1 do tmp[i,i]:=c_one;
   qregister_calc_I_gate:=tmp;
end;

function qregister_calc_H_gate(n:integer):TComplexMatrix;
var i:integer; tmp,tmp1,gateH:TComplexMatrix;
begin
   gateH:=c_matrix(2,2);
   gateH[0,0]:=c_complex(1.0/sqrt2,0);
   gateH[0,1]:=c_complex(1.0/sqrt2,0);
   gateH[1,0]:=c_complex(1.0/sqrt2,0);
   gateH[1,1]:=c_complex(-1.0/sqrt2,0);
   tmp:=c_matrix(2,2); c_matrix_copy(gateH,tmp);
   for i:=1 to n-1 do
   begin
      c_matrix_destroy(tmp1);
      tmp1:=c_matrix(pow2(i+1),pow2(i+1));
      c_matrix_kronmul_matrix(gateH,tmp,tmp1);
      c_matrix_destroy(tmp);
      tmp:=c_matrix(pow2(i+1),pow2(i+1));
      c_matrix_copy(tmp1,tmp);
   end;
   c_matrix_destroy(tmp1);
   c_matrix_destroy(gateH);
   qregister_calc_H_gate:=tmp;
end;

function qregister_calc_X_gate(k,n:integer):TComplexMatrix;
begin
qregister_calc_X_gate:=qregister_calc_gate1(k,n,c_zero,c_one,c_one,c_zero);
end;

function qregister_calc_Y_gate(k,n:integer):TComplexMatrix;
begin
qregister_calc_Y_gate:=qregister_calc_gate1(k,n,c_zero,c_minus_i,c_i,c_zero);
end;

function qregister_calc_Z_gate(k,n:integer):TComplexMatrix;
begin
qregister_calc_Z_gate:=qregister_calc_gate1(k,n,c_one,c_zero,c_zero,c_minus_one);
end;

function qregister_calc_PHI_gate(k,n:integer; phi:real):TComplexMatrix;
begin
qregister_calc_PHI_gate:=qregister_calc_gate1(k,n,c_one,c_zero,c_zero,c_exp_ix(phi));
end;

function qregister_calc_NOT_gate(k,n:integer):TComplexMatrix;
var i,nn,nn2,idx1,idx2:integer; tmp:TComplexMatrix;
begin
   nn:=pow2(n); nn2:=nn shr 1;
   tmp:=qregister_calc_I_gate(n);
   for i:=0 to nn2-1 do
   begin
      idx1:=insert_bit(i,k,0);
      idx2:=insert_bit(i,k,1);
      c_vectors_swap(tmp[idx1],tmp[idx2]);
   end;
   qregister_calc_NOT_gate:=tmp;
end;

function qregister_calc_SWAP_gate(c1_qbit,c2_qbit,n:integer):TComplexMatrix;
var i,nn,idx,b1,b2:integer; tmp:TComplexMatrix;
begin
   nn:=pow2(n);
   tmp:=c_matrix(nn,nn); c_matrix_fill(c_zero,tmp);
   for i:=0 to nn-1 do
   begin
      b1:=get_bit(i,c1_qbit); b2:=get_bit(i,c2_qbit);
      idx:=set_bit(i,c1_qbit,b2); idx:=set_bit(idx,c2_qbit,b1);
      tmp[i,idx]:=c_one;
   end;
   qregister_calc_SWAP_gate:=tmp;
end;

function qregister_calc_CNOT_gate(c_qbit,u_qbit,n:integer):TComplexMatrix;
var i,nn,nn2,idx1,idx2:integer; tmp:TComplexMatrix;
begin
   nn:=pow2(n); nn2:=nn shr 1;
   tmp:=qregister_calc_I_gate(n);
   for i:=0 to nn2-1 do
   begin
      idx1:=insert_bit(i,u_qbit,0);
      idx2:=insert_bit(i,u_qbit,1);
      if get_bit(idx1,c_qbit)=1 then c_vectors_swap(tmp[idx1],tmp[idx2]);
   end;
   qregister_calc_CNOT_gate:=tmp;
end;

procedure qregister_get_qbits(start_pos,n_res:integer; var qreg,res:tqregister);
var n,i,idx:integer;
begin
   n:=length(qreg);
   c_vector_fill(c_zero,res);
   for i:=0 to n-1 do
   begin
      idx:=get_bits(i,start_pos,n_res);
      res[idx]:=c_add(res[idx],qreg[i]);
   end;
end;

procedure qregister_erase_qbits(start_pos,n_erase:integer; var qreg,res:tqregister);
var n,nq,i,idx,end_pos,left_part,right_part:integer;
begin
   n:=length(qreg); nq:=log2(n);
   end_pos:=start_pos+n_erase;
   c_vector_fill(c_zero,res);
   for i:=0 to n-1 do
   begin
      left_part:=get_bits(i,end_pos,nq-end_pos);
      right_part:=get_bits(i,0,start_pos);
      idx:=insert_bits(left_part,0,right_part,start_pos);
      res[idx]:=c_add(res[idx],qreg[i]);
   end;
end;

procedure qregister_increase_qbits_num(n:integer; var qreg,res:tqregister);
var i,nn_qreg,nn_res:integer;
begin
   nn_qreg:=length(qreg);
   nn_res:=pow2(log2(nn_qreg)+n);
   for i:=0 to nn_qreg-1 do res[i]:=qreg[i];
   for i:=nn_qreg to nn_res-1 do res[i]:=c_zero;
end;

//====================================================================

procedure CCNOT_gate(x1,x2,x3:tbit; var y1,y2,y3:tbit);
var x_q:tqregister; x_b:tbit_vector;
begin
   setlength(x_b,3);
   x_b[2]:=x1; x_b[1]:=x2; x_b[0]:=x3;
   x_q:=qregister_create(3);
   qregister_set_tbits(x_q,x_b);
   qregister_apply_gate(x_q,CCNOT_gate_matrix);
   qregister_get_tbits(x_q,x_b);
   y1:=x_b[2]; y2:=x_b[1]; y3:=x_b[0];
   setlength(x_b,0);
   setlength(x_q,0);
end;

function q_not(op1:tbit):tbit;
var tmp_res,g1,g2:tbit;
begin CCNOT_gate(one,one,op1,g1,g2,tmp_res); q_not:=tmp_res; end;

function q_and(op1,op2:tbit):tbit;
var tmp_res,g1,g2:tbit;
begin CCNOT_gate(op1,op2,zero,g1,g2,tmp_res); q_and:=tmp_res; end;

function q_nand(op1,op2:tbit):tbit;
var tmp_res,g1,g2:tbit;
begin CCNOT_gate(op1,op2,one,g1,g2,tmp_res); q_nand:=tmp_res; end;

function q_xor(op1,op2:tbit):tbit;
var tmp_res,g1,g2:tbit;
begin CCNOT_gate(op1,one,op2,g1,g2,tmp_res); q_xor:=tmp_res; end;

procedure q_fanout(x:tbit; var y1,y2:tbit);
var g1:tbit;
begin CCNOT_gate(x,one,zero,y1,g1,y2); end;

function q_or(op1,op2:tbit):tbit;
begin q_or:=q_nand(q_not(op1),q_not(op2)); end;

function q_nor(op1,op2:tbit):tbit;
begin q_nor:=q_and(q_not(op1),q_not(op2)); end;

function q_and3(x1,x2,x3:tbit):tbit;
begin q_and3:=q_and(q_and(x1,x2),x3); end;

function q_and4(x1,x2,x3,x4:tbit):tbit;
begin q_and4:=q_and(q_and(x1,x2),q_and(x3,x4)); end;

function q_nand3(x1,x2,x3:tbit):tbit;
begin q_nand3:=q_not(q_and3(x1,x2,x3)); end;

function q_nand4(x1,x2,x3,x4:tbit):tbit;
begin q_nand4:=q_not(q_and4(x1,x2,x3,x4)); end;

function q_or4(x1,x2,x3,x4:tbit):tbit;
begin q_or4:=q_or(q_or(x1,x2),q_or(x3,x4)); end;

function q_or8(x1,x2,x3,x4,x5,x6,x7,x8:tbit):tbit;
begin q_or8:=q_or(q_or4(x1,x2,x3,x4),q_or4(x5,x6,x7,x8)); end;
//====================================================================

function bin_cut_bits(start_pos,end_pos:integer; var x:tbit_vector):tbit_vector;
var tmp:tbit_vector; i:integer;
begin
   setlength(tmp,end_pos-start_pos+1);
   for i:=start_pos to end_pos do tmp[i-start_pos]:=x[i];
   bin_cut_bits:=tmp;
end;

procedure bin_ins_bits(start_pos,end_pos:integer; var src,dst:tbit_vector);
var i:integer;
begin for i:=start_pos to end_pos do dst[i]:=src[i-start_pos]; end;

procedure bin_set_bits(start_pos,end_pos:integer; value:tbit; var x:tbit_vector);
var i:integer;
begin for i:=start_pos to end_pos do x[i]:=value; end;

{half-adder}
procedure bin_half_adder(a,b:tbit; var s,c:tbit);
begin
    c:=q_and(a,b);
    s:=q_xor(a,b);
end;

{full-adder}
procedure bin_full_adder(a,b,c_in:tbit; var s,c_out:tbit);
var s1,s2,p1,p2:tbit;
begin
    bin_half_adder(a,b,s1,p1);
    bin_half_adder(s1,c_in,s2,p2);
    s:=s2;
    c_out:=q_or(p1,p2);
end;

{n-bit adder}
procedure bin_add(a,b:tbit_vector; var s:tbit_vector);
var i,n:integer; c:tbit_vector;
begin
n:=length(a); setlength(c,n+1);
c[0]:=zero;
for i:=0 to n-1 do bin_full_adder(a[i],b[i],c[i],s[i],c[i+1]);
{s[n]:=c[n];}
setlength(c,0);
end;

{n-bit subtractor}
procedure bin_sub(a,b:tbit_vector; var s:tbit_vector);
var i,n:integer; c:tbit_vector;
begin
n:=length(a); setlength(c,n+1);
c[0]:=one;
for i:=0 to n-1 do bin_full_adder(a[i],q_not(b[i]),c[i],s[i],c[i+1]);
{s[n]:=c[n];}
setlength(c,0);
end;

{n-bit multiplier}
procedure bin_mul(a,b:tbit_vector; var s:tbit_vector);
var i,n:integer;
    tmp_sum,tmp_op1:tbit_table;
begin
n:=length(a);
setlength(tmp_op1,n); for i:=0 to n-1 do setlength(tmp_op1[i],n);
setlength(tmp_sum,n+1); for i:=0 to n do setlength(tmp_sum[i],n+1);
for i:=0 to n-1 do tmp_sum[0,i]:=zero;
for i:=0 to n-1 do
begin
    if b[i]=one then
    begin
       bin_set_bits(0,i-1,zero,tmp_op1[i]);
       bin_ins_bits(i,n-1,a,tmp_op1[i]);
    end
    else bin_set_bits(0,n-1,zero,tmp_op1[i]);
    bin_add(tmp_op1[i],tmp_sum[i],tmp_sum[i+1]);
end;
bin_ins_bits(0,n-1,tmp_sum[n],s);
for i:=0 to n-1 do setlength(tmp_op1[i],0); setlength(tmp_op1,0);
for i:=0 to n do setlength(tmp_sum[i],0); setlength(tmp_sum,0);
end;

{n-bit equal compare}
procedure bin_is_equal(a,b:tbit_vector; var res:tbit);
var res_tmp:tbit_vector; i,n:integer;
begin
   n:=length(a); setlength(res_tmp,n+1);
   res_tmp[0]:=zero;
   for i:=0 to n-1 do res_tmp[i+1]:=q_or(res_tmp[i],q_xor(a[i],b[i]));
   res:=q_not(res_tmp[n]);
   setlength(res_tmp,0);
end;

{n-bit greater compare. if a>b then res:=1}
procedure bin_is_greater_than(a,b:tbit_vector; var res:tbit);
var tmp_res,tmp_carry,tmp_cmp,tmp_equ:tbit_vector;
   i,n:integer;
begin
   n:=length(a);
   setlength(tmp_res,n+1); setlength(tmp_carry,n+1);
   setlength(tmp_cmp,n); setlength(tmp_equ,n);

   tmp_res[n]:=zero;
   tmp_carry[n]:=one;
   for i:=n-1 downto 0 do
   begin
      tmp_cmp[i]:=q_and(a[i],q_not(b[i]));
      tmp_equ[i]:=q_not(q_xor(a[i],b[i]));
      tmp_carry[i]:=q_and(tmp_carry[i+1],tmp_equ[i]);
      tmp_res[i]:=q_or(tmp_res[i+1],q_and(tmp_carry[i+1],tmp_cmp[i]));
   end;

   res:=tmp_res[0];
   setlength(tmp_res,0); setlength(tmp_carry,0);
   setlength(tmp_cmp,0); setlength(tmp_equ,0);
end;

{n-bit compare for a equ, greater, lower
if a=b then res_equ=1
if a>b then res_greater=1
if a<b then res_lower=1
}
procedure bin_cmp(a,b:tbit_vector; var res_equ,res_greater,res_lower:tbit);
var tmp_res_g,tmp_res_l,tmp_res_e: tbit_vector;
    tmp_greater,tmp_lower,tmp_equ: tbit_vector;
    i,n:integer;
begin
   n:=length(a);
   setlength(tmp_res_g,n+1); setlength(tmp_res_l,n+1); setlength(tmp_res_e,n+1);
   setlength(tmp_greater,n); setlength(tmp_lower,n); setlength(tmp_equ,n);

   tmp_res_g[n]:=zero;
   tmp_res_l[n]:=zero;
   tmp_res_e[n]:=one;
   for i:=n-1 downto 0 do
   begin
      tmp_greater[i]:=q_and(a[i],q_not(b[i]));
      tmp_lower[i]:=q_and(q_not(a[i]),b[i]);
      tmp_equ[i]:=q_xor(q_not(a[i]),b[i]);
      tmp_res_e[i]:=q_and(tmp_res_e[i+1],tmp_equ[i]);
      tmp_res_g[i]:=q_or(tmp_res_g[i+1],q_and(tmp_res_e[i+1],tmp_greater[i]));
      tmp_res_l[i]:=q_or(tmp_res_l[i+1],q_and(tmp_res_e[i+1],tmp_lower[i]));
   end;

   res_greater:=tmp_res_g[0];
   res_lower:=tmp_res_l[0];
   res_equ:=tmp_res_e[0];

   setlength(tmp_res_g,0); setlength(tmp_res_l,0); setlength(tmp_res_e,0);
   setlength(tmp_greater,0); setlength(tmp_lower,0); setlength(tmp_equ,0);
end;

{n-bit divider}
procedure bin_div(a,b:tbit_vector; var q,r:tbit_vector);
var tmp_q,tmp_equal,tmp_greater: tbit_vector;
   tmp_r,tmp_b: tbit_table;
   i,n:integer;
begin
   n:=length(a);
   setlength(tmp_q,n); setlength(tmp_equal,n); setlength(tmp_greater,n);
   setlength(tmp_r,n+1); setlength(tmp_b,n+1);
   for i:=0 to n do
   begin
      setlength(tmp_r[i],2*n-1);
      setlength(tmp_b[i],2*n-1);
   end;

   bin_set_bits(n,2*n-1,zero,tmp_r[0]);
   bin_ins_bits(0,n-1,a,tmp_r[0]);
   for i:=0 to n-1 do
   begin
     bin_is_greater_than(bin_cut_bits(n-i-1,n+n-i-2,tmp_r[i]),b,tmp_greater[n-i-1]);
     bin_is_equal(bin_cut_bits(n-i-1,n+n-i-2,tmp_r[i]),b,tmp_equal[n-i-1]);
     tmp_q[n-i-1]:=q_or(tmp_greater[n-i-1],tmp_equal[n-i-1]);
     bin_set_bits(n+n-i-1,n+n-1,zero,tmp_b[i]);
     bin_set_bits(0,n-i-2,zero,tmp_b[i]);
     if tmp_q[n-i-1]=zero then bin_set_bits(n-i-1,n+n-i-2,zero,tmp_b[i])
                          else bin_ins_bits(n-i-1,n+n-i-2,b,tmp_b[i]);
     bin_sub(tmp_r[i],tmp_b[i],tmp_r[i+1]);
   end;

   q:=tmp_q;
   bin_ins_bits(0,n-1,tmp_r[n],r);
   setlength(tmp_q,0); setlength(tmp_equal,0); setlength(tmp_greater,0);
   for i:=0 to n do
   begin
      setlength(tmp_r[i],0);
      setlength(tmp_b[i],0);
   end;
   setlength(tmp_r,0); setlength(tmp_b,0);
end;

//======================================================================

{ TForm1 }

procedure TForm1.calc_formula;
var clk_num,sim_time,i,n:LongInt;
    GCLK:tbit;
   a,b,c,d:tbit_vector;
   a_plus_b,apb_pow2,apb_pow3,b7,apb_pow3_minus_b7,
   apb_pow3_minus_b7_plus_a,seven:tbit_vector;
begin
//input data tuning
n:=10; setlength(a,n); setlength(b,n); setlength(c,n+1); setlength(d,n+1);

//intermediate variables tuning
setlength(a_plus_b,n);
setlength(apb_pow2,n);
setlength(apb_pow3,n);
setlength(b7,n);
setlength(apb_pow3_minus_b7,n);
setlength(apb_pow3_minus_b7_plus_a,n);
setlength(seven,n);
seven[0]:=one; seven[1]:=one; seven[2]:=one;
for i:=3 to n-1 do seven[i]:=zero;

//let's go
GCLK:=zero;  clk_num:=0; sim_time:=2;

while clk_num<sim_time do
begin
//get input data
for i:=0 to n-1 do
begin
     if CheckGroupA.Checked[i] then a[i]:=one else a[i]:=zero;
     if CheckGroupB.Checked[i] then b[i]:=one else b[i]:=zero;
end;

//-----------------------------------------
//test formula:
// c = ((a+b)^3 - 7*b + a) div b,
// d = ((a+b)^3 - 7*b + a) mod b
//-----------------------------------------
//1) a_plus_b=a+b
bin_add(a,b,a_plus_b);
//report
for i:=0 to n-1 do
    if a_plus_b[i]=one then CheckGroup_AplusB.Checked[i]:=true
                       else CheckGroup_AplusB.Checked[i]:=false;
//2) apb_pow2=a_plus_b*a_plus_b
bin_mul(a_plus_b,a_plus_b,apb_pow2);
//report
for i:=0 to n-1 do
    if apb_pow2[i]=one then CheckGroup_apb_pow2.Checked[i]:=true
                       else CheckGroup_apb_pow2.Checked[i]:=false;
//3) apb_pow3=apb_pow2*a_plus_b
bin_mul(apb_pow2,a_plus_b,apb_pow3);
//report
for i:=0 to n-1 do
    if apb_pow3[i]=one then CheckGroup_apb_pow3.Checked[i]:=true
                       else CheckGroup_apb_pow3.Checked[i]:=false;
//4) b7=b*7
bin_mul(b,seven,b7);
//report
for i:=0 to n-1 do
    if b7[i]=one then CheckGroup_b7.Checked[i]:=true
                 else CheckGroup_b7.Checked[i]:=false;
//5) apb_pow3_minus_b7=apb_pow3-b7
bin_sub(apb_pow3,b7,apb_pow3_minus_b7);
//report
for i:=0 to n-1 do
    if apb_pow3_minus_b7[i]=one then CheckGroup_apb_pow3_minus_b7.Checked[i]:=true
                                else CheckGroup_apb_pow3_minus_b7.Checked[i]:=false;
//6) apb_pow3_minus_b7_plus_a=apb_pow3_minus_b7+a
bin_add(apb_pow3_minus_b7,a,apb_pow3_minus_b7_plus_a);
//report
for i:=0 to n-1 do
    if apb_pow3_minus_b7_plus_a[i]=one then CheckGroup_apb_pow3_minus_b7_plus_a.Checked[i]:=true
                                       else CheckGroup_apb_pow3_minus_b7_plus_a.Checked[i]:=false;
//7) c=apb_pow3_minus_b7_plus_a div b; d=apb_pow3_minus_b7_plus_a mod b
bin_div(apb_pow3_minus_b7_plus_a,b,c,d);
//report
for i:=0 to n-1 do
begin
    if c[i]=one then CheckGroup_c.Checked[i]:=true else CheckGroup_c.Checked[i]:=false;
    if d[i]=one then CheckGroup_d.Checked[i]:=true else CheckGroup_d.Checked[i]:=false;
end;

GCLK:=q_not(GCLK); clk_num:=clk_num+1;
end;

//clear memory
setlength(a,0);
setlength(b,0);
setlength(c,0);
setlength(d,0);
setlength(a_plus_b,0);
setlength(apb_pow2,0);
setlength(apb_pow3,0);
setlength(b7,0);
setlength(apb_pow3_minus_b7,0);
setlength(apb_pow3_minus_b7_plus_a,0);
setlength(seven,0);
end;

procedure TForm1.Button_exitClick(Sender: TObject);
begin
  close;
end;

procedure TForm1.CheckGroupAItemClick(Sender: TObject; Index: LongInt);
begin
  calc_formula;
end;

procedure TForm1.CheckGroupBItemClick(Sender: TObject; Index: LongInt);
begin
  calc_formula;
end;

end.

