(* ::Package:: *)

<<"/users/gaolichen/gitroot/psl27cg/mpackage/cgframework.m";

(* common functions for psl27-g4 CG coefficients calculation. *)
ClearAll[DecomposePoly];
Options[DecomposePoly]={Vars->{},Phases->{}};
DecomposePoly[poly_,basis_,opts:OptionsPattern[]]:=
	Module[{ret,i,j,vars,terms,rep,pp,coef, term,phases,invertedPhases},
	ret = ConstantArray[0,Length[basis]];
	pp=Expand[poly];
	(*Print["pp=",pp];*)
	vars = OptionValue[Vars];
	phases=OptionValue[Phases];
	invertedPhases=Table[phases[[i]]->1/phases[[i]],{i,1,Length[phases]}];
	If[Length[vars]==0,vars=Variables[poly]];
	rep=Table[vars[[i]]->1,{i,1,Length[vars]}];
	For[i=1,i<= Length[basis],i++,
		terms=MonomialList[basis[[i]]];
		(*Print["terms=",terms];*)
		For[j=1,j<= Length[terms],j++,
		coef =terms[[j]]/.rep;
		term = Simplify[terms[[j]]/coef];
		(*Print["coef=",Coefficient[pp,term], "*",coef];*)
		ret[[i]] += Coefficient[pp,term] * ComplexExpand[Conjugate[coef/.invertedPhases]];
	];
	ret[[i]]=Simplify[ret[[i]]];
	(*Print["ret=",ret[[i]]];*)
	];

	Return[Simplify[ret]]
];

 
ClearAll[DecomposePolyMat];
Options[DecomposePolyMat]=Join[{},Options[DecomposePoly]];
DecomposePolyMat[mat_,basis_,opts:OptionsPattern[]]:=Table[
	DecomposePoly[mat[[i]], basis,FilterRules[{opts},Options[DecomposePoly]]],
	{i,1,Length[mat]}
];

TestConjugateTransform[op_,mat_]:=Module[{opc},
	opc=Simplify[mat.Conjugate[op].Inverse[mat]];
	Return[Simplify[opc-op]]
];
 
(* Verify Psl27 Generators.*)
VerifyPsl27Generator[A_,B_]:=Module[{AB,n,zero,ABc},
	n=Length[A];
	If[n!= Length[B],Return[False]];
	zero = ConstantArray[0,{n,n}];

	AB=N[A.B];
	ABc=N[A.B.A.B.B];
	If[SameQ[Chop[N[A.A]-IdentityMatrix[n]],zero]==False,
		Return[False]
	];

	If[SameQ[Chop[N[MatrixPower[B,3]]-IdentityMatrix[n]],zero]==False,
		Print["VerifyPsl27Generator failed: B^3 != I."];
		Return[False]
	];

	If[SameQ[Chop[N[MatrixPower[AB,7]]-IdentityMatrix[n]],zero]==False,
		Print["VerifyPsl27Generator failed: (AB)^7 != I."];
	Return[False]
	];

	If[SameQ[Chop[N[MatrixPower[ABc,4]]-IdentityMatrix[n]],zero]==False,
		Print["VerifyPsl27Generator failed: [A,B]^4 != I."];
		Return[False]
	];

	Return[True];
];

(* Transpose[LeftDiag[Y]].Y.RightDiag[Y] = diagonal matrix *)
LeftDiag[Y_]:=Module[{mat,res,ret},
	mat = Y.ConjugateTranspose[Y];
	res = Eigensystem[mat];
	ret = ConjugateTranspose[res[[2]]];
	Return[ret];
];

RightDiag[Y_]:=Module[{mat,res,ret},
	mat = ConjugateTranspose[Y].Y;
	res = Eigensystem[mat];
	ret = Transpose[res[[2]]];

	Return[ret];
];

ClearAll[IsIntegerSqure];
SetAttributes[IsIntegerSqure,Listable]
IsIntegerSqure[n_]:=Round[Sqrt[N[n]]]^2-n==0;

ClearAll[SqrtQudraticForm];
SetAttributes[SqrtQudraticForm,Listable]
SqrtQudraticForm[q_]:=Module[{nq,aq,a,d,b2c,x,pol,t,sgn,p1,p2},
	nq=N[q];
	If[Chop[nq]==0, Return[0]];
	aq=RootReduce[q];
	If[QuadraticIrrationalQ[aq]==False, Return[ToRadicals[RootReduce[Sqrt[q]]]]];
	If[nq<0, sgn=I;aq=-aq,sgn=1];
	d=Denominator[aq];

	(* Now the numerator part is a+b*Sqrt[c], the minimal polynomail is x^2-2ax+a^2-b^2c*)
	pol = MinimalPolynomial[Numerator[aq],x];
	a = -Coefficient[pol,x]/2;
	b2c=(a^2-(pol/.{x->0}));
	(*Print["a=",a, ", b^2c=",b2c, ", d=",d,", pol=",pol];*)
	If[IsIntegerSqure[a^2-b2c]==False, Return[ToRadicals[RootReduce[Sqrt[q]]]]];
	t=Sqrt[a^2-b2c];
	p1=Sqrt[(a+t)/2]/Sqrt[d];
	p2=Sqrt[(a-t)/2]/Sqrt[d];
	If[Chop[N[(p1+p2)^2-aq]]==0, Return[sgn*(p1+p2)],Return[sgn*(p1-p2)]]
];

ClearAll[SqrtComplex];
SetAttributes[SqrtComplex,Listable]
SqrtComplex[n_]:=Module[{a,b,x2,y2,dlt,x,y,arg,pol},
	pol = MinimalPolynomial[n,x];
	If[Exponent[pol,x]>4, Return[Simplify[Sqrt[n]]]];

	a=Re[n];
	b=Im[n];
	dlt=Simplify[a^2+b^2];
	dlt=SqrtQudraticForm[dlt];
	x2=Simplify[(dlt+a)/2];
	y2=Simplify[(dlt-a)/2];
	x = SqrtQudraticForm[x2];
	y = SqrtQudraticForm[y2];
	arg=Arg[N[n]]/2;
	x *= Sign[Cos[arg]];
	y *= Sign[Sin[arg]];
	(*Print["a=",a,", b=", b, ", dlt=",dlt, ",x2=",x2, ",y2=",y2];*)
	Return[x + y*I];
];

SetAttributes[Reciprocal,Listable]
Reciprocal[n_]:=SimplifyNum2[RootReduce[1/n]];

(* check whether or not a number is rational or quadratic irational *)
ClearAll[NiceRealQ,NiceComplexQ];
SetAttributes[NiceRealQ,Listable]
NiceRealQ[n_]:= Element[n,Rationals]===True || QuadraticIrrationalQ[n]===True;
SetAttributes[NiceComplexQ,Listable]
NiceComplexQ[n_]:=NiceRealQ[Re[n]] && NiceRealQ[Im[n]];

ClearAll[SimplifyNum2]
SetAttributes[SimplifyNum2,Listable]
SimplifyNum2::CannotSimplify="SimplifyNum2 cannot simplify `1`.";
SimplifyNum2[n_]:=Module[{nn,pol,x,ord, n2,ret},
	nn = RootReduce[n];
	pol = MinimalPolynomial[nn,x];
	ord = Exponent[pol,x];
	If[ord<=2, Return[nn]];
	If[ord != 4 || Coefficient[pol,x^3] != 0, Message[SimplifyNum2::CannotSimplify, nn]; Return[ToRadicals[nn]]];

	n2 = ToRadicals[nn^2];
	If[Im[n2]==0, 
		ret=SqrtQudraticForm[n2],
		ret = SqrtComplex[n2];
	];

	(*Print["n2=",n2, ", ret=", ret];*)

	If[Chop[N[ret-nn]] == 0, 
		Assert[Chop[N[ret-nn]]==0];
		Return[ret], 
		Assert[Chop[N[ret+nn]]==0];
		Return[-ret]];
];

SetAttributes[SimplifyNum,Listable]
SimplifyNum[n_]:=Module[{an,nn,norm,norm2,tmp,ph},
	an = Simplify[n];
	If[NiceComplexQ[an],Return[an]];

	nn=Chop[N[an]];
	If[nn==0, Return[0]];

	(* simplify norm. *)
	(* if n is real or pure imaginary*)
	If[Re[nn]==0,
		norm= Simplify[Abs[Im[an]]],
		If[Im[nn]==0, norm = Simplify[Abs[Re[an]]]],

		(* if n is complex *)
		norm = Sqrt[Simplify[an*Conjugate[an]]];
	];

	(* if ret is rational or quadratic irational form, we do not need to simplify it.*)
	If[Element[norm,Rationals]==False && QuadraticIrrationalQ[norm]==False,
		norm2=Simplify[norm^2];
		If[QuadraticIrrationalQ[norm2], 
			tmp=SqrtQudraticForm[norm2];
			If[SameQ[tmp,Null]==False, norm = tmp],
			norm = FullSimplify[norm];
		];
	];

	(* handle phases. *)
	If[Re[nn]==0 || Im[nn]==0,
		ph=Simplify[Exp[I*Arg[nn]]],
		ph = Simplify[an/norm];
		tmp=ToExactPhase[Arg[ph],ToNum->True];
		If[tmp!=Infinity, ph=tmp,
			ph=FullSimplify[tmp];
		];
	];

	Return[ph*norm];
];


TestCG[r1_,r2_,r3_,embed_,op_,cgList_]:=Module[{i,rr3,ret=True},
	For[i=1,i<= Length[cgList],i++,
		If[Length[cgList]==1,
			rr3=r3, rr3=SetRepMultiplicity[r3, i]
		];
		(*rr3=SetRepMultiplicity[r3, i];*)
		SetCG[r1,r2,rr3,embed,N[cgList[[i]]/.{et->Exp[I*2Pi/7]}]];
		If[VerifyCG[r1,r2,rr3,embed,op,b7ToNum]==False,
			Print["TestCG: failed for CG " <> r1 <> "*" <> r2 <>"->"<>rr3];
			ret = False;
		];
		ResetCG[r1,r2,rr3,embed];
	];

	Return[ret]
];

ClearAll[SolveCGEquations];
SolveCGEquations[eqs_,freecoef_:{}]:=Module[{neqs,mat,root,i},
	(*Print["rank=",MatrixRank[eqs/.{et\[Rule]Exp[I*2*Pi/7]}]];*)
	neqs = IndependentRows[eqs,Var2N->{et->Exp[I*2*Pi/7]}];
	mat=SimplifyCN[neqs[[1]],et,7];
	(*root=SolveLinearEquation[mat,FreeCoefficients\[Rule]neqs[[2]],Numeric\[Rule]False];
	root=SimplifyCN[root,et,7];
	root=Simplify[root/.et4ToB7];*)

	mat=N[mat/.et2Num/.b7ToNum];
	(*Print["mat=",mat];
	Print["freeparameter=",neqs[[2]]];*)
	root=SolveLinearEquation[mat,FreeCoefficients->freecoef,Numeric->True];
	root=N2Exact[root];
	root = SimplifyNum2[root];
	(*Print["root=",root];*)
	Return[root]
];

NormalizeCG[r1_,r2_,r3_,cgList_,embed_]:=Module[{vv,ncg,eigen,tmp,Dmhalf,ret,lg,matM,gamma,svd,matO,Pmhalf,i},
	ncg = cgList/.b7ToNum/.et2Num;
	vv=SimplifyNum2[ncg.ConjugateTranspose[ncg]];
	
	(*If[Length[cgList]\[Equal]1,
		(*Print["Length[embed[r3]]=",Length[embed[GetRepName[r3]]], ", vv=", vv];*)
		Return[cgList/SimplifyNum2[Sqrt[vv[[1,1]]/Length[embed[GetRepName[r3]]]]]];
	];*)

	eigen=Eigensystem[vv];
	(*eigen[[1]]=N2Exact[N[eigen[[1]]],ToNum\[Rule]True];
	eigen[[2]]=N2Exact[N[eigen[[2]]],ToNum\[Rule]True];*)
	eigen[[1]]=SimplifyNum2[eigen[[1]]];
	eigen[[2]]=SimplifyNum2[eigen[[2]]];
	eigen[[2]]=Table[eigen[[2,i]]/Norm[eigen[[2,i]]],{i,1,Length[eigen[[2]]]}];
	eigen[[2]]=Simplify[eigen[[2]]];
	Dmhalf=DiagonalMatrix[ToRadicals[SqrtComplex[Reciprocal[eigen[[1]]]]]];
	(*Print["NormalizeCG eigen=", eigen, ", dmhalf=", Dmhalf];*)
	ret = Dmhalf.Conjugate[eigen[[2]]];
	(*Print["NormalizeCG ret1=", ret];*)
	
	lg = embed[KeyLargeGroup];
	(* If r1, r2, r3 are real reps, we need to make the *)
	If[IsRealRep[lg, r1] && IsRealRep[lg, r2] && IsRealRep[lg, r3],
		matM = SimplifyNum2[ret.ncg];
		gamma = CGConjugateMat[r1,r2,r3,embed];
		matM = SimplifyNum2[matM.gamma.Transpose[matM]];
		eigen=Eigensystem[matM];
		(*Print["eigen=",eigen];*)
		eigen[[1]]=SimplifyNum2[eigen[[1]]];
		eigen[[2]]=SimplifyNum2[eigen[[2]]];
		(*Print["eigen2=",eigen];*)
		eigen[[2]]=Table[eigen[[2,i]]/Norm[eigen[[2,i]]],{i,1,Length[eigen[[2]]]}];
		matO=Transpose[eigen[[2]]];
		
		Pmhalf=DiagonalMatrix[SqrtComplex[Reciprocal[eigen[[1]]]]];
		(*Print["matM=", matM, ", eigen=", eigen, ", Pmhalf=",Pmhalf];*)
		(*tmp = SimplifyNum2[Pmhalf.ConjugateTranspose[matO]];*)
		ret = SimplifyNum2[Pmhalf.ConjugateTranspose[matO].ret];
		(*Print["NormalizeCG ret2=", ret];*)
	];

	(*Print["ret.ncg=",Simplify[ret.ncg]];*)
	ret = SimplifyNum2[ret.ncg];
	(*Print["NormalizeCG ret3=", ret];*)
	
	Return[ret*Sqrt[Length[embed[GetRepName[r3]]]]];
];

(* find the index of free parameter. The fp argument has the form: r1*r2\[Rule]r3 *)
ClearAll[FreeParameterToIndex]
SetAttributes[FreeParameterToIndex,Listable]
FreeParameterToIndex[r1_String,r2_String, r3_String, fp_String, embed_]:=Module[{term,tmp,tlist,ret,i},
	term = StringSplit[fp, {"*","->","\[Rule]"," "}];
	If[Length[term]!= 3, Print["Invalid free parameter input: ", fp]; Throw[$Failed]];

	tmp=term[[3]];
	term[[3]]=term[[2]];
	term[[2]]=term[[1]];
	term[[1]]=tmp;
	
	tlist = embed[r1,r2,r3,KeyCGTerms];
	ret = -1;
	For[i = 1, i <= Length[tlist], i++,
		If[Length[tlist[[i]]]==3 && tlist[[i]] == term, ret=i; Break[]]
		If[Length[tlist[[i]]]==4 && tlist[[i,1]]==term[[1]], 
			If[(tlist[[i,2]]==term[[2]] && tlist[[i,3]]==term[[3]])
				|| tlist[[i,3]]==term[[2]] && tlist[[i,2]]==term[[3]], ret=i; Break[]];
		];
	];
	
	If[ret == -1, 
		Print["Failed to find free parameter ", fp, " for ", r1, "*", r2, "->", r3];
		Throw[$Failed];
	];

	Return[ret]
];

Options[FinalizeCG]={FreeParameters->{}};
FinalizeCG[r1_,r2_,r3_,cgEqs_,embed_,opts:OptionsPattern[]]:=Module[
{coefsList,i,j,mat,nmat,mat2,evlist,cgmat,res,u,w,v,diag,fplist},
	fplist = OptionValue[FreeParameters];
	If[Length[fplist]>0, 
		coefsList=SolveCGEquations[cgEqs,FreeParameterToIndex[r1,r2,r3,fplist,embed]],
		coefsList=SolveCGEquations[cgEqs]
	];

	(* If there is only one cg term, then we manually set the CG coefficients to 1. *)
	If[Length[coefsList]==0 && Length[embed[r1,r2,r3,KeyCGTerms]]==1,
		coefsList={{1}};
	];
	(*Print["coefsList=",coefsList];*)

	If[TestCG[r1,r2,r3,embed,PslGenB,coefsList/.b7ToNum]==False, 
		Return[{}]
	];

	(* we need to build authonormal basis if there are more than one solutions.*)
	(*If[Length[coefsList]>1,
	mat=ConstantArray[0,{Length[coefsList],Length[coefsList]}];
	cgmat=CGConjugateMat[r1,r2,r3,embed];
	Do[mat[[i,j]]=coefsList[[i]].cgmat.coefsList[[j]],
	{i,1,Length[coefsList]},{j,1,Length[coefsList]}];
	mat=SimplifyCN[mat/.b7ToEt,et,7];
	nmat=Chop[N[mat/.{et\[Rule]Exp[I*2Pi/7]}]];
	Print["nmat=",MatrixForm[nmat]];
	u=LeftDiag[nmat];
	diag=Chop[Transpose[u].nmat.u];
	diag=Inverse[Sqrt[diag]];
	u=Chop[Transpose[u.diag]];
	Print["u=",MatrixForm[u],",u.nmat.u^T=",Chop[u.nmat.Transpose[u]]];
	Print["res=",Chop[Sum[u[[1,i]]*coefsList[[i]]/.{et\[Rule] Exp[I*2Pi/7]}/.b7ToNum,{i,1,Length[u]}]]];
	];
	*)
	(*Do[coefsList[[i]]=FixCGPhase[r1,r2,r3,coefsList[[i]], embed], {i,1,Length[coefsList]}];*)
	(*coefsList=OrthnormalizeCG[r1,r2,r3,coefsList/.b7ToEt, embed];
	Print["coefsList=",coefsList];
	coefsList=N2Exact[N[coefsList/.{et->Exp[I*2Pi/7]}]];*)
	coefsList=NormalizeCG[r1,r2,r3,coefsList,embed];
	Return[ToRadicals[coefsList]];
	(*Return[ToExp[ToRadicals[coefsList]]];*)
];

ClearAll[CalculateCG];
Options[CalculateCG]=Join[{},Options[FinalizeCG]];
CalculateCG[r1_String,r2_String,r3_String,embed_, input_,opList_,opts:OptionsPattern[]]:=
Module[{eqs,res,i,rr3,repName,repDec,m,largeG},
	largeG=embed[KeyLargeGroup];
	m=GetCGMultiplicity[largeG,r1,r2,r3];
	If[m>1,rr3=SetRepMultiplicity[r3,1],rr3=r3];
	ResetCG[r1,r2,rr3,embed];
	(*Print["GetCG=",GetCG[r1,r2,rr3,embed]];*)
	eqs=CgcEquations[input, r1,r2,rr3,embed, opList];
	(*Print["eqs=",eqs];*)
	res=FinalizeCG[r1,r2,GetRepWithSym[r3],eqs, embed, FilterRules[{opts},Options[FinalizeCG]]];
	(*Print["res=",res];*)
	For[i=1,i<= Length[res],i++,
		If[Length[res]==1,
			rr3=r3, rr3=SetRepMultiplicity[r3, i]
		];
		SetCG[r1,r2,rr3,embed,res[[i]]];
	];

	Return[ToExp[res]]
];

(* PSL27 Group Info*)
ClearAll[Psl27]
Psl27[KeyIrr]:={{"1",1},{"3",3},{"3b",3,"\\bar{3}"},{"6",6},{"7",7},{"8",8}};

ClearAll[Psl27Kronecker];
Psl27Kronecker[x_,y_]:=DefaultKronecker[Psl27,x,y];
Psl27Kronecker["3","3"]={"3b:-","6:+"};
Psl27Kronecker["3","3b"]={"1","8"};
Psl27Kronecker["3","6"]={"3b","7","8"};
Psl27Kronecker["3","7"]={"6","7","8"};
Psl27Kronecker["3","8"]={"3","6","7","8"};
Psl27Kronecker["6","6"]={"1:+","6:+1","6:+2","8:+","7:-","8:-"};
Psl27Kronecker["6","7"]={"3","3b","6","7:1","7:2","8:1","8:2"};
Psl27Kronecker["6","8"]={"3","3b","6:1","6:2","7:1","7:2","8:1","8:2"};
Psl27Kronecker["7","7"]={"1:+","6:+1","6:+2","7:+","8:+","3:-","3b:-","7:-","8:-"};
Psl27Kronecker["7","8"]={"3","3b","6:1","6:2","7:1","7:2","8:1","8:2","8:3"};
Psl27Kronecker["8","8"]={"1:+","6:+1","6:+2","7:+","8:+1","8:+2","3:-","3b:-","7:-1","7:-2","8:-"};
Psl27[KeyKronecker]:=Psl27Kronecker;
Psl27[KeyConjugateIrr]:={{"3","3b"}};


b7ToNum={b7->(-1+I*Sqrt[7])/(2Sqrt[2]),d7->(-1-I*Sqrt[7])/(2Sqrt[2])};
b7ToEt={b7->(et+et^2+et^4)/Sqrt[2],d7->(et^6+et^5+et^3)/Sqrt[2]};
et4ToB7={et^4-> Sqrt[2]*b7-et-et^2};
sq7Tob7={Sqrt[7]->-2I*Sqrt[2]b7-I};
(*latexTob7={Subscript[b,7]->b7,Subscript[Overscript[b, _], 7]->d7};*)
et2Num={et->Exp[2Pi*I/7]};
