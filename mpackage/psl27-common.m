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

SolveCGEquations[eqs_]:=Module[{neqs,mat,root,i},
	(*Print["rank=",MatrixRank[eqs/.{et\[Rule]Exp[I*2*Pi/7]}]];*)
	neqs = IndependentRows[eqs,Var2N->{et->Exp[I*2*Pi/7]}];
	mat=SimplifyCN[neqs[[1]],et,7];
	(*root=SolveLinearEquation[mat,FreeCoefficients\[Rule]neqs[[2]],Numeric\[Rule]False];
	root=SimplifyCN[root,et,7];
	root=Simplify[root/.et4ToB7];*)

	mat=Simplify[mat/.et4ToB7/.b7ToNum];
	(*Print["mat=",mat];
	Print["freeparameter=",neqs[[2]]];*)
	root=SolveLinearEquation[mat,FreeCoefficients->neqs[[2]],Numeric->True];
	root=N2Exact[root];
	(*Print["root=",root];*)
	(*Return[root];*)
	Return[Simplify[root]]
];

FinalizeCG[r1_,r2_,r3_,cgEqs_,embed_]:=Module[
{coefsList,i,j,mat,nmat,mat2,evlist,cgmat,res,u,w,v,diag},
	coefsList=SolveCGEquations[cgEqs];

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
	coefsList=OrthnormalizeCG[r1,r2,r3,coefsList/.b7ToEt, embed];
	coefsList=N2Exact[N[coefsList/.{et->Exp[I*2Pi/7]}]];
	Return[ToRadicals[coefsList]];
	(*Return[ToExp[ToRadicals[coefsList]]];*)
];

CalculateCG[r1_String,r2_String,r3_String,embed_, input_,opList_]:=Module[{eqs,res,i,rr3,repName,repDec,m,largeG},
	largeG=embed[KeyLargeGroup];
	m=GetCGMultiplicity[largeG,r1,r2,r3];
	If[m>1,rr3=SetRepMultiplicity[r3,1],rr3=r3];
	ResetCG[r1,r2,rr3,embed];
	(*Print["GetCG=",GetCG[r1,r2,rr3,embed]];*)
	eqs=CgcEquations[input, r1,r2,rr3,embed, opList];
	(*Print["eqs=",eqs];*)
	res=FinalizeCG[r1,r2,GetRepWithSym[r3],eqs, embed];
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
latexTob7={Subscript[b,7]->b7,Subscript[
\!\(\*OverscriptBox[\(b\), \(_\)]\), 7]->d7};
et2Num={et->Exp[2Pi*I/7]};
