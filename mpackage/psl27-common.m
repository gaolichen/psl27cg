DecomposePoly[poly_, basis_, opts:OptionsPattern[]] := 
    Module[{ret, i, j, vars, terms, rep, pp, coef, term, phases, 
      invertedPhases}, ret = ConstantArray[0, Length[basis]]; 
      pp = Expand[poly]; vars = OptionValue[Vars]; 
      phases = OptionValue[Phases]; invertedPhases = 
       Table[phases[[i]] -> 1/phases[[i]], {i, 1, Length[phases]}]; 
      If[Length[vars] == 0, vars = Variables[poly]]; 
      rep = Table[vars[[i]] -> 1, {i, 1, Length[vars]}]; 
      For[i = 1, i <= Length[basis], i++, terms = MonomialList[basis[[i]]]; 
        For[j = 1, j <= Length[terms], j++, coef = terms[[j]] /. rep; 
          term = Simplify[terms[[j]]/coef]; ret[[i]] += Coefficient[pp, term]*
            ComplexExpand[Conjugate[coef /. invertedPhases]]; ]; 
        ret[[i]] = Simplify[ret[[i]]]; ]; Return[Simplify[ret]]]
 
Options[DecomposePoly] = {Vars -> {}, Phases -> {}}
 
DecomposePolyMat[mat_, basis_, opts:OptionsPattern[]] := 
    Table[DecomposePoly[mat[[i]], basis, FilterRules[{opts}, 
       Options[DecomposePoly]]], {i, 1, Length[mat]}]
 
Options[DecomposePolyMat] = {Vars -> {}, Phases -> {}}
 
TestConjugateTransform[op_, mat_] := Module[{opc}, 
     opc = Simplify[mat . Conjugate[op] . Inverse[mat]]; 
      Return[Simplify[opc - op]]]
 
VerifyPsl27Generator[A_, B_] := Module[{AB, n, zero, ABc}, 
     n = Length[A]; If[n != Length[B], Return[False]]; 
      zero = ConstantArray[0, {n, n}]; AB = N[A . B]; 
      ABc = N[A . B . A . B . B]; If[(Chop[N[A . A] - IdentityMatrix[n]] === 
         zero) == False, Return[False]]; 
      If[(Chop[N[MatrixPower[B, 3]] - IdentityMatrix[n]] === zero) == False, 
       Print["VerifyPsl27Generator failed: B^3 != I."]; Return[False]]; 
      If[(Chop[N[MatrixPower[AB, 7]] - IdentityMatrix[n]] === zero) == False, 
       Print["VerifyPsl27Generator failed: (AB)^7 != I."]; Return[False]]; 
      If[(Chop[N[MatrixPower[ABc, 4]] - IdentityMatrix[n]] === zero) == 
        False, Print["VerifyPsl27Generator failed: [A,B]^4 != I."]; 
        Return[False]]; Return[True]; ]
