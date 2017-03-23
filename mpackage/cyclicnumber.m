CnRelation[n_, m_] := {}
 
CnFreeVarNumber[n_] := 0
 
InitCnBase[n_] := Module[{i, j, vars = {}, eqs = {}, count = 0, res, exp, 
      relation}, For[i = 1, i <= n, i++, AppendTo[vars, Unique["cnbase"]]]; 
      For[i = n - 1, i >= 1, i--, If[Mod[n, i] == 0, 
         For[j = 0, j < i, j++, If[j == 0 && count > 0, Continue[]]; 
            AppendTo[eqs, Sum[vars[[i*k + j + 1]], {k, 0, n/i - 1}]]; 
            count++; ]; ]; ]; res = Solve[eqs == 0, Take[vars, -count]]; 
      If[Length[res] != 1, Print["Failed to initialize cnbase: ", 
         Length[res], " solutions found."]; Return[]; ]; res = First[res]; 
      For[i = n - count, i < n, i++, relation = ConstantArray[0, n - count]; 
        exp = vars[[i + 1]] /. res; For[j = 1, j <= n - count, j++, 
         relation[[j]] = Coefficient[exp, vars[[j]]]; ]; 
        CnRelation[n, i] = relation; ]; CnFreeVarNumber[n] = n - count; 
      Return[CnFreeVarNumber[n]]; ]
 
CnToList[cn_, var_, n_] := Module[{i, e, ret, freeVarN}, 
     freeVarN = CnFreeVarNumber[n]; ret = ConstantArray[0, freeVarN]; 
      e = Exponent[cn, var]; For[i = e, i >= 1, i--, 
       If[Mod[i, n] >= freeVarN, ret += Coefficient[cn, var^i]*
           CnRelation[n, Mod[i, n]], ret[[Mod[i, n] + 1]] += 
          Coefficient[cn, var^i]]; ]; ret[[1]] += cn /. {var -> 0}; 
      Return[ret]; ]
 
ListToCn[clist_, var_] := clist . Table[var^i, {i, 0, Length[clist] - 1}]
 
CnTimes[clist1_, clist2_, n_] := Module[{ret, i, j, freeVarN}, 
     freeVarN = CnFreeVarNumber[n]; Assert[freeVarN == Length[clist1]]; 
      Assert[freeVarN == Length[clist2]]; ret = ConstantArray[0, freeVarN]; 
      For[i = 1, i <= Length[clist1], i++, For[j = 1, j <= Length[clist2], 
         j++, If[Mod[i + j - 2, n] >= freeVarN, 
           ret += Simplify[clist1[[i]]*clist2[[j]]*CnRelation[n, Mod[
                i + j - 2, n]]], ret[[Mod[i + j - 2, n] + 1]] += 
            clist1[[i]]*clist2[[j]]]; ]; ]; Return[ret]; ]
 
Attributes[Assert] = {HoldAllComplete}
 
InverseCn[clist_, n_] := Module[{ret, eqs, i, j, root, freeVarN}, 
     freeVarN = CnFreeVarNumber[n]; Assert[freeVarN == Length[clist]]; 
      ret = {}; For[i = 1, i <= freeVarN, i++, 
       AppendTo[ret, Unique["CnInv"]]; ]; eqs = CnTimes[clist, ret, n]; 
      eqs[[1]] -= 1; root = Solve[eqs == 0, ret]; Assert[Length[root] == 1]; 
      Return[Simplify[ret /. First[root]]]; ]
 
SimplifyCN[cn_, var_, n_] := Module[{num, denor, ret}, 
     num = CnToList[Numerator[cn], var, n]; 
      denor = CnToList[Denominator[cn], var, n]; 
      ret = Simplify[CnTimes[num, InverseCn[denor, n], n]]; 
      Return[ListToCn[ret, var]]; ]
 
SimplifyCnMat[mat_, var_, n_] := Table[SimplifyCN[mat[[i,j]], var, n], 
     {i, 1, Length[mat]}, {j, 1, Length[mat[[i]]]}]
 
FullSimplifyCoef[poly_, var_] := Module[{ret = 0, i, e}, 
     e = Exponent[poly, var]; ret = FullSimplify[poly /. {var -> 0}]; 
      For[i = 1, i <= e, i++, ret += FullSimplify[Coefficient[poly, var^i]]*
          var^i; ]; Return[ret]; ]
 
FullSimplifyCoefMat[mat_, var_] := Table[FullSimplifyCoef[mat[[i,j]], var], 
     {i, 1, Length[mat]}, {j, 1, Length[mat[[i]]]}]
 
Eigenvector4F7[mat_, val_, var_, n_] := Module[{dim, vec = {}, i, eqs, root, 
      ret}, dim = Length[mat]; For[i = 1, i <= dim, i++, 
       AppendTo[vec, Unique["x"]]; ]; eqs = (mat - val*IdentityMatrix[dim]) . 
        vec; root = First[Solve[eqs[[1 ;; dim - 1]] == 0, vec[[1 ;; dim]]]]; 
      ret = vec /. root; ret = ret /. Table[vec[[i]] -> 1, {i, 1, dim}]; 
      Do[ret[[i]] = SimplifyCN[ret[[i]], var, n], {i, 1, Length[ret]}]; 
      Return[ret]]
 
ToExpPhase[ph_] := Module[{b7, d7, i, j, k, v, omega, a, diff}, 
     v = N[ph]; b7 = (-1 + I*Sqrt[7])/(2*Sqrt[2]); 
      d7 = (-1 - I*Sqrt[7])/(2*Sqrt[2]); omega = (-1 + I*Sqrt[3])/2; 
      For[i = 0, i <= 4, i++, For[j = 0, j <= 2, j++, 
        a = N[Arg[b7^i*omega^j]]; diff = ((v - a)/Pi)*2; 
         If[Chop[diff - Round[diff]] == 0, Return[Subscript[b, 7]^i*
            \[Omega]^j*I^Round[diff]]]; a = N[Arg[d7^i*omega^j]]; 
         diff = ((v - a)/Pi)*2; If[Chop[diff - Round[diff]] == 0, 
          Return[ToExpression["\\Bar{b}_7", TeXForm]^i*\[Omega]^j*
            I^Round[diff]]]; ]]; Return[Infinity]]
 
omega = E^(((2*I)/3)*Pi)
 
Attributes[Subscript] = {NHoldRest}
 
Attributes[ToExp] = {Listable}
 
ToExp[a_] := Module[{r, c, i, ph}, If[Im[a] == 0, Return[a]]; c = Arg[a]; 
      If[IntegerQ[c*(2/Pi)], Return[a]]; If[IntegerQ[c*(6/Pi)], 
       ph = 0; i = Abs[c*(6/Pi)]; If[i == 5, ph = (-I)*\[Omega]^2; 
          If[c < 0, ph = I*\[Omega]]]; If[i == 4, ph = \[Omega]; 
          If[c < 0, ph = \[Omega]^2]]; If[i == 2, ph = -\[Omega]^2; 
          If[c < 0, ph = -\[Omega]]]; If[i == 1, ph = (-I)*\[Omega]; 
          If[c < 0, ph = I*\[Omega]^2]]; Assert[(ph === 0) == False]; 
        Return[Abs[a]*ph]; ]; ph = ToExpPhase[c]; If[ph === Infinity, 
       Return[a], Return[Simplify[Abs[a]]*ph]]]
