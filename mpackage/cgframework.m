KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetDimensionByRep[gi_, r_] := Module[{v}, 
     v = SelectFirst[gi[KeyIrr], First[#1] == GetRepName[r] & ]; 
      Assert[Length[v] >= 2]; Return[v[[2]]]]
 
Attributes[Assert] = {HoldAllComplete}
 
SingletRepresentation[gi_] := gi[KeyIrr][[1,1]]
 
Attributes[ToConjugateRep] = {Listable}
 
ToConjugateRep[gi_, r_] := Module[{conj, rn, rd}, rn = GetRepName[r]; 
      conj = SelectFirst[gi[KeyConjugateIrr], 
        #1[[1]] == rn || #1[[2]] == rn & ]; If[conj == Missing["NotFound"], 
       Return[r]]; rd = GetRepDecorate[r]; If[conj[[1]] == rn, 
       Return[ToFullRep[conj[[2]], rd]], Return[ToFullRep[conj[[1]], rd]]]; ]
 
Kronecker[gi_, r1_, r2_] := Module[{kfun, ret}, 
     If[r1 == SingletRepresentation[gi] && r1 == r2, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; 
      kfun = gi[KeyKronecker]; ret = kfun[r1, r2]; If[Length[ret] > 0, 
       Return[ret]]; ret = kfun[r2, r1]; If[Length[ret] > 0, Return[ret]]; 
      ret = kfun[ToConjugateRep[gi, r1], ToConjugateRep[gi, r2]]; 
      If[Length[ret] > 0, Return[ToConjugateRep[gi, ret]]]; 
      ret = kfun[ToConjugateRep[gi, r2], ToConjugateRep[gi, r1]]; 
      If[Length[ret] > 0, Return[ToConjugateRep[gi, ret]]]; Return[{}]; ]
 
VerifyGroupInfo[gi_] := Module[{irr, i, j, k, tot, res, res2}, 
     irr = gi[KeyIrr]; For[i = 1, i <= Length[irr], i++, 
       For[j = i, j <= Length[irr], j++, res = Kronecker[gi, irr[[i,1]], 
            irr[[j,1]]]; res2 = Kronecker[gi, ToConjugateRep[gi, irr[[i,1]]], 
            ToConjugateRep[gi, irr[[j,1]]]]; res2 = ToConjugateRep[gi, res2]; 
          res = Sort[res]; res2 = Sort[res2]; Assert[res == res2]; 
          tot = Sum[GetDimensionByRep[gi, res[[k]]], {k, 1, Length[res]}]; 
          Assert[tot == GetDimensionByRep[gi, irr[[i,1]]]*GetDimensionByRep[
              gi, irr[[j,1]]]]]; ]; ]
 
VerifyEmbed[embed_] := Module[{g, subg, irrs, list, i, j, conj, list2}, 
     g = embed[KeyLargeGroup]; subg = embed[KeySubGroup]; 
      irrs = g[KeyIrr][[All,1]]; For[i = 2, i <= Length[irrs], i++, 
       list = embed[irrs[[i]]]; Assert[GetDimensionByRep[g, irrs[[i]]] == 
          Sum[GetDimensionByRep[subg, list[[j]]], {j, 1, Length[list]}]]; 
        conj = ToConjugateRep[g, irrs[[i]]]; If[conj == irrs[[i]], 
         Continue[]]; list2 = embed[conj]; list2 = ToConjugateRep[subg, 
          list2]; list2 = Sort[list2]; list = Sort[list]; 
        Assert[list == list2]; ]; ]
 
ToSubRep[v_, r_, subr_, embed_] := Module[{tmat, res, allsubr, i, subg, 
      index = 1}, allsubr = embed[r]; subg = embed[KeySubGroup]; 
      For[i = 1, i <= Length[allsubr], i++, 
       If[allsubr[[i]] == subr, Break[]]; index += GetDimensionByRep[subg, 
          allsubr[[i]]]; ]; If[i > Length[allsubr], 
       Print[r, " does not contains ", subr]; Return[{}]]; 
      res = v[[index ;; index + GetDimensionByRep[subg, subr] - 1]]; 
      tmat = SelectFirst[embed[KeyTransformMatrix], 
        #1[[1]] == r && #1[[2]] == subr & ]; 
      If[(tmat === Missing["NotFound"]) == False, res = tmat[[3]] . res; ]; 
      Return[res]; ]
 
BuildGeneralCGSub[r1_, r2_, subr_, embed_] := 
    Module[{subg, subr1, subr2, i, j, k, list, res, dec, rn}, 
     subg = embed[KeySubGroup]; subr1 = embed[r1]; subr2 = embed[r2]; 
      dec = GetRepDecorate[subr]; rn = GetRepName[subr]; res = {}; 
      For[i = 1, i <= Length[subr1], i++, For[j = 1, j <= Length[subr2], j++, 
         list = Kronecker[subg, subr1[[i]], subr2[[j]]]; 
          For[k = 1, k <= Length[list], k++, If[GetRepName[list[[k]]] != rn, 
             Continue[]]; If[dec == "" || GetRepDecorate[list[[k]]] == dec, 
             AppendTo[res, {list[[k]], subr1[[i]], subr2[[j]]}], 
             If[GetRepDecorate[list[[k]]] == "" && i < j, AppendTo[res, 
                {list[[k]], subr1[[i]], subr2[[j]], ToExpression[StringJoin[
                   dec, "1"]]}]; ]]; ]; ]; ]; Return[res]; ]
 
BuildGeneralCG[r1_, r2_, r3_, embed_] := 
    Module[{subg, subr, i, dec, rn, res = {}}, subg = embed[KeySubGroup]; 
      dec = GetRepDecorate[r3]; rn = GetRepName[r3]; subr = embed[rn]; 
      For[i = 1, i <= Length[subr], i++, 
       res = Join[res, BuildGeneralCGSub[r1, r2, ToFullRep[subr[[i]], dec], 
          embed]]]; Return[res]]
