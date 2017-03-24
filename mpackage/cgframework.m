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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 1, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = Kronecker[lg, rlist[[i]], rlist[[j]]]; 
          For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i]], rlist[[j]], klist[[k]], embed]; 
            lg[rlist[[i]], rlist[[j]], klist[[k]], KeyCGTerms] = 
             terms; ]; ]; ]; ]
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 1, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = Kronecker[lg, rlist[[i,1]], rlist[[j,1]]]; 
          For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; ]; ]; ]
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 1, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = Kronecker[lg, rlist[[i,1]], rlist[[j,1]]]; 
          For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 1, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = GetReWithSym[Kronecker[lg, rlist[[i,1]], rlist[[j,1]]]]; 
          For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 1, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = GetRepWithSym[Kronecker[lg, rlist[[i,1]], rlist[[j,1]]]]; 
          For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 1, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 1, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; Print["subr=", subr]; 
      For[i = 1, i <= Length[subr], i++, 
       res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], dec], 
          embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; Print["subr=", subr]; 
      For[i = 1, i <= Length[subr], i++, 
       res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], dec], 
          embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; Print[rlist[[i,1]], " ", rlist[[j,1]], 
             " ", klist[[k]], "=", terms]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = lg[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[k]]], 
              KeyCGTerms]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGCoefficient] = Table[Unique["CG"], {h, 1, Length[
                terms]}]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = lg[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[k]]], 
              KeyCGTerms]; lg[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGCoefficient] = Table[Unique["CG"], {h, 1, Length[
                terms]}]; ]; ]; ]; ]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       AppendTo[vlist, ConstantArray[0, GetDimensionByRep[subg, subr[[i]]]]]; 
        For[j = 1, j <= Length[terms], j++, 
         If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, vlist[[i]] += cgc[[j]]*
              subdot[subv1, subv2, terms[[j,2]], terms[[j,3]], terms[[j,
                1]]]; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*subdot[subv1, subv2, terms[[j,
                2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             (cgc[[j]]/Sqrt[2])*terms[[j,4]]*subdot[subv3, subv4, terms[[j,
                3]], terms[[j,2]], terms[[j,1]]]; ]; ]; ]; Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       Print[subr[[i]], ":", GetDimensionByRep[subg, subr[[i]]]]; 
        AppendTo[vlist, ConstantArray[0, GetDimensionByRep[subg, 
           subr[[i]]]]]; For[j = 1, j <= Length[terms], j++, 
         If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, vlist[[i]] += cgc[[j]]*
              subdot[subv1, subv2, terms[[j,2]], terms[[j,3]], terms[[j,
                1]]]; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*subdot[subv1, subv2, terms[[j,
                2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             (cgc[[j]]/Sqrt[2])*terms[[j,4]]*subdot[subv3, subv4, terms[[j,
                3]], terms[[j,2]], terms[[j,1]]]; ]; ]; ]; Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       Print[subr[[i]], ":", GetDimensionByRep[subg, subr[[i]]]]; 
        AppendTo[vlist, ConstantArray[0, GetDimensionByRep[subg, 
           subr[[i]]]]]; Print[vlist]; For[j = 1, j <= Length[terms], j++, 
         If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, vlist[[i]] += cgc[[j]]*
              subdot[subv1, subv2, terms[[j,2]], terms[[j,3]], terms[[j,
                1]]]; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*subdot[subv1, subv2, terms[[j,
                2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             (cgc[[j]]/Sqrt[2])*terms[[j,4]]*subdot[subv3, subv4, terms[[j,
                3]], terms[[j,2]], terms[[j,1]]]; ]; ]; ]; Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       Print[subr[[i]], ":", GetDimensionByRep[subg, subr[[i]]]]; 
        Print[vlist]; AppendTo[vlist, ConstantArray[0, GetDimensionByRep[
           subg, subr[[i]]]]]; Print[vlist]; For[j = 1, j <= Length[terms], 
         j++, If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, vlist[[i]] += cgc[[j]]*
              subdot[subv1, subv2, terms[[j,2]], terms[[j,3]], terms[[j,
                1]]]; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*subdot[subv1, subv2, terms[[j,
                2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             (cgc[[j]]/Sqrt[2])*terms[[j,4]]*subdot[subv3, subv4, terms[[j,
                3]], terms[[j,2]], terms[[j,1]]]; ]; ]; ]; Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot, tmp}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       Print[subr[[i]], ":", GetDimensionByRep[subg, subr[[i]]]]; 
        Print[vlist]; AppendTo[vlist, ConstantArray[0, GetDimensionByRep[
           subg, subr[[i]]]]]; Print[vlist]; For[j = 1, j <= Length[terms], 
         j++, If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, tmp = subdot[subv1, subv2, 
              terms[[j,2]], terms[[j,3]], terms[[j,1]]]; Print["tmp=", tmp]; 
            vlist[[i]] += cgc[[j]]*tmp; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            tmp = subdot[subv3, subv4, terms[[j,3]], terms[[j,2]], 
              terms[[j,1]]]; Print["tmp=", tmp]; vlist[[i]] += 
             (cgc[[j]]/Sqrt[2])*subdot[subv1, subv2, terms[[j,2]], terms[[j,
                3]], terms[[j,1]]]; vlist[[i]] += (cgc[[j]]/Sqrt[2])*
              terms[[j,4]]*tmp; ]; ]; ]; Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
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
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot, tmp}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       Print[subr[[i]], ":", GetDimensionByRep[subg, subr[[i]]]]; 
        Print[vlist]; AppendTo[vlist, ConstantArray[0, GetDimensionByRep[
           subg, subr[[i]]]]]; Print[vlist]; For[j = 1, j <= Length[terms], 
         j++, If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, tmp = subdot[subv1, subv2, 
              terms[[j,2]], terms[[j,3]], terms[[j,1]]]; Print["tmp=", tmp]; 
            If[Length[tmp] == 0, Print["v1=", subv1, ",v2=", subv2, "term=", 
              terms[[j]]]]; vlist[[i]] += cgc[[j]]*tmp; ]; 
          If[Length[terms[[j]]] == 4, subv3 = ToSubRep[v1, r1, terms[[j,3]], 
              embed]; subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            tmp = subdot[subv3, subv4, terms[[j,3]], terms[[j,2]], 
              terms[[j,1]]]; Print["tmp=", tmp]; If[Length[tmp] == 0, 
             Print["v1=", subv3, ",v2=", subv3, "term=", terms[[j]]]]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*subdot[subv1, subv2, terms[[j,
                2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             (cgc[[j]]/Sqrt[2])*terms[[j,4]]*tmp; ]; ]; ]; Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
GetDimensionByRep[gi_, r_] := Module[{v}, 
     v = SelectFirst[gi[KeyIrr], First[#1] == GetRepName[r] & ]; 
      Assert[Length[v] >= 2]; Return[v[[2]]]]
 
Attributes[Assert] = {HoldAllComplete}
 
Assert /: Assert::asrtf = "Assertion `1` failed."
 
Assert /: Assert::asrttf = 
     "Assertion test `1` evaluated to `2` that is neither True nor False."
 
SingletRepresentation[gi_] := gi[KeyIrr][[1,1]]
 
Attributes[ToConjugateRep] = {Listable}
 
ToConjugateRep[gi_, r_] := Module[{conj, rn, rd}, rn = GetRepName[r]; 
      conj = SelectFirst[gi[KeyConjugateIrr], 
        #1[[1]] == rn || #1[[2]] == rn & ]; If[conj == Missing["NotFound"], 
       Return[r]]; rd = GetRepDecorate[r]; If[conj[[1]] == rn, 
       Return[ToFullRep[conj[[2]], rd]], Return[ToFullRep[conj[[1]], rd]]]; ]
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot, tmp}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       Print[subr[[i]], ":", GetDimensionByRep[subg, subr[[i]]]]; 
        Print[vlist]; AppendTo[vlist, ConstantArray[0, GetDimensionByRep[
           subg, subr[[i]]]]]; Print[vlist]; For[j = 1, j <= Length[terms], 
         j++, If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, tmp = subdot[subv1, subv2, 
              terms[[j,2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             cgc[[j]]*tmp; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            tmp = subdot[subv3, subv4, terms[[j,3]], terms[[j,2]], 
              terms[[j,1]]]; vlist[[i]] += (cgc[[j]]/Sqrt[2])*
              subdot[subv1, subv2, terms[[j,2]], terms[[j,3]], terms[[j,1]]]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*terms[[j,4]]*tmp; ]; ]; ]; 
      Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
GetDimensionByRep[gi_, r_] := Module[{v}, 
     v = SelectFirst[gi[KeyIrr], First[#1] == GetRepName[r] & ]; 
      Assert[Length[v] >= 2]; Return[v[[2]]]]
 
Attributes[Assert] = {HoldAllComplete}
 
Assert /: Assert::asrtf = "Assertion `1` failed."
 
Assert /: Assert::asrttf = 
     "Assertion test `1` evaluated to `2` that is neither True nor False."
 
SingletRepresentation[gi_] := gi[KeyIrr][[1,1]]
 
Attributes[ToConjugateRep] = {Listable}
 
ToConjugateRep[gi_, r_] := Module[{conj, rn, rd}, rn = GetRepName[r]; 
      conj = SelectFirst[gi[KeyConjugateIrr], 
        #1[[1]] == rn || #1[[2]] == rn & ]; If[conj == Missing["NotFound"], 
       Return[r]]; rd = GetRepDecorate[r]; If[conj[[1]] == rn, 
       Return[ToFullRep[conj[[2]], rd]], Return[ToFullRep[conj[[1]], rd]]]; ]
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot, tmp}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       Print[subr[[i]], ":", GetDimensionByRep[subg, subr[[i]]]]; 
        Print[vlist]; AppendTo[vlist, ConstantArray[0, GetDimensionByRep[
           subg, subr[[i]]]]]; Print[vlist]; For[j = 1, j <= Length[terms], 
         j++, If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, tmp = subdot[subv1, subv2, 
              terms[[j,2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             cgc[[j]]*tmp; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            tmp = subdot[subv3, subv4, terms[[j,3]], terms[[j,2]], 
              terms[[j,1]]]; vlist[[i]] += (cgc[[j]]/Sqrt[2])*
              subdot[subv1, subv2, terms[[j,2]], terms[[j,3]], terms[[j,1]]]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*terms[[j,4]]*tmp; ]; ]; ]; 
      Return[vlist]]
KeyIrr = "Irr"
 
KeyConjugateIrr = "ConjugateIrr"
 
KeyKronecker = "KroneckerProduct"
 
KeyDotFunction = "DotFunction"
 
KeyCGCoefficient = "CGCoefficient"
 
KeyLargeGroup = "LargeGroup"
 
KeySubGroup = "SubGroup"
 
KeyTransformMatrix = "TransformMatrix"
 
Attributes[GetRepName] = {Listable}
 
GetRepName[r_] := If[StringContainsQ[r, ":"], First[StringSplit[r, ":"]], r]
 
Attributes[GetRepWithSym] = {Listable}
 
GetRepWithSym[r_] := Module[{parts}, If[StringContainsQ[r, ":"] == False, 
       Return[r]]; parts = StringSplit[r, ":"]; 
      If[StringContainsQ[Last[parts], "+"], Return[ToFullRep[First[parts], 
         "+"]]]; If[StringContainsQ[Last[parts], "-"], 
       Return[ToFullRep[First[parts], "-"]]]; Return[First[parts]]; ]
 
ToFullRep[name_, decorate_] := If[decorate == "", name, 
     StringJoin[name, ":", decorate]]
 
GetRepDecorate[r_] := If[StringContainsQ[r, ":"], Last[StringSplit[r, ":"]], 
     ""]
 
GetDimensionByRep[gi_, r_] := Module[{v}, 
     v = SelectFirst[gi[KeyIrr], First[#1] == GetRepName[r] & ]; 
      Assert[Length[v] >= 2]; Return[v[[2]]]]
 
Attributes[Assert] = {HoldAllComplete}
 
Assert /: Assert::asrtf = "Assertion `1` failed."
 
Assert /: Assert::asrttf = 
     "Assertion test `1` evaluated to `2` that is neither True nor False."
 
SingletRepresentation[gi_] := gi[KeyIrr][[1,1]]
 
Attributes[ToConjugateRep] = {Listable}
 
ToConjugateRep[gi_, r_] := Module[{conj, rn, rd}, rn = GetRepName[r]; 
      conj = SelectFirst[gi[KeyConjugateIrr], 
        #1[[1]] == rn || #1[[2]] == rn & ]; If[conj == Missing["NotFound"], 
       Return[r]]; rd = GetRepDecorate[r]; If[conj[[1]] == rn, 
       Return[ToFullRep[conj[[2]], rd]], Return[ToFullRep[conj[[1]], rd]]]; ]
 
DefaultKronecker[gi_, r1_, r2_] := Module[{}, 
     If[r1 == SingletRepresentation[gi] && r2 == r1, 
       Return[{ToFullRep[SingletRepresentation[gi], "+"]}]]; 
      If[r1 == SingletRepresentation[gi], Return[{r2}]]; 
      If[r2 == SingletRepresentation[gi], Return[{r1}]]; Return[{}]]
 
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
 
DefaultEmbed[r_, largeG_, subG_] := If[r == SingletRepresentation[largeG], 
     {SingletRepresentation[subG]}, {}]
 
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
 
BuildCGTermsSub[r1_, r2_, subr_, embed_] := 
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
 
BuildCGTerms[r1_, r2_, r3_, embed_] := Module[{subg, subr, i, dec, rn, 
      res = {}}, subg = embed[KeySubGroup]; dec = GetRepDecorate[r3]; 
      rn = GetRepName[r3]; subr = embed[rn]; For[i = 1, i <= Length[subr], 
       i++, res = Join[res, BuildCGTermsSub[r1, r2, ToFullRep[subr[[i]], 
           dec], embed]]]; Return[res]]
 
KeyCGTerms = "CGTerms"
 
BuildCGTermsAll[embed_] := Module[{lg, rlist, i, j, k, klist, terms}, 
     lg = embed[KeyLargeGroup]; rlist = lg[KeyIrr]; 
      For[i = 2, i <= Length[rlist], i++, For[j = i, j <= Length[rlist], j++, 
         klist = DeleteDuplicates[GetRepWithSym[Kronecker[lg, rlist[[i,1]], 
              rlist[[j,1]]]]]; For[k = 1, k <= Length[klist], k++, 
           terms = BuildCGTerms[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              embed]; embed[rlist[[i,1]], rlist[[j,1]], klist[[k]], 
              KeyCGTerms] = terms; ]; klist = Kronecker[lg, rlist[[i,1]], 
            rlist[[j,1]]]; For[k = 1, k <= Length[klist], k++, 
           terms = embed[rlist[[i,1]], rlist[[j,1]], GetRepWithSym[klist[[
                k]]], KeyCGTerms]; embed[rlist[[i,1]], rlist[[j,1]], 
              klist[[k]], KeyCGCoefficient] = Table[Unique["CG"], 
              {h, 1, Length[terms]}]; ]; ]; ]; ]
 
DotRep[v1_, v2_, r1_, r2_, r3_, embed_] := 
    Module[{terms, cgc, lg, subg, i, j, subr, subv1, subv2, subv3, subv4, 
      vlist = {}, subdot, tmp}, lg = embed[KeyLargeGroup]; 
      subg = embed[KeySubGroup]; subdot = subg[KeyDotFunction]; 
      terms = embed[r1, r2, GetRepWithSym[r3], KeyCGTerms]; 
      cgc = embed[r1, r2, r3, KeyCGCoefficient]; 
      subr = embed[GetRepName[r3]]; For[i = 1, i <= Length[subr], i++, 
       AppendTo[vlist, ConstantArray[0, GetDimensionByRep[subg, subr[[i]]]]]; 
        For[j = 1, j <= Length[terms], j++, 
         If[GetRepName[terms[[j,1]]] != subr[[i]], Continue[]]; 
          subv1 = ToSubRep[v1, r1, terms[[j,2]], embed]; 
          subv2 = ToSubRep[v2, r2, terms[[j,3]], embed]; 
          If[Length[terms[[j]]] == 3, tmp = subdot[subv1, subv2, 
              terms[[j,2]], terms[[j,3]], terms[[j,1]]]; vlist[[i]] += 
             cgc[[j]]*tmp; ]; If[Length[terms[[j]]] == 4, 
           subv3 = ToSubRep[v1, r1, terms[[j,3]], embed]; 
            subv4 = ToSubRep[v2, r2, terms[[j,2]], embed]; 
            tmp = subdot[subv3, subv4, terms[[j,3]], terms[[j,2]], 
              terms[[j,1]]]; vlist[[i]] += (cgc[[j]]/Sqrt[2])*
              subdot[subv1, subv2, terms[[j,2]], terms[[j,3]], terms[[j,1]]]; 
            vlist[[i]] += (cgc[[j]]/Sqrt[2])*terms[[j,4]]*tmp; ]; ]; ]; 
      Return[vlist]]
