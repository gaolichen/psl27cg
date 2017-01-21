GAPToMathematica[pstr_] := StringReplace[pstr, {"[" -> "{", "]" -> "}", 
      RegularExpression["E\\((\\d+)\\)"] -> "Exp[2*Pi*I/$1]"}]
 
ReadGAP[fname_] := Module[{f, str}, f = OpenRead[fname]; 
      str = StringJoin[(StringJoin[GAPToMathematica[#1], "\n"] & ) /@ 
         ReadList[f, String]]; Close[f]; Return[str]; ]
 
ReadGAPDataFromFile[fname_] := Module[{f}, 
     f = StringToStream[ReadGAP[fname]]; Return[Read[f, Expression]]]
 
LoadGroupFromFile[filename_, opts:OptionsPattern[]] := 
    Module[{f, ret, name, value}, f = StringToStream[ReadGAP[filename]]; 
      While[True, name = Read[f, Expression]; If[name == EndOfFile, Break[]]; 
        ret[name] = Read[f, Expression]; ]; Return[ret]; ]
