
ListMultiply:=function(list1, list2)
    local i, res;
    res := [];

    for i in [1..Length(list1)] do
        Add(res, list1[i]*list2[i]);
    od;

    return res;
end;;

DecomposeDirectProduct:=function(G)
    local charTab, csizes, irr, isRealIrr, i, j, k, h, list1, list2, list3, mu, res;

    charTab:=CharacterTable(G);
    csizes := SizesConjugacyClasses(charTab);
    irr:=Irr(charTab);
    isRealIrr:=[];
    for i in [1..Length(irr)] do
        Add(isRealIrr, true);
        for j in [1..Length(irr[i])] do
            if ImaginaryPart(irr[i][j]) <> 0 then
                isRealIrr[i] := false;
                break;
            fi;
        od;
    od;

    Display(charTab);
    Display(isRealIrr);
    
    res := [];
    for i in [1..Length(irr)] do
        if irr[i][1] = 1 then 
            continue;
        fi;
        for j in [i..Length(irr)] do
            if irr[j][1] = 1 then
                continue;
            fi;

            list1 := ListMultiply(irr[i], irr[j]);
            list1 := ListMultiply(list1, csizes);
            
            list2 := [];
            for k in [1..Length(irr)] do
                mu := ComplexConjugate(List(irr[k])) * list1 / Size(G);
                
                for h in [1..mu] do
                    Add(list2, k);
                od;                
            od;
            Add(res, [i, j, list2]);
        od;
	od;

    return res;
end;;

IsCGFixed:=function(G)
    local charTab, csizes, irr, isRealIrr, i, j, k, list1, list2, mu;

    charTab:=CharacterTable(G);
    csizes := SizesConjugacyClasses(charTab);
    irr:=Irr(charTab);
    isRealIrr:=[];
    for i in [1..Length(irr)] do
        Add(isRealIrr, true);
        for j in [1..Length(irr[i])] do
            if ImaginaryPart(irr[i][j]) <> 0 then
                isRealIrr[i] := false;
                break;
            fi;
        od;
    od;
    
    for i in [2..Length(irr)] do
        for j in [i..Length(irr)] do
            list1 := ListMultiply(irr[i], irr[j]);
            list1 := ListMultiply(list1, csizes);
            
            list2 := [];
            for k in [1..Length(irr)] do
                if isRealIrr[k] = true then
                    continue;
                fi;

                mu := ComplexConjugate(List(irr[k])) * list1 / Size(G);
                if (i = j and mu > 3) or (i <> j and mu = 2) then
                    return false;
                fi;
            od;
        od;
	od;

    return true;
end;;

CheckAllSmallGroupCGFixed:=function(n)
    local i, glist;
    glist := AllSmallGroups(n);
    for i in [1..Length(glist)] do
        if IsCGFixed(glist[i]) = false then
            Print("CG is not fixed for ", StructureDescription(glist[i]), "\n");
            Print(DecomposeDirectProduct(glist[i]), "\n");
            return false;
        fi;
    od;

    Print("CG are fixed for ", Length(glist), " groups of order ", n, "\n");
    return true;
end;;

CheckSmallGroupCGFixedAll:=function(minN, maxN)
    local i;
    for i in [minN..maxN] do
        if CheckAllSmallGroupCGFixed(i) = false then
            return;
        fi;
    od;

    Print("All groups of order less than ", maxN, " are CG-fixed.");
end;;

