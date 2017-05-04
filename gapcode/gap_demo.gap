
# add all numbers between n and m.
SumOfNumbersBetween:=function(n,m)
    local i, res;
    if n > m then
        return 0;
    fi;

    res := 0;
    for i in [n..m] do
        res := res + i;
    od;

    Print("Sum of numbers between ", n, " and ", m, " is ", res, ".\n");

    return res;
end;


CreateGroupDemo := function()
    local G1, G2;
    G1 := PSL(2,7);
    G2 := SymmetricGroup(4);

    Print("G1 = ", StructureDescription(G1), "\n");
    Print("G2 = ", StructureDescription(G2), "\n");
end;
