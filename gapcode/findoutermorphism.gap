
# find finitely presented group for group G.
FindFinitelyPresentedGroup := function(G)
    local H, K;
    H:=Image(IsomorphismFpGroup(G));
    Print("Generators: ", GeneratorsOfGroup(H), "\n");
    #Print("Elements: ", Elements(H), "\n");
    K:=SimplifiedFpGroup(H);
    return RelatorsOfFpGroup(K);
end;

# find outermorphism of the group PSL(2,7)
FindPsl27Outermorphism := function()
    local G, a, b, autG, innG, innG2, inna, innb, ima, imb, clist, i, Gb, hom, res;
    G := PSL(2,7);

    # find the generator a and b of the group.
    b := First(GeneratorsOfGroup(G), x->Order(x)=3);
    a := First(Elements(G), x-> Order(x) = 2 and Order(x*b)=7 and Order(x*b*x^-1*b^-1)=4);

    # the generators 
    autG := AutomorphismGroup(G);
    innG := InnerAutomorphismsAutomorphismGroup(autG);

    # verify that inna and innb are elements of the inner automorphism group.
    inna := InnerAutomorphism(G, a);
    innb := InnerAutomorphism(G, b);
    Assert(0, inna in innG, "Inna is not an element of inner automorphism group.\n");
    Assert(0, innb in innG, "Innb is not an element of inner automorphism group.\n");
    
    # verify that inna and innb generate the inner automorphism group.
    innG2 := GroupByGenerators([inna, innb]);
    Assert(0, Size(innG2) = Size(innG), "Inna and Innb do not generate the inner automorphism group.\n");
    
    # find the outer automorphism c such that c^-1*inna*c=inna^-1 and c^-1*innb*c=innb^-1
    clist := Filtered(Elements(autG), x -> Order(x) = 2 and x^-1*inna*x = inna^-1 and x^-1*innb*x=innb^-1 and not x in innG);

    # find the fintely presentation of the mapping c.
    res := [];
    for i in [1..Length(clist)] do
        ima := Image(clist[i], a);
        imb := Image(clist[i], b);
        
        Gb := GroupByGenerators([a, b]);
        hom := EpimorphismFromFreeGroup(Gb:names:=["a","b"]);
        
        Add(res, [PreImagesRepresentative(hom, ima), PreImagesRepresentative(hom, imb)]);
    od;
    
    return res;
end;

# find finted presentation of PGL27. 
# The results shows that, the PGL can be presented as: a^2=b^3=(ab)^7=[a,b]^4=c^2=e, c^-1*a*c=a, c^-1*b*c=b^2,
# where a, b generate the PSL27 subgroup, and c generates the Z2 subgroup.
FinitedPresentationOfPGL27 := function()
    local G, a, b, clist, order2Ops, elist, subG, subElist, i, a2, b2, res, newG, hom, clist2;
    G := PGL(2,7);
    elist := Elements(G);
    
    # find PSL27 generators a, b.
    b := First(elist, x->Order(x) = 3);
    order2Ops := Filtered(elist, x->Order(x) = 2);
    a := First(order2Ops, x->Order(x*b) = 7 and Order(x*b*x^-1*b^-1)=4);
    
    # find all order 2 elements not in the PSL27 sub group.
    subG := GroupByGenerators([a, b]);
    subElist := Elements(subG);    
    clist := Filtered(order2Ops, x -> not x in subElist);

    # verify that the PSL27 subgroup does not contain an element c such that c^2=1, c^-1*a*c=a^-1, c^-1*b*c=b^-1.
    clist2 := Filtered(subElist, x -> Order(x) = 2 and Order(a^-1*x^-1*a*x) = 1 and Order(b^-1*x^-1*b*a) = 1);
    Print("clist2=", clist2, "\n");

    # find the results of c^-1*a*c and c^-1*b*c.
    res := [];

    for i in [1..Length(clist)] do
        newG := GroupByGenerators([a, b, clist[i]]);
        if Size(newG) <> Length(elist) then
            Print("wrong group.. ");
        fi;

        a2 := clist[i]^-1*a*clist[i];
        b2 := clist[i]^-1*b*clist[i];
        hom := EpimorphismFromFreeGroup(subG:names:=["a","b"]);
        
        Add(res, [PreImagesRepresentative(hom, a2), PreImagesRepresentative(hom, b2)]);
    od;

    return res;
end;

# find relation of inner automorphism group of PSL27
PSL27InnerAutomorphismGroup := function()
    local G, a, b, inna, innb;
    G := PSL(2,7);

    # b: the generator b of the group.
    b := GeneratorsOfGroup(G)[1];
    a := First(Elements(G), x-> Order(x) = 2 and Order(x*b)=7 and Order(x*b*x^-1*b^-1)=4);

    inna := InnerAutomorphism(G, a);
    innb := InnerAutomorphism(G, b);

    Print("order(a)=", Order(inna), ", order(b)=", Order(innb), ", order(a*b)=", Order(inna*innb), ",  order([a,b])=", Order(inna*innb*inna^-1*innb^-1));
end;

# identify A4 generators from PSL27 elements.
# The A4 generators are s and t fulfill
# s^2=t^3=(st)^3=e.
IdentifyA4FromPSL27:=function(first)
    local G, GFp, slist, t, a, b, elist, i, res, hom;
    
    G := PSL(2,7);
    # we identify t to generator b of PSL27.
    if first then
        b := First(GeneratorsOfGroup(G), x-> Order(x) = 3);
    else
        b := Filtered(GeneratorsOfGroup(G), x-> Order(x) = 3)[2];
    fi;
    t := b;

    # find generator a of PSL27.
    elist := Elements(G);
    a := First(elist, x-> Order(x) = 2 and Order(x*b)=7 and Order(x*b*x^-1*b^-1) = 4);

    # now we need to find s.
    slist := Filtered(elist, x-> Order(x) = 2 and Order(x*t) = 3);
    
    # find relationship between a and s.
    GFp := GroupByGenerators([a, b]);
    res := [];
    hom := EpimorphismFromFreeGroup(GFp:names:=["a","b"]);

    for i in [1..Length(slist)] do
        Add(res, [PreImagesRepresentative(hom, slist[i]), PreImagesRepresentative(hom, t)]);
    od;

    return res;
end;

# identify A4 generators from S4 elements.
# The A4 generators are s and t fulfill
# s^2=t^3=(st)^3=e.
# S4 generators are: a^4=b^2=(ab)^3=e.
IdentifyA4FromS4:=function()
    local G, a, b, s, elist, tlist, i, hom, GFp, res;
    G := SymmetricGroup(4);
    a := First(GeneratorsOfGroup(G), x -> Order(x) = 4);
    b := First(GeneratorsOfGroup(G), x -> Order(x) = 2);

    # identify s = a^2;
    s := a^2;

    # find all possible t.    
    elist := Elements(G);
    tlist := Filtered(elist, x -> Order(x) = 3 and Order(s*x) = 3);
    
    # find relationship between t and a, b.
    GFp := GroupByGenerators([a, b]);
    res := [];
    hom := EpimorphismFromFreeGroup(GFp:names:=["a","b"]);
    
    for i in [1..Length(tlist)] do
        Add(res, [PreImagesRepresentative(hom, s), PreImagesRepresentative(hom, tlist[i])]);
    od;

    return res;
end;

# identify S4 generators from PSL(2,7) generators.
# PSL(2,7): A^2=B^3=(AB)^7=[A,B]^4=e
# S4: a^4=b^2=(ab)^3=e
# possible try is that let b=A
IdentifyS4FromPSL27A:=function(first)
    local G, GFp, alist, A, B, a, b, elist, i, res, hom;
    
    G := PSL(2,7);
    elist := Elements(G);
    # first find A, B of PSL27
    if first then
        B := First(GeneratorsOfGroup(G), x-> Order(x) = 3);
    else
        B := Filtered(GeneratorsOfGroup(G), x-> Order(x) = 3)[2];
    fi;

    # find generator A of PSL27.    
    A := First(elist, x-> Order(x) = 2 and Order(x*B)=7 and Order(x*B*x^-1*B^-1) = 4);

    # Identify b to A
    b := A;
    # now we need to find a.
    alist := Filtered(elist, x-> Order(x) = 4 and Order(x*b) = 3);
    
    # find relationship between a and s.
    GFp := GroupByGenerators([A, B]);
    res := [];
    hom := EpimorphismFromFreeGroup(GFp:names:=["A","B"]);

    for i in [1..Length(alist)] do
        Add(res, [PreImagesRepresentative(hom, alist[i]), PreImagesRepresentative(hom, b)]);
    od;

    return res;
end;

# identify S4 generators from PSL(2,7) generators.
# PSL(2,7): A^2=B^3=(AB)^7=[A,B]^4=e
# S4: a^4=b^2=(ab)^3=e
# second try: let ab=B
IdentifyS4FromPSL27B:=function(first)
    local G, GFp, alist, A, B, a, ab, b, elist, i, res, hom;
    
    G := PSL(2,7);
    elist := Elements(G);
    # first find A, B of PSL27
    if first then
        B := First(GeneratorsOfGroup(G), x-> Order(x) = 3);
    else
        B := Filtered(GeneratorsOfGroup(G), x-> Order(x) = 3)[2];
    fi;

    # find generator A of PSL27.    
    A := First(elist, x-> Order(x) = 2 and Order(x*B)=7 and Order(x*B*x^-1*B^-1) = 4);

    # Identify ab to B
    ab := B;
    # now we need to find a.
    alist := Filtered(elist, x-> Order(x) = 4 and Order(x^-1*ab) = 2);
    
    # find relationship between a and s.
    GFp := GroupByGenerators([A, B]);
    res := [];
    hom := EpimorphismFromFreeGroup(GFp:names:=["A","B"]);

    for i in [1..Length(alist)] do
        b := alist[i]^-1*ab;
        Add(res, [PreImagesRepresentative(hom, alist[i]), PreImagesRepresentative(hom, b)]);
    od;

    return res;
end;

# identify S4 generators from PSL(2,7) generators.
# PSL(2,7): A^2=B^3=(AB)^7=[A,B]^4=e
# S4: a^4=b^2=(ab)^3=e
# third try: let a=[A,B]
IdentifyS4FromPSL27C:=function(first)
    local G, GFp, blist, ablist, A, B, a, b, elist, i, res, hom;
    
    G := PSL(2,7);
    elist := Elements(G);
    # first find A, B of PSL27
    if first then
        B := First(GeneratorsOfGroup(G), x-> Order(x) = 3);
    else
        B := Filtered(GeneratorsOfGroup(G), x-> Order(x) = 3)[2];
    fi;

    # find generator A of PSL27.    
    A := First(elist, x-> Order(x) = 2 and Order(x*B)=7 and Order(x*B*x^-1*B^-1) = 4);

    # Identify a to [A,B]
    a := A*B*A^-1*B^-1;
    # now we need to find b or ab.
    blist := Filtered(elist, x-> Order(x) = 2 and Order(a*x) = 3);
    ablist := Filtered(elist, x-> Order(x)=3 and Order(a^-1*x)=2);
    
    # find relationship between generators.
    GFp := GroupByGenerators([A, B]);
    res := [];
    hom := EpimorphismFromFreeGroup(GFp:names:=["A","B"]);
    Print("a=[A,B]\n");

    for i in [1..Length(blist)] do
        Add(res, ["b", PreImagesRepresentative(hom, blist[i])]);
    od;

    for i in [1..Length(ablist)] do
        Add(res, ["ab", PreImagesRepresentative(hom, ablist[i])]);
    od;

    return res;
end;
