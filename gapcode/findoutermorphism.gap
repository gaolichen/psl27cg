# this is the script to find outermorphism of the group PSL(2,7)

FindPsl27Outermorphism := function()
    local G, b, alist, autG, innG, s, news, imb, ima, i, Gb, hom, res;
    G := PSL(2,7);

    # b: the generator b of the group.
    b := GeneratorsOfGroup(G)[1];

    Print("Generator b of order ", Order(b), " is ", b, "\n");

    # alist: stores all the possible a generators.

    alist := Filtered(G, x->Order(x)=2);
    alist := Filtered(alist, x-> Order(x) = 2 and Order(x*b)=7 and Order(x*b*x^-1*b^-1)=4);
    Print("Number of possible a found: ", Length(alist), "\n");

    autG := AutomorphismGroup(G);
    innG := InnerAutomorphismsAutomorphismGroup(autG);
    s := First(GeneratorsOfGroup(autG),x->not x in innG);
    news := First(Elements(innG),x->Order(x*s)=2) * s;
    
    Print("Find an automorphism of order 2: ", news, "\n");
    
    imb := Image(news, b);
    res := [];
    for i in [1..Length(alist)] do
        ima := Image(news, alist[i]);
        
        Gb := GroupByGenerators([alist[i], b]);
        hom := EpimorphismFromFreeGroup(Gb:names:=["a","b"]);
        
        Add(res, [PreImagesRepresentative(hom, ima), PreImagesRepresentative(hom, imb)]);
    od;

    Print("res: ", res, "\n");
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
    local G, GFp, blist, A, B, a, b, elist, i, res, hom;
    
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
    a := A*B*A^-1*B^-1;
    # now we need to find a.
    blist := Filtered(elist, x-> Order(x) = 2 and Order(a*x) = 3);
    
    # find relationship between a and s.
    GFp := GroupByGenerators([A, B]);
    res := [];
    hom := EpimorphismFromFreeGroup(GFp:names:=["A","B"]);

    for i in [1..Length(blist)] do
        Add(res, [PreImagesRepresentative(hom, a), PreImagesRepresentative(hom, blist[i])]);
    od;

    return res;
end;

# find finitely presented group for group G.
FindFinitelyPresentedGroup := function(G)
    local H, K;
    H:=Image(IsomorphismFpGroup(G));
    Print("Generators: ", GeneratorsOfGroup(H), "\n");
    Print("Elements: ", Elements(H), "\n");
    K:=SimplifiedFpGroup(H);
    return RelatorsOfFpGroup(K);
end;
