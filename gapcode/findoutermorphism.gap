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
        hom := EpimorphismFromFreeGroup(Gb:names:=["x","y"]);
        
        Add(res, [PreImagesRepresentative(hom, ima), PreImagesRepresentative(hom, imb)]);
    od;

    Print("res: ", res, "\n");
end;
