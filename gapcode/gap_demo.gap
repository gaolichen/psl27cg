LoadPackage("repsn");;

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

# demo for list manipulation.
ListDemo := function()
    local x, y, z, w, u, v, y2;
    x := [];
    y := [1..9];
    z := ["a", "b", "c"];
    w := [[1, 2, 3], [4, 5]];
    Print("x=", x, "\n");
    Print("y=", y, "\n");
    Print("z=", z, "\n");
    Print("w=", w, "\n");

    w[2][1] := 2;
    Add(w, [6,7]);
    Print("w after change=", w, "\n");

    Print("all the numbers in y > 4 are ", List(y, h -> h > 4), "\n");
    Print("first prime in y is ", First(y, h -> IsPrime(h)), "\n");
    y2 := List(y, e -> e^2);
    Print("sqare of all elements in y = ", y2, "\n");
end;

CreateGroupDemo1 := function()
    local G1, G2;
    G1 := PSL(2,7);
    G2 := AlternatingGroup(4);

    Print("G1 = ", StructureDescription(G1), "\n");
    Print("G2 = ", StructureDescription(G2), "\n");
    Print("G2 generators = ", GeneratorsOfGroup(G2), "\n");
end;

CreateGroupDemo2 := function()
	local G;
	G := GroupByGenerators([(1, 2, 3), (2, 3, 4)]);
    Print("G = ", StructureDescription(G), "\n");
end;

CreateGroupDemo3 := function()
	local i, g, order, n;
	order := 12;
	n := NumberSmallGroups(order);
	for i in [1..n] do
		g := SmallGroup(order, i);
		Print("SmallGroup(", order, ", ", i, ")=", StructureDescription(g), "\n");
	od;
end;

CreateGroupDemo4 := function()
	local g, elm, i;
	g := AlternatingGroup(4);
	Print("A4 group info: \n");
	Print("Order=", Size(g), "\n");
	elm := Elements(g);
	for i in [1..Size(g)] do
		Print("Element ", i, " of A4: ", elm[i], "\n");
	od;
end;

CharacterTableDemo1 := function()
	local a4, charTab, classes, reps;
	a4 := AlternatingGroup(4);
	charTab := CharacterTable(a4);
	Display(charTab);

    classes := ConjugacyClasses(charTab);
    reps := List(classes, x->Representative(x));

    Print("Representative elements of classes: ", reps, "\n");
end;

CharacterTableDemo2 := function()
	local a4, charTab, irr, i;
	a4 := AlternatingGroup(4);
	irr := Irr(a4);
	Print("Character table of A4: \n");
	for i in [1..Length(irr)] do
		Print("Row ", i, ": ", List(irr[i]), "\n");
	od;
end;

RepresentationDemo1 := function()
	local irr, i, j, a4, gens, rep;
	a4 := AlternatingGroup(4);
	gens := GeneratorsOfGroup(a4);

	irr := Irr(a4);
	for i in [1..Length(irr)] do
		Print("Representation ", i, ": dimension ", irr[i][1], "\n\t");
		rep := IrreducibleAffordingRepresentation(irr[i]);
		for j in [1..Length(gens)] do
			Print("Generator ", j, " = ", gens[j]^rep, "\t");
		od;
		Print("\n");
	od;
end;

FindFinitelyPresentationGroup := function(G)
	local H, K;
	H := Image(IsomorphismFpGroup(G));
	K := SimplifiedFpGroup(H);
	return RelatorsOfFpGroup(K);
end;

FinitelyPresentationDemo := function()
    local g1, g2;
    g1 := SmallGroup(12, 3);
    g2 := AlternatingGroup(4);
    Print("g1=", StructureDescription(g1), "\n");
    Print("Generators of g1: ", GeneratorsOfGroup(g1), "\n");
    Print("Finitely presentation of g1: ", FindFinitelyPresentationGroup(g1), "\n");

    Print("g2=", StructureDescription(g2), "\n");
    Print("Generators of g2: ", GeneratorsOfGroup(g2), "\n");
    Print("Finitely presentation of g2: ", FindFinitelyPresentationGroup(g2), "\n");
end;

# find Automorphism of the group S4
AutomorphismDemo := function()
    local G, autG, innG, gens, inna, elms, i;
    G := AlternatingGroup(4);
    gens := GeneratorsOfGroup(G);
    autG := AutomorphismGroup(G);
    innG := InnerAutomorphismsAutomorphismGroup(autG);
    
    Print("AutomorphismGroup=", StructureDescription(autG), "\n");
    Print("Inner AutomorphismGroup=", StructureDescription(innG), "\n");

    inna := InnerAutomorphism(G, gens[1]);
    Print("Under inner automorphism: ", inna, "\n");
    elms := Elements(G);
    for i in [1..Length(elms)] do
        Print("\t", elms[i], "->", Image(inna, elms[i]), "\n");
    od;
end;

