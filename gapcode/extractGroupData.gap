LoadPackage("repsn");;

PrintString2File:=function(out, s)
    PrintTo(out, "\"", s, "\"\n");
end;;

PrintStringData := function(out, name, data)
    PrintString2File(out, name);
    PrintString2File(out, data);
end;;

PrintData := function(out, name, data)
    PrintString2File(out, name);
    PrintTo(out, data, "\n");
end;;

# Get group information about one group and store the info in file.
# Each data is stored with two lines. First line is the name of the data, and second line is the data, e.g.,
# "Size"
# 168
GetGroupInfo := function(G, output)
    local out, charTab, sizeRep, reps, gens, allMatrices, temp, i, j;

    out := OutputTextFile(output, false);;
    charTab :=Irr(G);;
    gens := GeneratorsOfGroup(G);;
    sizeRep := [];;
    reps := [];;
    allMatrices := [];;
    
    # get all representations.
    for i in [1..Length(charTab)] do
       Add(sizeRep,charTab[i][1]);;
	   Add(reps,IrreducibleAffordingRepresentation(charTab[i]));;
    od;;
    
    # get the matrix representations of generators.
    for i in [1..Length(charTab)] do
        temp:=[];;
	    for j in [1..Length(gens)] do
	        Add(temp, gens[j]^reps[i]);;
	    od;

        Add(allMatrices,temp);;
    od;;


    PrintStringData(out, "StructureDescription", StructureDescription(G));
    PrintData(out, "Order", Size(G));
    PrintData(out, "DimensionsOfReps", sizeRep);
    PrintData(out, "SizeConjugacyClasses", SizesConjugacyClasses(CharacterTable(G)));
    PrintData(out, "CharacterTable", List(charTab,h->List(h)));
    PrintData(out, "Generators", allMatrices);

    CloseStream(out);

    Print("Sucessfully write information of the group ", StructureDescription(G), " to ", output, "\n");
end;;

# Get all group elements in all irreducible representations.
GetAllGroupElements := function(G, output)
    local out, charTab, elem, allMatrices, reps, size, temp, i, j;

    out := OutputTextFile(output, false);;
    charTab := Irr(G);;
    elem := Elements(G);;
    size := Size(G);;
    allMatrices := [];;
    reps := [];;

    # get all representations.
    for i in [1..Length(charTab)] do
	   Add(reps, IrreducibleAffordingRepresentation(charTab[i]));;
    od;;

    # get the matrix representations of elements.
    for i in [1..Length(charTab)] do
        temp:=[];;
	    for j in [1..size] do
	        Add(temp, elem[j]^reps[i]);;
	    od;

        Add(allMatrices,temp);;
    od;;

    # output
    PrintTo(out, allMatrices, "\n");
    CloseStream(out);

    Print("Sucessfully write all elements of the group, ", StructureDescription(G), " to ", output, "\n");
end;;
