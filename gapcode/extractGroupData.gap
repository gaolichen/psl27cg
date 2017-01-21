LoadPackage("repsn");;

MBgetGroupInfo:=function(G,output)
	     local elem,size,charTab,sizeRep,reps,i,gens,allMatrices,temp,j,str,out,out2;
	     out := OutputTextFile(output, false);;
	     elem:=Elements(G);;
	     size:=Size(G);;
	     charTab:=Irr(G);;
	     sizeRep:=[];;
	     reps:=[];;
	     for i in [1..Length(charTab)] do
	        Add(sizeRep,charTab[i][1]);;
   		    Add(reps,IrreducibleAffordingRepresentation(charTab[i]));;
	     od;;

	     gens:= GeneratorsOfGroup(G);;

	     allMatrices:=[];;
	     for i in [1..Length(charTab)] do
	        temp:=[];;
		    for j in [1..size] do
		        Add(temp,elem[j]^reps[i]);;
		    od;
	        Add(allMatrices,temp);;
	     od;;

	     PrintTo(out,"\"",StructureDescription(G),"\"","\n");
	     PrintTo(out,size,"\n");
	     PrintTo(out,sizeRep,"\n");
	     PrintTo(out,SizesConjugacyClasses(CharacterTable(G)),"\n");
	     PrintTo(out,List(charTab,h->List(h)),"\n");
	     #PrintTo(out,"matrix elements: \n");
	     PrintTo(out,allMatrices,"\n");
	     #PrintTo(out,"generates: \n");
	     PrintTo(out,gens,"\n");
	     CloseStream(out);
end;;
