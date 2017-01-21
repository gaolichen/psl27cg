checkForCIAutomorphismsOutG:=function(G)
local i,j,autG,ordG,elG,tbl,chi,cclass,repcclass,innG,
eloutGList,res,resTemp,aut,ci,maclist,fslists;
autG:=AutomorphismGroup(G);
ordG:=Size(G);
elG:=Elements(G);
tbl:= CharacterTable(G);
chi:=Irr(tbl);
cclass:=ConjugacyClasses(tbl);
repcclass:=List(cclass,x->Representative(x));
innG:=InnerAutomorphismsAutomorphismGroup(autG);
eloutGList:=CosetDecomposition(autG,innG);
res:=[[],[]];
for i in [1..Length(eloutGList)] do
	resTemp:=[];
	aut:=eloutGList[i][1];
	ci:=isClassinverting(aut,cclass,repcclass);
	if ci then
		for j in [1..Length(eloutGList[i])] do
			aut:=eloutGList[i][j];
			maclist:=List(elG,x->x*x^aut);
			fslists:=List(chi,y->Sum(maclist,x->x^y))/ordG;
			if ForAll(fslists,x->x=1) then
				Add(resTemp,[i,1]);
				break;
			else
				Add(resTemp,[i,2]);
			fi;
		od;
		if ForAny(resTemp,x->x[2]=1) then
			Add(res[1],[eloutGList[i][1],IsInnerAutomorphism(aut)]);
		elif ForAny(resTemp,x->x[2]=2) then
			Add(res[2],[eloutGList[i][1],IsInnerAutomorphism(aut)]);
		fi;
	fi;
od;
Print(res[1]," are Bickerstaff-Damhus automorphisms.\n");
Print(res[2]," are class-inverting automorphisms.\n");

return res;

end;
