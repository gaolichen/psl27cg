isClassinverting:=function(aut,cclass,repcclass)
local j;
for j in repcclass do
	if PositionProperty(cclass,x->j^-1 in x) <> PositionProperty(cclass,x->(j^aut) in x) then
		return false;
	fi;
od;

return true;
end;

twistedFS:=function(G,aut)
local elG,tbl,irr,fsList;
elG:=Elements(G);
tbl:=CharacterTable(G);
irr:=Irr(tbl);
fsList:=List(elG,x->x*x^aut);
return List(irr,y->Sum(fsList,x->x^y))/Size(G);
end;

twistedFSn:=function(G,aut)
local ord,n,elG,tbl,irr,elGaut,fsList,i;
ord:=Order(aut);
if ord=0 mod 2 then
	n:=ord/2;
else
	n:=ord;
fi;
elG:=Elements(G);
tbl:=CharacterTable(G);
irr:=Irr(tbl);
elGaut:=List(elG,x->x*x^aut);
fsList:=ShallowCopy(elGaut);
for i in [1..n-1] do
	fsList:=List(fsList,x->List(elGaut,y->x*y));
	fsList:=Flat(fsList);
od;

return List(irr,y->Sum(fsList,x->x^y)*y[1]^(n-1))/Size(G)^n;
end;

