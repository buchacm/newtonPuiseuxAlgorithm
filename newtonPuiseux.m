(* ::Package:: *)

(* the goal of the following section is the implementation of a \
generalization of the Newton-Puiseux algorithm for solving algebraic \
equations for the last variable (for an arbitrary number of \
variables) *)


intersection[point_, 
  vector_] :=
 (* takes a point p and a vector v in R^n, and returns \
the intersection of the line L through p in direction v with the \
hyperplane {x_n=0} *)
 Module[{n, lambda},
  n = Length[point];
  If[n =!= Length[vector], 
   Throw["dimensions of point and vector do not match"]];
  If[vector[[n]] == 0, 
   Throw["last coordinate of vector is assumed to be non-zero"]];
  lambda = -point[[n]]/vector[[n]];
  Return[point + lambda*vector]];

barrierCone[polytope_, 
  edge_] :=
 (* takes a polytope in terms of its vertices, and one of \
its admissible edges, i.e. one whose last coordinate is different \
from zero, and returns its barrier cone *)
 
 Module[{vector, list, apex},
  vector = edge[[2]] - edge[[1]];
  list = intersection[#, vector] & /@ polytope;
  apex = intersection[edge[[1]], vector];
  list = (# - apex) & /@ list;
  If[Union[list]=={Table[0,{i,Length[vector]}]},Return[{Table[0,{i,Length[vector]-1}]}]];
  Return[SimplifyCone[Delete[#, Length[vector]] & /@ list]]
  ];


memberConeQ[list_, vector_] := Module[{X, x, sol},
  X = Array[x, Length[list]];
  sol = Quiet[Solve[{X . list == vector, And @@ Thread[X >= 0]}, X]];
  If[sol == {}, Return[False], Return[True]]];


Exponents[poly_, vars_] := 
 (* code for the extraction of the exponents of a polynomial \
*)
 Module[ {min},
     If[poly===0,Return[{Table[0,{i,1,Length[vars]}]}]];
 
     If[ ! FreeQ[{poly}, CUT[_]], 
   Return[Exponents[poly /. CUT[_] -> 0, vars]] ];
     If[ Together[poly] === 0, Return[ {} ]];
     min = Exponent[poly, vars, Min];
     Union[(# + 
       min) & /@ ((Exponent[#, vars] & /@
             
        Cases[Expand[(Times @@ vars) Times @@ (vars^(-min)) poly],
                _. Times @@ (vars^_.), {0, Infinity}]) - 1)]
  ];


NewtonPolytope[poly_, vars_] :=
   
 Module[ {points, p, P, x, CornerQ, e},
      (* given a polynomial, 
  find the vertices of the corresponding newton polytope *)
 OutsideQ[p_, P_] := Module[ {v}, 
    v = Array[x, Length[P]]; ! 
     Resolve[e[v, 
        And @@ Join[Thread[p == v . P], 
          Thread[v >= 0], {Plus @@ v == 1}]] /. e -> Exists, Reals]];
      points = Exponents[poly, vars];
      Select[points, OutsideQ[#, DeleteCases[points, #]] &]
    ];


coefficient[poly_,vars_,exp_]:=
(* determine the coefficient of the monomial vars^exp in the Puiseux polynomial poly *)
Module[{output},
output=poly;
Do[
output=Coefficient[output,vars[[i]],exp[[i]]]
,{i,1,Length[vars]}];
Return[output]
];


GetEdges[polytope_] :=
   
 Module[ {vertex, edges, possibleEdges, vector, vectors, p, e, E, 
   ex},
      (* given a polytope in terms of its vertices, 
  return its edges, as pairs of vertices *)
      
  InnerQ[e_, E_] := Module[ {v = Array[x, Length[E]]},
          
    Resolve[ex[v, And @@ Join[Thread[e == v . E], Thread[v >= 0]]] /. 
      ex -> Exists, Reals] ];
  
  edges = {};
  Do[
   possibleEdges = {vertex, #} & /@ Complement[polytope, {vertex}];
   Do[
    vector = edge[[2]] - edge[[1]];
    If[
     InnerQ[vector, 
      Complement[(#[[2]] - #[[1]]) & /@ possibleEdges, {vector}]],
     possibleEdges = Complement[possibleEdges, {edge}]
     ]
    , {edge, possibleEdges}];
   
   edges = Join[edges, possibleEdges]
   , {vertex, polytope}];
  
  Return[Union[Sort /@ edges]]
    ];

edgeEquation[poly_, vars_, edge_, 
  t_] :=
 (* takes a polynomial poly, and an edge e of its Newton \
polytope, and returns the corresponding edge equation, i.e. a \
polynomial in t, its coefficients being those of poly whose \
monomials' exponents lie on the edge e; the exponents of t are the \
last components of these *)
 Module[{a, b, n, edges, exponents, output, minorVertex},
  n = Length[vars];
  (*edges = GetEdges[NewtonPolytope[poly, vars]];
  If[! MemberQ[edges, Sort[edge]], 
   Return["edge is not an edge of the polytope associated with poly"]];*)
  exponents = 
   Select[Exponents[poly, vars], 
    Solve[{a*edge[[1]] + b*edge[[2]] == #, a >= 0, b >= 0, 
        a + b == 1}, {a, b}] =!= {} &];
  minorVertex = 
   If[edge[[1]][[n]] < edge[[2]][[n]], edge[[1]], edge[[2]]];
   Return[Sum[coefficient[poly,vars,exp]t^(Last[exp]-Last[minorVertex]),{exp,exponents}]]
  (*Return[Plus @@ ((If[MatchQ[#, {0 ..}], poly/.Thread[vars->0], 
          Coefficient[poly, Times @@ (vars^#)] /. 
           Thread[Complement[vars, Variables[vars^#]] -> 
             0]]) t^(Last[#] - Last[minorVertex]) & /@ exponents)]*)
  ];


leadingExp[
  edge_] :=
 (* given an egde e of the polytope associated with a \
polynomial (equation) the corresponding expansion of one of its \
solutions has leading exponent -s(e), where s(e) is the slope of e \
with respect to the last coordinate *)
 Module[{n},
  n = Length[edge[[1]]];
  Return[-(edge[[2]][[1 ;; n - 1]] - 
       edge[[1]][[1 ;; n - 1]])/(edge[[2]][[n]] - edge[[1]][[n]])]
  ];


Lexp[poly_, vars_, normal_] := Module[{min}, min = None;
   Do[Which[min === None, min = v, (v - min) . normal < 0, 
     min = v, (v - min) . normal === 0, 
     Throw["inappropriate normal vector cannot separate " <> 
       ToString[v] <> " " <> ToString[min]]], {v, 
     Exponents[poly, vars]}];
   If[min === None, 
    Throw["leading exponent of zero polynomial undefined"]];
   min];

Lterm[poly_, vars_, normal_] := 
  Times @@ (vars^Lexp[poly, vars, normal]);

LineFreeQ[cone_] := 
  Module[{e, vars, alpha}, 
  If[Union[cone]==={{0,0}},Return[True]];
   If[MemberQ[cone, {0 ..}], 
    Return[LineFreeQ[DeleteCases[cone, {0 ..}]]]];
   vars = Array[alpha, Length[cone]];
   ! Resolve[
     e[vars, And @@ 
        Join[Thread[vars >= 0], 
         Thread[vars . cone == 0], {Plus @@ vars == 1}]] /. e -> Exists,
      Reals]];

ConeMemberQ[cone_, p_] := 
  Module[{e, x}, 
   Resolve[e[Array[x, Length[cone]], 
      And @@ Join[Thread[Array[x, Length[cone]] >= 0], 
        Thread[-p + Array[x, Length[cone]] . cone == 0]]] /. 
     e -> Exists, {}, Reals]];

SimplifyCone[cone_] := 
  Module[{c},(*tries to identify some redundant generators and \
discards them*)(*1. cancel content and remove duplicates and zero \
vector*)c = Union[(#/GCD @@ #) & /@ DeleteCases[cone, {0 ..}]];
   (*2. sort by l1-norm*)c = Sort[c, (Norm[#1, 1] < Norm[#2, 1]) &];
   (*3. discard redundant members top-down*)
   Do[If[ConeMemberQ[DeleteCases[Delete[c, i], None], c[[i]]], 
     c[[i]] = None], {i, Length[c], 1, -1}];
   c = DeleteCases[c, None];
   (*4. done.*)c;
   If[c==={},Return[{{0,0}}],Return[c]]
   ];


projection[set_, coordinate_] :=
  (* projection of a set of vectors on a given coordinate *)
#[[coordinate]] & /@ set;



onLineQ[point1_, point2_, vector_] :=Module[{a},
  (* 
  tests if point1 lies on the line through point2 in direction vector \
*)
  If[Solve[point1 == point2 + a*vector, a] == {}, Return[False], 
   Return[True]]];


roots[poly_,var_]:=Root[poly/.var->#&,#]&/@Range[Exponent[poly,var]];


expand[poly_,vars_]:=
(* computes the expansion of a polynomial whose coefficients are algebraic numbers *)
Module[{polyT,T,exps,rules,el,p,u,i,j},

polyT=poly;

T={};
exps=Exponents[poly,vars];

Do[
AppendTo[T,Quiet[LCM@@(Denominator[#]&/@(#[[i]]&/@exps))]]
,{i,1,Length[vars]}];

polyT=polyT/.Thread[vars->vars^T];

polyT=Assuming[And@@Thread[vars>=0],Simplify[polyT]];

rules=CoefficientRules[polyT,Join[vars,vars^(-1)]];

Do[
el=rules[[i]];
p={el[[2]]};
p=Replace[p,Plus->Sequence,{2},Heads->True];
p=RootReduce[Plus@@ToNumberField[p]];
rules[[i]][[2]]=p;
,{i,1,Length[rules]}];

Return[FromCoefficientRules[rules,Join[vars,vars^(-1)]]/.Thread[vars->vars^(1/T)]/.(u_^i_)^j_->u^(i j)]
];


slope[edge_, 
  coord_] :=
 (* computes the slope of an edge with respect to a \
given coordinate *)
 Module[{vector, scalar},
  vector = edge[[2]] - edge[[1]];
  scalar = vector[[coord]];
  Return[Delete[vector, coord]/scalar]
  ];

normalQ[vector_, 
  cone_] :=
 (* tests if a vector is in the normal cone of a given \
cone *)
 And @@ Thread[vector . Transpose[cone] <= 0];

pathExt[vertices_, edges_, edge_, m_, ww_, 
  path_ ] :=
 (* takes the vertices and the edges of a polytope, a \
specific edge "edge", the multiplicity "m" of a root of the edge \
equation of "edge", a vector "ww" which is supposed to be the vector "w" \
from above, and the path constructed so far; extends this path if \
possible;
 attention: the path is given as a sequence of edges, not as a \
sequence of vertices!!! *)
 Module[{n, echeese, paath, edgeLast, v, verticesPlus, verticesMinus, 
   endpoints, p},

  n = Length[edge[[1]]];
  echeese = Sort /@ edges;
  paath = path;
  (*If[!MatchQ[paath,{{__}..}],paath={paath}];*)
  If[Length[paath] > 0,
   edgeLast = Last[paath];
   v = If[Last[edgeLast[[1]]] > Last[edgeLast[[2]]], edgeLast[[1]], 
     edgeLast[[2]]];
  
   If[onLineQ[v, edge[[1]], edge[[2]] - edge[[1]]], Return[paath]];
  
   verticesPlus = 
    Select[vertices, (MemberQ[echeese, Sort[{v, #}]] && 
        Last[v] < Last[#] && ww . slope[edge, n] < ww . slope[{v, #}, n] &&
         ww . slope[{v, #}, n] < ww . slope[edgeLast, n] && 
        normalQ[ww, barrierCone[vertices, {v, #}]] && Last[#] <= m) &];
   If[Length[verticesPlus] == 0, Return[path],
    Return[{path, {v, #}} & /@ verticesPlus]]];
  If[Length[paath] == 0,
   paath = 
    Select[echeese, (Last[#[[1]]] == 0 || 
         Last[#[[2]]] == 0) && (0 < Last[#[[1]]] <= m || 
         0 < Last[#[[2]]] <= m)  && ww . slope[edge, n] < ww . slope[#, n] &&
       normalQ[ww, barrierCone[vertices, #]] &];
   Return[paath]
   ]];

edgePath[poly_, vars_, edge_, multiplicity_, 
  w_] :=
 (* takes as input a polynomial - a partial solution plugged \
into the original polynomial - and outputs the unique coherent edge \
path described in Lemma 3.7 of the paper by McDonald *)
  
 Module[{time,n, vertices, edges, output},
  
  n = Length[vars];
  vertices = NewtonPolytope[poly, vars];
  edges = GetEdges[vertices];

  output = {{}};

  Do[
  
  
   output = 
     pathExt[vertices, edges, edge, multiplicity, w, #] & /@ output;
  
  
   , {e, edges}];
  
  
  (* select the unique path which ends on the line through edge *)
 
  output = 
   Select[output, (onLineQ[Last[#][[1]], edge[[1]], 
        edge[[2]] - edge[[1]]] || 
       onLineQ[Last[#][[2]], edge[[1]], edge[[2]] - edge[[1]]]) &];

  Return[Flatten[output, 1]]
  ];

nextTerm[poly_, vars_, edge_] := 
(* berechnet die m\[ODoubleDot]glichen n\[ADoubleDot]chsten Terme, zusammen mit der Vielfachheit mit 
der sie auftreten *)
 Module[{vertices, edges, s, t, zeros0, zeros, term, path, newpoly},
  (*vertices = NewtonPolytope[poly, vars];
  edges = GetEdges[vertices];
 
  If[ !MemberQ[edges, Sort[edge]], 
   Throw["edge is not an edge of the Newtonpolytope"]]; *)
  (* is "edge" admissible? *)
   
   
  If[Last[edge[[1]] - edge[[2]]] == 0, 
   Throw["edge is not admissible"]];
  s = slope[edge, Length[vars]];
 
  zeros0 = roots[edgeEquation[poly, vars, edge, t] ,t];
 
  zeros = {#, Count[zeros0, #]} & /@ zeros0;

zeros=Union[zeros];

  Return[{#[[1]]*Times @@ (Most[vars]^-s), #[[2]]} & /@ zeros]
  ];



verticesToEdges[
  set_] :=
 (* maps a list of subsequently neighbouring vertices to \
the corresponding list of edges *)
 Module[{n,edges},
  n = Length[set];
  If[n == 1, Throw["there's only one vertex"]];
  edges = {};
  
  Do[
   AppendTo[edges, {set[[i - 1]], set[[i]]}]
   , {i, 2, n}];
  
  Return[edges]];


edgePath1[poly_,vars_,edge_,multiplicity_,w_]:=Module[{set,exps,vertices},
exps=Exponents[poly,vars];
vertices={};
AppendTo[vertices,TakeLargestBy[Select[exps,Last[#]==0&],w . Most[#]&,1][[1]]];
While[Last[Last[vertices]]=!=multiplicity,
set=Select[exps,Last[#]>Last[Last[vertices]]&&Last[#]<=multiplicity&];
AppendTo[vertices,TakeLargestBy[TakeLargestBy[set,
w . Most[#]&,Length[set]],Last[#]&,1][[1]]]
];
Return[verticesToEdges[vertices]]
];


series[poly_, vars_, edge_, w_, 
  order_, order1_: False] :=
 (* computes the series solutions with respect to a \
given edge and total order on the exponents up to a given order + cones that contain their tails *)
 
 Module[{outputOld,outputold,lastT,n,P,counter,monomialList,coefficientList,output, output1, terms, newsol, path},


counter=0;
(* w has to be in the dual of the barrier cone of edge *)
(* Print["barrierCone: ",barrierCone[NewtonPolytope[poly,vars],edge]];
Print[Thread[w . #&/@barrierCone[NewtonPolytope[poly,vars],edge]>0]]; *)

If[Or@@Thread[w . #&/@barrierCone[NewtonPolytope[poly,vars],edge]>0],Throw["total order not compatible"]];


monomialList={};
coefficientList={};

(* output besteht aus einer Liste von Paaren bestehend aus 
der Reihenentwicklung einer Nullstelle bis zu einer gewissen Ordnung und 
einer Kante zur Berechnung des n\[ADoubleDot]chsten Terms *)
output = {{0, edge}};
 

 n=Abs[Last[edge[[1]]]-Last[edge[[2]]]];
 
  While[
  (counter==0 && (counter<order || Length[output]<n+1))||
  (counter>0 && (counter<order || Length[output]<n)),

   counter=counter+1;
  (*  was ist output1? die Liste der n\[ADoubleDot]chsten Terme + Kanten *)
   output1 = {};
  
   Do[
  
   If[output[[j]][[2]]===-1, AppendTo[output1,{output[[j]][[1]],-1}],
    
    terms = nextTerm[
      expand[poly /. Last[vars] -> Last[vars] + output[[j]][[1]],vars], 
      vars, output[[j]][[2]]];
   
    newsol = {output[[j]][[1]] + #[[1]], #[[2]], output[[j]][[2]]} & /@
       terms;   
       
    
    newsol =
     Flatten[Tuples[{{#[[1]]}, 
           If[expand[poly /. Last[vars] -> #[[1]],vars]===0,{-1},
           edgePath1[expand[poly /. Last[vars] -> Last[vars] + #[[1]],vars], 
           vars, #[[3]], #[[2]], w]]}] & /@ newsol, 1];
          
           
    
    output1 = Join[output1, newsol]]
    
    , {j, 1, Length[output]}];
    
   outputOld=output; 
   output = output1;
   ];
   
   lastT[sum_,v_]:=Module[{list},
   list=Exponents[sum,v];
   Return[Times@@(v^Last[list])]
   ];
  
outputold={};
Do[
Do[

AppendTo[outputold,outputOld[[i]]]

,{j,1,Abs[Last[Last[outputOld[[i]]][[1]]]-Last[Last[outputOld[[i]]][[2]]]]}]
,{i,1,Length[outputOld]}];


  Do[

  P=NewtonPolytope[poly/.Last[vars]->outputold[[i]][[1]]+Last[vars],vars];
  
  output[[i]][[2]]=If[output[[i]][[2]]===-1,{Table[0,Length[vars]-1]},barrierCone[P,outputold[[i]][[2]]]];
  (* Print[outputold[[i]][[2]]] *)
  ,{i,1,Length[output]}];
  
  Return[output]
  ];


getDual[cone_]:=
(* find an element of the dual of a cone *)
Module[{output,X},
If[cone==={{0,0}},Return[{Sqrt[2],1}]];
output=FindInstance[And@@Join[Thread[cone . Array[X,Length[First[cone]]]<0],{Times@@Array[X,Length[First[cone]]]!=0}],Array[X,Length[First[cone]]],Reals];
output=(Array[X,Length[First[cone]]]/.First[output]);
(* okay, this is now a very experimental way to make the components of output linearly independent over the rationals *)
Do[
If[output[[i]]=!=0,output[[i]]=output[[i]]+1/(1000Sqrt[2]);Break[]]
,{i,Length[output]}];

Return[output]
];
