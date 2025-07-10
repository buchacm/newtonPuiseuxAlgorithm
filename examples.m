(* ::Package:: *)

(* ::Input:: *)
(*(* This is a Mathematica notebook that accompanies the article on "The Newton-Puiseux Algorithm and Effective Algebraic Series" to make some of the computations more comprehensible *)*)


(* ::Input:: *)
(*Directory[]*)


(* ::Input:: *)
(*<<newtonPuiseux.m*)


(* ::Input:: *)
(*(* Example 2 *)*)


(* ::Input:: *)
(*m=4x^2y+(x^2y+x y^2+x y+y)^2\[Minus]Z^2*)


(* ::Input:: *)
(*vertices=NewtonPolytope[m,{x,y,Z}]*)


(* ::Input:: *)
(*edges=Select[GetEdges[vertices],Last[#[[1]]]!=Last[#[[2]]]&]*)


(* ::Input:: *)
(*edge=edges[[1]];*)


(* ::Input:: *)
(*bCone=barrierCone[vertices,edge]*)


(* ::Input:: *)
(*w={-Sqrt[2],-1};*)


(* ::Input:: *)
(*(* check whether w is indeed compatible with bCone *)*)
(*w . #&/@bCone//N*)


(* ::Input:: *)
(*series[m,{x,y,Z},edge,w,0]*)


(* ::Input:: *)
(*(* Example 3 *)*)


(* ::Input:: *)
(*m=x+y\[Minus](1+x+y)Z*)


(* ::Input:: *)
(*vertices=NewtonPolytope[m,{x,y,Z}]*)


(* ::Input:: *)
(*edges=Select[GetEdges[vertices],Last[#[[1]]]!=Last[#[[2]]]&]*)


(* ::Input:: *)
(*bCones=barrierCone[vertices,#]&/@edges*)


(* ::Input:: *)
(*w1={-2+1/Sqrt[2],-1};*)
(*w2={-1+1/Sqrt[2],-2};*)
(*w3={-1+1/Sqrt[2],1};*)
(*w4=-{-1+1/Sqrt[2],-1};*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[1]],w1,0]*)
(*series[m,{x,y,Z},edges[[2]],w2,0]*)
(*series[m,{x,y,Z},edges[[3]],w3,0]*)
(*series[m,{x,y,Z},edges[[4]],w4,0]*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[2]],w2,9]*)


(* ::Input:: *)
(*Sort[MonomialList[x-x^2+x^3-x^4+x^5-x^6+y],Exponents[#,{x,y}][[1]] . w1&]*)


(* ::Input:: *)
(*(* Example 5 *)*)


(* ::Input:: *)
(*m=x+y-(1+x+y)Z*)


(* ::Input:: *)
(*vertices=NewtonPolytope[m,{x,y,Z}]*)


(* ::Input:: *)
(*edges=Select[GetEdges[vertices],Last[#[[1]]]!=Last[#[[2]]]&]*)


(* ::Input:: *)
(*edge1=edges[[2]];*)
(*edge2=edges[[1]];*)


(* ::Input:: *)
(*bCones=barrierCone[vertices,#]&/@{edge1,edge2}*)


(* ::Input:: *)
(*w1={-1+1/Sqrt[2],-1};*)
(*w2={-1+1/Sqrt[2],1};*)


(* ::Input:: *)
(*series[m,{x,y,Z},edge1,w1,0]*)
(*series[m,{x,y,Z},edge2,w2,0]*)


(* ::Input:: *)
(*(* Example 6: see Example 5 *)*)


(* ::Input:: *)
(*(* Example 8 *)*)


(* ::Input:: *)
(*m=(1-x)((1-y)Z-1)*)


(* ::Input:: *)
(*vertices=NewtonPolytope[m,{x,y,Z}]*)


(* ::Input:: *)
(*edges=Select[GetEdges[vertices],Last[#[[1]]]!=Last[#[[2]]]&]*)


(* ::Input:: *)
(*bCones=barrierCone[vertices,#]&/@edges*)


(* ::Input:: *)
(*orders=getDual[#]&/@bCones*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[1]],orders[[1]],0]*)


(* ::Input:: *)
(*m=(1-y)Z-1*)


(* ::Input:: *)
(*vertices=NewtonPolytope[m,{y,Z}]*)


(* ::Input:: *)
(*edges=Select[GetEdges[vertices],Last[#[[1]]]!=Last[#[[2]]]&]*)


(* ::Input:: *)
(*bCones=barrierCone[vertices,#]&/@edges*)


(* ::Input:: *)
(*orders=getDual[#]&/@bCones*)


(* ::Input:: *)
(*series[m,{y,Z},edges[[1]],orders[[1]],0]*)


(* ::Input:: *)
(*(* Example 9 and Example 10 *)*)


(* ::Input:: *)
(*m=1+x+y+(1+x y+2y)Z+y Z^2*)


(* ::Input:: *)
(*vertices=NewtonPolytope[m,{x,y,Z}]*)


(* ::Input:: *)
(*edges=Select[GetEdges[vertices],Last[#[[1]]]!=Last[#[[2]]]&]*)


(* ::Input:: *)
(*bCones=barrierCone[vertices,#]&/@edges*)


(* ::Input:: *)
(*w={-1+1/Sqrt[2],-1}*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[1]],w,0]*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[1]],w,2]*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[1]],w,3]*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[1]],w,5]*)


(* ::Input:: *)
(*(* Example 11 *)*)


(* ::Input:: *)
(*m=(1-x)(Z-y)-1*)


(* ::Input:: *)
(*polytope=NewtonPolytope[m,{x,y,Z}]*)


(* ::Input:: *)
(*edges=Select[GetEdges[polytope],Last[#[[1]]]!=Last[#[[2]]]&]*)


(* ::Input:: *)
(*series[m,{x,y,Z},edges[[3]],{-1,Sqrt[2]},0]*)
