(* ::Package:: *)

(*:Mathematica Version: 12.0 *)

(*:Package Version: 0.6 *)

(*:Name: Dependability`StateDiagrams` *)

(*:Context: Dependability`StateDiagrams` *)

(*:Title:  *)

(*:Author: Bjarne E. Helvik *)

(*:History:
Preliminary version issued as an exercise notebook with solutions in 
PhD (Dr. Ing.) course " P\[ARing]litelighet i telematikk og 
datamaskinsystem"  at NTH, Trondheim Norway , 1996-10-01.
First Package version (0.1) issued 1999-12-23.
System composition functions included 2000-05-24 (version 0.2), 
Introduction of eigenvalues for computation of transient solutions 
2003-07-07 (version 0.3)
2008-01-24 Inclusion of a new version of the PlotDiagram function
based on version 6.0 functionality. This ha previous been separate, based on Combinatorica.
2008-11-13 Corrected cosmetic bug in Plotdiagram
2010-12-30 Fixed bug in MergeStates. Added function GenHomogeneousSystem
2017-12-06 Added functionality to obtain the down time distribition (pdf) 
           in Laplace and time domain. (version 0.5)
2019-10-21 Replaces options for plotting diagreams  (PlotDiagram ) with mathematica version 12. (version 0.6)
*)

(*:Copyright:
 All rights reserved Bjarne E. Helvik.
*)

(*:Reference: Usage messages.
              The notebook Analyse-StateDiagrams.nb
*)

(*:Summary:
This package evaluates dependability models based on
continuous time discrete state Markov chains, 
i.e. so called state models. It also includes functionality 
to compose system models from state models of subsystems.
Makes a plot for the state diagram.
*)

(*:Keywords: Dependability, Availability, Reliability,
 MTBF, MUT, MDT, MTFF *)

(*:Requirements: No special system requirements. *)

(*:Sources:

Buzacott, "Markov Approach to finding Failure Times 
of repairable systems", IEEE Trans. on Reliability, 
Vol. R-19, No. 6, pp. 128-133, 1970

*)
BeginPackage["Dependability`StateDiagrams`"]

SetDiagonal::usage =
"SetDiagonal[m] produces a complete transition matrix
by introducing diagonal elements. m is an incomplete
transition matrix where m[[j,i]] i=/=j is the transition rates from 
state i to j. 
NB! i is the matrix column no., j is the row no."

ProbStationary::usage =
"ProbStationary[m] determines the stationary state probability
vector for the system defined by the 
complete transition matrix m."

UnAvailability::usage =
"UnAvailability[ps,working] computes the unavailability 
for a system with state probability vector ps and a 
corresponding Boolean vector working, 
where working[[i]]=True if i is a working state."

ProbTransient::usage =
"ProbTransient[m,P0,t] determines the transient 
state probabilities for a system at time t. The system 
is defined by the complete transition matrix m. 
P0 is the initial state probability vector at time 0."

RelFunc::usage =
"RelFunc[m, WorkingQ, t, Ini] determines the reliability function 
(the probability that the system has not failed in [0,t])
at time t. The system is defined by the 
complete transition matrix m and the working states is 
given by the corresponding  Boolean vector WorkingQ,
where WorkingQ[[i]]=True if i is a working state.
The system is in state Ini at time t=0. 
The parameter Ini is optional with default value 1."

ArrangeMatrix::usage= 
"ArrangeMatrix[m, WorkingQ] sorts and divides the tamsition matrix so columns and rows for up and down states are
collected so the up-state transitions (EX) are in the upper left
corner and the down-state transitions are in the lower left (HX).
Furthermore the matrix is diveded into the four submatrixes {{EX,GX},{FX,HX}}"

MTFF::usage=
"MTFF[m, WorkingQ, Ini] determines the 
Mean Time First Failure. The system is defined by the 
complete transition matrix m and the working states is 
given by the corresponding  Boolean vector WorkingQ,
where WorkingQ[[i]]=True if i is a working state. 
The system is in state Ini at time t=0. 
The parameter Ini is optional wit default value 1."

MTTF::usage=
	"MTTF[m, WorkingQ] determines the 
    Mean Time To Failure when the system is found working 
    in a stationary state. 
    The system is defined by the 
    complete transition matrix m and the working states is 
    given by the corresponding  Boolean vector WorkingQ,
    where WorkingQ[[i]]=True if i is a working state."
	
MUT::usage=
	"MUT[m, WorkingQ] determines the Mean Up Time. 
    The system is defined by the 
    complete transition matrix m and the working states is 
    given by the corresponding  Boolean vector WorkingQ,
     where WorkingQ[[i]]=True if i is a working state."
	
MDT::usage=
	"MDT[m, WorkingQ] determines the Mean Down Time. 
    The system is defined by the 
    complete transition matrix m and the working states is 
    given by the corresponding  Boolean vector WorkingQ,
     where WorkingQ[[i]]=True if i is a working state."
	
MTBF::usage=
	"MTBF[m, WorkingQ] determines the Mean Time Between Failures. 
    The system is defined by the 
    complete transition matrix m and the working states is 
    given by the corresponding  Boolean vector WorkingQ,
    where WorkingQ[[i]]=True if i is a working state."
    
DownTimeDensityLaplace::usage=
	"DownTimeDensityLaplace[m,WorkingQ,s] yields the Laplacetransform of 
	the pdf the down time distribution in a system defined by the 
    complete transition matrix m and the working states is 
    given by the corresponding  Boolean vector WorkingQ,
    where WorkingQ[[i]]=True if i is a working state."
    
DownTimeDensity::usage=
	"DownTimeDensity[m,WorkingQ,s] yields 
	the pdf the down time distribution in a system defined by the 
    complete transition matrix m and the working states is 
    given by the corresponding  Boolean vector WorkingQ,
    where WorkingQ[[i]]=True if i is a working state."
     
KroenSum::usage=
    "KroenSum[M] where M = m1, m2, ..., mn yields the
    transition matrix of a system composed if independent 
    subsystems 1 to n. mi is the complete transition matrix
    of subsystem i. Note the order of subsystems is significant
    and order used in functions KroenSum, LabelList, SeriesMode,
    ParallelMode and KoutofNMode must be consistent. 
    (The mathematical operation performed by KroenSum is 
    denoted the Kroenecker sum.)"
    
LabelList::usage=
	"LabelList[L] where L = l1, l2, ..., ln yields a list 
	of state labels for a system composed if independent 
    subsystems 1 to n. li is the list of state labels for
    subsystem i. (li may be prefix with subsystem name 
    to avoid ambiguity.)
    Note the order of subsystems is significant
    and order used in functions KroenSum, LabelList, SeriesMode,
    ParallelMode and KoutofNMode must be consistent." 
    
    
SeriesMode::usage= 
	"SeriesMode[WQ] where WQ = WorkingQ1, WorkingQ2, ...,
    WorkingQn yields a Boolean vector where an element is True if 
    the corresponding system state is working a state, otherwise False.
    The system is composed if independent subsystems 1 to n, forming a
    SERIES RELIABILITY STRUCTURE, i.e., all subsystems must work 
    to have a working system.
	WorkingQi is a Boolean vector (list) where 
	WorkingQi[[j]]=True if j is a working state of subsystem i.
	Note the order of subsystems is significant
    and order used in functions KroenSum, LabelList, SeriesMode,
    ParallelMode and KoutofNMode must be consistent." 

ParallelMode::usage=
	"ParallelMode[WQ] where WQ = WorkingQ1, WorkingQ2, ...,
    WorkingQn yields a Boolean vector where an element is True if 
    the corresponding system state is working a state, otherwise False.
    The system is composed if independent subsystems 1 to n, forming a
    PARALLEL RELIABILITY STRUCTURE, i.e., at least one subsystem
    must work to have a working system.
	WorkingQi is a Boolean vector (list) where 
	WorkingQi[[j]]=True if j is a working state of subsystem i. 
	Note the order of subsystems is significant
    and order used in functions KroenSum, LabelList, SeriesMode,
    ParallelMode and KoutofNMode must be consistent." 
				 						

KoutofN::usage=
	"KoutofN[koutofn, k] generates a Boolean expression which is 
	True if at least k out of the elements in the Boolean vector 
	(list) koutofn is true."

KoutofNMode::usage=
	"KoutofNMode[WQ, k] where WQ = WorkingQ1, WorkingQ2, ...,
    WorkingQn yields a Boolean vector where an element is True if 
    the corresponding system state is working a state, otherwise False.
    The system is composed if independent subsystems 1 to n, forming a
    K-OUT-OF-N RELIABILITY STRUCTURE, i.e., at least k out of the 
    subsystems must work to have a working system.
	WorkingQi is a Boolean vector (list) where 
	WorkingQi[[j]]=True if j is a working state of subsystem i.
	Note the order of subsystems is significant
    and order used in functions KroenSum, LabelList, SeriesMode,
    ParallelMode and KoutofNMode must be consistent." 
    
MergeStates::usage=
    "MergeStates[M,j1,..,jn] or MergeStates[M,{j1,..,jn}] produces the complete transition matrix
    of a model where states  j1, .., jn are merged
    into one state, which is indexed Min[j1,..,jn]. M is the complete transition matrix
    of original system. The return transitions into the 'unmerged'
    states are those of the  state indexed Min[j1,..,jn].
    NOTE: For a merge which does not alter the model, 
    it is required that the transition rates out of each of
    the merged states into the rest of the system are identical."

GenHomogeneousSystem::usage =
"GenHomogeneousSystem[m, WorkingQ, Labels, n, k, Index] Generates a system 
consiting of n  transition matrix dependability model represented by a 
homogeneous subsystems (elements) where the parameters are the transition 
matrix m, the corresponding Boolean vector WorkingQ, where 
WorkingQ[[i]]=True if i is a working state, and the list of state labels 
denoted Labels. The system is working if k-out-of-n subsytems are working. 
The function returns {sm, sWorkingQ, sLabels}.
Index is an optional argument, where the indexing of states are returned, 
i.e.,Index[{a,b,..,c}]give the mapping between the number of 
subsytems in their local states and the system state no."


(***************** Other functions *****************)

StepPlot::usage=
"StepPlot[x_List] Creates a step plot at the specified points. The x_List
must contains (x, y)-values."

PlotDiagram::usage =
"PlotDiagram[m, WorkingQ, Labels] Plots a dependability model represented by a
state diagram. The parameters are the transition matrix m,
the corresponding Boolean vector WorkingQ, where WorkingQ[[i]]=True if i is a working state,
and the list of state labels denoted Labels. Labels must be stings or list of strings."



(* --------------------------------------------------------- *)

Begin["`Private`"]

(*Ensures that the "old" diagonal element is removed and ensures a row sum of zero *)
SetDiagonal[m_] :=
	Module[{mxx},
	mxx = m (Table[1,{Length[m]},{Length[m]}] - IdentityMatrix[Length[m]]);
 	mxx = mxx - (Transpose[mxx] . 
 	Table[1,{Length[m]}]) IdentityMatrix[Length[m]]]
 	
ProbStationary[m_] := 
	Module[{mxx, bxx},
	bxx = Append[Table[0,{Length[m]-1}],1];
	mxx = Append[Drop[m,-1],Table[1,{Length[m]}]];
	LinearSolve[mxx, bxx]]
	
UnAvailability[ps_,working_] :=
	ps . (If[#,0,1]& /@ working)
	
(* The transient state probabilities are obtained by the built in
mma routine DSolve. DSolve operates on set of explicitly defined
equations. Hence these must first be defined from the
transition matrix. Furthermore, the initial condition is defined
as a set of explicit equations. The solution is given as a rule
and is transformed to a linear vector. *)

ProbTransient[m_,P0_,t_] :=
	Module[{p, pt, eqn},
	p = Table[pt[i][t],{i,Length[m]}];
	eqn = Flatten[Append[
		Inner[Equal ,m . p , D[p,t], List],
		Inner[Equal ,(p/.t->0) , P0, List]]];
	p /. DSolve[eqn, p, t] //Simplify //Flatten]
	

(* Alternative to ProbTransient by using eigenvalues. 
Not thourghly tested*)

ProbTransientEigen[m_,P0_,t_]:=Module[{Vals,Vects, Ck},
(* a set of distinkt eienvalues are required *)
{Vals,Vects}=Eigensystem[m];
(* Test if eigenvalues are distinct *)
If[Length[Union[Vals]] < Length[Vals], 
  Print["Repeated eigenvalues, Not implemented"]; Abort[]];
  (* Finds constans by using the initial condition*)
    Ck=LinearSolve[Transpose[Vects],P0];
    (* Generates the solution *)
    Ck. (Exp /@ (Vals * t) Vects )]
	
RelFunc[m_, WorkingQ_,t_, Ini_:1] := Module[{M,Mx,My},
	If[!WorkingQ[[Ini]],
              Print["Initial state is a down state"];
                  Return[]];
    M = ArrangeMatrix[RotateLeft[
            Transpose[RotateLeft[Transpose[m],Ini-1]],Ini-1], 
              RotateLeft[WorkingQ,Ini-1]];
	Mx = Append[M[[1,1]], 
		 Transpose[M[[2,1]]] . Table[1,{Length[M[[2,1]]]}]];
	NullVec = Table[0, {Length[Mx]}];
	My = Transpose[Append[Transpose[Mx],NullVec]];
	NullVec[[1]] = 1;
	(1- Last[ProbTransient[My,NullVec,t]]) // Simplify
	]	 	


(* Based on the paper "Buzacott [Markov Approach to finding
Failure Times of repairable systems, IEEE Trans. on Reliability,
Vol. R-19, No. 6, pp. 128-133]" which shows how the moments of
the  system times may be obtained from the transition matrix of a
repairable system. *)

(* Splits a matrix columnwise into two submatrixes,
   dependent on whether the states are up or down states *)

SplitWrkng[mm_,WorkingQ_] := Module[{ColsW ={}, ColsF ={}},
		index = Table[i,{i,Length[mm]}];
		If[WorkingQ[[#]],ColsW=Append[ColsW,mm[[#]]],
          ColsF=Append[ColsF,mm[[#]]]]& /@ index; {ColsW ,ColsF}]	


(* Sorts the matrix so columns and rows for up and down states are
collected so the up-state transitions (EX) are in the upper left
corner and the down-state transitions are in the lower left (HX).
Furthermore the matrix is diveded into the four submatrixes. Note
that the paper operates with transposed matrixes.*)

ArrangeMatrix[m_, WorkingQ_] := Module[{XY,XZ,EX,HX,GX,FX},
	(* sorts columns *)
 	XY = SplitWrkng[m,WorkingQ];
 	(* from the working states*)	
	XZ = SplitWrkng[Transpose[XY[[1]]],WorkingQ];
	EX = Transpose[XZ[[1]]];
	GX = Transpose[XZ[[2]]];
	(* from the failed states *)
	XZ = SplitWrkng[Transpose[XY[[2]]],WorkingQ];
	FX = Transpose[XZ[[1]]];
	HX = Transpose[XZ[[2]]];
	{{EX,GX},{FX,HX}}]
	

(* NB! Deal with an arbitrary initial state;
   default value is 1*)
   
MTFF[m_, WorkingQ_, Ini_:1] := Module[{EX},
            If[!WorkingQ[[Ini]],
              Print["Initial state is a down state"];
                  Return[]];
            EX = ArrangeMatrix[RotateLeft[
            Transpose[RotateLeft[Transpose[m],Ini-1]],Ini-1], 
              RotateLeft[WorkingQ,Ini-1]][[1,1]];
            - LinearSolve[EX, 
			 Prepend[Table[0,{Length[EX]-1}],1]] .
			 	Table[1,{Length[EX]}]
			] 	

MTTF[m_, WorkingQ_] := Module[{bz,pp, h1},
	pp = SplitWrkng[ProbStationary[m],WorkingQ][[1]];
	h1 = Table[1,{Length[pp]}];
	bz = ArrangeMatrix[m,WorkingQ];
	(h1 . Inverse[- bz[[1,1]]] . pp ) / (pp . h1) //Simplify]
	
MUT[m_, WorkingQ_] := Module[{bz,pp, hw,hd},
	pp = SplitWrkng[ProbStationary[m],WorkingQ][[1]];
	bz = ArrangeMatrix[m,WorkingQ];
	hw = Table[1,{Length[bz[[1,1]]]}];
	hd = Table[1,{Length[bz[[2,2]]]}];
	(hw . pp ) / (hd . bz[[2,1]] . pp) //Simplify]
	
MDT[m_, WorkingQ_] := Module[{bx,pp, hw, hd,bz},
	pp = SplitWrkng[ProbStationary[m],WorkingQ][[1]];
	bz = ArrangeMatrix[m,WorkingQ];
	hw = Table[1,{Length[bz[[1,1]]]}];
	hd = Table[1,{Length[bz[[2,2]]]}];
	(1 - hw . pp ) / (hd . bz[[2,1]] . pp) //Simplify]
	
MTBF[m_, WorkingQ_] := Module[{bx,pp, hw, hd,bz},
	pp = SplitWrkng[ProbStationary[m],WorkingQ][[1]];
	bz = ArrangeMatrix[m,WorkingQ];
	hd = Table[1,{Length[bz[[2,2]]]}];
	1 / (hd . bz[[2,1]] . pp) //Simplify]

(* Based om B.E.H derivation in draft memo 2017-10-05 *)
	
DownTimeDensityLaplace[m_,WorkingQ_,s_]:= Module[{EX,GX,FX,HX, lH,lE, OnesR,X,PR,PX,PW},
{{EX,GX},{FX,HX}}=ArrangeMatrix[m,WorkingQ];
lH = Length[HX]; 
lE = Length[EX];
OnesR = Table[1,{lH}];
X=ArrayFlatten[{{EX,GX},{FX,HX}}];
PX= ProbStationary[X];
PW= Take[PX,lE];
PR= Take[PX,-lH];
1 - s OnesR.Inverse[s IdentityMatrix[lH]-HX].FX.PW  /(OnesR.FX.PW) //Simplify]

DownTimeDensity[m_,WorkingQ_,t_]:= Simplify[InverseLaplaceTransform[DownTimeDensityLaplace[m,WorkingQ,s],s,t]]

(*The Kroenecker product is most easily obtained by the outer product. 
  The outer product yields a tensor which must be appropriately 
  flattened to a two-dimentional structure. *)

KroenSum[a_,b_] := Flatten[Transpose[Flatten[Transpose[
				Outer[Times, a,IdentityMatrix[Length[b]]] + 
				Outer[Times, IdentityMatrix[Length[a]], b],
				   				{4,1,3,2}], 1],{3,2,1}],1] ;
KroenSum[a_,b_,c__] := KroenSum[KroenSum[a,b],c]
	
LabelList[C__] := Flatten[Outer[List,C],Length[{C}]-1]

SeriesMode[C__] := Apply[And,
				 Flatten[Outer[List,C],Length[{C}]-1], {1,1}];
				 


ParallelMode[C__] := Apply[Or,
				 Flatten[Outer[List,C],Length[{C}]-1],
				 						{1,1}];
				 						

(* The function KoutofN uses the function KSubsets from <
    DiscreteMath`Combinatorica`>. The function is included in this package in 
      order to reduce memory usage. Alternatively the Combinatorica package 
      may be loaded. *)
KSubsets[l_List,0] := { {} }
KSubsets[l_List,1] := Partition[l,1]
KSubsets[l_List,k_Integer?Positive] := {l} /; (k == Length[l])
KSubsets[l_List,k_Integer?Positive] := {}  /; (k > Length[l])

KSubsets[l_List,k_Integer?Positive] :=
	Join[
		Map[(Prepend[#,First[l]])&, KSubsets[Rest[l],k-1]],
		KSubsets[Rest[l],k]
	]	
(* <<DiscreteMath`Combinatorica`*)

KoutofN[koutofn_,k_] := 
  KSubsets[koutofn,k]/.{ List[x__List] :> Or[x]}/.{List -> And}				 						

KoutofNMode[C__, k_] :=KoutofN[#,k]& /@
				 Flatten[Outer[List,C],Length[{C}]-1];


(* Old version with Bug due to lack of order in drop operation

MergeStates[M_,i_,j__] := Module[{II,JJ},
            II = IdentityMatrix[Length[M]];
		      II = Fold[ Drop,II,Map[List,{j}]];
		      (* In an "ideal merge" the return transitions from the 
				 merged states should,     
			  be identical. If not return transitions from state i dominates *)
	         JJ=  M .Transpose[ II];
		      (* Add entrance rate to the merged states *)
		      II = IdentityMatrix[Length[M]];
              II[[i]] = II[[i]] + Table[If[ MemberQ[{j},x],1,0],{x, Length[II[[i]]]}];
		      II = Fold[ Drop,II,Map[List,{j}]];
              Transpose[ Transpose[JJ] .Transpose[ II]]
              ]
*)

MergeStates[M_,j__] := Module[{II,JJ,i,drops},
(* New version 2010-12-31. Bug fixed and take list as argument. Should be comptibel with 
old version, but not thourghly tested*)
            II = IdentityMatrix[Length[M]];
drops=Flatten[{j}];
If[Length[drops]<2,Print["No states to merge. ",drops, Return[M]]];
drops=Sort[drops, Greater];
i = Last[drops];
drops=Drop[drops,-1];
		      II = Fold[ Drop,II,Map[List,drops]];
		      (* In an "ideal merge" the return transitions from the 
				 merged states should,     
			  be identical. If not return transitions from state i (i.e. the lowest indexed) dominates *)
	         JJ=  M .Transpose[ II];
		      (* Add entrance rate to the merged states *)
		      II = IdentityMatrix[Length[M]];
              II[[i]] = II[[i]] + Table[If[ MemberQ[drops,x],1,0],{x, Length[II[[i]]]}];
		      II = Fold[ Drop,II,Map[List,drops]];
              Transpose[ Transpose[JJ] .Transpose[ II]]
              ]

ST4[\[CapitalOmega]_,m_,x_,y_]:=Module[{to, from,z},z=y - x;
If[( Abs /@ z).Table[1,{m}]==2,Table[{If[z[[i]]==-1,from=i],
If[z[[i]]==1,to=i]},{i,1,m}]; \[CapitalOmega][[to,from]] * x[[from]], 0]
]

GenHomogeneousSystem[\[CapitalOmega]_:List,wtestQ_:List,labels_:List,n_:Integer,K_:Integer]:=
Module[{Ix},GenHomogeneousSystem[\[CapitalOmega],wtestQ,labels,n,K,Ix]]

GenHomogeneousSystem[\[CapitalOmega]_:List,wtestQ_:List,labels_:List,n_:Integer,K_:Integer,Ix_]:=
  Module[{m, stategenerator,ii, iterator,xx,sm,sx,commas,syy,SysLables, Failed,
sWorkQ},
m=Length[\[CapitalOmega]];
(*gererate state space *)
stategenerator=Array[ii,{m}]; stategenerator[[1]] = n - (Drop[stategenerator,1]. Table[1,{m-1}]);
iterator= Table[{ii[j],0, n- Drop[Drop[stategenerator,{j,m}],1]. Table[1,{j-2}]},{j,2,m}];
(*Here,the generation may be bounded by introducing Max[Bound[[j]],n-Drop[Drop[stategenerator,{j,m}],1].Table[1,{j-2}]] as the upper iterator.Not tested. Tested 20019-03-25; does not work.*)
xx=Flatten[Table@@FlattenAt[{stategenerator, iterator},2],m-2];
Table[Ix[xx[[i]]]=i,{i,1,Length[xx]}]; 
(*Table[Ix=Function[i,xx[[i]] ],{i,1,Length[xx]}];*)
(* gererate matrix *)
sm=Table[ST4[\[CapitalOmega],m,xx[[i]],xx[[j]]],{j,1,Length[xx]},{i,1,Length[xx]}];
(*make lables*)
sx=Map[ToString, xx,{2}];
commas=Prepend[Table[",",{m-1}],""];
syy=(Apply[StringJoin,# labels, 2])& /@ sx;
SysLables=(Apply[StringJoin,Apply[StringJoin,# commas,1]])& /@ syy ;
Failed = If[#,0,1]&/@ wtestQ ;
sWorkQ=If[#>n-K,False,True]& /@ ((Failed . #)& /@ xx);
(* return*)
{SetDiagonal[sm], sWorkQ, SysLables}]


(***************** Other functions *****************)

StepPlot[x_List] := Module[{temp1, temp2, TempC},
    {temp1, temp2} = Transpose[x];
    TempC = Transpose[{Drop[temp1, 1], Drop[temp2, -1]}];
    ListPlot[Sort[Join[x, TempC], (#1[[1]] < #2[[1]]) &], Joined -> True]] 

(* THIS VERSION EXECUTES WITH MMA VERSIONS 6 -- 11
PlotDiagram[Mx_, WorkingQ_, Labels_] := 
 Module[{My, transitions, full, final, sl},
  (* ensures that composite labels are strings*)
  
  sl = ToString /@ Labels;

  (* sets diagonal elements to zero*)
  My = Mx Table[If[i==j,0,1], {i, Length[Mx]},{j,Length[Mx]}]
  (* This gives kosmetic failures with reals.
 My = Mx - DiagonalMatrix[Diagonal[Mx]]*);
  (*Build label to label transition*)
  
  transitions = Outer[(#2 -> #1) &, sl, sl];
  (*Merges the input for Graphplot*)
  
  full = Flatten[
    MapThread[{#1, #2} &, {transitions, My}, 2], {1, 2}];
  (*Remove the null transitions*)
  
  final = Reverse[Complement[full, Select[full, (#[[2]] == 0) &]]];
  GraphPlot[final,(* VertexLabeling->True,*) DirectedEdges -> True, Method -> "SpringElectricalEmbedding",
   VertexRenderingFunction -> ({White, EdgeForm[Black], 
       If[WorkingQ[[Position[sl, #2][[1, 1]]]], Disk[#, .15], 
        Rectangle[# - {0.15, 0.1}, # + {0.15, 0.1}]], Black, 
       Text[#2, #1]} &)]] *)
 (* THIS VERSION EXECUTES WITH MMA VERSIONS 12 --      *)
 PlotDiagram[Mx_, WorkingQ_, Labels_] := 
 Module[{My, transitions, full, final, sl},
  (* ensures that composite labels are strings*)
  
  sl = ToString /@ Labels;
\[NonBreakingSpace]\[NonBreakingSpace](* sets diagonal elements to zero*)
  My = Mx Table[If[i==j,0,1], {i, Length[Mx]},{j,Length[Mx]}]
  (* This gives kosmetic failures with reals.
 My = Mx - DiagonalMatrix[Diagonal[Mx]]*);
  (*Build label to label transition*)
  
  transitions = Outer[(#2 -> #1) &, sl, sl];
  (*Merges the input for Graphplot*)
  
  full = Flatten[
    MapThread[{#1, #2} &, {transitions, My}, 2], {1, 2}];
  (*Remove the null transitions*)
  
  final = Reverse[Complement[full, Select[full, (#[[2]] == 0) &]]];
  GraphPlot[final,(* VertexLabeling->True,*) DirectedEdges -> True, Method -> "SpringElectricalEmbedding",
   (* VertexRenderingFunction -> ({White, EdgeForm[Black], 
       If[WorkingQ[[Position[sl, #2][[1, 1]]]], Disk[#, .15], 
        Rectangle[# - {0.15, 0.1}, # + {0.15, 0.1}]], Black, 
       Text[#2, #1]} &)*)
     VertexLabels -> Placed["Name", Center] ,
     (*VertexShapeFunction ->  If[WorkingQ[[#]], {#-> "Circle"},{#2->"Square"}]*)
\[NonBreakingSpace]\[NonBreakingSpace]\[NonBreakingSpace]\[NonBreakingSpace]\[NonBreakingSpace]VertexShapeFunction -> (If[WorkingQ[[Position[sl, #2][[1, 1]]]], {EdgeForm[Black], White, Disk[#1, 0.15], 
     Black, Text[#2, #1]}, {EdgeForm[Black], White, Rectangle[# - {0.15, 0.1}, # + {0.15, 0.1}], 
     Black, Text[#2, #1]}] &), 
\[NonBreakingSpace]\[NonBreakingSpace]\[NonBreakingSpace]\[NonBreakingSpace]\[NonBreakingSpace]EdgeShapeFunction ->  GraphElementData[{"Arrow", "ArrowSize" -> .05}]
       ]]
End[ ]

EndPackage[ ]
