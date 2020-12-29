(* ::Package:: *)

(* ::Section:: *)
(*Day 5: Binary Boarding*)


SetDirectory@NotebookDirectory[];
<<"aoc_day5_input.wl"


(* ::Subsection:: *)
(*Code*)


Clear@plane
plane=ArrayReshape[Array[seat,{128,8},0],ConstantArray[2,10]];


Clear@pti
pti[pass_String]:=Characters@pass/.{"F"|"L"->1,"B"|"R"->2}
pti[passes:{__String}]:=pti/@passes


Clear@its
its[inds_]:=Extract[plane,inds];


Clear@pts
pts=its@*pti;


Clear@seats
seats=pts@StringSplit@day[5];


Clear@stoid
stoid=8#1+#2&;


(* ::Subsubsection:: *)
(*Tests*)


(* ::Input:: *)
(*pts@"FBFBBFFRLR"*)
(*(*Should be {44,5}*)*)


(* ::Input:: *)
(*pts@{"BFFFBBFRRR","FFFBBBFRRR","BBFFBBFRLL"}*)


(* ::Subsubsection:: *)
(*Part 1*)


stoid@@@seats//Max


(* ::Subsubsection:: *)
(*Part 2*)
