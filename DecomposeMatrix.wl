(* ::Package:: *)

(*    U is the 4x4 matrix given by user

%   Identity: 4x4 identity matrix 
%   SubSets: the 4x4 or 2x2 matrices given by user
%   Stack: a dummy 4x4 matrix
%   LastMatrix: result of multiplication of matrices in each loop

*)
identity4 = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}};



findMatrix = Function[{stack, lastMatrix, set, U, rec, limit},
	Module[{i},
		If[lastMatrix == U, Return[stack];,
			For[i=1,i<=Length[set],i++,
				If[rec < limit,If[lastMatrix.set[[i]] != identity4,findMatrix[Append[stack,set[[i]]], lastMatrix.set[[i]], set, U, rec+1, limit];]]
			]
		]
	]
]

decomposeMatrix = Function[U, 
	Module[{stack, lastMatrix, sets, len, i, mat, identity, set, ris},
		identity = {{1,0},{0,1}};
		stack = {};
		lastMatrix = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}};
		sets = {{{1/Sqrt[2],1/Sqrt[2]},{1/Sqrt[2],-1/Sqrt[2]}},{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,-1}}};
		len = Length[sets];
		set = {};
		For[i=1,i<=len,i++,
			If[Length[sets[[i]]] == 2,
				mat = sets[[i]];
				set = Append[set, KroneckerProduct[mat, identity]];
				set = Append[set, KroneckerProduct[identity, mat]];,
				set = Append[set, sets[[i]]];
			]
		]
		i = 0;
		While[Length[ris] == 0, ris = findMatrix[stack, lastMatrix, set, U, 0, i];i++]
		Print["Matrices:"]; 
		Print[ris];
	] 
]

decomposeMatrix[{{1,0,0,0},{0,1,0,0},{0,0,0,1},{0,0,1,0}}]





















