function fn = fibonacci(n)

	% golden ratio number and its conjugate are eigen-values
	phi = (1 + sqrt(5)) / 2;
	fi = 1 - phi; % (1 - sqrt(5)) / 2;

	% change of basis from B to C
	Mcb = [1  , 1;
	       phi, fi;];

	% change of basis from C to B
	aux = 1 / (fi - phi);
	% Mbc = aux * [ fi , -1;
	%              -phi,  1;];

	% Sc = [0;
	%       1;]; % fibonacci seed {0,1} in canonical basis C = {(1,0);(0,1)}

	% pre-computed Mbc * Sc
	MbcSc = aux * [-1;1];

	% linear Transformation T(a,b) = (b,a+b) in eigen-basis B = {(1,phi);(1,fi)}
	% Tb = [phi, 0 ;
	%       0  , fi;];

	% pre-computed Tb^n
	Tbn = [phi^n, 0   ;
	       0    , fi^n;];

	% final result vector [F(n);F(n-1)], in normal basis
	Fn = Mcb * Tbn * MbcSc;
	fn = round(Fn(1,1));

end
