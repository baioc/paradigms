function y = Horner(Pn, x)

	y = Pn(1);
	for i = 2 : length(Pn)
		y = y * x + Pn(i);
	end

end
