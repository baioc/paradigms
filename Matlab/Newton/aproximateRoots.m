% Finds aproximate roots R inside [from : step : to] for a function f that is
% (mostly) continuous in this interval using Bolzano's Intermediate Value Theorem
function R = aproximateRoots(f, from, to, step=1e-2)

    x = [from : step : to];
    R = [];
    root_count = 0;

    ya = f(x(1));
    for i = 2 : length(x)
        yb = f(x(i));

        if ya * yb <= 0 && abs(ya) < 1 && abs(yb) < 1
            root_count++;
            R(root_count,1) = (x(i-1) + x(i)) / 2;
        end

        ya = yb;
    end

end
