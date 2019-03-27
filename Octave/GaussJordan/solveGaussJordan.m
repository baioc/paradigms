function X = solveGaussJordan(A, B)
  
  n = length(A);
  A = [A B];
  
  for k = 1 : n-1
    
    A = pivotLine(A, k, n);
    
    for i = k+1 : n
      
      aux = A(i,k) / A(k,k);
      
      for j = k+1 : n+1
        A(i,j) -= aux * A(k,j);
      end
      
      A(i,k) = 0;
    end
    
  end
  
  if A(n,n) == 0
    if A(n,n+1) == 0
      X(n) = 1; % "chute" que resolve o sistema (tambem)
      "Sistema Indeterminado"
    else
      X(n) = NaN;
      "Sistema Impossivel"
    end
  end
  
  X(n) = A(n,n+1) / A(n,n);
  
  for i = n-1:-1:1
    s = sum(A(i,i+1:n) .* X(i+1:n));
    X(i) = (A(i,n+1) - s) / A(i,i);
  end
  
  X = transpose(X);
  
end
