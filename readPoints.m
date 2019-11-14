function pts = readPoints(image, n)
if nargin < 2
    n = Inf;
    pts = zeros(2, 0);
else
    pts = zeros(2, n);
end
imshow(image);
xold = 0;
yold = 0;
k = 0;
hold on;
while 1
    [xi, yi, but] = ginput(1);
    if ~isequal(but, 1)
        break
    end
    k = k + 1;
    pts(1,k) = xi;
    pts(2,k) = yi;

    plot(xi, yi, 'go');
    if isequal(k, n)
      break
    end
    xold = xi;
    yold = yi;
end
hold off;
if k < size(pts,2)
    pts = pts(:, 1:k);
end 
grayLevel = grayImage(round(yi), round(xi));
end