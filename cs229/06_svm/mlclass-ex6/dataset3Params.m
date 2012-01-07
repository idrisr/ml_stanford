function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
C = 1;
sigma = 0.3;
Cs =     [0.01, 0.03, 0.1, 0.3, 1, 3 ,10, 30];
sigmas = [0.01, 0.03, 0.1, 0.3, 1, 3 ,10, 30];

%Cs =     [0.01, 0.03];
%sigmas = [0.01, 0.03];
% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%
min_err = 1;
err = zeros(length(Cs) * length(sigmas),1);
for i = 1:length(Cs)
    for j = 1:length(sigmas)
        %C = Cs(i);
        %sigma = sigmas(j);
        model = svmTrain(X, y, Cs(i), @(x1, x2) gaussianKernel(x1, x2, sigmas(j))); 
        p = svmPredict(model, Xval);
        err = mean(double(p ~= yval));
        if err < min_err
            fprintf('new min\n');
            C = Cs(i);
            sigma = sigmas(j);
            min_err = err;
        end
        fprintf('C=%.2f, sigma=%.2f, err=%.2f\n', Cs(i), sigmas(j), err);
    end
end
% =========================================================================

end
