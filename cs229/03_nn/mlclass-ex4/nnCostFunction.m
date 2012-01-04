function [J grad] = nnCostFunction(nn_params, ...
                                   input_layer_size, ...
                                   hidden_layer_size, ...
                                   num_labels, ...
                                   X, y, lambda)
% NNCOSTFUNCTION Implements the neural network cost function for a two layer
% neural network which performs classification
%   [J grad] = NNCOSTFUNCTON(nn_params, hidden_layer_size, num_labels, ...
%   X, y, lambda) computes the cost and gradient of the neural network. The
%   parameters for the neural network are "unrolled" into the vector
%   nn_params and need to be converted back into the weight matrices. 
% 
%   The returned parameter grad should be a "unrolled" vector of the
%   partial derivatives of the neural network.
%

% Reshape nn_params back into the parameters Theta1 and Theta2, the weight matrices
% for our 2 layer neural network
Theta1 = reshape(nn_params(1:hidden_layer_size * (input_layer_size + 1)), ...
                 hidden_layer_size, (input_layer_size + 1));

Theta2 = reshape(nn_params((1 + (hidden_layer_size * (input_layer_size + 1))):end), ...
                 num_labels, (hidden_layer_size + 1));

% Setup some useful variables
m = size(X, 1);
         
% You need to return the following variables correctly 
J = 0;
Theta1_grad = zeros(size(Theta1));
Theta2_grad = zeros(size(Theta2));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ====================== YOUR CODE HERE ======================
% Instructions: You should complete the code by working through the
%               following parts.
%
% Part 1: Feedforward the neural network and return the cost in the
%         variable J. After implementing Part 1, you can verify that your
%         cost function computation is correct by verifying the cost
%         computed in ex4.m

%convert y to matrix form
y = eye(10)(y,:);

h1 = sigmoid([ones(m, 1) X] * Theta1');
h2 = sigmoid([ones(m, 1) h1] * Theta2');
for k=1:num_labels 
    J += 1/m * sum(  -y(:,k)'  *   log(h2(:,k)) - 
                  (1 -y(:,k))' * log(1-h2)(:,k));
    %grad = 1/m * X'*(sigmoid(X*theta) - y);
end

r = lambda / (2*m) * (sum(sum(Theta1(:,2:end).^2)) + 
                      sum(sum(Theta2(:,2:end).^2)));
J = J + r;

%
% Part 2: Implement the backpropagation algorithm to compute the gradients
%         Theta1_grad and Theta2_grad. You should return the partial derivatives of
%         the cost function with respect to Theta1 and Theta2 in Theta1_grad and
%         Theta2_grad, respectively. After implementing Part 2, you can check
%         that your implementation is correct by running checkNNGradients
%
%         Note: The vector y passed into the function is a vector of labels
%               containing values from 1..K. You need to map this vector into a 
%               binary vector of 1's and 0's to be used with the neural network
%               cost function.
%
%         Hint: We recommend implementing backpropagation using a for-loop
%               over the training examples if you are implementing it for the 
%               first time.
grad = 0;
for t=1:m
    % 1. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Set the input layer’s values (a(1) ) to the t-th training example x(t) .
    % Perform a feedforward pass (Figure 2), computing the activations (z (2) ,
    % a(2) , z (3) , a(3) ) for layers 2 and 3. 
    % Note that you need to add a+1 term to ensure that the vectors of 
    % activations for layers a(1) and a(2) also include the bias unit.  In 
    % Octave, if a 1 is a column vector, adding one corresponds to 
    % a_1 = [1 ; a 1].
    a1 = X(t,:);
    a1 = [1 a1];
    z2 = a1 * Theta1';
    a2 = sigmoid(z2);
    a2 = [1 a2];
    z3 = a2 * Theta2';
    a3 = sigmoid(z3);

    % 2. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % For each output unit k in layer 3 (the output layer), 
    % set δ(3)k = (a(3)k − yk )
    % δ is delta

    % where yk ∈ {0, 1} indicates whether the current training example belongs 
    % to class k (yk = 1), or if it belongs to a different class (yk = 0).  
    % You may find logical arrays helpful for this task 
    % (explained in the previous programming exercise).
    for k=1:num_labels
        delta3 = a3 - y(k,:);
        delta2 = delta3*Theta2 .* sigmoidGradient(z3);
        % Should have same dimensions as Theta1, which is 25x401
        Theta1_grad = 

        % Should have same dimensions as Theta2, which is 10x25
        Theta2_grad = 

        grad += delta2*a1' + delta3*a2';
    end
end

% Part 3: Implement regularization with the cost function and gradients.
%
%         Hint: You can implement this around the code for
%               backpropagation. That is, you can compute the gradients for
%               the regularization separately and then add them to Theta1_grad
%               and Theta2_grad from Part 2.
%
% -------------------------------------------------------------

% =========================================================================

% Unroll gradients
grad = [Theta1_grad(:) ; Theta2_grad(:)];


end
