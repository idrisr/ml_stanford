import numpy as np
import pdb
import pandas as pn
import matplotlib
import matplotlib.pyplot as plt

def compute_cost(X, Y, theta):
    m = len(Y)
    J = 0
    # X*theta - Y
    i = np.dot(X, theta) - Y
    i = np.square(i)
    J = (1.0 / (2*m)) * sum(i)
    return J

def read_data():
    data_file = '/home/id/code/ml/assn/mlclass-ex1/ex1data1.txt'
    data = np.loadtxt(data_file, delimiter=',')
    #data = pn.read_csv(data_file, header = None)
    X = data[:, 0]
    Y = data[:, 1]
    Y = np.reshape(Y, (len(Y), 1))
    return X, Y

def plot_data(X, Y):
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(X, Y, 'o')
    ax.set_title('Data Plot')
    plt.show()

def add_ones(features):
    #pdb.set_trace()
    X = np.ones((features.size, 2))
    X[:, 1] = features
    return X

def gradient_descent(X, Y, theta, alpha, iterations):
    m = len(Y)
    for i in xrange(0, iterations):
        h_theta_y = np.dot(X, theta) - Y
        dJ_dtheta = np.dot(np.transpose(X), h_theta_y)
        theta = theta - alpha/m * dJ_dtheta
        J = compute_cost(X, Y, theta)
        #print J
    return theta

if __name__ == '__main__':
    X, Y= read_data()
    #plot_data(X, Y)
    X = add_ones(X)
    theta = np.zeros((X.shape[-1], 1))
    iterations = 15000
    alpha = 0.01

    # compute and display initial cost
    J = compute_cost(X, Y, theta)
    print 'Initial Cost: %r' % (J,)


    # run gradient descent
    theta = gradient_descent(X, Y, theta, alpha, iterations)
    print 'theta: %r' % (theta,)
