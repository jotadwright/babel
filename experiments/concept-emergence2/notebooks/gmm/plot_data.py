import numpy as np
import matplotlib.pyplot as plt


def plot_data(x, y=None):
    dim = x.shape[1]
    fig = plt.figure()
    ax = fig.gca()

    if y is None:

        if dim == 1:
            ax.scatter(x, np.zeros_like(x), s=10, alpha=0.4)
            # ax.set_xlim(-0.1, 1.1)
            ax.set_ylim(-0.05, 1)
        if dim == 2:
            ax.scatter(x[:, 0], x[:, 1], s=3, alpha=0.4)
            ax.autoscale(enable=True)
    else:
        k_clusters = len(np.unique(y))
        if dim == 1:
            for i in range(k_clusters):
                ax.scatter(
                    x[y == i],
                    np.zeros_like(x[y == i]),
                    s=10,
                    label=f"Cluster{i}",
                    alpha=0.4,
                )
            # ax.set_xlim(-0.1, 1.1)
            ax.set_ylim(-0.05, 1)
        if dim == 2:
            for i in range(k_clusters):
                plt.scatter(
                    x[y == i, 0], x[y == i, 1], s=3, label=f"Cluster{i}", alpha=0.4
                )


def plot_gmm(X, Y, x_min, x_max, x, y):
    """Plot the density function based on samples from a GMM."""
    fig = plt.figure()
    ax = fig.gca()

    # plot the original data
    # ax.scatter(X, np.zeros_like(X), s=10, alpha=0.1, label='Data')
    k_clusters = len(np.unique(Y))
    for i in range(k_clusters):
        ax.scatter(
            X[Y == i], np.zeros_like(X[Y == i]), s=10, label=f"Cluster{i}", alpha=0.4
        )

    # plot the GMM density function
    ax.plot(x, y, color="green", alpha=0.2)
    ax.set_ylim(-0.05, 1)
    ax.set_xlim(x_min, x_max)
