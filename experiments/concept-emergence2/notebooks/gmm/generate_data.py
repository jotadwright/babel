import numpy as np

from sklearn.datasets import make_blobs


def gen_data(k, dim, points_per_cluster, lim, spread):
    # select n_clusters by sampling from a uniform distribution
    centers = np.random.uniform(lim[0], lim[1], (k, dim))
    # select cluster_std by sampling from a uniform distribution
    cluster_std = np.random.uniform(spread[0], spread[1], k)
    # make blobs with sklearn
    X, y = make_blobs(
        n_samples=points_per_cluster,
        cluster_std=cluster_std,
        centers=centers,
        n_features=dim,
        random_state=1,
    )
    return X, y


def shuffle_data(X, Y):
    p = np.random.permutation(len(X))
    X, Y = X[p], Y[p]
    return X, Y
