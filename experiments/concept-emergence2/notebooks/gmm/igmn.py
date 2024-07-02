import numpy as np
from scipy.stats import chi2


class IGMN:
    """Incremental Gaussian Mixture Network

    Implements the IGMN algorithm as described in:
        - https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0139931&type=printable

    Args:
        input_dim (int): dimension of the input data
        beta (float): confidence level for the chi-square test
        all_adults_criterion (bool): whether to use the all_adults_criterion
        age_min (int): minimum age for a component to be considered an adult
        acc_min (float): minimum accumulation for a component to be considered an adult
        max_components (int): maximum number of components in the model
        rank_type (str): type of covariance matrix to use, either "full" or "diag"
        closest_n (int): number of closest components to merge into
    """

    def __init__(
        self,
        input_dim,
        beta,
        all_adults_criterion,
        age_min,
        acc_min,
        max_components,
        rank_type,
        closest_n,
    ):
        self.input_dim = input_dim

        # hyperparameters
        self.beta = beta
        self.all_adults_criterion = all_adults_criterion
        self.age_min = age_min
        self.acc_min = acc_min
        self.max_components = max_components
        self.rank = rank_type
        self.closest_n = closest_n

        # components
        self.components = []

    def create_new_component(self, x, components):
        """Create a new component with the given input x

        As described in section 2.2 (Creating new components)
        """
        mu = x
        acc = 1
        age = 1

        # p => mixing coefficient
        if len(components) > 0:
            p = 1 / (sum([comp["acc"] for comp in components]) + 1)
        else:
            p = 1

        sigma_ini = 0.01
        sigma = sigma_ini * np.eye(len(x))  # equation 13
        return {"mu": mu, "sigma": sigma, "p": p, "acc": acc, "age": age}

    def remove_spurious_components(self):
        """A component j is removed whenever u_j > u_min and sp_j < sp_min, where u_min and sp_min are user-defined thresholds.

        In that case, also, p(k) must be adjusted for all k in K (with k != j).
        In other words, each component is given some tim eu_min to show its importance to the model in the form of an accumulation of its posterior probability sp_j

        As described in section 2.3 (Removing spurious components)
        """
        for j, component_j in enumerate(self.components):
            if component_j["age"] > self.age_min and component_j["acc"] < self.acc_min:
                self.components.pop(j)
        self.update_priors()

    def squared_mahalanobis_distance(self, x, mu, cov):
        """Squared Mahalanobis distance for a multivariate gaussian distribution"""
        sigma_inv = np.linalg.inv(cov)
        diff = x - mu  # error vector
        d_sq = diff.T @ sigma_inv @ diff
        return d_sq.item()

    def pdf(self, x, mu, cov):
        """Multivariate Gaussian PDF"""
        D = self.input_dim
        d_sq = self.squared_mahalanobis_distance(x, mu, cov)  # equation 1
        norm_constant = 1 / np.sqrt((2 * np.pi) ** D * np.linalg.det(cov))
        return norm_constant * np.exp(-0.5 * d_sq)  # equation 2

    def update_with_x(self, component_j, x):
        """Equations 2 to 12 in the paper"""

        # TODO: problem if no component is close and none can be created
        # then this denom value goes possibly to zero, which results in p_j_x =  NaN
        denom = 0
        for comp in self.components:
            p_x_k = self.pdf(x, comp["mu"], comp["sigma"])
            denom += p_x_k * comp["p"]
        
        p_x_j = self.pdf(x, component_j["mu"], component_j["sigma"])  # equation 2
        p_j_x = (p_x_j * component_j["p"]) / denom  # equation 3
        component_j["age"] = component_j["age"] + 1  # equation 4
        component_j["acc"] = component_j["acc"] + p_j_x  # equation 5
        e_j = x - component_j["mu"]  # equation 6
        omega_j = p_j_x / component_j["acc"]  # equation 7

        delta_mu_j = omega_j * e_j  # equation 8

        component_j["mu"] = component_j["mu"] + delta_mu_j  # equation 9
        e_j2 = x - component_j["mu"]  # equation 10

        # clip delta_mu_j to avoid making the covariance matrix non positive definite
        # TODO: find a better way to clip this value
        # delta_mu_j = np.clip(delta_mu_j, -0.1, 0.001)
        # analysis of why this can get non-positive:
        # term 1: 1-omega_j is a scalar, copy is a matrix -> term is positive definite
        # term 2: + (e_j2 @ e_j2.T)
        #               1. outer-product of a vector with itself -> positive definite,
        #               2. multiplication by scalar omega_j -> positive definite,
        #               3. addition of two positive definite matrices -> positive definite
        # term 3: - (delta_mu_j @ delta_mu_j.T)
        #               1. outer-product of a vector with itself -> positive definite
        #               2. BUT subtraction of two positive definite matrices -> not necessarily positive definite
        # BUT if delta_mu_j is too large, it can make the matrix non-positive definite
        # I currently clip delta_mu_j to avoid this issue, but it is not the best solution as I am losing information
        sigma_prev = component_j["sigma"].copy()
        component_j["sigma"] = (
            ((1 - omega_j) * sigma_prev)
            + (omega_j * (e_j2 @ e_j2.T))
            - (delta_mu_j @ delta_mu_j.T)
        )  # equation 11

        # ensure positive definite
        # component_j["sigma"] = self.ensure_positive_definite(component_j["sigma"])

        # add epsilon to the diagonal to avoid singular matrix
        # eps = 1e-6
        # component_j["sigma"] += eps * np.eye(self.input_dim)

        # do not use full rank covariance matrix, only keep variances
        # if self.rank == "diag":
        #     component_j["sigma"] = np.diag(np.diag(component_j["sigma"]))

        try:
            res = np.linalg.eigvals(component_j["sigma"])
            if not np.all(res > 0):
                component_j["sigma"] = sigma_prev
        except:
            component_j["sigma"] = sigma_prev

        component_j["p"] = component_j["acc"] / sum(
            [comp["acc"] for comp in self.components]
        )  # equation 12

    def merge_to_closest_n(self, x):
        distances = []
        for component in self.components:
            d_sq = self.squared_mahalanobis_distance(
                x, component["mu"], component["sigma"]
            )

            distances.append(d_sq)

        # sort the components by distance
        sorted_components = [
            comp for _, comp in sorted(zip(distances, self.components))
        ]
        closest_components = sorted_components[: self.closest_n]

        for comp in closest_components:
            self.update_with_x(comp, x)

    def update_priors(self):
        total_acc = sum([comp["acc"] for comp in self.components])
        for comp in self.components:
            comp["p"] = comp["acc"] / total_acc

    def update(self, x):
        # section 2.2 - creating new components
        if not self.components:
            # print("No components yet, creating new one")
            new_component = self.create_new_component(x, self.components)
            self.components.append(new_component)
            return

        # section 2.3 - removing spurious components
        self.remove_spurious_components()

        # section 2.2 - learning
        needs_update = False
        chi_sq_threshold = chi2.ppf(1 - self.beta, self.input_dim)
        for component_j in self.components:
            d_sq = self.squared_mahalanobis_distance(
                x, component_j["mu"], component_j["sigma"]
            )
            if d_sq < chi_sq_threshold:
                needs_update = True

        if needs_update:
            for component_j in self.components:
                self.update_with_x(component_j, x)

        # criterion 2: only create a new neuron if all neurons of that model have an age greater than the parameter age_min (taken from follow-up paper -> IGMN-NSE paper)
        all_adults = (
            all([comp["age"] > self.age_min for comp in self.components])
            if self.all_adults_criterion
            else True
        )

        if not needs_update and all_adults:
            if len(self.components) < self.max_components:
                new_component = self.create_new_component(x, self.components)
                self.components.append(new_component)
            else:
                self.merge_to_closest_n(x)

        self.update_priors()

    def likelihood(self, x):
        """Marginal likelihood [p(x|𝜃)] of the data x given the model"""
        components = self.components
        prob = 0
        for comp in components:
            p_x_j = self.pdf(x, comp["mu"], comp["sigma"])
            p = comp["p"]
            prob += p_x_j * p
        return prob

    def fit(self, X):
        for x in X:
            self.update(x)

    def score_samples(self, X):
        return np.array([np.log(self.likelihood(x)) for x in X])
