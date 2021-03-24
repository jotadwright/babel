#!/usr/bin/env python

import os

# The same gnuplot colors as used in plot-raw-data
global GREAT_GNUPLOT_COLORS
GREAT_GNUPLOT_COLORS = ["darkcyan", "darkgoldenrod", "darkred", "navy",
                        "darkgreen", "gray", "r", "green", "darkorange",
                        "royalblue", "seagreen", "deeppink", "purple",
                        "orangered", "darkgray", "darkkhaki", "darkturquoise",
                        "salmon", "darkmagenta", "y", "violet", "lightgreen"]


# It's assumed this folder is placed at
# Babel/libraries/py-plot-raw-data/
# From this, the Babel pathname is being derived
global BABEL_PATHNAME
BABEL_PATHNAME = os.path.dirname(
    os.path.dirname(
        os.path.dirname(
            os.path.abspath(__file__))))


def pathname_name(path):
    """
    Extracts the filename of a pathname

    :param path: a pathname
    :returns: a filename
    """
    return os.path.splitext(os.path.basename(path))[0]


def pathname_type(path):
    """
    Extracts the file type of a pathname

    :param path: a pathname
    :returns: a file type
    """
    return os.path.splitext(path)[1]


def pathname_directory(path):
    """
    Extracts the directory of a pathname

    :param path: a pathname
    :returns: a directory pathname
    """
    return os.path.dirname(path)


def get_error_bar_distance(n):
    """
    Compute an appropriate spacing for the error bars, depending
    on the number of data points

    :param n: Number of data points
    :returns: The spacing for the error bars
    """
    for x in range(0, 1000000, 100):
        if n <= x+1:
            return round(x/10)


def apply_window_over_each_series(data, window):
    """
    Apply a rolling mean over each serie in the data.

    :param data: a Pandas DataFrame
    :param window: the size of the window for the rolling mean
    :returns: the Pandas DataFrame with rolling mean applied
    """
    return data.rolling(window=window, min_periods=1).mean()


def apply_average_across_series(data, average_mode, error_mode,
                                percentiles=[5, 95]):
    """
    Apply some averaging operation across the series in the data.

    :param data: a Pandas DataFrame
    :param average_mode: What operation to use for averaging across the series.
    :param error_mode: What operation to use to indicate the variation across
        the series.
    :param percentiles: When using the 'percentile' error_mode, the quantiles
        used for the low and high end can be specified using this argument.
        Values specified should be between 0 and 100.
    :returns: a Pandas DataFrame with three extra columns, one for specifying
        the average value and the other two for specifying the low and high
        end of the error bar.
    """
    if average_mode == 'mean':
        data['average'] = data.mean(axis=1)
    elif average_mode == 'median':
        data['average'] = data.median(axis=1)
    else:
        raise Exception(f"""Unknown average_mode {average_mode}. Supported
                            modes are: 'mean' and 'median'""")

    if error_mode == 'stdev':
        std = data.std(axis=1)
        data['error_low'] = std
        data['error_high'] = std
    elif error_mode == 'min_max':
        all_min = data.min(axis=1).to_numpy()
        all_max = data.max(axis=1).to_numpy()
        data['error_low'] = data['average'] - all_min
        data['error_high'] = all_max - data['average']
    elif error_mode == 'percentile':
        if percentiles is None or len(percentiles) != 2:
            raise Exception(f"""'percentiles' should be a list of 2 elements,
                                received {percentiles}""")
        if type(percentiles[0]) is not int:
            raise Exception(f'''Expected an integer for the low-end percentile,
                                received {percentiles[0]}''')
        if type(percentiles[1]) is not int:
            raise Exception(f'''Expected an integer for the high-end
                                percentile, received {percentiles[1]}''')
        if percentiles[0] > 100 or percentiles[0] < 0:
            raise Exception(f'''Expected a value between 0 and 100 for low-end,
                                percentile received {percentiles[0]}''')
        if percentiles[1] > 100 or percentiles[1] < 0:
            raise Exception(f'''Expected a value between 0 and 100 for high-end
                                percentile, received {percentiles[1]}''')
        low_percentile = data.quantile(percentiles[0]/100, axis=1).to_numpy()
        high_percentile = data.quantile(percentiles[1]/100, axis=1).to_numpy()
        data['error_low'] = data['average'] - low_percentile
        data['error_high'] = high_percentile - data['average']
    else:
        raise Exception(f"""Unknown error_mode {error_mode}. Supported modes
                            are: 'stdev', 'min_max' and 'percentile'""")

    return data
