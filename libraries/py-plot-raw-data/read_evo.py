import os

import pandas as pd
from utils import (BABEL_PATHNAME, apply_average_across_series,
                   apply_window_over_each_series)


def get_collect_func(file_type):
    if file_type == "csv":
        return read_csv
    elif file_type == "lisp":
        return read_lisp
    else:
        return ValueError(f"File type {file_type} is not valid.")

def read_csv(path, file_type):
    """
    Read in the raw data at path. If no file type is specified
    in path, the file_type argument is used.
    The raw data is returned as a Pandas DataFrame, where each
    interaction is a row and each series is a column. The column
    names are integers starting from 0.

    :param path: a pathname
    :param file_type: a csv 
    :returns: a Pandas DataFrame
    """
    if '.' in path:
        data_path = os.path.join(BABEL_PATHNAME, path)
    else:
        data_path = os.path.join(BABEL_PATHNAME, f'{path}.{file_type}')
    data = pd.read_csv(data_path, comment='#').dropna(how='all', axis=1)
    # We duplicate the first row, to have a nicer plot...
    data.loc[-1] = data.loc[0]
    data.index = data.index + 1
    data.sort_index(inplace=True)
    return data

def read_lisp(path, file_type):
    data_path = os.path.join(BABEL_PATHNAME, f"{path}.{file_type}")
    with open(data_path, 'r') as f:
        data = f.read().splitlines()
    data = data[5][3:-3].split(") (")
    data = [i.split(" ") for i in data]
    data = [[float(j) for j in i] for i in data]
    data = pd.DataFrame(data)
    data = data.T # Transpose
    # We duplicate the first row, to have a nicer plot...
    # TODO: to delete?
    data.loc[-1] = data.loc[0]
    data.index = data.index + 1
    data.sort_index(inplace=True)
    return data

def collect_data_for_evo_plot(raw_file_paths, file_type='dat', windows=None,
                              only_x_last_interactions=None,
                              start=None, end=None, series_numbers=None,
                              average_mode='mean', error_mode='stdev',
                              percentiles=[5, 95]):
    """
    Collect all data for the evo plot.

    :param raw_file_paths: The paths to the data
    :param file_type: The file type of the data files. Default='dat'
    :param windows: The window for the rolling mean,
        specified for each data file.
    :param only_x_last_interactions: Only keep the 'x' last interactions.
    :param start: Only keep interactions after 'start'
    :param end: Only keep interactions before 'end'
    :param series_numbers: Only keep these series.
        Series start counting from 1.
    :param average_mode: Use this average mode across the series.
    :param error_mode: Use this error mode across the series.
    :param percentiles: Use this percentiles for the error mode (if necessary).
    :returns: A list of Pandas DataFrames containing all data for the evo plot.
    """
    result = []
    for path, window in zip(raw_file_paths, windows):
        data = get_collect_func(path, file_type)
        series_length = len(data)

        if window is not None:
            data = apply_window_over_each_series(data, window)

        if only_x_last_interactions is not None:
            x = series_length - only_x_last_interactions
            data = data.iloc[x:, :]
            data = data.reset_index(drop=True)

        if start is not None or end is not None:
            s = start if start is not None else 0
            e = end if end is not None else len(data)
            data = data.iloc[s:e, :]
            data = data.reset_index(drop=True)

        if series_numbers is not None:
            if type(series_numbers) is not list:
                raise Exception(f'''series_numbers should be a list,
                                    got {series_numbers}''')
            else:
                series_indexes = [s-1 for s in series_numbers]
                data = data.iloc[:, series_indexes]

        if len(data.columns) > 1:
            data = apply_average_across_series(data, average_mode, error_mode,
                                               percentiles)

        result.append(data)
    return result
