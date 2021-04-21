#!/usr/bin/env python

import os
import pandas as pd

from collections import OrderedDict
from utils import BABEL_PATHNAME
from utils import apply_window_over_each_series
from utils import apply_average_across_series


def read_raw_alist_data(path, file_type):
    if '.' in path:
        data_path = os.path.join(BABEL_PATHNAME, path)
    else:
        data_path = os.path.join(BABEL_PATHNAME, f'{path}.{file_type}')
    all_data = pd.read_csv(data_path, comment='#', index_col=[0])
    data_set = OrderedDict()
    for s in all_data.index:
        symbol = s.lower()
        if symbol not in data_set:
            df = all_data.loc[s, :].reset_index(drop=True).copy(deep=True)
            # We duplicate the first row, to have a nicer plot...
            df.loc[-1] = df.loc[0]
            df.index = df.index + 1
            df.sort_index(inplace=True)
            data_set[symbol] = df
    return data_set


def collect_data_for_alist_plot(raw_file_path, file_type='dat',
                                only_x_last_interactions=None,
                                start=None, end=None, series_numbers=None,
                                windows=None, average_mode='mean',
                                error_mode='stdev', percentiles=[5, 95]):
    data_set = read_raw_alist_data(raw_file_path, file_type)
    new_data_set = OrderedDict()

    for i, (symbol, data) in enumerate(data_set.items()):
        series_length = len(data)

        if type(windows) is list:
            data = apply_window_over_each_series(data, windows[i])
        if type(windows) is int:
            data = apply_window_over_each_series(data, windows)

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
            data = apply_average_across_series(data,
                                               average_mode,
                                               error_mode,
                                               percentiles)

        new_data_set[symbol] = data
    return new_data_set
