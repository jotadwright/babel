#!/usr/bin/env python

import os
import argparse
import matplotlib.pyplot as plt

from matplotlib.figure import figaspect
from utils import GREAT_GNUPLOT_COLORS, BABEL_PATHNAME
from utils import pathname_name, pathname_directory
from utils import get_error_bar_distance
from plot_raw_alist_data import collect_data_for_alist_plot


parser = argparse.ArgumentParser()
parser.add_argument('--raw-file-path', required=True, type=str,
                    help='''The path to the raw data files,
                            relative to your Babel folder.''')
parser.add_argument('--title', default=None, type=str,
                    help='The title of the plot. Default: no title.')
parser.add_argument('--captions', default=[], nargs='*',
                    help='''Captions for the lines on the plot.
                            Default: use the symbols stored in the alist.''')
parser.add_argument('--average-windows', default=100, nargs='*',
                    help='''The size of the windows used for the rolling mean.
                            Either specify a single number used for all lines,
                            or specify a separate number per line or specify
                            None if no averaging needs to be performed.''')
parser.add_argument('--average-mode', default='mean', type=str,
                    choices=['mean', 'median'],
                    help='Mode used to compute the average across the series.')
parser.add_argument('--error-bars', default='stdev', type=str,
                    choices=['stdev', 'min_max', 'percentile'],
                    help='Mode used to compute error bars across the series.')
parser.add_argument('--percentiles', default=[5, 95], type=int, nargs=2,
                    help='''When using the percentile error bars,
                            use this argument to specify the quantiles used for
                            the lower- and higher-end, respectively.''')
parser.add_argument('--error-bar-modes', default='filled', type=str,
                    choices=['filled', 'lines'],
                    help='Mode used to draw the error bars.')
parser.add_argument('--plot-file-name', default=None, type=str,
                    help='''Filename of the graphic file.
                            Default: combine the symbols in the alist''')
parser.add_argument('--plot-directory', default=None, type=str,
                    help='''Directory of the graphic file.
                            Default: the same directory as the
                            first raw data file.''')
parser.add_argument('--graphic-type', default='pdf', type=str,
                    choices=['pdf', 'png', 'svg', 'ps'],
                    help='Type of the graphic file.')
parser.add_argument('--file-type', default='csv', type=str,
                    help='Type of the raw data files.')
parser.add_argument('--only-x-last-interactions', default=None, type=int,
                    help='Only plot the x last interactions.')
parser.add_argument('--start', default=None, type=int,
                    help='Only plot the interactions after start.')
parser.add_argument('--end', default=None, type=int,
                    help='Only plot the interactions before end.')
parser.add_argument('--series-numbers', default=None, type=int, nargs='*',
                    help='Which series to plot. Series start from 1.')
parser.add_argument('--key-location', default='best', type=str,
                    choices=['best', 'upper right', 'upper left',
                             'lower right', 'lower left',
                             'center left', 'center right',
                             'lower center', 'upper center', 'center'],
                    help='Location of the legend on the plot.')
parser.add_argument('--line-width', default=2, type=int,
                    help='Width of the lines on the plot.')
parser.add_argument('--dashed', dest='dashed', action='store_true',
                    help='Whether to use dashed lines.')
parser.add_argument('--solid', dest='dashed', action='store_false',
                    help='Whether not to use dashed lines')
parser.add_argument('--colors', default=GREAT_GNUPLOT_COLORS,
                    type=str, nargs='*',
                    help='Colors to use for plotting the lines.')
parser.add_argument('--font-size', default=10, type=int,
                    help='Font size on the plot.')
parser.add_argument('--y-min', default=None, type=float,
                    help='Minimum value of the left y-axis.')
parser.add_argument('--y-max', default=None, type=float,
                    help='Maximum value of the left y-axis.')
parser.add_argument('--draw-y-grid', dest='draw_y_grid', action='store_true',
                    help='Whether to draw the grid for the left y-axis.')
parser.add_argument('--hide-y-grid', dest='draw_y_grid',
                    action='store_false',
                    help='Whether to hide the grid for the left y-axis.')
parser.add_argument('--grid-line-width', default=0.5, type=float,
                    help='Line width to use for drawing the grid.')
parser.add_argument('--x-label', default='Number of games played', type=str,
                    help='Label on the x-axis.')
parser.add_argument('--y-label', default=None, type=str,
                    help='Label of the left y-axis.')
parser.add_argument('--logscale', action='store_true',
                    help='Whether to use a logarithmic scale for the y-axes.')
parser.add_argument('--open', dest='open', action='store_true',
                    help='Whether to open the file.')
parser.add_argument('--not-open', dest='open', action='store_false',
                    help='Whether not to open the file.')
parser.add_argument('--export', dest='export', action='store_true',
                    help='Whether to export the file.')
parser.add_argument('--not-export', dest='export', action='store_false',
                    help='Whether not to export the file.')
parser.set_defaults(dashed=True, draw_y_grid=True, open=True, export=True)


def raw_file_to_alist_plot(raw_file_path=None,
                           title=None, captions=[],
                           average_windows=100, average_mode='mean',
                           error_bars='stdev', percentiles=[5, 95],
                           error_bar_modes='filled',
                           plot_file_name=None, plot_directory=None,
                           graphic_type='pdf', file_type='csv',
                           only_x_last_interactions=None,
                           start=None, end=None,
                           series_numbers=None,
                           key_location='best',
                           line_width=2,
                           dashed=True,
                           colors=GREAT_GNUPLOT_COLORS,
                           font_size=10,
                           y_min=None, y_max=None,
                           draw_y_grid=True,
                           grid_line_width=0.5,
                           x_label='Number of games played',
                           y_label=None,
                           logscale=False,
                           open=True, export=True):
    if raw_file_path is None:
        raise Exception('Please provide raw_file_paths!')

    data_set = collect_data_for_alist_plot(
        raw_file_path, file_type=file_type,
        only_x_last_interactions=only_x_last_interactions,
        start=start, end=end, series_numbers=series_numbers,
        windows=average_windows, average_mode=average_mode,
        error_mode=error_bars, percentiles=percentiles
    )

    if plot_file_name is None:
        plot_file_name = pathname_name(raw_file_path)

    if plot_directory is None:
        plot_directory = pathname_directory(raw_file_path)

    cl = len(captions)
    captions = captions + list(data_set.keys())[cl:]

    colors = colors + GREAT_GNUPLOT_COLORS[len(colors):]

    w, h = figaspect(0.5625)
    fig, ax = plt.subplots(figsize=(w, h))

    for i, (symbol, data) in enumerate(data_set.items()):
        plot_kwargs = {
            'linestyle': 'dashed' if dashed else 'solid',
            'color': colors[i],
            'label': captions[i],
            'linewidth': line_width
        }

        if len(data.columns) == 1:
            ax.plot(data, **plot_kwargs)

        else:
            if error_bar_modes == 'lines':
                error_steps = get_error_bar_distance(len(data))
                selected_indexes = data.index.isin(data[::error_steps].index)
                data.loc[~selected_indexes, 'error_low'] = 0.0
                data.loc[~selected_indexes, 'error_high'] = 0.0

                plot_kwargs.update({
                    'yerr': data.loc[:, ['error_low', 'error_high']].T.to_numpy(),
                    'ecolor': colors[i]
                })
                ax.errorbar(list(range(len(data))),
                            data['average'],
                            **plot_kwargs)

            elif error_bar_modes == 'filled':
                ax.plot(data['average'], **plot_kwargs)
                low_end = data['average'] - data['error_low']
                high_end = data['average'] + data['error_high']
                ax.fill_between(list(range(len(data))),
                                y1=low_end, y2=high_end,
                                alpha=0.25, linewidth=0,
                                color=colors[i])

            else:
                raise Exception(f"""Unknown 'error_bar_modes'
                                    {error_bar_modes}. Supported modes are
                                    'lines' and 'filled'""")

    ax.set_xmargin(0)
    if title:
        fig.suptitle(title)

    if y_min is not None:
        ax.set_ylim(bottom=y_min)

    if y_max is not None:
        ax.set_ylim(top=y_max)

    if draw_y_grid:
        ax.grid(True, axis='y', linewidth=grid_line_width)

    if x_label is not None:
        ax.set_xlabel(x_label)

    if y_label is not None:
        ax.set_ylabel(y_label)

    if logscale:
        ax.set_yscale('log')

    ax.legend(loc=key_location)

    if open:
        plt.show()

    if export:
        output_path = os.path.join(BABEL_PATHNAME,
                                   plot_directory,
                                   f'{plot_file_name}.{graphic_type}')
        fig.savefig(output_path, bbox_inches='tight')


if __name__ == '__main__':
    args = parser.parse_args()
    if len(args.average_windows) == 1:
        if args.average_windows[0] == 'None':
            args.average_windows = None
        else:
            args.average_windows = int(args.average_windows[0])
    raw_file_to_alist_plot(**vars(args))
