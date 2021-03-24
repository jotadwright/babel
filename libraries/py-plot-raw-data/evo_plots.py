#!/usr/bin/env python

import os
import argparse
import matplotlib.pyplot as plt

from matplotlib.figure import figaspect
from utils import GREAT_GNUPLOT_COLORS, BABEL_PATHNAME
from utils import pathname_name, pathname_directory
from utils import get_error_bar_distance
from plot_raw_evo_data import collect_data_for_evo_plot


parser = argparse.ArgumentParser()
parser.add_argument('--raw-file-paths', required=True, nargs='+',
                    help='''The paths to the raw data files,
                            relative to your Babel folder.''')
parser.add_argument('--title', default=None, type=str,
                    help='The title of the plot. Default: no title.')
parser.add_argument('--captions', default=[], nargs='*',
                    help='''Captions for the lines on the plot.
                            Default: use the filenames of the
                            raw data files.''')
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
                            Default: combine the filenames of
                            the raw data files.''')
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
parser.add_argument('--use-y-axis', default=None, type=int,
                    choices=[1, 2], nargs='*',
                    help='''Which y-axis to use for each line.
                            The left y-axis is denoted by 1.
                            The right y-axis is denoted by 2.''')
parser.add_argument('--y1-min', default=None, type=float,
                    help='Minimum value of the left y-axis.')
parser.add_argument('--y1-max', default=None, type=float,
                    help='Maximum value of the left y-axis.')
parser.add_argument('--y2-min', default=None, type=float,
                    help='Minimum value of the right y-axis.')
parser.add_argument('--y2-max', default=None, type=float,
                    help='Maximum value of the right y-axis.')
parser.add_argument('--draw-y1-grid', dest='draw_y1_grid', action='store_true',
                    help='Whether to draw the grid for the left y-axis.')
parser.add_argument('--hide-y1-grid', dest='draw_y1_grid',
                    action='store_false',
                    help='Whether to hide the grid for the left y-axis.')
parser.add_argument('--draw-y2-grid', dest='draw_y2_grid', action='store_true',
                    help='Whether to draw the grid for the right y-axis.')
parser.add_argument('--hide-y2-grid', dest='draw_y2_grid',
                    action='store_false',
                    help='Whether to hide the grid for the right y-axis.')
parser.add_argument('--grid-line-width', default=0.5, type=float,
                    help='Line width to use for drawing the grid.')
parser.add_argument('--x-label', default='Number of games played', type=str,
                    help='Label on the x-axis.')
parser.add_argument('--y1-label', default=None, type=str,
                    help='Label of the left y-axis.')
parser.add_argument('--y2-label', default=None, type=str,
                    help='Label on the right y-axis.')
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
parser.set_defaults(dashed=True, draw_y1_grid=True, open=True, export=True)


def raw_files_to_evo_plot(raw_file_paths=None,
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
                          use_y_axis=None,
                          y1_min=None, y1_max=None,
                          y2_min=None, y2_max=None,
                          draw_y1_grid=True, draw_y2_grid=False,
                          grid_line_width=0.5,
                          x_label='Number of games played',
                          y1_label=None, y2_label=None,
                          logscale=False,
                          open=True, export=True):
    """
    The main function for creating evo plots.

    :param raw_file_paths: A list of raw file paths,
        always relative to the Babel directory.
    :param title: A title for the plot. Default: no title.
    :param captions: A list of captions, one for each line on the plot.
        Default: use the filenames of the datafiles.
    :param average_windows: Either a list, specifying the averaging window
        to use for each series separately, or an integer, specifying the
        averaging window to use for all series. Default: 100.
    :param average_mode: Mode to use when averaging across series.
        Supported modes are 'mean' and 'median'. Default: 'mean'.
    :param error_bars: Determine the data that is presented by the error bars.
        Supported modes are 'stdev', 'min_max' and 'percentile'.
        Default: 'stdev'.
    :param percentiles: When using the 'percentile' error_bars, this parameter
        can be used to specify the quantiles used. Default: [5, 95].
    :param error_bar_modes: Mode to use for drawing the error bars. Supported
        modes are 'filled' and 'lines'. Default: 'filled'.
    :param plot_file_name: Name for the resulting graphic file.
        Default: combine the filenames of the datafiles.
    :param plot_directory: Directory to store the graphic file.
        Default: use the same directory as the raw datafiles.
    :param graphic_type: Filetype of the graphic file. Default: 'pdf'.
    :param file_type: Filetype of the raw datafiles. Default: 'csv'.
    :param only_x_last_interactions: Only plot the 'x' last interactions.
    :param start: Only plot the interactions after 'start'.
    :param end: Only plot the interactions before 'end'.
    :param series_numbers: Only plot the interactions from these series.
        Series are denoted starting from 1.
    :param key_location: Location of the legend on the plot. Default: 'best'.
    :param line_width: Width of the lines on the plots. Default: 2.
    :param dashed: Whether or not to use dashed lines. Default: True.
    :param colors: What colors to use for the lines.
        Default: the great gnuplot colors.
    :param font_size: Font size on the plot. Default: 10.
    :param use_y_axis: Which y-axis to use for each line on the plot. Should be
        a list of the same length as the number of raw file paths. The left
        axis is specified as 1. The right axis is specified as 2.
        Default: use y-axis 1 (left) for all lines.
    :param y1_min: Minimum value for the left y-axis.
    :param y1_max: Maximum value for the left y-axis.
    :param y2_min: Minimum value for the right y-axis.
    :param y2_max: Maximum value for the right y-axis.
    :param draw_y1_grid: Whether or not to draw a grid for the left y-axis.
        Default: True.
    :param draw_y2_grid: Whether or not to draw a grid for the right y-axis.
        Default: False.
    :param grid_line_width: Line width to use for drawing a grid. Default: 0.5.
    :param x_label: Label of the x-axis. Default: "Number of games played".
    :param y1_label: Label of the left y-axis. Default: no label.
    :param y2_label: Label of the right y-axis. Default: no label.
    :param logscale: Whether to use a logarithmic scale for the y-axes.
        Default: False.
    :param open: Whether to open the file after making it. Default: True.
    :param export: Whether to save the file after making it. Default: True.
    """
    if raw_file_paths is None:
        raise Exception('Please provide raw_file_paths!')

    if average_windows is None:
        average_windows = [None for path in raw_file_paths]
    if type(average_windows) is int:
        # Support for passing a single number to average_windows,
        # it is set to each raw_file_path
        average_windows = [average_windows for path in raw_file_paths]
    if type(average_windows) is list:
        if len(average_windows) != len(raw_file_paths):
            raise Exception('''Length of raw-file-names should
                               be equal to length of average-windows,
                               or it can be a single number.''')

    if plot_file_name is None:
        plot_file_name = '+'.join([pathname_name(p) for p in raw_file_paths])

    if plot_directory is None:
        plot_directory = pathname_directory(raw_file_paths[0])

    if use_y_axis is None:
        use_y_axis = [1 for path in raw_file_paths]

    cl = len(captions)
    captions = captions + [pathname_name(p) for p in raw_file_paths[cl:]]

    colors = colors + GREAT_GNUPLOT_COLORS[len(colors):]

    data_set = collect_data_for_evo_plot(
        raw_file_paths, file_type=file_type,
        only_x_last_interactions=only_x_last_interactions,
        start=start, end=end, series_numbers=series_numbers,
        windows=average_windows, average_mode=average_mode,
        error_mode=error_bars, percentiles=percentiles
    )

    w, h = figaspect(0.5625)
    fig, ax1 = plt.subplots(figsize=(w, h))

    if 2 in use_y_axis:
        ax2 = ax1.twinx()

    for i, (data, y_axis) in enumerate(zip(data_set, use_y_axis)):
        ax = ax1 if y_axis == 1 else ax2
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

    ax1.set_xmargin(0)
    if title:
        fig.suptitle(title)

    if y1_min is not None:
        ax1.set_ylim(bottom=y1_min)

    if y1_max is not None:
        ax1.set_ylim(top=y1_max)

    if draw_y1_grid:
        ax1.grid(True, axis='y', linewidth=grid_line_width)

    if x_label is not None:
        ax1.set_xlabel(x_label)

    if y1_label is not None:
        ax1.set_ylabel(y1_label)

    if logscale:
        ax1.set_yscale('log')

    if 2 in use_y_axis:
        ax2.set_xmargin(0)

        if y2_min is not None:
            ax2.set_ylim(bottom=y2_min)

        if y2_max is not None:
            ax2.set_ylim(top=y2_max)

        if draw_y2_grid:
            ax2.grid(True, axis='y', linewidth=grid_line_width)

        if y2_label is not None:
            ax2.set_ylabel(y2_label)

        if logscale:
            ax2.set_yscale('log')

    if 2 in use_y_axis:
        lines, labels = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax2.legend(lines + lines2, labels + labels2, loc=key_location)
    else:
        ax1.legend(loc=key_location)

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
    raw_files_to_evo_plot(**vars(args))
