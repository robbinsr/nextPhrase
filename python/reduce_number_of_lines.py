__author__ = 'Russ Robbins'


def reduce_num_lines(filein, mean_length, keep, discard, log):
    """
    Creates a new file where the only lines are those that have lengths
    between 0 and the number that is twice the average line length. This
    effectively removes from the analysis any lines that have lengths that
    are outliers.
    Note: Mean is used exclusively because standard deviation would
    include outliers.
    :param filein: name of file which will have lines culled, string
    :param mean_length: this times 2 will be the clip for culling lines, float
    :param keep: name of file for lines to use for further cleaning
    :param discard: name of file for lines to know what was culled
    :param log: name of file for problems, if size zero all cases handled
    :return: None, output outputs files that have lines to keep, discard,
    and log if they were not handled.
    """

    with open(filein, mode='r',
              encoding='utf8') as f_in:
        with open(keep, mode='w',
                  encoding='utf8') as f_keep:
            with open(discard,
                      mode='w',
                      encoding='utf8') as f_discard:
                with open(log,
                          mode='w', encoding='utf8') as f_log:
                    clip = mean_length*2
                    for line in f_in:
                        line_length = len(line)
                        if line_length > clip:
                            f_discard.writelines(line)
                        elif line_length <= clip:
                            f_keep.writelines(line)
                        else:
                            f_log.writelines(line)
    return None
