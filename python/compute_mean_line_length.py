__author__ = 'Russ Robbins'
import numpy as np
from sys import argv


def mean_line_length(data_file):
    """
    Computes the average length of a line in number of words for a data file.
    :param data_file: name of input file, string
    :return: mean: mean of line lengths, float
    """
    with open(data_file, mode='r',
              encoding='utf8') as f_in:
        line_lengths = np.array([])
        for line in f_in:
            line_lengths = np.append(line_lengths, len(line))
        mean = np.mean(line_lengths)
        return float(mean)

if __name__ == "__main__":
    script, file_in = argv
    mean_line_length(file_in)