__author__ = 'Russ Robbins'

from sys import argv

__author__ = 'Russ Robbins'
import re


def replace_spaces_with_commas(data_file_in, data_file_out):
    """
    Replaces spaces with commas for each line.
    :param data_file_in: name of input file, string
    :param data_file_out: name of output file, string
    :return: None, function outputs file
    """

    with open(data_file_in, encoding='utf8') as f_in:
        with open(data_file_out, "w+", encoding='utf8') as f_out:
            for line in f_in:
                new_line = ''.join(line)
                new_line = re.sub(' ', ',', new_line)
                f_out.writelines('{0:s}'.format(new_line))

    return None


if __name__ == "__main__":
    script, file_in, file_out = argv
    replace_spaces_with_commas(data_file_in=file_in, data_file_out=file_out)
