from sys import argv

__author__ = 'Russ Robbins'
import re


def clarify_pause_punct(data_file_in, data_file_out):
    """
    :param data_file_in: file to add spaces around pause-oriented punctuation
    :param data_file_out: file with spaces added
    :return: None, function outputs file with spaces around pause-oriented
    punctuation to support word identification.
    """

    with open(data_file_in, encoding='utf8') as f_in:
        with open(data_file_out, "w+", encoding='utf8') as f_out:
            for line in f_in:
                new_line = re.sub(r'[,\.!?:;]', ' . ', line)
                f_out.writelines('{0:s}\n'.format(new_line))
    return None

if __name__ == "__main__":
    script, file_in, file_out = argv
    clarify_pause_punct(data_file_in=file_in, data_file_out=file_out)