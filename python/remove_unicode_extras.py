from sys import argv

__author__ = 'Russ Robbins'
from unicodedata import normalize


def remove_unicode_extras(data_file_in, data_file_out):
    """
    :param data_file_in: file to remove unicode_additions from
    :param data_file_out: file with spaces added
    :return: None, function outputs file
    """

    with open(data_file_in, encoding='utf8') as f_in:
        with open(data_file_out, "w+", encoding='utf8') as f_out:
            for line in f_in:
                temp = normalize('NFD', line)
                new_line = temp.encode('ascii', 'ignore').decode('ascii')
                f_out.writelines('{0:s}\n'.format(new_line))
    return None

if __name__ == "__main__":
    script, file_in, file_out = argv
    remove_unicode_additions(data_file_in=file_in, data_file_out=file_out)