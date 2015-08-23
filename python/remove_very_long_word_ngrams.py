__author__ = 'Russ'

import re


def remove_very_long_words(data_file_in, data_file_keep, data_file_discard,
                           data_file_log):
    """
    Remove any ngram with a word over 18 characters
    :param data_file_in: input file, string
    :param data_file_keep: output file, string
    :param data_file_discard: output file indicating what not to use, string
    :param data_file_log: output file for cases not handled
    :return: None, function writes file
    """
    with open(data_file_in, mode='r', encoding='utf8') as f_in:
        with open(data_file_keep, mode='w', encoding='utf8') as f_keep:
            with open(data_file_discard, mode='w',
                      encoding='utf8') as f_discard:
                with open(data_file_log, mode='w', encoding='utf8') as f_log:
                    for line in f_in:
                        if re.findall(',([a-zA-z]{18,100}),', line):
                            f_discard.writelines('{0:s}'.format(line))
                        elif re.findall('^([a-zA-z]{18,100}),', line) and not \
                                re.findall(',([a-zA-z]{18,100}),', line):
                            f_discard.writelines('{0:s}'.format(line))
                        elif not re.findall(',([a-zA-z]{18,100}),', line) and\
                                not re.findall('^([a-zA-z]{18,100}),', line):
                            f_keep.writelines('{0:s}'.format(line))
                        else:
                            f_log.writelines('{0:s}'.format(line))
