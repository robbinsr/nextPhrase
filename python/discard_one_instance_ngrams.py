__author__ = 'Russ'
import re


def discard_one_instance_ngrams(data_file_in, keep, discard, log):
    """
    Create a file that does not have ngrams that have had only one instance.
    :param data_file_in: name of input file, string
    :param keep: name of file for ngrams to keep, string
    :param discard: name of file for ngrams discarded, string
    :param log: name of file for ngrams not handled, string
    :return: None, function writes file
    """
    with open(data_file_in, mode='r', encoding='utf8') as f_in:
        with open(keep, mode='w', encoding='utf8') as f_keep:
            with open(discard, mode='w', encoding='utf8') as f_discard:
                with open(log, mode='w', encoding='utf8') as f_log:
                    for line in f_in:
                        match_count = re.search('([0-9]*)$', str(line))
                        if int(match_count.group()) == 1:
                            f_discard.writelines(line)
                        elif int(match_count.group()) != 1:
                            f_keep.writelines(line)
                        else:
                            f_log.writelines(line)

    return None