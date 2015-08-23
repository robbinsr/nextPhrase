__author__ = 'Russ Robbins'
import re


def remove_non_pause_punct(data_file_in, data_file_out):
    """
    :param data_file_in: file to remove certain punctuation
    :param data_file_out: file with certain punctuation removed
    :return: None, file outputs file without punctuation below
    """
    with open(data_file_in, encoding='utf8') as f_in:
        with open(data_file_out, "w+", encoding='utf8') as f_out:
            for line in f_in:
                temp = re.sub('[\',\,*,+,\-,\\\\,=,#,$,%,^,&]', "", line)
                temp2 = re.sub('[\(,\),\[,\],\{,\},|,~,"`",_]','',temp)
                new_line = re.sub(r'[>,<,/,@]','',temp2)
                f_out.writelines('{0:s}'.format(new_line))

    return None
