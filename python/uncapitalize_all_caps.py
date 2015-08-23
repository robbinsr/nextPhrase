__author__ = 'Russ'


def uncapitalize_all_caps(data_file_in, data_file_out):
    """
    Takes any fully capitalized words changes them to
    lower case.
    :param data_file_in: name of input file, string
    :param data_file_out: name of output file, string
    """

    with open(data_file_in, mode='r', encoding='utf8') as f_in:
        with open(data_file_out, mode='w', encoding='utf8') as f_out:
            for line in f_in:
                new_line = ''
                line_list = line.split()
                new_line_list = []
                for word in line_list:
                    if word.isupper() and word != "I":
                        word = word.lower()
                    else:
                        word = word
                    new_line_list.append(word)
                    new_line = ' '.join(new_line_list) + '\n'
                f_out.writelines('{0:s}'.format(new_line))