__author__ = 'Russ'


def remove_numbers(data_file_in, data_file_out):
    """
    Takes any fully capitalized words changes them to
    lower case.
    :param data_file_in: name of input file, string
    :param data_file_out: name of output file, string
    """

    with open(data_file_in, mode='r', encoding='utf8') as f_in:
        with open(data_file_out, mode='w', encoding='utf8') as f_out:
            for line in f_in:
                if len(line) == 0:
                        pass
                else:
                    new_line = ''
                    new_line_list = []
                    for char in line:
                        if char.isdigit():
                            char = ""
                        else:
                            char = char
                        new_line_list.append(char)
                        new_line = ''.join(new_line_list)
                    f_out.writelines('{0:s}'.format(new_line))