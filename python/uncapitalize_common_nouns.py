__author__ = 'Russ Robbins'


def uncapitalize_common_nouns(common_nouns_in, data_file_in, data_file_out):
    """
    Takes any capitalized common_nouns in common_nouns_in and changes them to
    lower case.
    :param common_nouns_in: name of file of capital, lowercase common_nouns,
    string
    :param data_file_in: name of file where capitalized common_nouns will be
    chanted to lower case
    :param data_file_out: name of new file with more lower case common_nouns
    :return: None, function outputs file named data_file_out
    """
    common_nouns = {}
    with open(common_nouns_in) as f:
        for line in f:
            (key, val) = line.split(",")
            common_nouns[key] = val

    with open(data_file_in, encoding='utf8') as f_in:
            with open(data_file_out, encoding='utf8', mode='w') as f_out:
                line_list = []
                for line in f_in:
                    line = line.split()
                    if len(line) == 0:
                        pass
                    else:
                        for word in line:
                            if word in common_nouns:
                                temp = common_nouns[word]
                                new_word = temp.strip()
                                line_list.append(new_word)
                            else:
                                line_list.append(word)
                        new_line = ' '.join(line_list)
                        f_out.write(str(new_line) + '\n')
                        line_list.clear()
    return None