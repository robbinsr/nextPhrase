__author__ = 'Russ'


def uncapitalize_adjectives(adjectives_in, data_file_in, data_file_out):
    """
    Takes any capitalized adjectives in adjectives_in and changes them to
    lower case.
    :param adjectives_in: name of file of capital, lowercase adjectives,
    string
    :param data_file_in: name of file where capitalized adjectives will be
    chanted to lower case
    :param data_file_out: name of new file with more lower case adjectives
    :return: None, function outputs file named data_file_out
    """
    adjectives = {}
    with open(adjectives_in) as f:
        for line in f:
            (key, val) = line.split(",")
            adjectives[key] = val

    with open(data_file_in, encoding='utf8') as f_in:
            with open(data_file_out, encoding='utf8', mode='w') as f_out:
                line_list = []
                for line in f_in:
                    line = line.split()
                    if len(line) == 0:
                        pass
                    else:
                        for word in line:
                            if word in adjectives:
                                temp = adjectives[word]
                                new_word = temp.strip()
                                line_list.append(new_word)
                            else:
                                line_list.append(word)
                        new_line = ' '.join(line_list)
                        f_out.write(str(new_line) + '\n')
                        line_list.clear()
    return None
