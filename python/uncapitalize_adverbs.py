__author__ = 'Russ Robbins'


def uncapitalize_adverbs(adverbs_in, data_file_in, data_file_out):
    """
    Takes any capitalized adverbs in adverbs_in and changes them to
    lower case.
    :param adverbs_in: name of file of capital, lowercase adverbs,
    string
    :param data_file_in: name of file where capitalized adverbs will be
    chanted to lower case
    :param data_file_out: name of new file with more lower case adverbs
    :return: None, function outputs file named data_file_out
    """
    adverbs = {}
    with open(adverbs_in) as f:
        for line in f:
            (key, val) = line.split(",")
            adverbs[key] = val

        with open(data_file_in, encoding='utf8') as f_in:
            with open(data_file_out, encoding='utf8', mode='w') as f_out:
                line_list = []
                for line in f_in:
                    line = line.split()
                    if len(line) == 0:
                        pass
                    else:
                        for word in line:
                            if word in adverbs:
                                temp = adverbs[word]
                                new_word = temp.strip()
                                line_list.append(new_word)
                            else:
                                line_list.append(word)
                        new_line = ' '.join(line_list)
                        f_out.write(str(new_line) + '\n')
                        line_list.clear()
    return None
