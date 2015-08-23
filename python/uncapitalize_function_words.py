__author__ = 'Russ Robbins'


def uncapitalize_function_words(data_file_in, data_file_out):
    """
    :param data_file_in: file name for file that has capitalized function
    words, string
    :param data_file_out: file name for file to be created which has no
    capitalized function words, string
    :return: None, function outputs a file
    """
    function_words = {}
    with open("function_words.txt") as f:
        for line in f:
            (key, val) = line.split(",")
            function_words[key] = val

        with open(data_file_in, encoding='utf8') as f_in:
            with open(data_file_out, encoding='utf8', mode='w') as f_out:
                line_list = []
                for line in f_in:
                    line = line.split()
                    if len(line) == 0:
                        pass
                    else:
                        for word in line:
                            if word in function_words:
                                temp = function_words[word]
                                new_word = temp.strip()
                                line_list.append(new_word)
                            else:
                                line_list.append(word)
                        new_line = ' '.join(line_list)
                        f_out.write(str(new_line) + '\n')
                        line_list.clear()
    return None
