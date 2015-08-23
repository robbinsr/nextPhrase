__author__ = 'Russ'


def remove_profanity(data_file_in, data_file_out):
    """
    Removes profanity.
    :param data_file_in: name of input data file, string
    :param data_file_out: name of output data file, string
    :return: None, function writes data file named data_file_out
    """
    profanity_dict = {
        "anal": "",
        "anus": "",
        "arse": "",
        "arselicker": "",
        "ass": "",
        "ass-master": "",
        "assmaster": "",
        "ass-kisser": "",
        "asskisser": "",
        "ass-nugget": "",
        "ass-wipe": "",
        "asshole": "",
        "ass-hole": "",
        "balls": "",
        "ballsack": "",
        "bastard": "",
        "bitch": "",
        "bloody": "",
        "blowjob": "",
        "boner": "",
        "bum": "",
        "bum-fucker": "",
        "butt": "",
        "buttfucker": "",
        "butt-fucker": "",
        "child-fucker": "",
        "clitoris": "",
        "cock": "",
        "cock-master": "",
        "cockmaster": "",
        "cockboy": "",
        "cock-boy": "",
        "cockfucker": "",
        "cock-fucker": "",
        "cunt": "",
        "cunt-sucker": "",
        "cuntsucker": "",
        "dick": "",
        "dickhead": "",
        "dick-head": "",
        "dildo": "",
        "dogshit": "",
        "dog-shit": "",
        "dyke": "",
        "fag": "",
        "fuck": "",
        "fucker": "",
        "goddamn": "",
        "god-damn": "",
        "godsdamn": "",
        "gods-damn": "",
        "jack-ass": "",
        "jackass": "",
        "jizz": "",
        "labia": "",
        "motherfucker": "",
        "mother-fucker": "",
        "muff": "",
        "nigga": "",
        "nigger": "",
        "penis": "",
        "piss": "",
        "pornofreak": "",
        "pussy": "",
        "queer": "",
        "scrotum": "",
        "sex": "",
        "shit": "",
        "shit-eater": "",
        "shiteater": "",
        "shithead": "",
        "shit-head": "",
        "slut": "",
        "smegma": "",
        "spunk": "",
        "twat": "",
        "vagina": "",
        "wank": "",
        "whore": ""

    }

    with open(data_file_in, encoding='utf8') as f_in:
        with open(data_file_out, "w+", encoding='utf8') as f_out:
            for line in f_in:
                line_list = []
                for word in line.split():
                    if word in profanity_dict:
                        new_word = profanity_dict[word]
                        new_word = new_word.strip()
                    else:
                        new_word = word
                    line_list.append(new_word)
                new_line = ' '.join(line_list)
                new_line = str(new_line + '\n')
                for item in new_line:
                    f_out.writelines("%s" % item)

    return None