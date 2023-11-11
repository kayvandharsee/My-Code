

def is_valid_text(text, ch_min, ch_max):
    '''
    Returns True if the ASCII code for each character in the variable text is
    greater than or equal to ch_min and less than or equal to ch_max. When
    ch_min and ch_max are whole numbers, it will ignore the negative
    sign in text at the beginning
    
    Parameters:
        text(str): A string
        ch_min(str): A string that will represent the minimal value
        ch_max(str): A string that will represent the maximum value
    Returns:
        (bool): True or False depending on the value of the characters in text
    
    Examples:
        >>>is_valid_text("abcndf", "a", "z")
        True
        >>>is_valid_text("abcndf", "A", "Z")
        False
        >>> is_valid_text("-324", "1", "9")
        True
    '''
# This variable will help if ignoring the negative is needed
    start = 0
    
# This if statement will ignore the negative if needed
    if (ch_min.isdecimal() and ch_max.isdecimal() and text[0] == "-"):
        
        start += 1

# This for loop will traverse the characters in text to return the correct bool
    for i in range(start, len(text)):
        
        if (text[i] < ch_min or text[i] > ch_max):
            
            return False
    
    return True

def direction_check(value):
    '''
    This function makes sure a proper direction value is given by the user
    
    Parameters:
        value: The value given by the user
    Returns:
        int: -1 for leftwards, and 1 for rightwards
        
    Examples:
        >>> direction_check("abc")
        Please type -1 for right to left or 1 for left to right: 1
        1
        >>> direction_check(123)
        Please type -1 for right to left or 1 for left to right: 2
        Please type -1 for right to left or 1 for left to right: -1
        -1
        >>> direction_check(1)
        1
    '''
    
    while (value != 1 and value != -1):
        
        value = input("Please type -1 for right to left or 1 for left "\
                          "to right: ")
        
        if is_valid_text(value, "0", "9"):
            
            value = int(value)
            
    return value
            
def first_index_check(index, letter_list):
    '''
    This function will make sure the index representing the first letter of a
    word is inside the list of letters
    
    Parameters:
        index(int): The index for the first letter
        indices(int): The amount of positive indices in the list
        (zero included)
    Returns:
        index(int): The index of the first letter inside the list
        
    Examples:
        >>> first_index_check(4, ['A', 'B', 'C', 'D'])
        Please give an index within the list's boundaries (positive indices
        only): 3
        3
        >>> first_index_check(-4, ['A', 'B', 'C', 'D'])
        Please give an index within the list's boundaries (positive indices
        only): 3
        3
        >>> first_index_check("s", ['A', 'B', 'C', 'D'])
        Please give an index within the list's boundaries (positive indices
        only): three
        Please give an index within the lists boundaries (positive indices
        only): two
        Please give an index within the lists boundaries (positive indices
        only): 1
        1
    '''
# This loop will check if the index is an integer, and if it is in the list
    while is_outside_list(letter_list, index):
        
        index = input("Please give an index within the list's boundaries " \
        "(positive indices only): ")

# This nested loop will avoid a type error if the user doesnt give an int
        while not is_valid_text(str(index), "0", "9"):
            
            index = input("Please give an index within the list's " \
            "boundaries (positive indices only): ")
            
        index = int(index)
    
    return index

def last_index_check(index, indices):
    '''
    This function will check if the index representing the last letter of a
    word is inside the list of letters
    
    Parameters:
        index(int): The index for the last letter
        indices(int): The amount of positive indices in the list
        (zero included)
    Returns:
        bool: True if the index is in the list, and False for any other
        outcome
        
    Examples:
        >>> last_index_check('g', 4)
        False
        >>> last_index_check(2, 4)
        True
        >>> last_index_check(-2, 3)
        False
    '''
# This block will check if the index is an integer, and if it is in the list
    if((not is_valid_text(str(index), "0", "9")) or
          index < 0 or index > (indices - 1)):
        
        return False
    
    return True

def integer_check(value):
    '''
    This function checks if the value given is an int and keeps asking the
    user for a proper integer until it is given
    
    Parameters:
        value: The value that is being checked
    Returns:
        value(int): The proper integer value selected by the user
        
    Examples:
        >>> integer_check(123)
        123
        >>> integer_check("123")
        Please provide a proper integer value: 32
        32
        >>> integer_check(54.4)
        Please provide a proper integer value: -32
        -32
    '''
# This while loop will make sure that value is an int
    while type(value) != int:
        
        value = input("Please provide a proper integer value: ")
        
        if is_valid_text(value, "0", "9"):
            
            value = int(value)
    
    return value

def capital_word_check(value):
    '''
    This function checks to see if the value given is a word in all caps,
    and keeps asking the user for such a value until it is given
    
    Parameters:
        value: The value in question
    Returns:
        value(str): The proper word in all caps selected by the user
    
    Examples:
        >>> capital_word_check("CBd")
        Please provide a proper fully uppercase word: CBD
        'CBD'
        >>> capital_word_check("BDC")
        'BDC'
        >>> capital_word_check(123)
        Please provide a proper fully uppercase word: "ABC"
        Please provide a proper fully uppercase word: ABC
        'ABC'
    '''
# This loop will make sure the value ends up being an all caps word  
    while not (type(value) == str and is_valid_text(value, "A", "Z")):
        
        value = input("Please provide a proper fully uppercase word: ")
    
    return value
    
def is_outside_list(letter_list, integer):
    '''
    This fuction returns True or False depending whether the variable integer
    is out of bounds regarding the length of the list. Negative integers are
    out of bounds
    
    Parameters:
        letter_list(list): A list of uppercase characters
        integer(int): An index number
    Returns:
        bool: a boolean variable indicating if the given index corresponds to a
        position outside the list of letters or not
    
    Examples:
        >>> is_outside_list(['A', 'B', 'C', 'D'], -4)
        True
        >>> is_outside_list(['A', 'B', 'C', 'D'], 3)
        False
        >>> is_outside_list(['A', 'B', 'C', 'D'], 4)
        True
    '''
    
# This return statement will return the correct bool using boolean variables
    return(integer > (len(letter_list) - 1) or integer < 0)

def letter_positions(letter_list, character):
    '''
    The function returns all the positive indices where the character is
    found. If the character is not found in the list, then the function
    returns an empty list
    
    Parameters:
        letter_list(list): A list of characters
        character(str): An uppercase character to search for in the list
    Returns:
        position_list(list): A list of indices where the character has been
        found in the list
    
    Examples:
        >>> letter_positions(['A', 'B', 'C', 'D'], "D")
        [3]
        >>> letter_positions(['C', 'B', 'C', 'D'], "C")
        [0, 2]
        >>> letter_positions(['A', 'B', 'C', 'S'], Z)
        []
    '''
# This variable will be appended with the correct indices and returned
    position_list = []
    
# This loop will traverse the entire list for correct indices and append them
    for index in range(len(letter_list)):
        
        if letter_list[index] == character:
            
            position_list.append(index)
    
    return position_list

def valid_word_pos_direction(letter_list, word, index, direction):
    '''
    This function will check if there is a word in the letter list starting
    from index and going in the direction given
    
    Parameters:
        letter_list(list): A list of letters
        word(str): The word in the list of letters in all caps
        index(int): The integer where the first letter of the word is located
        direction(int): Either 1 for left to right, or -1 for right to left
    Returns:
        bool: A boolean stating whether the word is located at that index and
        direction
    
    Examples:
        >>> valid_word_pos_direction(['A', 'K', 'C', 'D'], "AKC", 0, 1)
        True
        >>> valid_word_pos_direction(['Y', 'L', 'F', 'D'], "FLY", 2, -1)
        True
        >>> valid_word_pos_direction(['Y', 'K', 'G', 'D'], "FLY", 3, 1)
        False
    '''
# This variable will store the letters taken from the letter list
    word_found = ''
    
# This for loop will traverse the list only for the indexes needed
    for i in range(index, index + len(word) * direction,
    direction):
        
        if is_outside_list(letter_list, i):
        
            return False
        
        word_found = word_found + letter_list[i]
 
    return (word_found == word)
    
def direction_word_given_position(letter_list, word, index):
    '''
    This function calls valid_word_pos_direction to return the directions in
    which the word is found. An empty list is returned if the word doesn't
    begin at the given index
    
    Parameters:
        letter_list(list): A list of letters
        word(str): The word being searched for
        index(int): The positive index of the first letter of the word in
        letter_list
    Returns:
        list_directions(list): A list of the directions in which the word
        is present
    
    Examples:
        >>> direction_word_given_position(['A', 'B', 'C', 'D', 'C', 'M'],
        "BCD", 1)
        [1]
        >>> direction_word_given_position(['A', 'B', 'C', 'D', 'C', 'M'],
        "DC", 3)
        [-1, 1]
        >>> direction_word_given_position(['A', 'B', 'C', 'D', 'C', 'M'],
        "ABC", 5)
        []
    '''
# This variable will store the directions where the word is
    list_directions = []

# This for loop will find all the direction where the word is
    for direction in range(-1, 3, 2):
        
        if valid_word_pos_direction(letter_list, word, index, direction):
            
            list_directions.append(direction)
            
    return list_directions

def position_direction_word(letter_list, word):
    '''
    This function uses functions "letter_positions" and "direction_word_given_
    position" to store the indices and their directions into a dictionary
    which is returned
    
    Parameters:
        letter_list(list): A list of letters
        word(str): The word being searched for
    Returns:
        dict_position_direction(dict): A dictionary of the indices of the
        first letter of the word in the list, and the direction the word
        appears in
    
    Examples:
        >>> position_direction_word(['A', 'B', 'C', 'D', 'C', 'M'], "BA")
        {1: [-1]}
        >>> position_direction_word(['A', 'B', 'C', 'D', 'C', 'M'], "MAB")
        {5: []}
        >>> position_direction_word(['A', 'B', 'C', 'D', 'C', 'M'], "CBA")
        {2: [-1], 4: []}
    '''
# This variable will store the indices and their directions
    dict_position_direction = {}
    
# This variable stores all the indices of the first letter of the word
    index_list = letter_positions(letter_list, word[0])

# This loop finds all the directions for the indices, and adds them to
# dict_position_direction
    for i in range(len(index_list)):
        
        directions_list = direction_word_given_position(letter_list, word,
        index_list[i])
    
        dict_position_direction[index_list[i]] = directions_list
    
    return dict_position_direction

def cross_word_position_direction(letter_list, word, index, direction):
    '''
    This function crosses out the given word inside the given list of letters
    
    Parameters:
        letter_list(list): A list of letters
        word(str): The word being searched for
        index(int): The positive index of the first letter of the word in
        letter_list
        direction(int): -1 for right to left, 1 for left to right
    Returns:
    
    Examples:
        >>> cross_word_position_direction(['A', 'B', 'C', 'D', 'C', 'M'],
        "AB", 0, 1)
        >>> cross_word_position_direction(['A', 'B', 'C', 'D', 'C', 'M'],
        "BCDC", 1, 1)
        >>> cross_word_position_direction(['A', '*', '*', 'D', 'C', 'M'],
        "MCD", 5, -1)
    '''
# This loop will traverse the indices of the word, and cross out the letters  
    for i in range(index, index + len(word) * direction, direction):
        
        letter_list[i] = "*"

def cross_word_all_position_direction(letter_list, word,
dict_position_direction):
    '''
    This function takes a word, a dictionary of its indices and their
    direction values, and calls function "cross_word_position_direction" to
    cross out the word in the letter_list
    
    Parameters:
        letter_list(list): A list of letters
        word(str): The word being searched for
        dict_position_direction(dict): A dictionary of indices and their
        directional values
    Returns:

    Examples:
        >>> cross_word_all_position_direction(['A', 'B', 'C', 'D', 'C',
        'B', 'A'], "ABC",{0:[1], 6:[-1]})
        >>> cross_word_all_position_direction(['F', 'B', 'B', 'A', 'B',
        'B', 'A'], "ABB",{3:[-1], 6:[-1]})
        >>> cross_word_all_position_direction(['F', 'H', 'I', 'A', 'B',
        'B', 'A'], "HI",{1:[1]})
    '''

# This loop accounts for all the indexes in the dictionary
    for index in dict_position_direction:

# This loop accounts for all directions of said indices
        for direction in dict_position_direction[index]:

# This section crosses out the letters of the word in letter_list
            cross_word_position_direction(letter_list, word, index,
            direction)

def find_magic_word(letter_list):
    '''
    This function goes through a list and combines all the letters that are
    not crossed out, then combines them into a string to create a magic word
    which is returned
    
    Parameters:
        letter_list(list): A list of letters
    Returns:
        str: The magic word
    
    Examples:
        >>> find_magic_word(['A', '*', '*', 'D', 'C', 'M'])
        'ADCM'
        >>> find_magic_word(['*', '*', '*', '*', '*', '*'])
        ''
        >>> find_magic_word(['H', '*', 'E', 'L', 'L', '*', 'O'])
        'HELLO'
    '''

# This variable creates a list to store the remaining letters
    final_list = []

# This for loop adds all the remaining letters to final_list
    for i in range(len(letter_list)):
        
        if letter_list[i] != "*":
            
            final_list.append(letter_list[i])

# This section converts the list to a string and returns the magic word 
    return (''.join(final_list))

def word_search(letter_list, word_list):
    '''
    This function calls functions "position_direction_word" and "cross_word_
    all_position_direction" to find and cross out the words given, and then
    combines the remaining letters to form the magic word which is returned
    
    Parameters:
        letter_list(list): A list of letters
        word_list(list): A list of words
    Returns:
        str: The magic word
    
    Examples:
        >>> word_search(['C','W','I','K','I','P','E','D','I','A','O','M','R',
        'E','G','N','A','D','P'], ["WIKIPEDIA", "Danger"])
        Please provide a proper fully uppercase word: DANGER
        'COMP'
        >>> word_search(['S','S','M','O','O','T','H','I','E','N','A','K','R',
        'E','G','N','A','D','E'], ["SMOOTHIE", "DANGER"])
        'SNAKE'
        >>> word_search(['S','S','M','O','U','T','H','I','E','N','A','K','R',
        'E','G','M','A','D','E'], ["SMOOTHIE", "DANGER"])
        'SSMOUTHIENAKREGMADE'
    '''

    for i in range(len(word_list)):

# This section finds the position and direction of the word
        position_direction_dict = position_direction_word(letter_list,
        word_list[i])

# This section crosses out the words
        cross_word_all_position_direction(letter_list, word_list[i],
        position_direction_dict)

# This section finds and returns the magic word      
    return find_magic_word(letter_list)
        
def word_search_main(filepath):
    '''
    This function reads the given file, collects a list of letters and words
    from the file, and calls the function "word_search" to find the magic
    word in the list of letters which is returned
    
    Parameters:
        filepath(str): A string containing the path of a text file
    Returns:
        str: The magic word
    
    Examples:
        >>> word_search_main("word_search_file1.txt")
        'KAYVAN'
        >>> word_search_main("word_search_file2.txt")
        'METERS'
        >>> word_search_main("word_search_file3.txt")
        'SOCCER'
    '''
    
    fobj = open(filepath, "r")

# These lines create lists and a variable to store data and identify the first
# line
    first_line = 0
    
    letter_list = []
    
    word_list = []

# This loop will traverse the lines, and create letter_list and word_list
    for line in fobj:

# This section will capitalize and remove any "/n" in the lines
        line = line.upper()
        
        line = line.strip("\n")

# The if statement only applies to the first line
        if first_line == 0:
            
            for i in range(len(line)):
                
                letter_list.append(line[i])
            
            first_line += 1
        
        else:
            
            word_list.append(line)
    
    return word_search(letter_list, word_list)

