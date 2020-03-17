# DATA 620 Assignment 12
# Written by Kenneth Lulie
# Last Updated April 21, 2018

#coded using PyCharm

# Word frequency analysis on Tesla Motors Letters to Shareholders.

#Will be looking at 1Q12, 1Q15 and 4Q17 tesla letter to shareholders
#I have downloaded them from the telsa website http://ir.tesla.com/downloads.cfm?view=all
#Then i copied the text from the pdf file into 3 note pads, one for each quarter



#If I decide for some reason down the line that I need to write it directly into excel, i can start from the below research
#https://stackoverflow.com/questions/11766833/export-a-python-list-to-excel
#However, at this point in time I can't justify the time it would take to adapt for this project.


#Used Starter code below.  Adjusted for my needs on April 21, 2018 for assignment 12, using TESLA.

    # Starter Code -
    # Based on Toby Donaldson's Python: Visual QuickStart Guide
    # function print_file_stats (location 5347)
    #
    # Modified for UMUC DATA 620 by Paula Colliver and Nivedita Bijlani
    # Last updated:  November 24, 2016
    # Program to open a text file
    # This program will remove all words in stopwords dictionary and give a word count of the top 30 words used.



#Will run 3 sections, one for each file
#Copied and pasted results out of console and into excel
#Used text to columns to seperate, and added Quarter, and Labels for Columns.

#Again, I think i could get this to write directly into excel using the Ironworks package
#I would probably do a for loop that would increment, giving me the rank numbering, while also giving instructions on what to write
#I would then hand code in the labels
#and i would hand code in the year/quarter, ie Cells from 4,2 to 4, 32 are 1Q15



#
#
# Section 1 for 1Q12
#
#

fileOpen = open('1Q12.txt', 'r').read().split()

# Open the stopwords file. This contains common words that we want to exclude when compiling our top 30. We can easily add more specific stop words to this file
stopwords = open('stopwords_sample_en.txt', 'r').read()

# Convert text to all lowercase and remove word if in stopwords
lowerWords = [word.lower() for word in fileOpen]  # convert to lower case

filteredFile = [word for word in lowerWords if word not in stopwords]  # remove word if in stopwords
filteredFile = ' '.join(filteredFile)
# count characters
num_chars = len(filteredFile)

# count lines
num_lines = filteredFile.count('\n')


#Manually added in more stop words to file
words = filteredFile.split()  # split file into words
d = {}  # create 'd' dictionary
for w in words:  # look through file and count word
    if w in d:  # seen w before?
        d[w] += 1
    else:
        d[w] = 1

num_words = sum(d[w] for w in d)

lst = [(d[w], w) for w in d]
lst.sort()
lst.reverse()

print('Your input file has characters = ' + str(num_chars))
print('Your input file has num_lines = ' + str(num_lines))
print('Your input file has num_words = ' + str(num_words))
#added year
print('\n The 30 most frequent words for 1Q12 are \n')

i = 1
for count, word in lst[:30]:
    print('%2s.  %4s %s' % (i, count, word))
    i += 1
#
#
#Section 2 for 1Q15
#updated text file name, took away read command for stopwords, already open from section 1
#updated print to show year


fileOpen = open('1Q15.txt', 'r').read().split()

# Open the stopwords file. This contains common words that we want to exclude when compiling our top 30. We can easily add more specific stop words to this file

# Convert text to all lowercase and remove word if in stopwords
lowerWords = [word.lower() for word in fileOpen]  # convert to lower case

filteredFile = [word for word in lowerWords if word not in stopwords]  # remove word if in stopwords
filteredFile = ' '.join(filteredFile)
# count characters
num_chars = len(filteredFile)

# count lines
num_lines = filteredFile.count('\n')


#Manually added in more stop words to file
words = filteredFile.split()  # split file into words
d = {}  # create 'd' dictionary
for w in words:  # look through file and count word
    if w in d:  # seen w before?
        d[w] += 1
    else:
        d[w] = 1

num_words = sum(d[w] for w in d)

lst = [(d[w], w) for w in d]
lst.sort()
lst.reverse()

print('Your input file has characters = ' + str(num_chars))
print('Your input file has num_lines = ' + str(num_lines))
print('Your input file has num_words = ' + str(num_words))
#added quarter in string to make sure i dont grab the wrong year
print('\n The 30 most frequent words for 1Q15 are \n')

i = 1
for count, word in lst[:30]:
    print('%2s.  %4s %s' % (i, count, word))
    i += 1

#
#
#Section 3, 4Q17 most recent available
#updated print to show year
#
#

fileOpen = open('4Q17.txt', 'r').read().split()

# Open the stopwords file. This contains common words that we want to exclude when compiling our top 30. We can easily add more specific stop words to this file

# Convert text to all lowercase and remove word if in stopwords
lowerWords = [word.lower() for word in fileOpen]  # convert to lower case

filteredFile = [word for word in lowerWords if word not in stopwords]  # remove word if in stopwords
filteredFile = ' '.join(filteredFile)
# count characters
num_chars = len(filteredFile)

# count lines
num_lines = filteredFile.count('\n')


#Manually added in more stop words to file
words = filteredFile.split()  # split file into words
d = {}  # create 'd' dictionary
for w in words:  # look through file and count word
    if w in d:  # seen w before?
        d[w] += 1
    else:
        d[w] = 1

num_words = sum(d[w] for w in d)

lst = [(d[w], w) for w in d]
lst.sort()
lst.reverse()

print('Your input file has characters = ' + str(num_chars))
print('Your input file has num_lines = ' + str(num_lines))
print('Your input file has num_words = ' + str(num_words))
#added quarter in string to make sure i dont grab the wrong year
print('\n The 30 most frequent words for 4Q17 are \n')

i = 1
for count, word in lst[:30]:
    print('%2s.  %4s %s' % (i, count, word))
    i += 1
