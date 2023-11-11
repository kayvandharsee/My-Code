#include <cs50.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

int count_letters(string text);

int count_words(string text);

int count_sentences(string text);

void reading_level(int letters, int words, int sentences);

int main(void)
{
    // Gets text from user and prints it
    string text = get_string("Text: ");
    int letters = count_letters(text);
    int words = count_words(text);
    int sentences = count_sentences(text);
    reading_level(letters, words, sentences);
}

int count_sentences(string text)
{
    // Finds the amount of sentences
    int scount = 0;
    for (int i = 0, length = strlen(text); i < length; i++)
    {
        if (text[i] == '.' || text[i] == '?' || text[i] == '!')
        {
            scount += 1;
        }
    }
    // Returns the amount of sentences
    return scount;
}

int count_words(string text)
{
    // Finds the amount of words by tracking the spaces
    int wcount = 1;
    for (int i = 0, length = strlen(text); i < length; i++)
    {
        if (text[i] == 32)
        {
            wcount += 1;
        }
    }
    // Returns the amount of words
    return wcount;
}

int count_letters(string text)
{
    // Finds the amount of letters in the text
    int lcount = 0;
    for (int i = 0, length = strlen(text); i < length; i++)
    {
        if (isupper(text[i]) || islower(text[i]))
        {
            lcount += 1;
        }
    }
    // Returns the amount of letters
    return lcount;
}

void reading_level(int letters, int words, int sentences)
{
    // Finds per 100 words number
    float word_avg = (float) words / 100.0;
    // Finds L and S for formula
    float L = letters / word_avg;
    float S = sentences / word_avg;
    // Computes formula
    float index = (0.0588 * L - 0.296 * S - 15.8);
    // Rounds results
    int grade = round(index);
    // Prints reading grade level
    if (grade < 1)
    {
        printf("Before Grade 1\n");
    }
    if (grade > 15)
    {
        printf("Grade 16+\n");
    }
    if (grade > 1 && grade < 15)
    {
        printf("Grade %i\n", grade);
    }
}
