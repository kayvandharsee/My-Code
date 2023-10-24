// Implements a dictionary's functionality

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "dictionary.h"

// Represents a node in a hash table
typedef struct node
{
    char word[LENGTH + 1];
    struct node *next;
} node;

// TODO: Choose number of buckets in hash table
const unsigned int N = 5850;
// Controls how many of the first letters are used in hash function
const int FIRST = 5;

// Hash table
node *table[N];

// Returns true if word is in dictionary, else false
bool check(const char *word)
{
    // TODO
    unsigned int index = hash(word);
    node *exists = table[index];
    // Checks if there is anything in the bucket
    if (exists != NULL)
    {
        // Compares words case insensitively
        while (strcasecmp(exists->word, word) != 0)
        {
            exists = exists->next;
            if (exists == NULL)
            {
                return false;
            }
        }
    }
    else
    {
        return false;
    }
    return true;
}

// Hashes word to a number
unsigned int hash(const char *word)
{
    // TODO: Improve this hash function
    int length = strlen(word);
    int total = 0;
    // Sets the amount of letters to be converted to ASCII value
    if (length > FIRST)
    {
        length = FIRST;
    }
    // Converts letters to ASCII value with exception for apostraphe
    for (int i = 0; i < length; i++)
    {
        if (word[i] != '\'')
        {
            total += (toupper(word[i]) - 'A');
        }
        else
        {
            total += 26;
        }
    }
    return (total * strlen(word));
}

// Loads dictionary into memory, returning true if successful, else false
bool load(const char *dictionary)
// BUG FOUND: for some reason when you are reading the dictionary, all it includes is the dictionaries name with no spaces or
// newlines (ex. dictionarylarge) this is why no words are being added to dictionary. figure out how to properly read the
// dictionary. need to update variable k to the actual length and not just the length of the title of dictionary. Try using fseek
// and ftell from reverse.c
{
    // TODO
    // Sets all buckets to NULL to avoid garbage values
    for (int j = 0; j < N; j++)
    {
        table[j] = NULL;
    }
    // Opens dictionary
    FILE *file = fopen(dictionary, "r");
    if (file == NULL)
    {
        return false;
    }
    // Finds the length of the dictionary since every char is one byte
    fseek(file, 0, SEEK_END);
    long k = ftell(file);
    fseek(file, 0, SEEK_SET);
    // Sets variables to be used
    int counter = 0;
    char c = '\0';
    // Allocates memory for linked lists
    node *new = malloc(sizeof(node));
    if (new == NULL)
    {
        return false;
    }
    node *list = NULL;
    // Copies dictionary words down into memory
    for (int i = 0; i < k; i++)
    {
        fscanf(file, "%c", &c);
        if (isalpha(c) || c == '\'')
        {
            new->word[counter] = c;
            counter++;
        }
        // Resets and stores word every time there is a new line
        else if (c == '\n')
        {
            new->word[counter] = '\0';
            counter = 0;
            unsigned int bucket = hash(new->word);
            list = table[bucket];
            table[bucket] = new;
            new->next = list;
            new = malloc(sizeof(node));
            if (new == NULL)
            {
                return false;
            }
        }
    }
    // Frees memory
    fclose(file);
    free(new);
    return true;
}

// Returns number of words in dictionary if loaded, else 0 if not yet loaded
unsigned int size(void)
{
    // TODO
    int counter = 0;
    node *ptr = NULL;
    for (int i = 0; i < N; i++)
    {
        ptr = table[i];
        while (ptr != NULL)
        {
            counter++;
            ptr = ptr->next;
        }
    }
    return counter;
}

// Unloads dictionary from memory, returning true if successful, else false
bool unload(void)
{
    // TODO
    node *ptr = NULL;
    node *next = NULL;
    for (int i = 0; i < N; i++)
    {
        ptr = table[i];
        while (ptr != NULL)
        {
            next = ptr->next;
            free(ptr);
            ptr = next;
        }
    }
    return true;
}
