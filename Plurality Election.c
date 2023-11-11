#include <cs50.h>
#include <stdio.h>
#include <string.h>

// Max number of candidates
#define MAX 9

// Candidates have name and vote count
typedef struct
{
    string name;
    int votes;
}
candidate;

// Array of candidates
candidate candidates[MAX];

// Number of candidates
int candidate_count;

// Function prototypes
bool vote(string name);
void print_winner(void);

int main(int argc, string argv[])
{
    // Check for invalid usage
    if (argc < 2)
    {
        printf("Usage: plurality [candidate ...]\n");
        return 1;
    }

    // Populate array of candidates
    candidate_count = argc - 1;
    if (candidate_count > MAX)
    {
        printf("Maximum number of candidates is %i\n", MAX);
        return 2;
    }
    for (int i = 0; i < candidate_count; i++)
    {
        candidates[i].name = argv[i + 1];
        candidates[i].votes = 0;
    }

    int voter_count = get_int("Number of voters: ");

    // Loop over all voters
    for (int i = 0; i < voter_count; i++)
    {
        string name = get_string("Vote: ");

        // Check for invalid vote
        if (!vote(name))
        {
            printf("Invalid vote.\n");
        }
    }

    // Display winner of election
    print_winner();
}

// Update vote totals given a new vote
bool vote(string name)
{
    // Checks if vote is valid or not
    for (int i = 0; i < candidate_count; i++)
    {
        if (strcmp(candidates[i].name, name) == 0)
        {
            candidates[i].votes += 1;
            return true;
        }
    }
    return false;
}

// Print the winner (or winners) of the election
void print_winner(void)
{
    // Creates local variables to store winners and amount of winners
    candidate winners[candidate_count];
    int winner_votes = 0;
    int num_wins = 0;
    for (int i = 0; i < candidate_count; i++)
    {
        // Accounts for ties
        if (candidates[i].votes == winner_votes)
        {
            winners[num_wins] = candidates[i];
            num_wins++;
        }
        // Replaces old winners with new winners
        if (candidates[i].votes > winner_votes)
        {
            num_wins = 0;
            winner_votes = candidates[i].votes;
            winners[num_wins] = candidates[i];
            num_wins++;
        }
    }
    // Prints out winner(s)
    for (int j = 0; j < num_wins; j++)
    {
        printf("%s\n", winners[j].name);

    }
    return;
}
