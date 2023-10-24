#include <cs50.h>
#include <stdio.h>

int main(void)
{
    // This line will get the card number
    long card = get_long("Number: ");
    // This section will store the card number to find the type of card later
    long card_type = card;
    // This block will get the necessary digits
    int counter = 0;
    long sum = 0;
    long sum_mult = 0;
    int temp_mult = 0;
    while (card != 0)
    {
        // This statement will add all the non multiplied numbers
        if (counter % 2 == 0)
        {
            sum += (card % 10);
            card = (card / 10);
        }
        // This statement will add all the multiplied numbers
        if (counter % 2 == 1)
        {
            temp_mult = ((card % 10) * 2);
            // This statement will get the digits of the multiple if needed
            if (temp_mult >= 10)
            {
                while (temp_mult != 0)
                {
                    sum_mult += (temp_mult % 10);
                    temp_mult = (temp_mult / 10);
                }
            }
            else
            {
                sum_mult += temp_mult;
            }
            card = (card / 10);
        }
        counter++;
    }
    // This statement will find and print out the type of card
    char valid = 'n';
    if ((sum + sum_mult) % 10 == 0)
    {
        // This loop will get the first two digits of the card
        do
        {
            card_type = card_type / 10;
        }
        while (card_type > 99);
        // This statement is for amex
        if ((card_type == 34 || card_type == 37) && (counter == 15))
        {
            printf("AMEX\n");
            valid = 'y';
        }
        // This statement is for mastercard
        if (card_type >= 51 && card_type <= 55 && counter == 16)
        {
            printf("MASTERCARD\n");
            valid = 'y';
        }
        // This statement is for visa
        if (card_type >= 40 && card_type <= 49 && (counter == 13 || counter == 16))
        {
            printf("VISA\n");
            valid = 'y';
        }
        if (valid == 'n')
        {
            printf("INVALID\n");
        }
    }
    // This statement will occur if the card is invalid
    else
    {
        printf("INVALID\n");
    }
}

