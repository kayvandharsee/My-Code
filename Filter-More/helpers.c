#include "helpers.h"
#include <math.h>
#include <stdlib.h>

const float COLOR_COUNT = 3.0;
// Convert image to grayscale
void grayscale(int height, int width, RGBTRIPLE image[height][width])
{
    for (int i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            // Takes the average of the rgb values and makes all the values set to that average
            int avg = round((image[i][j].rgbtBlue + image[i][j].rgbtGreen + image[i][j].rgbtRed) / COLOR_COUNT);
            image[i][j].rgbtBlue = avg;
            image[i][j].rgbtGreen = avg;
            image[i][j].rgbtRed = avg;
        }
    }
    return;
}

// Reflect image horizontally
void reflect(int height, int width, RGBTRIPLE image[height][width])
{
    // Creates a deep copy of the image so we maintain the original values when blurring
    RGBTRIPLE copy[height][width];
    for (int i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            copy[i][j] = image[i][j];
        }
    }
    // creates variables to reflect bytes and to store the middle pixel
    RGBTRIPLE temp;
    for (int k = 0; k < height; k++)
    {
        for (int l = 0; l < width; l++)
        {
            // swaps each pixel to reflect
            temp = copy[k][l];
            image[k][width - (l + 1)] = copy[k][l];
            image[k][width - (l + 1)] = temp;
        }
    }
    return;
}

// Blur image
void blur(int height, int width, RGBTRIPLE image[height][width])
{
    // Creates a deep copy of the image so we maintain the original values when blurring
    RGBTRIPLE copy[height][width];
    for (int i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            copy[i][j] = image[i][j];
        }
    }
    // Loops through every pixel and checks every pixel around it to create blur
    int avg_red, avg_green, avg_blue;
    avg_red = avg_green = avg_blue = 0;
    // Issue is with the counter variable and dividing to get the average
    float counter = 0;
    for (int k = 0; k < height; k++)
    {
        for (int l = 0; l < width; l++)
        {
            for (int m = (k - 1); m < (k + 2); m++)
            {
                for (int n = (l - 1); n < (l + 2); n++)
                {
                    // Adds up pixel values if they exist
                    if (m >= 0 && m < height && n >= 0 && n < width)
                    {
                        avg_red += copy[m][n].rgbtRed;
                        avg_blue += copy[m][n].rgbtBlue;
                        avg_green += copy[m][n].rgbtGreen;
                        counter++;
                    }
                }
            }
            // Creates the blurred pixel
            avg_red = round(avg_red / counter);
            avg_blue = round(avg_blue / counter);
            avg_green = round(avg_green / counter);
            image[k][l].rgbtRed = avg_red;
            image[k][l].rgbtBlue = avg_blue;
            image[k][l].rgbtGreen = avg_green;
            // Resets values to 0
            avg_red = avg_blue = avg_green = 0;
            counter = 0;
        }
    }
    return;
}

// Sobel Filter
int sobel(int gx, int gy)
{
    // Computes sobel alg and caps values at 255
    int sobel_num = round(sqrt((pow(gx, 2)) + (pow(gy, 2))));
    if (sobel_num > 255)
    {
        sobel_num = 255;
    }
    return sobel_num;
}
// Detect edges
void edges(int height, int width, RGBTRIPLE image[height][width])
{
    // Creates a deep copy of the image so we maintain the original values when blurring
    RGBTRIPLE copy[height][width];
    for (int i = 0; i < height; i++)
    {
        for (int j = 0; j < width; j++)
        {
            copy[i][j] = image[i][j];
        }
    }
    // Creates kernels, a counter variable, and variables to add the pixel values to
    int gx[9] = {-1, 0, 1, -2, 0, 2, -1, 0, 1};
    int gy[9] = {-1, -2, -1, 0, 0, 0, 1, 2, 1};
    int counter = 0;
    float gxr, gxg, gxb, gyr, gyg, gyb;
    gxr = gxg = gxb = gyr = gyg = gyb = 0;
    // Iterates through each pixel
    for (int k = 0; k < height; k++)
    {
        for (int l = 0; l < width; l++)
        {
            // Iterates through the pixels around the given pixel
            for (int m = (k - 1); m < (k + 2); m++)
            {
                // DOESNT WORK SINCE YOU ARE ACCESSING A NEGATIVE SEQUENCE. USE IF STATEMENT FROM BLUR TO FIX
                for (int n = (l - 1); n < (l + 2); n++)
                {
                    if (m >= 0 && m < height && n >= 0 && n < width)
                    {
                        // Computes weighted sum values
                        gxr += copy[m][n].rgbtRed * gx[counter];
                        gxg += copy[m][n].rgbtGreen * gx[counter];
                        gxb += copy[m][n].rgbtBlue * gx[counter];
                        gyr += copy[m][n].rgbtRed * gy[counter];
                        gyg += copy[m][n].rgbtGreen * gy[counter];
                        gyb += copy[m][n].rgbtBlue * gy[counter];
                        counter++;
                    }
                    else
                    {
                        counter++;
                    }

                }
            }
            // Uses the x variables to store the sobel values for each color
            gxr = sobel(gxr, gyr);
            gxg = sobel(gxg, gyg);
            gxb = sobel(gxb, gyb);
            // Uses edge detection on the picture
            image[k][l].rgbtRed = gxr;
            image[k][l].rgbtGreen = gxg;
            image[k][l].rgbtBlue = gxb;
            // Resets values to 0
            gxr = gxg = gxb = gyr = gyg = gyb = counter = 0;
        }
    }
    return;
}
