#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "wav.h"

int check_format(WAVHEADER header);
int get_block_size(WAVHEADER header);
const int ITERATE = 4;

int main(int argc, char *argv[])
{
    // Ensure proper usage
    // TODO #1
    if (argc != 3)
    {
        printf("Usage: ./reverse input.wav output.wav\n");
        return 1;
    }

    // Open input file for reading
    // TODO #2
    FILE *input = (fopen(argv[1], "r"));
    if (input == NULL)
    {
        printf("Could not open file.\n");
        return 1;
    }

    // Read header
    // TODO #3
    WAVHEADER header;
    // Iterates to store values in arrays
    for (int i = 0; i < ITERATE; i++)
    {
        fread(&header.chunkID[i], sizeof(BYTE), 1, input);
    }
    fread(&header.chunkSize, sizeof(DWORD), 1, input);
    for (int j = 0; j < ITERATE; j++)
    {
        fread(&header.format[j], sizeof(BYTE), 1, input);
    }
    for (int k = 0; k < ITERATE; k++)
    {
        fread(&header.subchunk1ID[k], sizeof(BYTE), 1, input);
    }
    // Stores values in correct variables
    fread(&header.subchunk1Size, sizeof(DWORD), 1, input);
    fread(&header.audioFormat, sizeof(WORD), 1, input);
    fread(&header.numChannels, sizeof(WORD), 1, input);
    fread(&header.sampleRate, sizeof(DWORD), 1, input);
    fread(&header.byteRate, sizeof(DWORD), 1, input);
    fread(&header.blockAlign, sizeof(WORD), 1, input);
    fread(&header.bitsPerSample, sizeof(WORD), 1, input);
    for (int l = 0; l < ITERATE; l++)
    {
        fread(&header.subchunk2ID[l], sizeof(BYTE), 1, input);
    }
    fread(&header.subchunk2Size, sizeof(DWORD), 1, input);

    // Use check_format to ensure WAV format
    // TODO #4
    if (!check_format(header))
    {
        printf("Input is not a WAV file.\n");
        return 1;
    }

    // Open output file for writing
    // TODO #5
    FILE *output = fopen(argv[2], "w");
    if (output == NULL)
    {
        printf("Could not open file.\n");
        return 1;
    }

    // Write header to file
    // TODO #6
    // Iterates to store values in arrays
    for (int i = 0; i < ITERATE; i++)
    {
        fwrite(&header.chunkID[i], sizeof(BYTE), 1, output);
    }
    fwrite(&header.chunkSize, sizeof(DWORD), 1, output);
    for (int j = 0; j < ITERATE; j++)
    {
        fwrite(&header.format[j], sizeof(BYTE), 1, output);
    }
    for (int k = 0; k < ITERATE; k++)
    {
        fwrite(&header.subchunk1ID[k], sizeof(BYTE), 1, output);
    }
    // Stores values in correct variables
    fwrite(&header.subchunk1Size, sizeof(DWORD), 1, output);
    fwrite(&header.audioFormat, sizeof(WORD), 1, output);
    fwrite(&header.numChannels, sizeof(WORD), 1, output);
    fwrite(&header.sampleRate, sizeof(DWORD), 1, output);
    fwrite(&header.byteRate, sizeof(DWORD), 1, output);
    fwrite(&header.blockAlign, sizeof(WORD), 1, output);
    fwrite(&header.bitsPerSample, sizeof(WORD), 1, output);
    for (int l = 0; l < ITERATE; l++)
    {
        fwrite(&header.subchunk2ID[l], sizeof(BYTE), 1, output);
    }
    fwrite(&header.subchunk2Size, sizeof(DWORD), 1, output);

    // Use get_block_size to calculate size of block
    // TODO #7
    int block_size;
    block_size = get_block_size(header);

    // Write reversed audio to file
    // TODO #8
    long *buffer = malloc(block_size);
    long header_end = ftell(input);
    // IF ERROR, TRY IMPLEMENTING AN IF STATEMENT WITH THE DO WHILE ARG BEFORE DO WHILE LOOP
    fseek(input, 0, SEEK_END);
    if (ftell(input) >= header_end)
    {
        fseek(input, (block_size * -1), SEEK_CUR);
        do
        {
            fread(buffer, block_size, 1, input);
            fwrite(buffer, block_size, 1, output);
            fseek(input, (block_size * -2), SEEK_CUR);
        }
        while (ftell(input) >= header_end);
    }
    fclose(input);
    fclose(output);
    free(buffer);
}

int check_format(WAVHEADER header)
{
    // TODO #4
    char wave[4] = {'W', 'A', 'V', 'E'};
    for (int i = 0; i < ITERATE; i++)
    {
        if (header.format[i] != wave[i])
        {
            return 0;
        }
    }
    return 1;
}

int get_block_size(WAVHEADER header)
{
    // TODO #7
    int block_size;
    block_size = header.numChannels * (header.bitsPerSample / 8);
    return block_size;
}
