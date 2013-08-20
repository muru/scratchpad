#include <iostream>
#include <cstdio>
#include "mygrep.h"

int main(int argc, char* argv[])
{
	int i = 0;
	string pattern = argv[1];
	if (argc != 3)
	{
		cout << "Usage: " << argv[0] << " [PATTERN] [FILE]" << endl;
		return -1;
	}
	FILE * fin = fopen(argv[2], "r");
	char buffer[1024];
	while (fgets(buffer, 1023, fin))
	{
		if (find(string(buffer),pattern) != -1) 
		{
			cout << buffer;
		}
	}
	fclose(fin);

	return 0;
}
