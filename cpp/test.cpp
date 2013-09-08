#include <iostream>
#include <cstring>
#include <bitset>
//#include <asm/bitops.h>
using namespace std;
typedef unsigned long long ul;
const ul ul1 = ul(1);
int find_first_unset(ul i, int max_pos)
{	
	unsigned long long position = 1;
	if ((~i == 0) || (i >= (ul1 << max_pos - 1)))
		return -1;
	else while(1 == ffsll(i))
	{
		i = i >> 1;
		position++;
	}
	return position;
}

int main()
{
	unsigned long long i = 0x2FFFF;
	short position = 1;
//	cout << find_first_zero_bit(&i, sizeof(i)) << endl;
	unsigned long long *buf = &i;
	bitset<64> b(i);
	cout << b << endl;
	cout << find_first_unset(i, 19) << endl;
	if (~i == 0)
		cout << -1 << endl;
	else while(1 == ffsll(i))
	{
		i = i >> 1;
		position += 1;
	}
	cout << (position > 8*sizeof(i) ? -1 : position) << endl;
	return 0;
}
