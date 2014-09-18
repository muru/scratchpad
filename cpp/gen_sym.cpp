#include <iostream>

using namespace std;

int main()
{
	int i, j, n;
	cin >> n;
	bool square[n*n], base[n/2];
	for (i = 0; i < (n / 2); i++)
		base[i] = i % 2;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < n; j++)
		{
			if ((i < n/2) && (j < n/2))
				square[i*n + j] = (i%2) ? base[j] : !base[j];
			if ((i >= n/2) && (j < n/2))
				square[i*n + j] = (i%2) ? !base[j] : base[j];
			if ((i < n/2) && (j >= n/2))
				square[i*n + j] = (i%2) ? !base[j - n/2] : base[j - n/2];
			if ((i >= n/2) && (j >= n/2))
				square[i*n + j] = (i%2) ? base[j - n/2] : !base[j - n/2];
			cout << square[n*i + j] << " ";
		}
		cout << endl;
	}
	return 0;
}
