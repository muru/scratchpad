#include <iostream>
typedef unsigned long long ul;
using namespace std;

class MagicSquare
{
	ul n, n2, sum;
	ul* square;
	void create_truth_square(bool tsquare[]);
	void create_square_numbers(ul numbers[]);
	bool create_series_square();
	bool find_sequence();
public:
	ul a, d;
	MagicSquare(ul n = 4, ul sum = 34)
	{
		setValues(4, 34);
	}
	~MagicSquare() 
	{
		delete[] square;
	}

	void setValues(ul n = 4, ul sum = 34)
	{
		this->n = n;
		this->sum = sum;
		square = new ul[n2];
		n2 = n*n;
	}

	bool check_series();
	bool create_square();

	friend ostream & operator << (ostream & out, const MagicSquare & square);
};

void MagicSquare::create_truth_square(bool square[])
{
	int i, j;
	bool  base[n/2];
	for (i = 0; i < (n / 2); i++)
		base[i] = i % 2;
	for (i = 0; i < n; i++)
	{
		for (j = 0; j < n; j++)
		{
			if ((i < n/2) && (j < n/2))										// Top-left corner
				square[i*n + j] = (i%2) ? base[j] : !base[j];
			if ((i >= n/2) && (j < n/2))									// Bottom-left corner
				square[i*n + j] = (i%2) ? !base[j] : base[j];
			if ((i < n/2) && (j >= n/2))									// Top-right corner
				square[i*n + j] = (i%2) ? !base[j - n/2] : base[j - n/2];
			if ((i >= n/2) && (j >= n/2))									// Bottom-right corner
				square[i*n + j] = (i%2) ? base[j - n/2] : !base[j - n/2];
		}
	}
	return;
}

void MagicSquare::create_square_numbers(ul numbers[])
{
	if (!numbers)
		return;
	long long i, row, col;
	if (n % 2)
	{
	// Siamese method
		for (i = 0, row = 0, col = n / 2; i < n2; i++)
		{
			square[row*n + col] = numbers[i];
//			cout << row << " " << col << " " << numbers[i] << endl;
			if (square[((n + row - 1) % n) * n + (col + 1) % n])		// Next position is already filled
				row = (row + 1) % n;									// Drop a row.
			else
			{
				row = (n + row - 1) % n;
				col = (col + 1) % n;
			}
		}
	}
	else if (!(n % 4))
	{
		// Truth table method.
		bool tsquare[n*n];
		create_truth_square(tsquare);
		for (i = 0, row = 0, col = 0; i < n2; i++)
		{
			square[i] = tsquare[i] ? numbers[i] : numbers[n2 - i - 1];
		}
	}
	else
	{
		// Strachey method.
	}
}

bool MagicSquare::check_series()
{
	if ((2*sum) % n)
		return false;
	ul n2_1 = n*(n2 - 1), temp = 2*sum, d_n2_1 = 0;
	for (d_n2_1 = n2_1; d_n2_1 < (temp - 2); d_n2_1 += n2_1)
	{
		if ((temp - d_n2_1) % (2 * n) == 0)
		{
			a = (temp - d_n2_1) / (2 * n);
			d = d_n2_1 / n2_1;
			return true;
		}
	}
	return false;
}

bool MagicSquare::create_series_square()
{
	if (!a || !d)
		return false;
	ul numbers[n2];
	for (int i = 0; i < n2; i++)
		numbers[i] = a + i*d;
	create_square_numbers(numbers);
	return true;
}

bool MagicSquare::find_sequence()
{
	cout << "Yet to be implemented..." << endl;
	return false;
}

bool MagicSquare::create_square()
{
	if (check_series())
	{
//		cout << "An arithmetic series can be used." << endl;
		return create_series_square();
	}
//	cout << "No suitable arithmetic series was found. Switching to backtracking..." << endl;
	return false;
}	

ostream & operator << (ostream & out, const MagicSquare & square)
{
	ul n = square.n, n2 = n * n;
	for (int i = 0; i < n2; i++)
	{
		if (!(i % n))
			out << endl;
		out << square.square[i] << '\t';
	}
	return out;
}

int main()
{
	ul n = 7, s = 175;
//	cin >> n >> s;
	MagicSquare test(n, s);
	for (s = 175; s < 300; s++)
	{
		test.setValues(n, s);
//		cout << test.check_series() << endl;
//		cout << test.a << " " << test.d << endl;
		if (test.create_square())
			cout << n << " " << s << test << endl;
	}
	return 0;
}
