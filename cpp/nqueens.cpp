#include<iostream>
#include<bitset>
#include<cstring>
#include<cstdlib>
#include<string>
#include<sys/types.h>
#include<unistd.h>
#include<sys/wait.h>
using namespace std;
typedef unsigned long long ul;
const ul ul1 = ul(1);

ul* copy_board(int n, ul* board)
{
	if (!n || !board)
		return NULL;
	ul *copy = new ul[n];
	memcpy(copy, board, n*sizeof(ul));
	return copy;
}

void print_board(const int n, ul* board, string label = "", ostream& out = cout)
{
	if (label != "")
		out << label << endl;
	
 	int i, j, col;
	for (i = 0; i < n; i++)
	{
		col = n - ffsll(board[i]);
		for (j = 0; j < col; j++)
			out << ". ";
		out << "# ";
		for (j = col + 1; j < n; j++)
			out << ". ";

		out << endl;
	}

}

int find_first_unset(ul i, int max_pos)
{	
	ul position = 1;
	if ((~i == 0) || (((ul1 << max_pos) - 1) <= i))
		return -1;
	else while(1 == ffsll(i))
	{
		i = i >> 1;
		position++;
	}
	return position - 1;
}

bool set_marks(const short n, ul *markboard, const int row, const int col)
{
	if (!n || !markboard || (row > n) || (col > n))
		return false;
	int i;

	ul col_mark = ul1 << col;
	markboard[row] = (ul1 << n) - 1;

	for (i = row; i < n; i++)
		markboard[i] |= col_mark;

	for (i = 1; i < (n - row); i++)
	{
		markboard[row + i] |= (((col + i) < n) ? (ul1 << (col + i)) : 0) |
							  (((col - i) >= 0) ? (ul1 << (col - i)) : 0);
		if (markboard[row + i] == markboard[row])
			return false;
	}
	
	return true;
}

bool fill_board(int n, ul* board, ul *markboard, const int row = 0)
{
	if (!board || !markboard || !n)
		return false;
	bool successful = false;
	int col = find_first_unset(markboard[row], n);

	if (row + 1 == n)
		successful = (col != -1);
	else while (col != -1)
	{
		markboard[row] |= ul1 << col;
		ul* cpmarkboard = copy_board(n, markboard);
		if (set_marks(n, cpmarkboard, row, col))
			successful = fill_board(n, board, cpmarkboard, row + 1);
		delete[] cpmarkboard;
		if (successful)
			break;
		col = find_first_unset(markboard[row], n);
	}

	if (successful)
	{
		cerr << "fill_board: " << row + 1 << " " << col + 1 << endl;
		board[row] = ul1 << col;
	}
	return successful;
}
		
int main(int argc, char* argv[])
{
	short n;
	if (argc > 1)
		n = atoi(argv[1]);
	else
	{
		cout << "Enter N, where N is at most " << sizeof(ul)*8 - 1 << "." << endl;
		cin >> n;
	}
	ul *board = new ul[n];
	ul *markboard = new ul[n];
	ul columns = 0;
	for (int i = 0, n_childs = 0; i < n; i++)
	{
		markboard[0] |= 1 << i;
		if (n_childs > 5)
		{
			wait(&columns);
			n_childs--;
		}
		if (fork() == 0)
		{
			cout << "Forking for first row, column " << i + 1 << "." << endl;
			if (fill_board(n, board, markboard))
				print_board(n, board, "", cerr);
			cout << "First row, column " << i + 1 << " done." << endl;
			break;
		}
		else
			n_childs++;
		columns = 1;
	}
	if (columns) for (int i = 0, j; i < n; i++)
	{
		wait(&j);
	}
			
	return 0;
}
