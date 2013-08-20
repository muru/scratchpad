package magic

import "fmt"

/*var (
	m.N, m.Sum int = 4, 34
	n2 = m.N*m.N
	m.Square = make([]int, n2)
	m.A, m.D int
)*/

type MagicSquare struct {
	N, Sum int
	Square []int
	n2, A, D int
	numbers []int
}

func (m *MagicSquare) CheckSeries () bool {
	nn2_1 := m.N*(m.N*m.N - 1)
	sum2 := m.Sum * 2
	n2 := m.N*2
	for dn := nn2_1; dn < sum2; dn = dn + nn2_1 {
		if (sum2 - dn) % n2 == 0 {
			m.A = (sum2 - dn) / n2
			m.D = dn / nn2_1
	 		//fmt.Println(m.A, m.D)
			return true
		}
	}
	// fmt.Println(m.N, m.Sum, m.A, m.D)
	return false
}

func (m *MagicSquare) GenSeries() bool {
	if m.CheckSeries() {
		var i int
		m.numbers = make([]int, m.n2)
		for i = 0; i < m.n2; i++ {
			m.numbers[i] = m.A + i*m.D
		}
		return true
	}
	return false
}

func (m *MagicSquare) CreateSquare() bool {
	m.n2 = m.N*m.N

	if ! m.GenSeries() {
		return false	// Try backtracking
	}

	var row, col int = 0, m.N/2
	var tmprow, tmpcol int

	switch {
	case (m.N % 2 == 1):
		for _, num := range m.numbers {
			m.Square[row*m.N + col] = num

			tmprow = (m.N + row - 1) % m.N
			tmpcol = (col + 1) % m.N

			if (m.Square[tmprow*m.N + tmpcol] != 0) {
				row = (row + 1) % m.N
			} else {
				row = tmprow
				col = tmpcol
			}
		}
	case (m.N % 4 == 0):
		ts := make([]bool, m.N)
		notReversed := make([]bool, m.n2)
		for i, _:= range ts {
			ts[i] = (i % 2) == 0
		}
		for i := int(0); i < m.N; i++ {
			for j := int(0); j < m.N; j++ {
				index := i*m.N + j
				switch {
				case (i < m.N/2) && (j < m.N/2) :										// Top-left corner
					if (i % 2 == 1) { notReversed[index] = ts[j] } else { notReversed[index] = !ts[j] }
				case (i >= m.N/2) && (j < m.N/2) :									// Bottom-left corner
					if (i % 2 == 1) { notReversed[index] = !ts[j] } else { notReversed[index] = ts[j] }
				case (i < m.N/2) && (j >= m.N/2) :									// Top-right corner
					if (i % 2 == 1) { notReversed[index] = !ts[j - m.N/2] } else { notReversed[index] = ts[j - m.N/2] }
				case (i >= m.N/2) && (j >= m.N/2) :									// Bottom-right corner
					if (i % 2 == 1) { notReversed[index] = ts[j - m.N/2] } else { notReversed[index] = !ts[j - m.N/2] }
				}
			}
			for index, val := range m.numbers {
				if notReversed[index] {
					m.Square[index] = val
				} else {
					m.Square[index] = m.numbers[m.n2 - index - 1]
				}
			}
		}
	}
	return true
}

func (m *MagicSquare) PrintSquare () {
	//fmt.Println(m.N, m.Square)
	if m.N == 0 {
		return
	}
	for i, j:= 0, m.N; j <= m.n2; i, j = i + m.N, j + m.N {
		fmt.Println(m.Square[i:j])
	}
}

