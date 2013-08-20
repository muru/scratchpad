package main

import "fmt"
import "os"
import "magic"
import "math/rand"

func test(a []int, i int) {
	a[rand.Intn(len(a))] = i
	fmt.Println(a)
}

func square(i, sum int) bool {
	m := magic.MagicSquare{N: i, Sum: sum, Square: make([]int, i*i)}
	if m.CreateSquare() {
		fmt.Println(i, sum)
		m.PrintSquare()
	}
	return false
}

func main() {
	var n = 0
	done := make(chan int, 31)
	for i, j := 31, 31*(31*31 + 1)/2; j < 100000; j++ {
		go func (i, j int) {
			square(i, j)
			done <- j
		} (i, j);
		n++
	}
	for i, j := 0, 0; i < n; i++ {
		j = <- done
		fmt.Fprintln(os.Stderr, j)
	}
}
