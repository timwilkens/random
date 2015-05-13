package main

import "fmt"
import "math/rand"
import "time"

var numBuckets = 20
type Buckets []int

func randBall() int {
	return rand.Intn(3)
}

func RandBuckets() Buckets {
	b := make(Buckets, numBuckets)
	for i := 0; i < numBuckets; i++ {
		b[i] = randBall()
	}
	return b
}

func (b Buckets) Show() {
	s := " "
	for _, bucket := range b {
		switch bucket {
		case 0:
			s += "RED "
		case 1:
			s += "GREEN "
		case 2:
			s += "BLUE "
		}
	}
	fmt.Println(s)
}

func (b *Buckets) Swap(m,n int) {
		(*b)[n], (*b)[m] = (*b)[m], (*b)[n]
}

func (b Buckets) RunRobot() {
	b.Show()

	lastRed := 0
	firstBlue := (numBuckets-1)

	i := 0

	for i < firstBlue {
		switch b[i] {
		case 0:
			b.Swap(i, lastRed)
			lastRed++
			i++
		case 1:
			i++
		case 2:
			b.Swap(i, firstBlue)
			firstBlue--
		}
	}
	b.Show()
}

func init() {
	rand.Seed( time.Now().UTC().UnixNano())
}

func main() {
	b := RandBuckets()
	b.RunRobot()
}
