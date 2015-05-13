package main

import (
    "fmt"
    "strconv"
    "time"
)

const Height = 100
const Width = 120

const Dead = ' '
const Alive = '*'

var reset string

type Line [Width]rune
type Grid [Height]Line

type NeighborStats struct {
	NumAlive int
	NumDead int
}

func NewGrid() Grid {
    var g Grid

	for i := 0; i < Height; i++ {
        var l Line
		for j := 0; j < Width; j++ {
            l[j] = Dead
        }
		g[i] = l
    }
	return g
}

func (g *Grid) String() string {
	var s string

	for i := 0; i < Height; i++ {
        s += "  |"
        for j := 0; j < Width; j++ {
            s += string(g[i][j])
        }
        s += "|\n"
    }
    return s
}

func (g *Grid) Show() {
    fmt.Print(reset)
    fmt.Print(g.String())
}

func (g *Grid) GetNeighborStats(h, w int) NeighborStats {
    var stats NeighborStats

    var left int
    var right int
    var above int
    var below int

    // Wrap around to the bottom
    if h == 0 {
        above = (Height-1)
    } else {
        above = (h-1)
    }

    // Wrap around to the top
    if h == (Height-1) {
        below = 0
    } else {
        below = (h+1)
    }

    // Wrap around to the right
    if w == 0 {
        left = (Width-1)
    } else {
        left = (w-1)
    }

    // Wrap around to the left
    if w == (Width-1) {
        right = 0
    } else {
        right = (w+1)
    }

    neighbors := [][2]int{
        [2]int{h,left},
        [2]int{h,right},
        [2]int{above,w},
        [2]int{below,w},
        [2]int{above,left},
        [2]int{above,right},
        [2]int{below,left},
        [2]int{below,right},
    }

    for _, c := range neighbors {
        if g[c[0]][c[1]] == Alive {
            stats.NumAlive++
        } else {
            stats.NumDead++
        }
    }

    return stats
}

func (g *Grid) Next() (*Grid, int) {
	next := NewGrid()

    changes := 0

    for i := 0; i < Height; i++ {
        for j := 0; j < Width; j++ {
            stats := g.GetNeighborStats(i,j)

			if g[i][j] == Dead {
				if stats.NumAlive == 3 {
                    next[i][j] = Alive
					changes++
                }
            } else {
                if stats.NumAlive < 2 {
                    next[i][j] = Dead
					changes++
                } else if stats.NumAlive < 4 {
                    next[i][j] = Alive
                } else {
                    next[i][j] = Dead
					changes++
                }
            }
        }
    }

    return &next,changes
}

func CopyGrid(g *Grid) *Grid {
    var copy Grid

    for i := 0; i < Height; i++ {
        for j := 0; j < Width; j++ {
            copy[i][j] = g[i][j]
        }
    }
    return &copy
}

func (g *Grid) Run() {
    var changes int
	initial := CopyGrid(g)

    for {
        g.Show()
        g,changes = g.Next()
		time.Sleep(time.Millisecond*65)
		if changes == 0 {
            g = initial
        }
    }
}

func Blinker() Grid {
	g := NewGrid()

    h := Height/2
    w := Width/2

    g[h-1][w] = Alive
    g[h][w] = Alive
    g[h+1][w] = Alive

	return g
}

func Glider() Grid {
    g := NewGrid()

    h := Height/2
    w := Width/2

    g[h][w] = Alive
    g[h+1][w] = Alive
    g[h+2][w] = Alive
    g[h+2][w-1] = Alive
    g[h+1][w-2] = Alive

    return g
}

func RPentimino() Grid {
    g := NewGrid()

    h := Height/2
    w := Width/2

    g[h][w] = Alive
    g[h][w+1] = Alive
    g[h+1][w] = Alive
    g[h+1][w-1] = Alive
    g[h+2][w] = Alive

    return g
}

func Acorn() Grid {
    g := NewGrid()

    h := Height/2
    w := Width/2

    g[h][w] = Alive
    g[h+1][w+1] = Alive
    g[h+1][w+2] = Alive
    g[h+1][w+3] = Alive

    g[h+1][w-2] = Alive
    g[h+1][w-3] = Alive

    g[h-1][w-2] = Alive

    return g
}

func StraightLine() Grid {
    g := NewGrid()

    h := Height/2

    for i := 0; i < Width; i++ {
        g[h][i] = Alive
    }

    return g
}

func TwoLines() Grid {
    g := NewGrid()

    h := Height/2

    for i := 0; i < Width; i++ {
        g[h-1][i] = Alive
        g[h+1][i] = Alive
    }

    return g
}


func Cross() Grid {
    g := NewGrid()

    h := Height/2
    w := Width/2

    for i := 0; i < Width; i++ {
        g[h][i] = Alive
    }

    for i := 0; i < Height; i++ {
        g[i][w] = Alive
    }

    return g
}

func Hershcel() Grid {
    g := NewGrid()

    h := Height/2
    w := Width/2

    g[h][w] = Alive

    g[h-1][w-1] = Alive
    g[h][w-1] = Alive
    g[h+1][w-1] = Alive

    g[h][w+1] = Alive
    g[h][w+2] = Alive
    g[h][w+3] = Alive

    return g
}

func init_reset() {
    reset = "\033["
    reset += strconv.Itoa(Height)
    reset += "A"
}

func main() {
    init_reset()
	b := RPentimino()
	b.Run()
}
