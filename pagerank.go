package main

import (
    "fmt"
    "log"
    "math"
)

type Matrix []Vector
type Vector []float64

func dotProduct(v1 Vector, v2 Vector) float64 {
    if len(v1) != len(v2) {
        log.Fatal("Mismatched vector lengths in dotProduct")
    }

    product := 0.0
    for i,item1 := range v1 {
        product += (item1 * v2[i])
    }
    return product
}

func add(v1 Vector, v2 Vector) Vector {
    if len(v1) != len(v2) {
        log.Fatal("Mismatched vector lengths in add")
    }

    var result Vector
    for i,item1 := range v1 {
        result = append(result, (item1 + v2[i]))
    }
    return result
}

func vecDiff(v1 Vector, v2 Vector) float64 {
    if len(v1) != len(v2) {
        log.Fatal("Mismatched vector lengths in add")
    }

    change := 0.0
    for i,item1 := range v1 {
        change += math.Abs(item1 - v2[i])
    }
    return change
}

func multVec(m Matrix, v Vector) Vector {

    if len(v) != len(m[0]) {
        log.Fatal("Mistmatched vector and matrix")
    }

    var result Vector

    for _,vec := range m {
        result = append(result, dotProduct(vec, v))
    }
    return result
}

func multScalar(m Matrix, s float64) Matrix {

    for i := 0; i < len(m); i++ {
        for j := 0; j < len(m[i]); j++ {
            m[i][j] = (m[i][j] * s)
        }
    }
    return m
}

func main() {

    M := []Vector{
      Vector{0,1.0/2.0,0,0},
      Vector{1.0/3.0,0,0,1.0/2.0},
      Vector{1.0/3.0,0,1,1.0/2.0},
      Vector{1.0/3.0,1.0/2.0,0,0},
    }

    if len(M) != len(M[0]) {
        log.Fatal("Must be square matrix")
    }

    for i := 0; i < len(M); i++ {
        columnVal := 0.0
        for j := 0; j < len(M); j++ {
            columnVal += M[j][i]
        }
        if columnVal != 1.0 {
            log.Fatal("Must be stochastic")
        }
    }

    n := float64(len(M[0]))
    var v Vector
    for i := 0.0; i < n; i++ {
        v = append(v, 1.0/n)
    }
    beta := 0.8

    var inverseBeta Vector
    for i := 0.0; i < n; i++ {
        inverseBeta = append(inverseBeta, (1 - beta) * (1/n))
    }

    M = multScalar(M, beta)

    var change float64
    epsilon := 0.001
    maxIterations := 70
    iter := 0

    for iter < maxIterations {
        mTimesV := multVec(M,v)
        vPrime := add(mTimesV, inverseBeta)
        fmt.Println(vPrime)
        change = vecDiff(v,vPrime)
        v = vPrime
        if change < epsilon {
            break
        }

        iter++
    }

    fmt.Println(v)
}
