// discovery and thought process documented in Python/project_euler810.py
package main

import (
	"fmt"
	"math/bits"
)

func XorProduct(x, y int) int {
	product := 0

	exp := 1
	for exp <= x {
		product ^= (x & exp) * y
		exp <<= 1
	}

	return product
}

func XorSieve(nth, upperBound int) (int, int) {
	isXorPrime := make([]bool, upperBound)
	isXorPrime[2] = true
	isXorPrime[3] = true
	for i := 5; i < upperBound; i += 2 {
		bitCount := bits.OnesCount(uint(i))
		isXorPrime[i] = (bitCount%2 == 1)
	}

	// the index of most significant bit; right most bit is index 0
	nMSB := bits.Len(uint(upperBound)) - 1

	for i := 5; i < upperBound; i += 2 {
		if !isXorPrime[i] {
			continue
		}
		iMSB := bits.Len(uint(i)) - 1

		for j := i; j < upperBound; j += 2 {
			jMSB := bits.Len(uint(j)) - 1
			if iMSB+jMSB > nMSB {
				break
			}

			product := XorProduct(i, j)
			if product < upperBound {
				isXorPrime[product] = false
			}
		}
	}

	// 2 is a xor-prime
	nthPrime := 2
	totalCount := 1
	for i := 3; i < upperBound; i += 2 {
		if isXorPrime[i] {
			totalCount++

			if totalCount == nth {
				nthPrime = i
			}
		}
	}

	return nthPrime, totalCount
}

func main() {
	const nth int = 5e6
	const upperBound int = 1.5e8
	nthPrime, totalCount := XorSieve(nth, upperBound)

	fmt.Printf("%dth prime: %d\n", nth, nthPrime)
	fmt.Printf("Total number of xor-primes found under %d: %d\n", upperBound, totalCount)
}
