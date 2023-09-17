package main

import "fmt"

func SRecursive(num, numSum int) bool {
	if num == numSum {
		return true
	}
	if num <= 0 || num < numSum {
		return false
	}

	for mod := 10; mod < num; mod *= 10 {
		if numSum < num%mod {
			break
		}
		if SRecursive(num/mod, numSum-num%mod) {
			return true
		}
	}

	return false
}

func S(sqrt int) bool {
	// https://projecteuler.net/thread=719#359366
	//
	// digitsum(i^2) === digitsum(i) (mod 9)
	// i**2 === i (mod 9)
	// ==> i === 0 or 1 (mod 9)
	if !(sqrt%9 == 0 || sqrt%9 == 1) {
		return false
	}
	return SRecursive(sqrt*sqrt, sqrt)
}

func main() {
	const N int = 1e6

	sum := 0
	for i := 4; i <= N; i++ {
		if S(i) {
			// fmt.Println(i, i*i)
			sum += i * i
		}
	}
	fmt.Println(sum)
}
