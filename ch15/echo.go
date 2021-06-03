package main

import (
	"bufio"
	"os"
)

func echo() {
	for {
		reader := bufio.NewReader(os.Stdin)
		bytes, _ := reader.ReadBytes('\n')
		os.Stdout.Write(bytes)
	}
}

func sum(x int, y int) {
	return x + y
}

func double(x int) {
	return 2 * x
}
