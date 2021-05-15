package main

import (
	"bufio"
	"os"
)

func main() {
	for {
		reader := bufio.NewReader(os.Stdin)
		bytes, _ := reader.ReadBytes('\n')
		os.Stdout.Write(bytes)
	}
}
