//Only first solution atm
package main

import  (
	"fmt"
	"io/ioutil"
	"strings"
	"strconv"

)

func check (e error) {
	if e != nil {
		panic (e)
	}
}

func sliceAtoi(sa []string) ([]int){
	si := make([]int, 0, len(sa))
	for _, a := range sa {
		i, err := strconv.Atoi(a)
		if err != nil {
			return si
		}
		si = append(si, i)
	}
	return si
}

func findFrequency(sa []int)(int){
	cur := 0
	for _, a := range sa {
		cur = a + cur
	}
	return cur
}

func main () {
	b, err := ioutil.ReadFile("input")
	check(err)
	str := string(b)
	splits := strings.Split(str, "\n")
	fmt.Println(findFrequency(sliceAtoi(splits)))
	
}
