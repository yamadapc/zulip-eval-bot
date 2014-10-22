package main

import (
	"fmt"
	"github.com/zond/gosafe"
	"io/ioutil"
	"os"
)

func main() {
	c := gosafe.NewCompiler()
	c.Allow("compress")
	c.Allow("crypto")
	c.Allow("encoding")
	c.Allow("fmt")
	c.Allow("hash")
	c.Allow("math")
	c.Allow("path")
	c.Allow("regexp")
	c.Allow("strings")
	c.Allow("sort")
	c.Allow("text")
	c.Allow("time")
	c.Allow("unicode")

	cmd, err := c.Run(os.Args[1])

	if err != nil {
		fmt.Println(err)
		return
	}

	b, _ := ioutil.ReadAll(cmd.Stdout)
	fmt.Print(string(b))
}
