package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
	"time"
)

func fetch(url string) ([]byte, error) {
	response, err := http.Get(url)
	if err != nil {
		return []byte{}, errors.New(fmt.Sprintf("Fetch had an error getting '%s': %s\n", url, err))
	}
	defer response.Body.Close()
	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return []byte{}, errors.New(fmt.Sprintf("Fetch could not read body: %s\n", err))
	}
	return body, nil
}

type Response struct {
	Items []Post
}

type Post struct {
	Title          string
	Url            string
	Id             int
	Points         int
	PostedAgo      string
	PositionChange string
}

func truncateString(s string, length int) string {
	if len(s) > length {
		s = s[:maxCharsPerLine-3]
		s += "..."
	}
	return s
}

func (r *Response) makeOutputLines() []string {
	output := make([]string, (numberOfItems*linesPerItem)+1)
	output[0] = fmt.Sprintf("[%s]", nowString())

	j := 1
	for i := 1; i <= numberOfItems; i += 1 {
		title := truncateString(r.Items[i].Title, maxCharsPerLine-7)
		output[j] = fmt.Sprintf("%s %s%s%s [%d]", r.Items[i].PositionChange, boldOn, title, boldOff, r.Items[i].Points)
		commentLine := fmt.Sprintf("  https://news.ycombinator.com/item?id=%d", r.Items[i].Id)
		output[j+1] = truncateString(commentLine, maxCharsPerLine)
		linkLine := fmt.Sprintf("  %s", r.Items[i].Url)
		output[j+2] = truncateString(linkLine, maxCharsPerLine)
		j += linesPerItem
	}
	return output
}

func nowString() string {
	t, err := time.Parse("2006-01-02 15:04:05 -0700 MST", time.Now().String())
	if err != nil {
		fmt.Println("parse error", err.Error())
	}
	return t.Format(time.ANSIC)
}

// The number of posts to print.
const numberOfItems = 15

const linesPerItem = 3

const maxCharsPerLine = 90

// Number of lines to print initially to line up cursor.
var newLines = strings.Repeat("\n", numberOfItems*linesPerItem+1)

// How many lines to jump back to reset cursor.
var returns = strings.Repeat("\033[F", numberOfItems*linesPerItem+1)

// Escape sequences to turn on and off bold.
const boldOn = "\033[1m"
const boldOff = "\033[0m"

const upArrow = `▲`
const downArrow = `▼`
const neutral = `—`
const newPost = `★`

const hackerMain = "http://api.ihackernews.com/page"

func main() {

	var response Response
	var previousLines []string
	rankings := make(map[int]int)

	content, err := fetch(hackerMain)
	if err != nil {
		panic(fmt.Sprintf("[ERROR] content fetch failed: %s", err))
	}

	err = json.Unmarshal(content, &response)
	if err != nil {
		panic(fmt.Sprintf("[ERROR] bad json: %s", err))
	}

	// Set cursor to end of normal output.
	fmt.Printf(newLines)

	for {
		content, err := fetch(hackerMain)
		if err != nil {
			panic(fmt.Sprintf("[ERROR] content fetch failed: %s", err))
		}

		err = json.Unmarshal(content, &response)
		if err != nil {
			panic(fmt.Sprintf("[ERROR] bad json: %s", err))
		}

		// Reset cursor to print over previous.
		fmt.Printf("%s", returns)

		if len(response.Items) == 0 {
			panic("[ERROR] Json had no posts.")
		}

		for i, post := range response.Items {
			if rank, ok := rankings[post.Id]; ok {
				if rank > i {
					response.Items[i].PositionChange = downArrow
				} else if rank < i {
					response.Items[i].PositionChange = upArrow
				} else {
					response.Items[i].PositionChange = neutral
				}
			} else {
				response.Items[i].PositionChange = newPost
			}
			rankings[post.Id] = i
		}

		lines := response.makeOutputLines()
		if len(lines) == 0 {
			panic("[ERROR] No lines found.")
		}

		for i, line := range lines {
			var spaces string
			if previousLines != nil {
				if len(previousLines[i]) >= len(line) {
					// Add spaces to wipe out previous line if it was longer.
					spaces = strings.Repeat(" ", (len(previousLines[i])-len(line))+1)
				}
			}
			fmt.Println(lines[i] + spaces)
		}
		// Store current for next round of printing.
		previousLines = lines
		time.Sleep(time.Second * 60 * 5) // Every five minutes.

		content, err = fetch(hackerMain)
		if err != nil {
			panic(fmt.Sprintf("[ERROR] content fetch failed: %s", err))
		}

		err = json.Unmarshal(content, &response)
		if err != nil {
			panic(fmt.Sprintf("[ERROR] bad json: %s", err))
		}
	}
}
