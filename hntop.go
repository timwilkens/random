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
    Items       []Post
}

type Post struct {
    Title        string
    Url          string
    Id           int
    Points       int
    PostedAgo    string
}

func (r *Response) makeOutputLines() []string {
    output := make([]string, numberOfItems * linesPerItem + 1)
    output[0] = fmt.Sprintf("[%s]", nowString())

    for i := 1; i < numberOfItems * linesPerItem + 1; i += linesPerItem {
        output[i] = fmt.Sprintf("%s%s%s [%d]", boldOn, r.Items[i].Title, boldOff, r.Items[i].Points)
        output[i + 1] = fmt.Sprintf("  %s", r.Items[i].Url)
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

const linesPerItem = 2

// Number of lines to print initially to line up cursor.
var newLines = strings.Repeat("\n", numberOfItems * linesPerItem + 1)

// How many lines to jump back to reset cursor.
var returns = strings.Repeat("\033[F", numberOfItems * linesPerItem + 1)

// Escape sequences to turn on and off bold.
const boldOn = "\033[1m"
const boldOff = "\033[0m"

const hackerMain = "http://api.ihackernews.com/page"

func main() {

    var response Response
    var previousLines []string

    // Set cursor to end of normal output.
    fmt.Printf(newLines)

    for {
        content,err := fetch(hackerMain)
        if err != nil {
            panic(err)
        }

        err = json.Unmarshal(content, &response)
        if err != nil {
            panic(err)
        }

        // Reset cursor to print over previous.
        fmt.Printf("%s", returns)

        if len(response.Items) == 0 {
            panic("[ERROR] Json had no posts.")
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
                    spaces = strings.Repeat(" ", (len(previousLines[i]) - len(line)) + 1)
                }
            }
            fmt.Println(lines[i] + spaces)
        }
        // Store current for next round of printing.
        previousLines = lines
        time.Sleep(time.Second * 60 * 5) // Every five minutes.
    }
}
