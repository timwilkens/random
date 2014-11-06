package main

import (
	"bufio"
	"errors"
	"flag"
	"fmt"
	"os"
	"strings"

	"github.com/ChimeraCoder/anaconda"
)

type Keys struct {
	consumerPublic, consumerSecret, accessPublic, accessSecret string
}

func readConfig(config string) (*Keys, error) {
	file, err := os.Open(config)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	keys := Keys{}

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		// Expecting there to not be whitespace before or after the colon for now.
		parts := strings.Split(line, ":")
		if len(parts) != 2 {
			return nil, errors.New("Malformed config line: " + line)
		}
		keyType := parts[0]
		keyValue := parts[1]
		if keyType == "ConsumerSecret" {
			keys.consumerSecret = keyValue
		} else if keyType == "ConsumerPublic" {
			keys.consumerPublic = keyValue
		} else if keyType == "AccessSecret" {
			keys.accessSecret = keyValue
		} else if keyType == "AccessPublic" {
			keys.accessPublic = keyValue
		} else {
			return nil, errors.New("Unrecognized config directive: " + keyType)
		}
	}

	if keys.consumerSecret == "" || keys.consumerPublic == "" ||
		keys.accessSecret == "" || keys.accessPublic == "" {
		return nil, errors.New("Missing key in config\nConsumerSecret, ConsumerPublic, AccessSecret, AccessPublic required.")
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return &keys, nil
}

func initializeApi(config string) (*anaconda.TwitterApi, error) {

	keys, err := readConfig(config)
	if err != nil {
		return nil, err
	}

	anaconda.SetConsumerKey(keys.consumerPublic)
	anaconda.SetConsumerSecret(keys.consumerSecret)
	api := anaconda.NewTwitterApi(keys.accessPublic, keys.accessSecret)

	return api, nil
}

func main() {

	configPtr := flag.String("config", "", "Location of config")
	tweetPtr := flag.String("tweet", "", "Tweet this message")
	flag.Parse()
	if *configPtr == "" {
		fmt.Println("Usage: --config=/path/to/config --tweet=\"Hello, World!\"")
		return
	}
	if *tweetPtr == "" {
		fmt.Println("Usage: --config=/path/to/config --tweet=\"Hello, World!\"")
		return
	}

	api, err := initializeApi(*configPtr)
	if err != nil {
		fmt.Println(err)
		return
	}
	api.PostTweet(*tweetPtr, nil)
}
