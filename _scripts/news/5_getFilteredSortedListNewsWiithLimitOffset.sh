#!/bin/sh
curl -X GET "http://localhost:8080/news?author=Sam&sort_by=category&`
                                                    `limit=5&offset=1"
