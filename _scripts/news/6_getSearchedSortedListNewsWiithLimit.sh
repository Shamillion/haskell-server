#!/bin/sh
curl -X GET "http://localhost:8080/news?search=will&sort_by=date&limit=5"
