#!/bin/sh
curl -X POST "http://Adam:sixthDay@localhost:8080/news/create?title=Amazing%21`
  `+The+new+news%21&category_id=3&content=I+am+sitting+in+the+morning+at+the+`
  `diner+on+the+corner.+I+am+waiting+at+the+counter+for+the+man+to+pour+the+`
  `coffee.&photo=data%3Aimage%2Fpng%3Bbase64%2CaaaExempleNeedDelete&`
  `photo=data%3Aimage%2Fpng%3Bbase64%2CccHvExempleNeedDelete&is_published=false"
