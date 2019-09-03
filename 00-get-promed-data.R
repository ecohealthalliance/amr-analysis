library(ssh)
library(mongolite)
library(sys)
library(glue)
library(dplyr)

# Set up an SSH tunnel to the server.  Note this needs a relevant git in ~/.ssh/grits
# and the ssh library installed
str <- "session <- ssh::ssh_connect('ubuntu@grits.eha.io',  keyfile = '~/.ssh/grits');ssh::ssh_tunnel(session, port=27018, target='localhost:27018')"
R <- R.home('bin/R')
pid <- sys::exec_background(R, c("-e", str), std_out = FALSE)

# Wait for connection to be established
Sys.sleep(2)

# Connect to the database:
con <- mongo(collection = "posts", db="promed", url = "mongodb://localhost:27018", verbose = TRUE)

# Testing the connection
# con$run('{"listCollections":1}')
# con$count()

# Set start and end dates
startDate <- "2008-01-01T00:00:00Z"
endDate <- "2018-01-01T00:00:00Z"

# Run a query on the database.
# The "$match" section filters to the dates above
# "$unwind" is like "unnest" - it splits up records into repeated rows
# of sub-records according to the fields specified
# Then "$group" specifies the grouping and summary, in this case adding
# one per record
out <- con$aggregate(glue('
      [{
        "$match": {
           "$and": [
             {
              "promedDate": { "$gte": { "$date": "{{startDate}}" } }
             },
             {
              "promedDate": { "$lte": { "$date": "{{endDate}}" } }
             }
          ]
        }
      },  
      { "$unwind": "$articles" },
      { "$unwind": "$articles.geoannotations" },
      { "$unwind": "$articles.geoannotations.geoname" },
      {
        "$group": {
          "_id": "$articles.geoannotations.geoname.country_code",
          "mentions": { "$sum": 1 }
        }
      }]', .open ="{{", .close = "}}"))


# End the SSH tunnel
tools::pskill(pid)
out <- dplyr::rename(out, country_code=`_id`)
# Export teh data
readr::write_csv(out, "data/promed_mentions.csv")
