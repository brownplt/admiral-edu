#/bin/bash
file="$(pwd)/submission-times-$1.csv"
cmd="SELECT user_id, last_modified "
cmd="$cmd FROM submission"
cmd="$cmd WHERE assignment_id='$1'"
cmd="$cmd AND user_id NOT LIKE 'default%'"
cmd="$cmd ORDER BY last_modified ASC"
cmd="$cmd INTO OUTFILE '$file'"
cmd="$cmd FIELDS TERMINATED BY ','"
cmd="$cmd ENCLOSED BY \"'\""
cmd="$cmd LINES TERMINATED BY '\\n'"

rm $file -f
mysql -u root --database="captain_teach" --execute="$cmd"
echo "user_id, last_modified\n" | cat - $file > temp && mv temp $file


